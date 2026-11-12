# ==============================================================================
# PAR RUE — PLANO DE AÇÃO REGIONAL DE ATENÇÃO ÀS URGÊNCIAS E EMERGÊNCIAS
# Macrorregião Noroeste do Paraná — perfil de morbimortalidade
#
# Refaz, a partir dos microdados do SIM e do SIH/SUS, as tabelas e os gráficos
# do capítulo "O perfil de morbimortalidade da Macrorregião Noroeste" do PAR
# (dados da SCVGE, tabulados em 06/11/26), e compara os resultados com os
# valores publicados (dados_apoio/referencia_par_2025.csv).
#
# Antes de rodar:
#   Rscript baixar_populacao.R     # população por município e sexo (Tabnet)
#   Rscript baixar_datasus.R       # SIM 2015-2025 e SIH 2025 → ~/PAR_RUE_dados
# Uso:
#   Rscript analise_par.R
# Saídas (só dados agregados):
#   saidas/tabelas/par_noroeste_<ano>.xlsx   — uma aba por quadro/tabela do PAR
#   saidas/graficos/*.png                    — gráficos 1, 2, 3, 4 e 8
#   saidas/conferencia_par.csv               — valor recalculado × valor do PAR
#   saidas/resultados_longos.csv             — todos os indicadores em formato longo
# ==============================================================================

# Nomes de colunas com acento exigem sessão em UTF-8
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(stringr)
})

# ==============================================================================
# BLOCO 0 — PARÂMETROS EDITÁVEIS
# ==============================================================================
MACRO         <- "MACRORREGIONAL NOROESTE"   # nome em dados_apoio/municipios_pr_regionais.csv
ROTULO_MACRO  <- "Macrorregião Noroeste"
ANOS_SERIE    <- 2015:2025                   # série histórica do SIM
ANO_REF       <- 2025                        # ano dos quadros transversais
ANOS_COVID    <- 2020:2025
DATA_TABULACAO <- "06/11/26"                 # só para o rodapé dos gráficos

# Internações (SIH/SUS): só AIH tipo 1 (normal); as do tipo 5 (longa
# permanência) são continuação de uma internação já contada.
SIH_SO_AIH_TIPO1 <- TRUE

# --- Grupos de causas (CID-10, 3 ou 4 caracteres, sem ponto) -------------------
# Óbitos: causa básica (CAUSABAS). Internações: diagnóstico principal (DIAG_PRINC).
# [Definição a confirmar com a SCVGE] — o PAR não descreve todos os recortes;
# o Quadro 4 cita "CID I63/I64" para a mortalidade por AVC.
CID <- list(
  isquemicas          = sprintf("I%02d", 20:25),
  iam                 = "I21",
  avc_mortalidade     = c("I63", "I64"),
  avc_internacao      = "I64",
  outras_cerebrovasc  = setdiff(sprintf("I%02d", 60:69), c("I63", "I64")),
  outras_isquemicas   = setdiff(sprintf("I%02d", 20:25), "I21"),
  covid               = c("B342", "U071", "U072", "U109"),
  pulmao              = "C34",
  transporte          = c(sprintf("V%02d", 1:99)),
  quedas              = sprintf("W%02d", 0:19),
  agressoes           = c(sprintf("X%02d", 85:99), sprintf("Y%02d", 0:9)),
  autoprovocadas      = sprintf("X%02d", 60:84),
  tabagismo           = c("F17", "Z720")       # procurado nas linhas A–D e II da DO
)

# Grupos de acidente de transporte (Tabela 11/12 do PAR)
GRUPOS_TRANSPORTE <- tribble(
  ~ini, ~fim, ~grupo,
   1,   9, "V01-V09 Pedestre traumatizado",
  10,  19, "V10-V19 Ciclista traumatizado",
  20,  29, "V20-V29 Motociclista traumatizado",
  30,  39, "V30-V39 Ocupante de triciclo motorizado",
  40,  49, "V40-V49 Ocupante de automóvel",
  50,  59, "V50-V59 Ocupante de caminhonete",
  60,  69, "V60-V69 Ocupante de veículo de transporte pesado",
  70,  79, "V70-V79 Ocupante de ônibus",
  80,  89, "V80-V89 Outros acidentes de transporte terrestre",
  90,  94, "V90-V94 Acidentes de transporte por água",
  95,  97, "V95-V97 Acidentes de transporte aéreo e espacial",
  98,  99, "V98-V99 Outros e não especificados"
)

# Grandes grupos da CBO 2002 (1º dígito do código da ocupação)
GRANDES_GRUPOS_CBO <- c(
  "0" = "Membros das forças armadas, policiais e bombeiros militares",
  "1" = "Membros superiores do poder público, dirigentes e gerentes",
  "2" = "Profissionais das ciências e das artes",
  "3" = "Técnicos de nível médio",
  "4" = "Trabalhadores de serviços administrativos",
  "5" = "Trabalhadores dos serviços, vendedores do comércio em lojas e mercados",
  "6" = "Trabalhadores agropecuários, florestais e da pesca",
  "7" = "Trabalhadores da produção de bens e serviços industriais",
  "8" = "Trabalhadores da produção de bens e serviços industriais (processos contínuos)",
  "9" = "Trabalhadores em serviços de reparação e manutenção"
)

# ==============================================================================
# BLOCO 1 — CAMINHOS E LEITURA
# ==============================================================================
DIR_PROJETO <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg))) else getwd()
})
DIR_DADOS <- Sys.getenv("PAR_RUE_DADOS", path.expand("~/PAR_RUE_dados"))
DIR_SAIDA <- file.path(DIR_PROJETO, "saidas")
dir.create(file.path(DIR_SAIDA, "tabelas"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_SAIDA, "graficos"), recursive = TRUE, showWarnings = FALSE)

municipios <- read.csv(file.path(DIR_PROJETO, "dados_apoio", "municipios_pr_regionais.csv"),
                       colClasses = c(codigo_ibge_6 = "character"), encoding = "UTF-8") %>%
  filter(macrorregiao == MACRO) %>%
  mutate(rs = paste0(as.integer(substr(regional, 1, 2)), "ª RS")) %>%
  select(codigo_ibge_6, municipio, rs, regional)
if (!nrow(municipios)) stop("Macrorregião não encontrada: ", MACRO)
NIVEIS_RS <- sort(unique(municipios$rs))
NIVEIS_RECORTE <- c("Macro", NIVEIS_RS)

# --- População (por município e sexo; ano mais próximo se faltar o ano) -------
DIR_POP <- file.path(DIR_PROJETO, "dados_apoio", "populacao")
anos_pop <- as.integer(str_extract(list.files(DIR_POP, "^populacao_pr_sexo_\\d{4}\\.csv$"), "\\d{4}"))
if (!length(anos_pop)) stop("Sem população em ", DIR_POP, " — rode baixar_populacao.R")
ler_pop <- function(ano) {
  a <- anos_pop[which.min(abs(anos_pop - ano))]
  if (a != ano) message("População de ", ano, " não encontrada; usando ", a)
  read.csv(file.path(DIR_POP, sprintf("populacao_pr_sexo_%d.csv", a)),
           colClasses = c(codigo_ibge_6 = "character")) %>%
    inner_join(municipios, by = "codigo_ibge_6") %>%
    mutate(ano = ano)
}
pop_mun <- bind_rows(lapply(union(ANOS_SERIE, ANO_REF), ler_pop))

# população por recorte (Macro + cada RS) e por sexo
pop_recorte <- bind_rows(
  pop_mun %>% group_by(ano) %>% summarise(populacao = sum(populacao), .groups = "drop") %>% mutate(recorte = "Macro"),
  pop_mun %>% group_by(ano, recorte = rs) %>% summarise(populacao = sum(populacao), .groups = "drop")
)
pop_sexo <- pop_mun %>% group_by(ano, sexo) %>% summarise(populacao = sum(populacao), .groups = "drop")

# --- SIM ------------------------------------------------------------------------
ler_rds <- function(prefixo, ano) {
  f <- file.path(DIR_DADOS, sprintf("%s_%d.rds", prefixo, ano))
  if (!file.exists(f)) { message("Faltando: ", f); return(NULL) }
  readRDS(f) %>% mutate(ano = ano)
}
sim <- bind_rows(lapply(ANOS_SERIE, function(a) ler_rds("sim", a)))
if (!nrow(sim)) stop("Nenhuma base do SIM em ", DIR_DADOS, " — rode baixar_datasus.R")

# idade em anos a partir do código do SIM (1º dígito = unidade; 4 = anos, 5 = 100+)
idade_sim <- function(x) {
  u <- substr(x, 1, 1); v <- suppressWarnings(as.integer(substr(x, 2, 3)))
  case_when(u %in% c("0", "1", "2", "3") ~ 0L, u == "4" ~ v, u == "5" ~ 100L + v, TRUE ~ NA_integer_)
}

sim <- sim %>%
  mutate(codigo_ibge_6 = substr(CODMUNRES, 1, 6)) %>%
  inner_join(municipios, by = "codigo_ibge_6") %>%
  mutate(
    cid    = toupper(gsub("[^A-Za-z0-9]", "", CAUSABAS)),
    cid3   = substr(cid, 1, 3),
    sexo   = case_when(SEXO %in% c("1", "M") ~ "Masculino", SEXO %in% c("2", "F") ~ "Feminino",
                       TRUE ~ "Ignorado"),
    idade  = idade_sim(IDADE),
    ocup   = coalesce(gsub("[^0-9]", "", OCUP), ""),
    trab   = ACIDTRAB %in% "1",
    linhas = toupper(paste(LINHAA, LINHAB, LINHAC, LINHAD, LINHAII))
  )
anos_sim <- sort(unique(sim$ano))
message("SIM: ", nrow(sim), " óbitos de residentes da macro em ", paste(range(anos_sim), collapse = "–"))

# --- SIH ------------------------------------------------------------------------
sih <- ler_rds("sih", ANO_REF)
if (!is.null(sih)) {
  sih <- sih %>%
    mutate(codigo_ibge_6 = substr(MUNIC_RES, 1, 6)) %>%
    inner_join(municipios, by = "codigo_ibge_6") %>%
    mutate(cid = toupper(gsub("[^A-Za-z0-9]", "", DIAG_PRINC)), cid3 = substr(cid, 1, 3))
  if (SIH_SO_AIH_TIPO1) sih <- filter(sih, IDENT %in% "1")
  message("SIH ", ANO_REF, ": ", nrow(sih), " internações de residentes da macro")
}

# ==============================================================================
# BLOCO 2 — FUNÇÕES AUXILIARES
# ==============================================================================
CAPITULOS <- tribble(
  ~cap,    ~ini,  ~fim,  ~rotulo,
  "I",     "A00", "B99", "I. Algumas doenças infecciosas e parasitárias",
  "II",    "C00", "D48", "II. Neoplasias (tumores)",
  "III",   "D50", "D89", "III. Doenças sangue órgãos hemat e transt imunitár",
  "IV",    "E00", "E90", "IV. Doenças endócrinas nutricionais e metabólicas",
  "V",     "F00", "F99", "V. Transtornos mentais e comportamentais",
  "VI",    "G00", "G99", "VI. Doenças do sistema nervoso",
  "VII",   "H00", "H59", "VII. Doenças do olho e anexos",
  "VIII",  "H60", "H95", "VIII. Doenças do ouvido e da apófise mastóide",
  "IX",    "I00", "I99", "IX. Doenças do aparelho circulatório",
  "X",     "J00", "J99", "X. Doenças do aparelho respiratório",
  "XI",    "K00", "K93", "XI. Doenças do aparelho digestivo",
  "XII",   "L00", "L99", "XII. Doenças da pele e do tecido subcutâneo",
  "XIII",  "M00", "M99", "XIII. Doenças sist osteomuscular e tec conjuntivo",
  "XIV",   "N00", "N99", "XIV. Doenças do aparelho geniturinário",
  "XV",    "O00", "O99", "XV. Gravidez parto e puerpério",
  "XVI",   "P00", "P96", "XVI. Algumas afec originadas no período perinatal",
  "XVII",  "Q00", "Q99", "XVII. Malf cong deformid e anomalias cromossômicas",
  "XVIII", "R00", "R99", "XVIII. Sint sinais e achad anorm ex clín e laborat",
  "XIX",   "S00", "T98", "XIX. Lesões enven e alg out conseq causas externas",
  "XX",    "V01", "Y98", "XX. Causas externas de morbidade e mortalidade",
  "XXI",   "Z00", "Z99", "XXI. Contatos com serviços de saúde",
  "XXII",  "U00", "U99", "XXII. Códigos para propósitos especiais"
)
capitulo_de <- function(cid3) {
  out <- rep(NA_character_, length(cid3))
  for (i in seq_len(nrow(CAPITULOS))) {
    out[is.na(out) & cid3 >= CAPITULOS$ini[i] & cid3 <= CAPITULOS$fim[i]] <- CAPITULOS$cap[i]
  }
  out
}
sim$cap <- capitulo_de(sim$cid3)

em <- function(cid, grupo) substr(cid, 1, 3) %in% grupo | substr(cid, 1, 4) %in% grupo

faixa_etaria <- function(idade) {
  cut(idade, c(-Inf, 0, 4, 9, 14, 19, 29, 39, 49, 59, 69, 79, Inf),
      labels = c("<01a", "01-04a", "05-09a", "10-14a", "15-19a", "20-29a", "30-39a",
                 "40-49a", "50-59a", "60-69a", "70-79a", "80 e+"))
}

# Conta eventos por recorte (Macro + RS) e ano, junta a população e calcula a taxa
contar_recorte <- function(df, por = character(), mult = 1e5) {
  bind_rows(
    df %>% count(ano, across(all_of(por)), name = "n") %>% mutate(recorte = "Macro"),
    df %>% count(ano, recorte = rs, across(all_of(por)), name = "n")
  ) %>%
    left_join(pop_recorte, by = c("ano", "recorte")) %>%
    mutate(taxa = n / populacao * mult, recorte = factor(recorte, NIVEIS_RECORTE))
}

# Grade completa recorte × ano (zeros onde não houve evento)
completar <- function(tab, anos, mult = 1e5) {
  tab %>% mutate(recorte = as.character(recorte)) %>%
    right_join(expand_grid(recorte = NIVEIS_RECORTE, ano = anos), by = c("recorte", "ano")) %>%
    select(-populacao) %>% left_join(pop_recorte, by = c("ano", "recorte")) %>%
    mutate(n = coalesce(n, 0L), taxa = n / populacao * mult,
           recorte = factor(recorte, NIVEIS_RECORTE)) %>%
    arrange(recorte, ano)
}

# Resultados em formato longo, para a conferência com o PAR
resultados <- list()
registrar <- function(tabela, indicador, recorte, ano, valor) {
  resultados[[length(resultados) + 1]] <<- tibble(
    tabela = tabela, indicador = indicador, recorte = as.character(recorte),
    ano = as.integer(ano), valor = as.numeric(valor))
}

tabelas <- list()   # abas do Excel
r2 <- function(x) round(x, 2)

# ==============================================================================
# BLOCO 3 — TABELAS DO PAR
# ==============================================================================
sim_ref <- filter(sim, ano == ANO_REF)

# --- Gráfico 1: taxa e proporção por capítulo, série -------------------------
g1 <- sim %>% filter(!is.na(cap)) %>% count(ano, cap, name = "n") %>%
  group_by(ano) %>% mutate(proporcao = 100 * n / sum(n)) %>% ungroup() %>%
  left_join(filter(pop_recorte, recorte == "Macro") %>% select(ano, populacao), by = "ano") %>%
  mutate(taxa = n / populacao * 1e5) %>%
  left_join(select(CAPITULOS, cap, rotulo), by = "cap")
with(g1, registrar("Gráfico 1", paste0("taxa_cap_", cap), "Macro", ano, taxa))
tabelas[["G1 capitulos serie"]] <- g1 %>%
  transmute(Ano = ano, Causa = rotulo, Obitos = n, `Taxa/100 mil` = r2(taxa), `Proporção (%)` = r2(proporcao))

# --- Quadro 2: óbitos e taxas por capítulo × RS, ano de referência ------------
q2 <- sim_ref %>% filter(!is.na(cap)) %>% contar_recorte(por = "cap") %>%
  mutate(recorte = as.character(recorte)) %>%
  complete(ano, cap = CAPITULOS$cap, recorte = NIVEIS_RECORTE, fill = list(n = 0L)) %>%
  select(-populacao) %>% left_join(pop_recorte, by = c("ano", "recorte")) %>%
  mutate(taxa = n / populacao * 1e5, recorte = factor(recorte, NIVEIS_RECORTE))
with(filter(q2, recorte == "Macro"), registrar("Quadro 2", paste0("obitos_cap_", cap), recorte, ano, n))
with(q2, registrar("Quadro 2", paste0("taxa_cap_", cap), recorte, ano, taxa))
tot <- contar_recorte(sim_ref)
with(filter(tot, recorte == "Macro"), {
  registrar("Quadro 2", "obitos_total", recorte, ano, n)
  registrar("Quadro 2", "taxa_total", recorte, ano, taxa)
})
tabelas[["Q2 capitulos RS"]] <- q2 %>%
  select(cap, recorte, n, taxa) %>%
  pivot_wider(names_from = recorte, values_from = c(n, taxa), values_fill = 0) %>%
  left_join(select(CAPITULOS, cap, rotulo), by = "cap") %>%
  transmute(`Causa (Cap CID10)` = rotulo, `Macro nº óbitos` = n_Macro,
            across(starts_with("taxa_"), r2)) %>%
  arrange(desc(`Macro nº óbitos`)) %>%
  rename_with(~ sub("^taxa_", "Taxa ", .x))

# --- Tabela 3: por sexo -----------------------------------------------------------
t3 <- sim_ref %>% filter(!is.na(cap), sexo != "Ignorado") %>% count(ano, sexo, cap, name = "n") %>%
  left_join(pop_sexo, by = c("ano", "sexo")) %>% mutate(taxa = n / populacao * 1e5)
with(t3, {
  registrar("Tabela 3", paste0("obitos_cap_", cap), sexo, ano, n)
  registrar("Tabela 3", paste0("taxa_cap_", cap), sexo, ano, taxa)
})
with(filter(pop_sexo, ano == ANO_REF), registrar("Tabela 3", "populacao", sexo, ano, populacao))
tabelas[["T3 capitulos sexo"]] <- t3 %>% select(cap, sexo, n, taxa) %>%
  pivot_wider(names_from = sexo, values_from = c(n, taxa), values_fill = 0) %>%
  left_join(select(CAPITULOS, cap, rotulo), by = "cap") %>%
  mutate(across(starts_with("taxa_"), r2), `Razão M/F` = r2(taxa_Masculino / taxa_Feminino)) %>%
  arrange(desc(n_Masculino + n_Feminino)) %>% select(rotulo, everything(), -cap)

# --- Gráfico 2 e Tabelas 4/5: IAM, AVC e outras cardiovasculares ---------------
serie_grupo <- function(grupo) completar(sim %>% filter(em(cid, grupo)) %>% contar_recorte(), anos_sim)
iam <- serie_grupo(CID$iam); avc <- serie_grupo(CID$avc_mortalidade)
outras_cv <- serie_grupo(CID$outras_cerebrovasc); outras_isq <- serie_grupo(CID$outras_isquemicas)
with(iam, { registrar("Tabela 4", "mort_iam_taxa", recorte, ano, taxa); registrar("Gráfico 2", "mort_iam_taxa", recorte, ano, taxa) })
with(avc, { registrar("Tabela 5", "mort_avc_taxa", recorte, ano, taxa); registrar("Gráfico 2", "mort_avc_taxa", recorte, ano, taxa) })
with(outras_cv,  registrar("Gráfico 2", "mort_outras_cerebrovasc_taxa", recorte, ano, taxa))
with(outras_isq, registrar("Gráfico 2", "mort_outras_isquemicas_taxa", recorte, ano, taxa))
largo_taxa <- function(df) df %>% select(recorte, ano, taxa) %>% mutate(taxa = r2(taxa)) %>%
  pivot_wider(names_from = ano, values_from = taxa) %>% arrange(recorte)
tabelas[["T4 IAM serie"]] <- largo_taxa(iam)
tabelas[["T5 AVC serie"]] <- largo_taxa(avc)
g2 <- bind_rows(
  mutate(filter(iam, recorte == "Macro"), grupo = "IAM"),
  mutate(filter(avc, recorte == "Macro"), grupo = "AVC"),
  mutate(filter(outras_cv, recorte == "Macro"), grupo = "Outras doenças cerebrovasculares"),
  mutate(filter(outras_isq, recorte == "Macro"), grupo = "Outras doenças isquêmicas")
)
tabelas[["G2 cardiovasc serie"]] <- g2 %>% select(grupo, ano, taxa) %>% mutate(taxa = r2(taxa)) %>%
  pivot_wider(names_from = ano, values_from = taxa)

# --- Quadros 3 e 4: internação (SIH) × mortalidade, ano de referência ---------
if (!is.null(sih)) {
  sih_ref <- mutate(sih, ano = ANO_REF)
  int <- function(grupo) completar(sih_ref %>% filter(em(cid, grupo)) %>% contar_recorte(mult = 1e4),
                                   ANO_REF, mult = 1e4)
  i_isq <- int(CID$isquemicas); i_iam <- int(CID$iam); i_avc <- int(CID$avc_internacao)
  m_iam <- filter(iam, ano == ANO_REF); m_avc <- filter(avc, ano == ANO_REF)
  for (x in list(list("Quadro 3", "int_isquemicas", i_isq), list("Quadro 3", "int_iam", i_iam),
                 list("Quadro 4", "int_avc", i_avc))) {
    with(x[[3]], { registrar(x[[1]], paste0(x[[2]], "_n"), recorte, ano, n)
                   registrar(x[[1]], paste0(x[[2]], "_taxa"), recorte, ano, taxa) })
  }
  with(m_iam, registrar("Quadro 3", "mort_iam_taxa", recorte, ano, taxa))
  with(m_avc, registrar("Quadro 4", "mort_avc_taxa", recorte, ano, taxa))
  with(filter(pop_recorte, ano == ANO_REF), registrar("Quadro 3", "populacao", recorte, ano, populacao))
  tabelas[["Q3 IAM internacao"]] <- tibble(
    `Região de saúde` = i_isq$recorte, `População` = i_isq$populacao,
    `Isquêmicas I20-I25 (n)` = i_isq$n, `Isquêmicas taxa/10 mil` = r2(i_isq$taxa),
    `IAM I21 (n)` = i_iam$n, `IAM taxa/10 mil` = r2(i_iam$taxa),
    `Mortalidade IAM /100 mil` = r2(m_iam$taxa[match(i_isq$recorte, m_iam$recorte)]))
  tabelas[["Q4 AVC internacao"]] <- tibble(
    `Região de saúde` = i_avc$recorte, `População` = i_avc$populacao,
    `AVC internações (n)` = i_avc$n, `AVC taxa/10 mil` = r2(i_avc$taxa),
    `Mortalidade AVC /100 mil` = r2(m_avc$taxa[match(i_avc$recorte, m_avc$recorte)]))
}

# --- Tabela 6: neoplasias por CID 3 caracteres --------------------------------
por_cid3 <- function(df, cap_sel, tab, top = 15) {
  pop_m <- pop_recorte$populacao[pop_recorte$ano == ANO_REF & pop_recorte$recorte == "Macro"]
  out <- df %>% filter(cap %in% cap_sel) %>% count(cid3, name = "n", sort = TRUE) %>%
    mutate(taxa = n / pop_m * 1e5) %>% slice_head(n = top)
  registrar(tab, paste0("obitos_", out$cid3), "Macro", ANO_REF, out$n)
  registrar(tab, paste0("taxa_", out$cid3), "Macro", ANO_REF, out$taxa)
  transmute(out, `Causa (CID10 3D)` = cid3, `Óbitos` = n, `Taxa/100 mil` = r2(taxa))
}
neo_ref <- filter(sim_ref, cap == "II", substr(cid3, 1, 1) == "C")   # neoplasias malignas
tabelas[["T6 neoplasias"]] <- por_cid3(neo_ref, "II", "Tabela 6")

# --- Tabela 7 e 8: câncer de pulmão por RS, sexo, ocupação e tabagismo --------
pulmao <- filter(sim_ref, em(cid, CID$pulmao))
t7 <- bind_rows(
  pulmao %>% count(recorte = rs, sexo),
  pulmao %>% count(sexo) %>% mutate(recorte = "Macro")
) %>% pivot_wider(names_from = sexo, values_from = n, values_fill = 0)
for (s in c("Masculino", "Feminino")) if (!s %in% names(t7)) t7[[s]] <- 0L
t7 <- t7 %>% mutate(Total = rowSums(across(any_of(c("Masculino", "Feminino", "Ignorado")))),
                    recorte = factor(recorte, NIVEIS_RECORTE)) %>% arrange(recorte)
with(t7, { registrar("Tabela 7", "obitos_C34_masc", recorte, ANO_REF, Masculino)
           registrar("Tabela 7", "obitos_C34_fem", recorte, ANO_REF, Feminino)
           registrar("Tabela 7", "obitos_C34_total", recorte, ANO_REF, Total) })
tabelas[["T7 pulmao RS sexo"]] <- t7

gg <- substr(pulmao$ocup, 1, 1)
pulmao$grande_grupo <- ifelse(nchar(pulmao$ocup) == 6 & !startsWith(pulmao$ocup, "99") & gg %in% names(GRANDES_GRUPOS_CBO),
                              unname(GRANDES_GRUPOS_CBO[gg]), "Sem ocupação, aposentado, dona de casa ou ignorada")
tabelas[["T8 pulmao ocupacao"]] <- pulmao %>% count(`Grande grupo (CBO)` = grande_grupo, sexo) %>%
  pivot_wider(names_from = sexo, values_from = n, values_fill = 0) %>%
  mutate(`Óbitos` = rowSums(across(any_of(c("Masculino", "Feminino", "Ignorado")))),
         `% do total` = r2(100 * `Óbitos` / sum(`Óbitos`))) %>% arrange(desc(`Óbitos`))
# códigos de ocupação mais frequentes (para conferir a classificação acima)
tabelas[["T8 pulmao codigos CBO"]] <- pulmao %>% count(`Código OCUP` = ifelse(ocup == "", "(vazio)", ocup),
                                                       `Grande grupo` = grande_grupo, sort = TRUE)
fumo <- mean(grepl(paste(CID$tabagismo, collapse = "|"), gsub("[^A-Z0-9]", "", pulmao$linhas)))
registrar("Texto", "pulmao_pct_tabagismo_linhas", "Macro", ANO_REF, 100 * fumo)

# --- Tabela 9: respiratórias por CID 3 caracteres -----------------------------
tabelas[["T9 respiratorias"]] <- por_cid3(sim_ref, "X", "Tabela 9")

# --- Tabela 10: Covid-19 --------------------------------------------------------
covid <- completar(sim %>% filter(ano %in% ANOS_COVID, em(cid, CID$covid)) %>% contar_recorte(),
                   intersect(ANOS_COVID, anos_sim))
with(covid, registrar("Tabela 10", "mort_covid_taxa", recorte, ano, taxa))
tabelas[["T10 covid"]] <- largo_taxa(covid)

# --- Quadro 5 e Gráfico 8: causas externas -----------------------------------
q5 <- completar(sim_ref %>% filter(cap == "XX") %>% contar_recorte(), ANO_REF)
with(q5, { registrar("Quadro 5", "obitos_cap_XX", recorte, ano, n)
           registrar("Quadro 5", "taxa_cap_XX", recorte, ano, taxa) })
tabelas[["Q5 causas externas RS"]] <- q5 %>% transmute(`Região de saúde` = recorte, `População` = populacao,
                                                       `Óbitos cap. XX` = n, `Taxa/100 mil` = r2(taxa))

g8 <- bind_rows(lapply(c("transporte", "quedas", "agressoes", "autoprovocadas"), function(g) {
  completar(sim %>% filter(em(cid, CID[[g]])) %>% contar_recorte(), anos_sim) %>%
    filter(recorte == "Macro") %>% mutate(grupo = g)
}))
with(g8, registrar("Gráfico 8", paste0("taxa_", grupo), recorte, ano, taxa))
ROTULO_EXT <- c(transporte = "Acidentes de transporte", quedas = "Quedas",
                agressoes = "Agressões", autoprovocadas = "Lesões autoprovocadas")
tabelas[["G8 causas externas serie"]] <- g8 %>% transmute(grupo = ROTULO_EXT[grupo], ano, n, taxa = r2(taxa)) %>%
  pivot_wider(names_from = ano, values_from = c(n, taxa))

# --- Tabelas 11–15: transporte e quedas -----------------------------------------
serie_por <- function(df, col, tab, ind) {
  tot <- df %>% count(ano, name = "n") %>% right_join(tibble(ano = anos_sim), by = "ano") %>%
    mutate(n = coalesce(n, 0L))
  registrar(tab, ind, "Macro", tot$ano, tot$n)
  df %>% count(.data[[col]], ano) %>% pivot_wider(names_from = ano, values_from = n, values_fill = 0) %>%
    mutate(Total = rowSums(across(-1))) %>% arrange(desc(Total))
}
grupo_transporte <- function(cid3) {
  k <- suppressWarnings(as.integer(substr(cid3, 2, 3)))
  GRUPOS_TRANSPORTE$grupo[findInterval(k, GRUPOS_TRANSPORTE$ini)]
}
transp <- sim %>% filter(em(cid, CID$transporte)) %>% mutate(grupo = grupo_transporte(cid3))
tabelas[["T11 transporte"]]          <- serie_por(transp, "grupo", "Tabela 11", "obitos_transporte")
tabelas[["T12 transporte trabalho"]] <- serie_por(filter(transp, trab), "grupo", "Tabela 12", "obitos_transporte_trabalho")
quedas <- sim %>% filter(em(cid, CID$quedas)) %>% mutate(faixa = faixa_etaria(idade))
tabelas[["T13 quedas"]]          <- serie_por(quedas, "cid3", "Tabela 13", "obitos_quedas")
tabelas[["T14 quedas faixa"]]    <- quedas %>% count(cid3, faixa) %>%
  pivot_wider(names_from = faixa, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(-1))) %>% arrange(desc(Total))
tabelas[["T15 quedas trabalho"]] <- serie_por(filter(quedas, trab), "cid3", "Tabela 15", "obitos_quedas_trabalho")

# ==============================================================================
# BLOCO 4 — GRÁFICOS
# ==============================================================================
rodape <- sprintf("Fonte: SIM/SIH-SUS (DATASUS). População: estimativas MS/DATASUS. Recalculado em %s.",
                  format(Sys.Date(), "%d/%m/%Y"))
tema <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.caption = element_text(size = 8, hjust = 0),
        panel.grid.minor = element_blank())
salvar <- function(g, nome, w = 9, h = 5.5) {
  ggsave(file.path(DIR_SAIDA, "graficos", nome), g, width = w, height = h, dpi = 200, bg = "white")
}
linha <- function(df, cor, titulo, ylab = "Taxa por 100 mil hab.") {
  ggplot(df, aes(ano, taxa, colour = .data[[cor]])) + geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
    scale_x_continuous(breaks = anos_sim) + labs(title = titulo, x = NULL, y = ylab, colour = NULL, caption = rodape) + tema
}

top5 <- g1 %>% filter(ano == max(anos_sim)) %>% slice_max(n, n = 5) %>% pull(cap)
salvar(linha(filter(g1, cap %in% top5) %>% mutate(rotulo = factor(rotulo, CAPITULOS$rotulo[match(top5, CAPITULOS$cap)])),
             "rotulo", paste0("Gráfico 1. Principais causas de óbito — ", ROTULO_MACRO)) +
         guides(colour = guide_legend(ncol = 2)), "g01_capitulos_serie.png")
salvar(linha(g2, "grupo", paste0("Gráfico 2. Mortalidade por doenças cardiovasculares — ", ROTULO_MACRO)),
       "g02_cardiovasculares_serie.png")
salvar(linha(iam, "recorte", "Gráfico 3. Mortalidade por IAM por região de saúde"), "g03_iam_rs.png")
salvar(linha(avc, "recorte", "Gráfico 4. Mortalidade por AVC por região de saúde"), "g04_avc_rs.png")
salvar(linha(mutate(g8, grupo = ROTULO_EXT[grupo]), "grupo",
             paste0("Gráfico 8. Principais causas externas — ", ROTULO_MACRO)), "g08_causas_externas_serie.png")

# ==============================================================================
# BLOCO 5 — CONFERÊNCIA COM OS VALORES DO PAR
# ==============================================================================
res <- bind_rows(resultados) %>% distinct(tabela, indicador, recorte, ano, .keep_all = TRUE)
write.csv(res, file.path(DIR_SAIDA, "resultados_longos.csv"), row.names = FALSE, fileEncoding = "UTF-8")

ref <- read.csv(file.path(DIR_PROJETO, "dados_apoio", "referencia_par_2025.csv"), encoding = "UTF-8")
conf <- ref %>% left_join(res, by = c("tabela", "indicador", "recorte", "ano")) %>%
  mutate(diferenca = r2(valor - valor_par),
         dif_pct = r2(100 * (valor - valor_par) / valor_par),
         situacao = case_when(is.na(valor) ~ "não calculado",
                              abs(valor - valor_par) <= 0.01 + 0.005 * abs(valor_par) ~ "confere",
                              TRUE ~ "diverge"),
         valor = r2(valor))
write.csv(conf, file.path(DIR_SAIDA, "conferencia_par.csv"), row.names = FALSE, fileEncoding = "UTF-8")
tabelas[["Conferencia PAR"]] <- conf
tabelas[["Conferencia resumo"]] <- conf %>% count(tabela, situacao) %>%
  pivot_wider(names_from = situacao, values_from = n, values_fill = 0)

message("\nConferência com o PAR (tolerância: 0,5% ou 0,01):")
print(as.data.frame(tabelas[["Conferencia resumo"]]), row.names = FALSE)

arq <- file.path(DIR_SAIDA, "tabelas", sprintf("par_noroeste_%d.xlsx", ANO_REF))
openxlsx::write.xlsx(lapply(tabelas, as.data.frame), arq, overwrite = TRUE)
message("Gravado: ", arq)
