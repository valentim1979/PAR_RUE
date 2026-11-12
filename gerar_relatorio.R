# ==============================================================================
# PAR RUE — MACRORREGIÃO NOROESTE (PR)
# Relatório em HTML do perfil de morbimortalidade
#
# Monta um único arquivo HTML (gráficos embutidos, abre sem internet) a partir
# dos indicadores calculados por analise_par.R.
#
# Uso:
#   Rscript gerar_relatorio.R              # usa saidas/resultados_longos.csv
#   Rscript gerar_relatorio.R --previa-par # prévia com os valores publicados no
#                                          # PAR (dados_apoio/referencia_par_2025.csv)
# Saída: saidas/relatorio_par_noroeste_<ano>.html
#        (na prévia: saidas/previa_relatorio_valores_par.html)
#
# Os textos são montados a partir dos números (maior e menor taxa, diferença em
# relação à macro, variação na série); não há interpretação escrita à mão.
# ==============================================================================

if (!isTRUE(l10n_info()$`UTF-8`)) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

ANO_REF <- 2025
ROTULO_MACRO <- "Macrorregião Noroeste"
NOMES_RS <- c("11ª RS" = "11ª RS Campo Mourão", "12ª RS" = "12ª RS Umuarama",
              "13ª RS" = "13ª RS Cianorte", "14ª RS" = "14ª RS Paranavaí",
              "15ª RS" = "15ª RS Maringá", "Macro" = ROTULO_MACRO)

DIR_PROJETO <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg))) else getwd()
})
DIR_SAIDA <- file.path(DIR_PROJETO, "saidas")
dir.create(DIR_SAIDA, showWarnings = FALSE)
PREVIA <- "--previa-par" %in% commandArgs(trailingOnly = TRUE)

if (PREVIA) {
  res <- read.csv(file.path(DIR_PROJETO, "dados_apoio", "referencia_par_2025.csv"), encoding = "UTF-8") %>%
    rename(valor = valor_par)
  conf <- NULL
  arq_saida <- file.path(DIR_SAIDA, "previa_relatorio_valores_par.html")
  fonte_txt <- "Valores transcritos do PAR RUE (SIM e SIH/SUS, dados da SCVGE tabulados em 06/11/26)."
} else {
  f <- file.path(DIR_SAIDA, "resultados_longos.csv")
  if (!file.exists(f)) stop("Rode antes: Rscript analise_par.R")
  res  <- read.csv(f, encoding = "UTF-8")
  conf <- read.csv(file.path(DIR_SAIDA, "conferencia_par.csv"), encoding = "UTF-8")
  arq_saida <- file.path(DIR_SAIDA, sprintf("relatorio_par_noroeste_%d.html", ANO_REF))
  fonte_txt <- "SIM e SIH/SUS (DATASUS ou base estadual), recalculados por analise_par.R."
}

CAP <- c(I = "Infecciosas e parasitárias", II = "Neoplasias", III = "Sangue e órgãos hematopoéticos",
         IV = "Endócrinas, nutricionais e metabólicas", V = "Transtornos mentais", VI = "Sistema nervoso",
         IX = "Aparelho circulatório", X = "Aparelho respiratório", XI = "Aparelho digestivo",
         XII = "Pele e tecido subcutâneo", XIII = "Osteomuscular", XIV = "Aparelho geniturinário",
         XV = "Gravidez, parto e puerpério", XVI = "Afecções perinatais", XVII = "Malformações congênitas",
         XVIII = "Sintomas e achados anormais (mal definidas)", XX = "Causas externas")

# --- auxiliares -----------------------------------------------------------------
num <- function(x, d = 2) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ","))
int <- function(x) ifelse(is.na(x), "–", formatC(round(x), format = "d", big.mark = ".", decimal.mark = ","))
pct <- function(a, b) { v <- 100 * (a - b) / b; paste0(ifelse(!is.na(v) & v > 0, "+", ""), num(v, 1)) }
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
pega <- function(tab, ind, rec = "Macro", ano = ANO_REF) {
  v <- res$valor[res$tabela == tab & res$indicador == ind & res$recorte == rec & res$ano == ano]
  if (length(v)) v[1] else NA_real_
}
serie <- function(tab, ind) res %>% filter(tabela == tab, indicador == ind) %>% select(recorte, ano, valor)

tabela_html <- function(df, num_cols = integer(), destaque = NULL) {
  cab <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  linhas <- vapply(seq_len(nrow(df)), function(i) {
    cel <- vapply(seq_along(df), function(j) {
      v <- df[[j]][i]; cls <- if (j %in% num_cols) " class='n'" else ""
      st <- if (!is.null(destaque) && !is.na(destaque[i, j])) sprintf(" style='background:%s'", destaque[i, j]) else ""
      sprintf("<td%s%s>%s</td>", cls, st, esc(as.character(v)))
    }, character(1))
    paste0("<tr>", paste(cel, collapse = ""), "</tr>")
  }, character(1))
  sprintf("<div class='tw'><table><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>", cab, paste(linhas, collapse = ""))
}

# cor da célula conforme a razão em relação à macro (acima = laranja, abaixo = azul)
cor_razao <- function(r) {
  ifelse(is.na(r), NA, ifelse(r >= 1.10, "#fbd3b4", ifelse(r >= 1.0, "#fdeadb",
         ifelse(r <= 0.90, "#c9def2", "#e3eef8"))))
}

PALETA <- c("Macro" = "#222222", "11ª RS" = "#1f77b4", "12ª RS" = "#2ca02c",
            "13ª RS" = "#9467bd", "14ª RS" = "#d62728", "15ª RS" = "#ff7f0e")
tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot")
img <- function(g, w = 8.5, h = 4.4) {
  f <- tempfile(fileext = ".png"); ggsave(f, g, width = w, height = h, dpi = 150, bg = "white")
  sprintf("<img src='data:image/png;base64,%s' alt=''>", base64enc::base64encode(f))
}
grafico_rs <- function(df, ylab = "Taxa por 100 mil hab.") {
  df <- mutate(df, recorte = factor(NOMES_RS[recorte], NOMES_RS[c(6, 1:5)]))
  pal <- setNames(PALETA[c("Macro", paste0(11:15, "ª RS"))], NOMES_RS[c(6, 1:5)])
  ggplot(df, aes(ano, valor, colour = recorte, linewidth = recorte == ROTULO_MACRO)) +
    geom_line() + geom_point(size = 1.6) +
    scale_colour_manual(values = pal) + scale_linewidth_manual(values = c(0.7, 1.4), guide = "none") +
    scale_x_continuous(breaks = unique(df$ano)) +
    labs(x = NULL, y = ylab, colour = NULL) + tema + guides(colour = guide_legend(nrow = 2))
}

# frase "maior/menor taxa" para um indicador por RS
frase_rs <- function(tab, ind, nome) {
  v <- sapply(paste0(11:15, "ª RS"), function(r) pega(tab, ind, r)); m <- pega(tab, ind)
  if (all(is.na(v))) return("")
  a <- names(which.max(v)); b <- names(which.min(v))
  sprintf("Em %s, a maior taxa de %s foi na %s (%s; %s%% em relação à macro, %s) e a menor na %s (%s; %s%%).",
          ANO_REF, nome, NOMES_RS[a], num(v[a]), pct(v[a], m), num(m), NOMES_RS[b], num(v[b]), pct(v[b], m))
}

secoes <- list()
add <- function(titulo, ...) secoes[[length(secoes) + 1]] <<- sprintf("<section><h2>%s</h2>%s</section>", titulo, paste0(...))

# --- 1. Resumo ------------------------------------------------------------------
tot <- pega("Quadro 2", "obitos_total"); tx_tot <- pega("Quadro 2", "taxa_total")
caps <- res %>% filter(tabela == "Quadro 2", recorte == "Macro", ano == ANO_REF, grepl("^obitos_cap_", indicador)) %>%
  mutate(cap = sub("obitos_cap_", "", indicador)) %>% arrange(desc(valor))
top5 <- head(caps, 5)
cards <- paste0(
  sprintf("<div class='card'><b>%s</b><span>óbitos de residentes em %d</span></div>", int(tot), ANO_REF),
  sprintf("<div class='card'><b>%s</b><span>óbitos por 100 mil hab.</span></div>", num(tx_tot)),
  sprintf("<div class='card'><b>%s%%</b><span>dos óbitos nas 5 principais causas</span></div>",
          num(100 * sum(top5$valor) / tot, 0)),
  sprintf("<div class='card'><b>%s</b><span>internações por IAM (SIH/SUS)</span></div>", int(pega("Quadro 3", "int_iam_n"))))
add("Resumo", "<div class='cards'>", cards, "</div>",
    sprintf("<p>As cinco principais causas de óbito em %d foram: %s.</p>", ANO_REF,
            paste(sprintf("%s (%s óbitos)", tolower(CAP[top5$cap]), int(top5$valor)), collapse = "; ")))

# --- 2. Mortalidade por capítulo e região ---------------------------------------
q2 <- res %>% filter(tabela == "Quadro 2", ano == ANO_REF, grepl("^taxa_cap_", indicador)) %>%
  mutate(cap = sub("taxa_cap_", "", indicador)) %>% select(cap, recorte, valor) %>%
  pivot_wider(names_from = recorte, values_from = valor) %>%
  left_join(select(caps, cap, obitos = valor), by = "cap") %>% arrange(desc(obitos)) %>% head(10)
rs_cols <- intersect(paste0(11:15, "ª RS"), names(q2))
tab2 <- tibble(Causa = CAP[q2$cap], `Óbitos` = int(q2$obitos), Macro = num(q2$Macro))
for (r in rs_cols) tab2[[r]] <- num(q2[[r]])
dest <- matrix(NA_character_, nrow(tab2), ncol(tab2))
for (k in seq_along(rs_cols)) dest[, 3 + k] <- cor_razao(q2[[rs_cols[k]]] / q2$Macro)
add("Mortalidade por grupo de causas e região de saúde",
    sprintf("<p>Taxa por 100 mil habitantes, %d, dez principais capítulos da CID-10. Laranja: taxa acima da macro (forte = 10%% ou mais); azul: abaixo.</p>", ANO_REF),
    tabela_html(tab2, 2:ncol(tab2), dest),
    "<p>", frase_rs("Quadro 2", "taxa_cap_IX", "doenças do aparelho circulatório"), " ",
    frase_rs("Quadro 2", "taxa_cap_X", "doenças do aparelho respiratório"), " ",
    frase_rs("Quadro 2", "taxa_cap_XX", "causas externas"), "</p>")

# --- 3. Por sexo -----------------------------------------------------------------
t3 <- res %>% filter(tabela == "Tabela 3", grepl("^taxa_cap_", indicador)) %>%
  mutate(cap = sub("taxa_cap_", "", indicador)) %>% select(cap, recorte, valor) %>%
  pivot_wider(names_from = recorte, values_from = valor)
if (nrow(t3) && all(c("Masculino", "Feminino") %in% names(t3))) {
  t3 <- t3 %>% mutate(razao = Masculino / Feminino) %>% arrange(desc(Masculino + Feminino)) %>% head(7)
  g3 <- t3 %>% mutate(causa = factor(CAP[cap], rev(CAP[cap]))) %>%
    pivot_longer(c(Masculino, Feminino), names_to = "sexo", values_to = "valor") %>%
    ggplot(aes(valor, causa, fill = sexo)) + geom_col(position = position_dodge(0.75), width = 0.7) +
    scale_fill_manual(values = c(Masculino = "#1f77b4", Feminino = "#e377c2")) +
    labs(x = "Taxa por 100 mil hab.", y = NULL, fill = NULL) + tema
  maior <- t3[which.max(t3$razao), ]
  add("Mortalidade por sexo", img(g3, h = 4),
      tabela_html(tibble(Causa = CAP[t3$cap], Masculino = num(t3$Masculino), Feminino = num(t3$Feminino),
                         `Razão M/F` = num(t3$razao)), 2:4),
      sprintf("<p>A maior diferença entre os sexos foi em %s: razão de %s entre as taxas masculina e feminina.</p>",
              tolower(CAP[maior$cap]), num(maior$razao)))
}

# --- 4. Doenças cardiovasculares --------------------------------------------------
cv <- tibble(rec = c("Macro", paste0(11:15, "ª RS"))) %>%
  mutate(`Região de saúde` = NOMES_RS[rec],
         `População` = int(sapply(rec, function(r) pega("Quadro 3", "populacao", r))),
         `Intern. isquêmicas /10 mil` = num(sapply(rec, function(r) pega("Quadro 3", "int_isquemicas_taxa", r))),
         `Intern. IAM /10 mil` = num(sapply(rec, function(r) pega("Quadro 3", "int_iam_taxa", r))),
         `Óbitos IAM /100 mil` = num(sapply(rec, function(r) pega("Quadro 3", "mort_iam_taxa", r))),
         `Intern. AVC /10 mil` = num(sapply(rec, function(r) pega("Quadro 4", "int_avc_taxa", r))),
         `Óbitos AVC /100 mil` = num(sapply(rec, function(r) pega("Quadro 4", "mort_avc_taxa", r)))) %>% select(-rec)
s_iam <- serie("Tabela 4", "mort_iam_taxa"); s_avc <- serie("Tabela 5", "mort_avc_taxa")
var_serie <- function(s, nome) {
  m <- filter(s, recorte == "Macro") %>% arrange(ano)
  if (nrow(m) < 2) return("")
  sprintf("Na macro, a taxa de mortalidade por %s passou de %s em %d para %s em %d (%s%%).",
          nome, num(m$valor[1]), m$ano[1], num(m$valor[nrow(m)]), m$ano[nrow(m)], pct(m$valor[nrow(m)], m$valor[1]))
}
add("Infarto agudo do miocárdio e AVC",
    sprintf("<p>Internações (SIH/SUS) por 10 mil habitantes e óbitos (SIM) por 100 mil habitantes, %d.</p>", ANO_REF),
    tabela_html(cv, 2:7),
    "<p>", frase_rs("Quadro 3", "int_iam_taxa", "internação por IAM"), " ",
    frase_rs("Quadro 3", "mort_iam_taxa", "mortalidade por IAM"), " ",
    frase_rs("Quadro 4", "mort_avc_taxa", "mortalidade por AVC"), "</p>",
    if (nrow(s_iam)) paste0("<h3>Mortalidade por IAM</h3>", img(grafico_rs(s_iam)), "<p>", var_serie(s_iam, "IAM"), "</p>"),
    if (nrow(s_avc)) paste0("<h3>Mortalidade por AVC</h3>", img(grafico_rs(s_avc)), "<p>", var_serie(s_avc, "AVC"), "</p>"))

# --- 5. Neoplasias e doenças respiratórias ---------------------------------------
top_cid <- function(tab) res %>% filter(tabela == tab, grepl("^obitos_", indicador)) %>%
  mutate(cid = sub("obitos_", "", indicador)) %>% arrange(desc(valor)) %>% head(8) %>%
  mutate(taxa = sapply(cid, function(k) pega(tab, paste0("taxa_", k))))
g_cid <- function(d, cor) ggplot(d, aes(taxa, reorder(cid, taxa))) + geom_col(fill = cor, width = 0.7) +
  geom_text(aes(label = num(taxa)), hjust = -0.15, size = 3.4) + scale_x_continuous(expand = expansion(c(0, 0.15))) +
  labs(x = "Taxa por 100 mil hab.", y = NULL) + tema
neo <- top_cid("Tabela 6"); resp <- top_cid("Tabela 9")
pul <- tibble(rec = c(paste0(11:15, "ª RS"), "Macro")) %>%
  mutate(`Região de saúde` = NOMES_RS[rec], Masculino = int(sapply(rec, function(r) pega("Tabela 7", "obitos_C34_masc", r))),
         Feminino = int(sapply(rec, function(r) pega("Tabela 7", "obitos_C34_fem", r))),
         Total = int(sapply(rec, function(r) pega("Tabela 7", "obitos_C34_total", r)))) %>% select(-rec)
fumo <- pega("Texto", "pulmao_pct_tabagismo_linhas")
add("Neoplasias e doenças respiratórias",
    if (nrow(neo)) paste0("<h3>Neoplasias malignas — principais localizações (CID-10, 3 caracteres)</h3>", img(g_cid(neo, "#8c6bb1"), h = 3.6)),
    "<h3>Óbitos por câncer de brônquios e pulmões (C34) por sexo</h3>", tabela_html(pul, 2:4),
    if (!is.na(fumo)) sprintf("<p>%s%% dos óbitos por C34 têm tabagismo (F17 ou Z72.0) registrado nas linhas da declaração de óbito.</p>", num(fumo, 1)),
    if (nrow(resp)) paste0("<h3>Doenças respiratórias — principais causas</h3>", img(g_cid(resp, "#41b6c4"), h = 3.6)))

# --- 6. Covid-19 -------------------------------------------------------------------
s_cov <- serie("Tabela 10", "mort_covid_taxa")
if (nrow(s_cov)) add("Covid-19", img(grafico_rs(s_cov)), "<p>", var_serie(s_cov, "covid-19"), "</p>")

# --- 7. Causas externas ---------------------------------------------------------------
q5 <- tibble(rec = c("Macro", paste0(11:15, "ª RS"))) %>%
  mutate(`Região de saúde` = NOMES_RS[rec], `Óbitos` = int(sapply(rec, function(r) pega("Quadro 5", "obitos_cap_XX", r))),
         `Taxa /100 mil` = num(sapply(rec, function(r) pega("Quadro 5", "taxa_cap_XX", r)))) %>% select(-rec)
s_ext <- bind_rows(serie("Tabela 11", "obitos_transporte") %>% mutate(grupo = "Acidentes de transporte"),
                   serie("Tabela 13", "obitos_quedas") %>% mutate(grupo = "Quedas"),
                   serie("Tabela 12", "obitos_transporte_trabalho") %>% mutate(grupo = "Transporte relacionado ao trabalho"))
g_ext <- if (nrow(s_ext)) ggplot(s_ext, aes(ano, valor, colour = grupo)) + geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = unique(s_ext$ano)) + scale_colour_manual(values = c("#d62728", "#ff7f0e", "#7f7f7f")) +
  labs(x = NULL, y = "Óbitos", colour = NULL) + tema
q <- filter(s_ext, grupo == "Quedas") %>% arrange(ano)
add("Causas externas", tabela_html(q5, 2:3),
    if (!is.null(g_ext)) paste0("<h3>Óbitos por acidentes de transporte e quedas</h3>", img(g_ext)),
    if (nrow(q) >= 2) sprintf("<p>Os óbitos por quedas passaram de %s em %d para %s em %d (%s%%).</p>",
                              int(q$valor[1]), q$ano[1], int(q$valor[nrow(q)]), q$ano[nrow(q)], pct(q$valor[nrow(q)], q$valor[1])))

# --- 8. Conferência com o PAR ------------------------------------------------------------
if (!is.null(conf)) {
  resumo <- conf %>% count(tabela, situacao) %>% pivot_wider(names_from = situacao, values_from = n, values_fill = 0)
  piores <- conf %>% filter(situacao == "diverge") %>% arrange(desc(abs(dif_pct))) %>% head(15) %>%
    transmute(Tabela = tabela, Indicador = indicador, Recorte = recorte, Ano = ano,
              PAR = num(valor_par), Recalculado = num(valor), `Dif. %` = num(dif_pct, 1))
  add("Conferência com o PAR",
      "<p>Comparação de cada número publicado no PAR com o valor recalculado (confere = diferença de até 0,5%).</p>",
      tabela_html(resumo, 2:ncol(resumo)), "<h3>Maiores divergências</h3>", tabela_html(piores, 4:7))
}

# --- HTML -----------------------------------------------------------------------------------
aviso <- if (PREVIA) "<div class='aviso'><b>Prévia.</b> Este relatório mostra os valores publicados no PAR, transcritos do PDF, e não valores recalculados. Serve para conferir o formato. Rode <code>analise_par.R</code> e depois <code>gerar_relatorio.R</code> para gerar a versão com os microdados.</div>" else ""
css <- "
:root{--tx:#1d1d1f;--mut:#5f6368;--bg:#fff;--ln:#e3e3e3;--ac:#0b5394}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--tx);font:16px/1.55 system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}
main{max-width:960px;margin:0 auto;padding:24px 16px 64px}
header h1{font-size:1.6rem;margin:0 0 4px}header p{color:var(--mut);margin:0}
h2{font-size:1.25rem;border-bottom:2px solid var(--ac);padding-bottom:4px;margin-top:40px}
h3{font-size:1.02rem;margin:24px 0 8px}
img{max-width:100%;height:auto;display:block;margin:8px 0}
.tw{overflow-x:auto;margin:8px 0}table{border-collapse:collapse;font-size:.88rem;min-width:100%}
th,td{border-bottom:1px solid var(--ln);padding:6px 8px;text-align:left;white-space:nowrap}th{background:#f4f6f8}
td.n{text-align:right;font-variant-numeric:tabular-nums}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:12px;margin:12px 0}
.card{border:1px solid var(--ln);border-radius:8px;padding:12px}.card b{display:block;font-size:1.5rem;color:var(--ac)}
.card span{color:var(--mut);font-size:.88rem}
.aviso{background:#fff4ce;border-left:4px solid #e0a800;padding:10px 14px;margin:16px 0;border-radius:4px}
footer{color:var(--mut);font-size:.82rem;margin-top:40px;border-top:1px solid var(--ln);padding-top:12px}
@media print{.aviso{border:1px solid #e0a800}section{break-inside:avoid-page}}"
html <- sprintf("<!doctype html><html lang='pt-BR'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>
<title>Perfil de morbimortalidade — %s</title><style>%s</style></head><body><main>
<header><h1>Perfil de morbimortalidade — %s</h1><p>PAR RUE · regiões de saúde de residência · %d</p></header>%s%s
<footer>Fonte: %s População: estimativas MS/DATASUS. Gerado em %s.</footer></main></body></html>",
  ROTULO_MACRO, css, ROTULO_MACRO, ANO_REF, aviso, paste(secoes, collapse = "\n"), fonte_txt, format(Sys.Date(), "%d/%m/%Y"))
writeLines(html, arq_saida, useBytes = TRUE)
message("Gravado: ", arq_saida)
