# ==============================================================================
# PAR RUE — MACRORREGIÃO NOROESTE (PR)
# Download da população residente por município e sexo (Paraná)
#
# Fonte: Ministério da Saúde — "Estudo de Estimativas Populacionais por
# Município, Idade e Sexo 2000-2025" (DATASUS/Tabnet). Mesma fonte usada no
# painel de SRAG (virus-respiratorios/baixar_populacao.R), aqui só por sexo.
#
# Uso:   Rscript baixar_populacao.R            (2015 até o ano mais recente)
#        Rscript baixar_populacao.R 2015 2025  (intervalo de anos)
# Saída: dados_apoio/populacao/populacao_pr_sexo_<ano>.csv
#        colunas codigo_ibge_6, sexo (Masculino/Feminino), populacao
# Anos já baixados não são baixados de novo (apague o CSV para forçar).
# ==============================================================================

DEF_TABNET <- "ibge/cnv/popsvs2024br.def"
URL_FORM   <- paste0("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?", DEF_TABNET)
URL_TABCGI <- paste0("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?", DEF_TABNET)
UF_PARANA  <- "16"

DIR_PROJETO <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg))) else getwd()
})
DIR_POP <- file.path(DIR_PROJETO, "dados_apoio", "populacao")
dir.create(DIR_POP, recursive = TRUE, showWarnings = FALSE)

form <- readLines(URL_FORM, encoding = "latin1", warn = FALSE)
anos_disponiveis <- as.integer(paste0("20", regmatches(
  form, regexpr('(?<=VALUE="pop)[0-9]{2}(?=\\.dbf")', form, perl = TRUE))))

args <- as.integer(commandArgs(trailingOnly = TRUE))
ano_ini <- if (length(args) >= 1) args[1] else 2015L
ano_fim <- if (length(args) >= 2) args[2] else max(anos_disponiveis)
anos <- intersect(ano_ini:ano_fim, anos_disponiveis)
faltam <- setdiff(ano_ini:ano_fim, anos_disponiveis)
if (length(faltam)) message("Sem estimativa no Tabnet para: ", paste(faltam, collapse = ", "))

# O Tabnet só entende latin1; URLencode() codifica em UTF-8.
codificar <- function(x) vapply(x, function(s) {
  bytes <- charToRaw(iconv(s, from = "UTF-8", to = "latin1"))
  chars <- rawToChar(bytes, multiple = TRUE)
  paste0(ifelse(grepl("[A-Za-z0-9._~-]", chars, useBytes = TRUE) & as.integer(bytes) < 128,
                chars, sprintf("%%%02X", as.integer(bytes))), collapse = "")
}, character(1), USE.NAMES = FALSE)

consultar_tabnet <- function(ano) {
  campos <- c(
    "Linha"                 = "Município",
    "Coluna"                = "Sexo",
    "Incremento"            = "População_residente",
    "Arquivos"              = sprintf("pop%02d.dbf", ano %% 100),
    "SUnidade_da_Federação" = UF_PARANA,
    "formato"               = "prn",
    "mostre"                = "Mostra"
  )
  corpo <- paste(paste0(codificar(names(campos)), "=", codificar(campos)), collapse = "&")
  resp <- httr::POST(URL_TABCGI, body = corpo,
                     httr::content_type("application/x-www-form-urlencoded"),
                     httr::timeout(180))
  httr::stop_for_status(resp)
  html <- httr::content(resp, as = "text", encoding = "latin1")
  pre  <- regmatches(html, regexpr("(?s)<PRE>.*?</PRE>", html, perl = TRUE, ignore.case = TRUE))
  if (length(pre) == 0) stop("Resposta do Tabnet sem tabela (ano ", ano, ")")
  txt <- gsub("(?i)</?PRE>", "", pre, perl = TRUE)

  tab <- utils::read.csv2(text = txt, check.names = FALSE, colClasses = "character",
                          strip.white = TRUE)
  tab <- tab[grepl("^[0-9]{6} ", tab[[1]]), ]
  stopifnot(all(c("Masculino", "Feminino") %in% names(tab)))
  num <- function(v) as.integer(gsub("[^0-9]", "", v))
  data.frame(
    codigo_ibge_6 = rep(as.integer(substr(tab[[1]], 1, 6)), 2),
    sexo          = rep(c("Masculino", "Feminino"), each = nrow(tab)),
    populacao     = c(num(tab$Masculino), num(tab$Feminino))
  )
}

for (ano in anos) {
  saida <- file.path(DIR_POP, paste0("populacao_pr_sexo_", ano, ".csv"))
  if (file.exists(saida)) { message(ano, ": já existe, mantido"); next }
  pop <- consultar_tabnet(ano)
  pop$populacao[is.na(pop$populacao)] <- 0L
  n_mun <- length(unique(pop$codigo_ibge_6)); total <- sum(pop$populacao)
  if (n_mun != 399) stop(ano, ": esperados 399 municípios do PR, vieram ", n_mun)
  if (total < 10e6 || total > 14e6) stop(ano, ": total do PR fora do esperado: ", total)
  utils::write.csv(pop, saida, row.names = FALSE, fileEncoding = "UTF-8")
  message(ano, ": PR = ", format(total, big.mark = ".", decimal.mark = ","), " hab. → ", basename(saida))
  Sys.sleep(2)
}
