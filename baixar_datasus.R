# ==============================================================================
# PAR RUE — MACRORREGIÃO NOROESTE (PR)
# Download e preparo dos microdados do SIM (óbitos) e do SIH/SUS (internações)
#
# Fontes:
#   SIM  — Declarações de óbito, arquivos DOPR<ano>.dbc do FTP do DATASUS
#          (anos fechados em SIM/CID10/DORES; anos preliminares em SIM/PRELIM/DORES).
#   SIH  — AIH reduzida, arquivos RDPR<aa><mm>.dbc (SIHSUS/200801_/Dados),
#          por mês de competência (processamento).
#
# Base estadual: o PAR foi tabulado na base do SIM da SESA (06/11/26), que para
# 2025 pode ter mais registros que o arquivo preliminar do DATASUS. Se você tiver
# o DBF/DBC estadual de um ano, coloque-o em ~/PAR_RUE_entrada/ com o ano no nome
# (ex.: DO_PR_2025.dbf, RD_PR_202503.dbf). Arquivos locais têm prioridade sobre o FTP.
#
# Uso:   Rscript baixar_datasus.R                    (SIM 2015-2025 e SIH 2025)
#        Rscript baixar_datasus.R sim 2015 2025
#        Rscript baixar_datasus.R sih 2025
# Saída (fora do projeto, pasta PAR_RUE_DADOS, padrão ~/PAR_RUE_dados):
#        sim_<ano>.rds  — óbitos de residentes no PR, só as colunas de COLUNAS_SIM
#        sih_<ano>.rds  — AIHs de residentes no PR, só as colunas de COLUNAS_SIH
#        brutos/        — cache dos .dbc baixados (pode apagar depois)
#
# LGPD: a base estadual traz nome e outros identificadores. Este script guarda
# só as colunas listadas abaixo e nunca imprime valores de registros.
# ==============================================================================

suppressPackageStartupMessages({ library(dplyr) })

DIR_DADOS   <- Sys.getenv("PAR_RUE_DADOS",   path.expand("~/PAR_RUE_dados"))
DIR_ENTRADA <- Sys.getenv("PAR_RUE_ENTRADA", path.expand("~/PAR_RUE_entrada"))
DIR_BRUTOS  <- file.path(DIR_DADOS, "brutos")
dir.create(DIR_BRUTOS, recursive = TRUE, showWarnings = FALSE)

FTP <- "ftp://ftp.datasus.gov.br/dissemin/publicos"

COLUNAS_SIM <- c("DTOBITO", "CODMUNRES", "SEXO", "IDADE", "CAUSABAS",
                 "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII",
                 "OCUP", "ACIDTRAB", "CIRCOBITO")
COLUNAS_SIH <- c("ANO_CMPT", "MES_CMPT", "MUNIC_RES", "SEXO", "IDADE", "COD_IDADE",
                 "DIAG_PRINC", "IDENT", "DT_INTER", "DT_SAIDA", "MORTE", "MUNIC_MOV")
PROIBIDAS   <- c("NOME", "NOMEPAI", "NOMEMAE", "NUMERODO", "NUMERODN", "N_AIH",
                 "CPF", "CNS", "NUMSUS", "ENDRES", "NUMRES", "COMPLRES", "CEPRES",
                 "BAIRES", "DTNASC", "NASC", "CEP", "LOGR", "NUM_LOGR")
stopifnot(!any(c(COLUNAS_SIM, COLUNAS_SIH) %in% PROIBIDAS))

# --- Leitura de DBC/DBF -------------------------------------------------------
ler_arquivo <- function(caminho) {
  ext <- tolower(tools::file_ext(caminho))
  df <- if (ext == "dbc") read.dbc::read.dbc(caminho, as.is = TRUE)
        else foreign::read.dbf(caminho, as.is = TRUE)
  names(df) <- toupper(names(df))
  df[] <- lapply(df, function(x) trimws(as.character(x)))
  df
}

baixar_ftp <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) return(TRUE)
  ok <- tryCatch({
    utils::download.file(url, destino, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok && file.exists(destino)) unlink(destino)
  ok
}

arquivo_local <- function(prefixo, chave) {
  # procura em ~/PAR_RUE_entrada arquivos como DO*2025*.dbf ou RD*202503*.dbc
  cand <- list.files(DIR_ENTRADA, pattern = paste0("^", prefixo, ".*", chave, ".*\\.(dbf|dbc)$"),
                     ignore.case = TRUE, full.names = TRUE)
  if (length(cand)) cand[which.max(file.mtime(cand))] else NA_character_
}

manter_colunas <- function(df, colunas, fonte) {
  faltando <- setdiff(colunas, names(df))
  if (length(faltando)) {
    message("  ", fonte, ": colunas ausentes (ficam NA): ", paste(faltando, collapse = ", "))
    for (cc in faltando) df[[cc]] <- NA_character_
  }
  df[, colunas]
}

# --- SIM ----------------------------------------------------------------------
preparar_sim <- function(ano) {
  local <- arquivo_local("DO", ano)
  if (!is.na(local)) {
    origem <- paste0("arquivo local ", basename(local))
    df <- ler_arquivo(local)
  } else {
    nome <- sprintf("DOPR%d.dbc", ano)
    destino <- file.path(DIR_BRUTOS, nome)
    urls <- c(file.path(FTP, "SIM/CID10/DORES", nome),
              file.path(FTP, "SIM/PRELIM/DORES", nome))
    ok <- FALSE
    for (u in urls) if (baixar_ftp(u, destino)) { ok <- TRUE; origem <- u; break }
    if (!ok) { message("SIM ", ano, ": não encontrado no FTP nem em ", DIR_ENTRADA); return(invisible(NULL)) }
    df <- ler_arquivo(destino)
  }
  n_bruto <- nrow(df)
  df <- manter_colunas(df, COLUNAS_SIM, paste("SIM", ano))
  # residentes no PR (CODMUNRES pode vir com 6 ou 7 dígitos)
  df$CODMUNRES <- substr(df$CODMUNRES, 1, 6)
  df <- df[!is.na(df$CODMUNRES) & startsWith(df$CODMUNRES, "41"), ]
  # ano do óbito (DTOBITO = ddmmaaaa); confere se o arquivo é do ano pedido
  ano_obito <- as.integer(substr(df$DTOBITO, 5, 8))
  prop <- mean(ano_obito == ano, na.rm = TRUE)
  if (is.na(prop) || prop < 0.95) stop("SIM ", ano, ": só ", round(100 * prop), "% dos óbitos são do ano")
  df <- df[!is.na(ano_obito) & ano_obito == ano, ]
  saveRDS(df, file.path(DIR_DADOS, sprintf("sim_%d.rds", ano)))
  message(sprintf("SIM %d: %d registros no arquivo, %d óbitos de residentes no PR (%s)",
                  ano, n_bruto, nrow(df), origem))
}

# --- SIH ----------------------------------------------------------------------
preparar_sih <- function(ano) {
  partes <- list()
  for (mes in 1:12) {
    chave <- sprintf("%d%02d", ano, mes)
    local <- arquivo_local("RD", chave)
    if (!is.na(local)) {
      df <- ler_arquivo(local)
    } else {
      nome <- sprintf("RDPR%02d%02d.dbc", ano %% 100, mes)
      destino <- file.path(DIR_BRUTOS, nome)
      if (!baixar_ftp(file.path(FTP, "SIHSUS/200801_/Dados", nome), destino)) {
        message("SIH ", chave, ": não disponível"); next
      }
      df <- ler_arquivo(destino)
    }
    df <- manter_colunas(df, COLUNAS_SIH, paste("SIH", chave))
    df$MUNIC_RES <- substr(df$MUNIC_RES, 1, 6)
    partes[[chave]] <- df[!is.na(df$MUNIC_RES) & startsWith(df$MUNIC_RES, "41"), ]
  }
  if (!length(partes)) return(invisible(NULL))
  df <- bind_rows(partes)
  saveRDS(df, file.path(DIR_DADOS, sprintf("sih_%d.rds", ano)))
  message(sprintf("SIH %d: %d meses, %d AIHs de residentes no PR", ano, length(partes), nrow(df)))
}

# --- Execução -------------------------------------------------------------------
for (p in c("read.dbc", "foreign")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Instale o pacote ", p, " (Rscript instalar_dependencias.R)")
}
args <- commandArgs(trailingOnly = TRUE)
sistema <- if (length(args)) tolower(args[1]) else "todos"
anos <- as.integer(args[-1])

if (sistema %in% c("sim", "todos")) {
  a <- if (sistema == "sim" && length(anos)) seq(anos[1], anos[length(anos)]) else 2015:2025
  for (ano in a) preparar_sim(ano)
}
if (sistema %in% c("sih", "todos")) {
  a <- if (sistema == "sih" && length(anos)) seq(anos[1], anos[length(anos)]) else 2025
  for (ano in a) preparar_sih(ano)
}
