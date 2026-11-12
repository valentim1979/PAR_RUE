# Instala os pacotes R usados pelo projeto. Rodar uma vez:
#   Rscript instalar_dependencias.R
#
# O read.dbc (leitura dos .dbc do DATASUS) foi retirado do CRAN em 14/12/2025;
# por isso é instalado a partir do arquivo do CRAN (precisa de compilador C).

repos <- "https://cloud.r-project.org"
pacotes <- c("dplyr", "tidyr", "ggplot2", "stringr", "foreign", "openxlsx", "httr")
faltam <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltam)) install.packages(faltam, repos = repos)

if (!requireNamespace("read.dbc", quietly = TRUE)) {
  install.packages("https://cran.r-project.org/src/contrib/Archive/read.dbc/read.dbc_1.2.0.tar.gz",
                   repos = NULL, type = "source")
}
message("Pacotes prontos.")
