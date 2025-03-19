# Os dados podem ser encontrados no seguinte link: 
################################################################################
# https://www.sciencedirect.com/science/article/pii/S2352340919312594?via%3Dihub
################################################################################


# Instalar e carregar pacman (se ainda não estiver instalado)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(foreign, haven, tidyverse)

# Carregar o dataset com foreign
dataset <- read.spss("./data/dib_104904_etikabisnis19.sav", to.data.frame = TRUE)

str(dataset)
head(dataset)
(dim_df <- c(nrow(dataset),ncol(dataset)))

View(dataset)


# Guardar os dados no formato csv
write.csv(dataset, "./data/dados_raw.csv", row.names = FALSE)
