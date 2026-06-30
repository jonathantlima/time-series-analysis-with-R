# ----------------------------------------------------------------------------
# 3 - PREPARAÇÃO DOS DADOS METEOROLÓGICOS
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Carregamento dos pacotes
# ----------------------------------------------------------------------------
library(dplyr)
library(lubridate)

# ----------------------------------------------------------------------------
# Carregamento da base de dados com 'read.csv', pois esta usa '.' como
# separador. Os dados 'null' faziam com que os dados fossem lidos como do
# tipo 'char', então, foi realizado ajuste para reconhecer 'null' como 'NA'
# ----------------------------------------------------------------------------
dados_itu <- read.csv("dados/ituporanga.csv", 
                      sep = ";", 
                      dec = ".", 
                      na.strings = "null")

# ----------------------------------------------------------------------------
# Converte a coluna 'data' para um formato apropriado
# Como o formato é AAAA-MM-DD, as.Date já funciona
# ----------------------------------------------------------------------------
dados_itu <- dados_itu %>%
  mutate(Data = as.Date(Data))

# ----------------------------------------------------------------------------
# Resumo dos dados tratados
# ----------------------------------------------------------------------------
summary(dados_itu)

# ----------------------------------------------------------------------------
# Salvar o objeto em um arquivo chamado 'dados_ituporanga_processados.csv'
# ----------------------------------------------------------------------------
write.csv(dados_itu, "dados/dados_ituporanga_processados.csv", row.names = FALSE)
