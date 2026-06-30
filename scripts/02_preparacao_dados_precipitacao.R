###############################################################
# Preparação de dados de Chuva - HidroWeb
# Estação: 2749041
###############################################################

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Caminhos dos arquivos baseados na sua estrutura
arquivo_entrada <- "dados/2749041_Chuvas.csv"
arquivo_saida   <- "dados/2749041_Chuvas_Tratado.csv"

# Leitura (pular 14 linhas conforme cabeçalho do arquivo)
dados_chuva <- read_delim(
  file = arquivo_entrada,
  delim = ";",
  skip = 14,
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# Processamento: Pivotagem de colunas para formato Tidy (longo)
chuvas_longo <- dados_chuva %>%
  select(Data, matches("^Chuva\\d{2}$")) %>%
  pivot_longer(
    cols = matches("^Chuva\\d{2}$"),
    names_to = "Dia",
    values_to = "Chuva"
  ) %>%
  mutate(
    Dia = as.numeric(gsub("Chuva", "", Dia)),
    Data = dmy(Data),
    Data = update(Data, day = Dia)
  ) %>%
  filter(!is.na(Data)) %>%
  select(Data, Chuva) %>%
  arrange(Data)

# Exportação
write_csv(chuvas_longo, arquivo_saida)

cat("Dados tratados com sucesso em:", arquivo_saida, "\n")