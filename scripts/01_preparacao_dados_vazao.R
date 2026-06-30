###############################################################
# Universidade Federal de Santa Catarina
# INE5649 - Técnicas Estatísticas de Predição
#
# Preparação de dados HidroWeb
#
# Estação: 83300200
# Município: Rio do Sul/SC
###############################################################

#==============================================================
# Pacotes
#==============================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#==============================================================
# Configurações
#==============================================================

arquivo_entrada <- "83300200_Vazoes.csv"
arquivo_saida   <- "83300200_Vazoes_Tratado.csv"

#==============================================================
# Leitura do arquivo
#==============================================================

cat("----------------------------------------\n")
cat("Lendo arquivo HidroWeb...\n")
cat("----------------------------------------\n")

dados <- read_delim(
  file = arquivo_entrada,
  delim = ";",
  skip = 15,
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE,
  trim_ws = TRUE
)

cat("Leitura concluída.\n\n")

#==============================================================
# Seleciona apenas as colunas necessárias
#==============================================================

dados <- dados %>%
  select(
    Data,
    matches("^Vazao\\d{2}$")
  )

#==============================================================
# Converte para formato longo
#==============================================================

dados <- dados %>%
  pivot_longer(
    cols = starts_with("Vazao"),
    names_to = "Dia",
    values_to = "Vazao"
  )

#==============================================================
# Cria a data completa
#==============================================================

dados <- dados %>%
  mutate(
    
    Dia = as.integer(gsub("Vazao", "", Dia)),
    
    Data_Base = dmy(Data),
    
    Ano = year(Data_Base),
    
    Mes = month(Data_Base),
    
    Data = make_date(Ano, Mes, Dia)
    
  )

#==============================================================
# Limpeza
#==============================================================

dados <- dados %>%
  
  filter(!is.na(Data)) %>%
  
  mutate(
    
    Vazao = suppressWarnings(as.numeric(Vazao))
    
  ) %>%
  
  filter(!is.na(Vazao)) %>%
  
  arrange(Data) %>%
  
  select(Data, Vazao)

#==============================================================
# Estatísticas básicas
#==============================================================

cat("----------------------------------------\n")
cat("Resumo da série\n")
cat("----------------------------------------\n")

cat("Primeira data :",
    format(min(dados$Data)),
    "\n")

cat("Última data   :",
    format(max(dados$Data)),
    "\n")

cat("Observações   :",
    nrow(dados),
    "\n")

cat("Vazão mínima  :",
    min(dados$Vazao),
    "\n")

cat("Vazão média   :",
    mean(dados$Vazao),
    "\n")

cat("Vazão máxima  :",
    max(dados$Vazao),
    "\n\n")

#==============================================================
# Verifica valores ausentes
#==============================================================

cat("----------------------------------------\n")
cat("Valores ausentes\n")
cat("----------------------------------------\n")

cat("NA encontrados:",
    sum(is.na(dados$Vazao)),
    "\n\n")

#==============================================================
# Exporta CSV tratado
#==============================================================

write_csv(dados, arquivo_saida)

cat("----------------------------------------\n")
cat("Arquivo exportado com sucesso!\n")
cat("----------------------------------------\n")

cat(arquivo_saida, "\n\n")

#==============================================================
# Gráfico da série temporal
#==============================================================

grafico <- ggplot(
  dados,
  aes(
    x = Data,
    y = Vazao
  )
) +
  geom_line(color = "steelblue") +
  labs(
    title = "Série histórica de vazões",
    subtitle = "Estação 83300200 - Rio do Sul/SC",
    x = "Ano",
    y = expression("Vazão (m"^3*"/s)")
  ) +
  theme_minimal(base_size = 12)

print(grafico)

#==============================================================
# Salva o gráfico
#==============================================================

ggsave(
  filename = "serie_historica.png",
  plot = grafico,
  width = 10,
  height = 5,
  dpi = 300
)

cat("Gráfico salvo como serie_historica.png\n")
