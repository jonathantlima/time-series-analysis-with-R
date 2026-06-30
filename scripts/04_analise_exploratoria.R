# -----------------------------------------------------------------------------
# 4 - ANÁLISE EXPLORATÓRIA DOS DADOS - EDA
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Carregamento dos pacotes
# -----------------------------------------------------------------------------
library(tidyverse)
library(GGally)
library(broom)
library(forecast)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

# -----------------------------------------------------------------------------
# Carregamento das bases de dados
# Vazões: 83300200_Vazoes_Tratado.csv
# Precipitação: 2749041_Chuvas_Tratado.csv (chuvas)
# Metereológico: dados_ituporanga_processados.csv
# -----------------------------------------------------------------------------
vazoes <- read.csv("dados/83300200_Vazoes_Tratado.csv")
chuvas <- read.csv("dados/2749041_Chuvas_Tratado.csv")
meteoro <- read.csv("dados/dados_ituporanga_processados.csv")

# -----------------------------------------------------------------------------
# GRÁFICOS ESSENCIAIS
# Existem duas formas de plotar as séries de vazões e chuvas: separadas ou
# juntas (esse artifício foi criado apenas para gerar um gráfico de interesse
# para o relatório final)
# -----------------------------------------------------------------------------
# Vazões
# -----------------------------------------------------------------------------
graph_vazoes <- ts(vazoes$Vazao, start=c(1978, 5), frequency=365)
p1 <- autoplot(graph_vazoes) +
  labs(x="Data",
       y="Vazão (m3/s)",
       title="Série temporal de Vazões para a estação 83300200 (ANA)")

ggsave("figuras/4_ts_vazoes.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Precipitação
# -----------------------------------------------------------------------------
graph_chuvas <- ts(chuvas$Chuva, start=c(1978, 5), end=c(2025, 12), frequency=365)
p2 <- autoplot(graph_chuvas) +
  labs(x="Data",
       y="Precipitação (mm)",
       title="Série histórica de precipitações para a estação 2749041 (ANA)")

ggsave("figuras/4_ts_chuvas.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Vazões e Precipitação juntas
# 1. Certifique-se de que ambos os dataframes tenham uma coluna 'Data' em
# formato Date
# 2. Criar uma série de datas completa (de 1978 a 2025)
# 3. Fazer um 'left_join' para garantir que os dados fiquem no seu devido lugar
# temporal
# 4. Plotar com ggplot (geom_line lida naturalmente com NAs deixando lacunas)
# -----------------------------------------------------------------------------
vazoes <- vazoes %>% mutate(Data = as.Date(Data))
chuvas <- chuvas %>% mutate(Data = as.Date(Data))

periodo_completo <- data.frame(Data = seq(as.Date("1978-05-01"), as.Date("2025-12-31"), by="day"))

vazoes_final <- left_join(periodo_completo, vazoes, by = "Data")
chuvas_final <- left_join(periodo_completo, chuvas, by = "Data")

p1 <- ggplot(vazoes_final, aes(x = Data, y = Vazao)) +
  geom_line(color = "#00688B") +
  labs(y = "Vazão (m³/s)") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) # Remove o título do eixo X

p2 <- ggplot(chuvas_final, aes(x = Data, y = Chuva)) +
  geom_line(color = "#1874CD") +
  labs(y = "Precipitação (mm)") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) # Remove o título do eixo X
combinado <- p2 / p1
print(combinado)

ggsave("figuras/4_combinado.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Metereologia
# -----------------------------------------------------------------------------
# 1. Garanta que a coluna Data é realmente uma data
# Se estiver no formato "AAAA-MM-DD", use as.Date()
meteoro <- meteoro %>%
  mutate(Data = as.Date(Data))

# 2. Renomear e transformar para formato longo
# Certifique-se de que não haja NAs em excesso na data
meteoro_longo <- meteoro %>%
  rename(
    "Precipitação (mm)" = precipitacao_total,
    "Pressão (mB)" = pressao_atmosferica,
    "Temp. Máxima (°C)" = temperatura_maxima,
    "Temp. Média (°C)" = temperatura_media,
    "Temp. Mínima (°C)" = Temperatura_minima,
    "Umidade Rel. Média (%)" = umidade_relativa_media,
    "Umidade Rel. Mínima (%)" = umidade_relativa_minima,
    "Vento Máx. (m/s)" = vel_vento_max,
    "Vento Médio (m/s)" = vel_vento_med
  ) %>%
  pivot_longer(cols = -Data, names_to = "Variavel", values_to = "Valor")

# 3. Plotar corrigindo o grupo
ggplot(meteoro_longo, aes(x = Data, y = Valor, group = Variavel)) +
  geom_line(color = "darkblue", alpha = 0.7) +
  facet_wrap(~ Variavel, scales = "free_y", ncol = 3) + 
  labs(x = "Data", y = "Valor Medido") +
  theme_bw() + 
  theme(strip.text = element_text(size = 9, face = "bold"))

ggsave("figuras/4_ts_meteoro.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
summary(vazoes)
summary(chuvas)
summary(meteoro)

# -----------------------------------------------------------------------------
# Análise preliminar das séries temporais
# -----------------------------------------------------------------------------
autoplot(mstl(graph_vazoes)) +
  geom_line(color = "#00688B")
ggsave("figuras/4_ts_residuals.png", width = 8, height = 5, dpi = 300)
