# -----------------------------------------------------------------------------
# 7 - AUTOREGRESSÃO & ARIMA
# -----------------------------------------------------------------------------
# Carregamento dos pacotes
# -----------------------------------------------------------------------------
library(forecast)
library(ggplot2)
library(tseries)
library(zoo)

# ----------------------------------------------------------------------------
# Carrega as bases de dados
# ----------------------------------------------------------------------------
vazoes <- read.csv("dados/83300200_Vazoes_Tratado.csv")

# ----------------------------------------------------------------------------
# Timestamp das datas
# ----------------------------------------------------------------------------
vazoes <- vazoes %>% mutate(Data = as.Date(Data))

# ----------------------------------------------------------------------------
# Cria a série temporal
# ----------------------------------------------------------------------------
# vazoes_mensal <- vazoes %>%
#   mutate(Mes = floor_date(Data, "month")) %>%
#   group_by(Mes) %>%
#   summarise(
#     Vazao = mean(Vazao, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# vazoes_ts <- ts(
#   vazoes_mensal$Vazao,
#   frequency = 12,
#   start = c(
#     lubridate::year(min(vazoes_mensal$Mes)),
#     lubridate::month(min(vazoes_mensal$Mes))
#   )
# )

vazoes_ts <- ts(vazoes$Vazao, start = c(1978, 6), frequency = 365)

autoplot(vazoes_ts)
autoplot(mstl(vazoes_ts))

# ----------------------------------------------------------------------------
# Autocorrelação
# ----------------------------------------------------------------------------
png("figuras/7_lags.png", width = 800, height = 500, res = 100)
lag.plot(vazoes_ts, lags = 6)
dev.off()

acf(vazoes_ts)

png("figuras/7_PACF.png", width = 800, height = 500, res = 100)
pacf(vazoes_ts, main = empty.dump())
dev.off()

# ----------------------------------------------------------------------------
# Diferenciação da série não estacionária
# ----------------------------------------------------------------------------
tail(vazoes_ts,8)
tail(diff(vazoes_ts), 8)
tail(diff(vazoes_ts, 2), 8)

dif1 = diff(vazoes_ts)
autoplot(dif1)
lag.plot(dif1, lags = 9)
acf(dif1)

dif2 = diff(vazoes_ts, 2)
autoplot(dif2)
lag.plot(dif2, lags = 9)
acf(dif2)

d = ndiffs(vazoes_ts)

# ----------------------------------------------------------------------------
# Testando a estacionariedade
# ----------------------------------------------------------------------------
adf.test(vazoes_ts)
adf.test(dif1)

# ----------------------------------------------------------------------------
# Ajuste do modelo autorregressivo
# 1. Ajuste do modelo usando a série original (estacionária)
# O "d" (segundo parâmetro) deve ser 0 pois a série já é estacionária
# ----------------------------------------------------------------------------
modelo_ar1 <- arima(vazoes_ts, order = c(1, 0, 0)) 
summary(modelo_ar1)

forecast::checkresiduals(modelo_ar1)

forecast::accuracy(modelo_ar1)

previsao <- forecast(modelo_ar1, h = 730)
autoplot(previsao) +
  # Destacar a linha da previsão (a média)
  autolayer(previsao, series = "Previsão", PI = TRUE) +
  # Customizar as cores (ex: linha da previsão em vermelho)
  scale_color_manual(values = c("Data" = "gray", "Previsão" = "red")) +
  # Customizar o preenchimento da área de confiança (sombreamento)
  scale_fill_manual(values = c("Previsão" = "pink")) +
  theme_minimal() +
  labs(x = empty.dump(), 
       y = "Vazão (m³/s)")

ggsave("figuras/7_previsao.png", width = 8, height = 5, dpi = 300)

previsao$mean


# ----------------------------------------------------------------------------
# Avaliação das previsões
# ----------------------------------------------------------------------------
# Separar 365 dias para teste (o último ano do seu dataset)
# ----------------------------------------------------------------------------
n_treino <- length(vazoes$Vazao) - 365
treino_ts <- ts(vazoes$Vazao[1:n_treino], start = c(1978, 6), frequency = 365)
teste_reais <- vazoes$Vazao[(n_treino + 1):length(vazoes$Vazao)]

# Treinar o modelo APENAS com o conjunto de treino
modelo_treino <- arima(treino_ts, order = c(1, 0, 0))

# Fazer a previsão para o período que reservamos (h=365)
previsao_teste <- forecast(modelo_treino, h = 365)

# Comparação formal
metricas <- accuracy(previsao_teste, teste_reais)
print(metricas)

# Verifica se os resíduos são "ruído branco"
# Se o p-value do teste Ljung-Box for > 0.05, o modelo é bom
checkresiduals(modelo_treino)

# Criar um dataframe para plotagem comparativa
df_avaliar <- data.frame(
  Data = seq(as.Date("2024-01-01"), length.out = 365, by = "day"), # Ajuste a data inicial
  Real = teste_reais,
  Previsto = as.numeric(previsao_teste$mean)
)

ggplot(df_avaliar, aes(x = Data)) +
  geom_line(aes(y = Real, color = "Real"), size = 1) +
  geom_line(aes(y = Previsto, color = "Previsto"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "#00688B", "Previsto" = "red")) +
  theme_minimal() +
  labs(title = "Avaliação da Previsão: Real vs. Modelo AR(1)", y = "Vazão")

ggsave("figuras/7_AR1.png", width = 8,height = 5, dpi = 300)


# -----------------------------------------------------------------------------
# PREVISÃO DE VAZÃO (ARIMA vs SARIMA)
# -----------------------------------------------------------------------------
# 1. CARREGAR DADOS E CRIAR SÉRIE TEMPORAL
# vazoes_ts já deve estar carregado com frequency=365
# Aqui assumimos que você quer usar o último ano (2025) como teste
# Calcule o tamanho total da sua série
n_total <- length(vazoes_ts)

# Defina o tamanho do teste (ex: os últimos 365 dias)
n_teste <- 365
n_treino <- n_total - n_teste

# Crie os objetos de treino e teste usando índices numéricos
treino <- window(vazoes_ts, end = c(start(vazoes_ts)[1] + (n_treino / 365), n_treino %% 365))
# OU, mais simples ainda:
treino <- ts(vazoes_ts[1:n_treino], start = start(vazoes_ts), frequency = 365)
teste  <- ts(vazoes_ts[(n_treino + 1):n_total], start = c(2025, 1), frequency = 365)

# 2. AJUSTE DOS MODELOS
# O auto.arima procura a melhor estrutura de ordens (p,d,q)
mod.arima2 <- auto.arima(treino, seasonal = FALSE) 
mod.sarima2 <- auto.arima(treino, seasonal = TRUE) 

# 3. REALIZAR PREVISÕES
prev_arima <- forecast(mod.arima2, h = length(teste))
prev_sarima <- forecast(mod.sarima2, h = length(teste))

# 4. DIAGNÓSTICO DE RESÍDUOS
# Se o p-value > 0.05, os resíduos são ruído branco (modelo ideal)
checkresiduals(mod.arima2)
checkresiduals(mod.sarima2)

# 5. ANÁLISE GRÁFICA DO AJUSTE
autoplot(prev_sarima) +
  autolayer(prev_sarima$mean, series = "Previsto (SARIMA)", color = "blue") +
  autolayer(teste, series = "Real", linetype = "dashed", color = "black") +
  labs(title = "Previsão SARIMA vs Valores Reais (Vazão)", x = "Ano", y = "Vazão (m³/s)") +
  theme_minimal()

# 6. ACURÁCIA (EAM - Erro Absoluto Médio)
df_teste <- data.frame(
  Real = as.numeric(teste),
  Arima = as.numeric(prev_arima$mean),
  Sarima = as.numeric(prev_sarima$mean)
)

eam <- function(yreal, yprev) { mean(abs(yreal - yprev), na.rm = TRUE) }

cat("EAM ARIMA:", eam(df_teste$Real, df_teste$Arima), "\n")
cat("EAM SARIMA:", eam(df_teste$Real, df_teste$Sarima), "\n")




# Assumindo que você já tem 'vazoes' carregado e 'prev_sarima' calculado.

# Passo 1: Criar uma sequência completa de datas para o Zoom (2020-2025)
# Isso força a criação de 'NA' onde faltam dias no seu CSV original.
datas_completas <- data.frame(Data = seq(as.Date("2022-01-01"), as.Date("2025-03-31"), by = "day"))

# Passo 2: Mesclar com os dados reais para criar a falha visual
# O join garantirá que dias sem vazão fiquem como NA na coluna Vazao.
vazoes_com_falha <- left_join(datas_completas, vazoes, by = "Data")

# 1. DEFINIÇÃO DAS VARIÁVEIS FALTANTES
n_total <- nrow(vazoes_com_falha)
# Garantir que n_prev seja o tamanho correto da previsão
n_prev <- length(prev_sarima$mean)

# Passo 3: Preparar o DataFrame final para o ggplot
# Alinhando a previsão (que começa quando o treino termina, ex: 2025)
# Vamos supor que n_teste=365 para alinhar 'prev_sarima'
# 1. Ajuste do data.frame para garantir nomes consistentes
df_zoom <- data.frame(
  Data = vazoes_com_falha$Data,
  Real = vazoes_com_falha$Vazao,
  ARIMA = c(rep(NA, n_total - n_prev), as.numeric(prev_arima$mean)),
  SARIMA = c(rep(NA, n_total - n_prev), as.numeric(prev_sarima$mean))
)

# 2. Plotagem corrigida
ggplot(df_zoom, aes(x = Data)) +
  # Linha Real
  geom_line(aes(y = Real, color = "Real"), size = 0.7) +
  # Linhas de Previsão
  geom_line(aes(y = SARIMA, color = "SARIMA"), size = 1) +
  geom_line(aes(y = ARIMA, color = "ARIMA"), size = 1) +
  
  # Customização de Cores (corrigida para coincidir com as chaves do aes)
  scale_color_manual(values = c(
    "Real" = "#00688B", 
    "SARIMA" = "blue", 
    "ARIMA" = "red"
  )) +
  
  # Estrutura do Eixo X
  scale_x_date(
    date_labels = "%b\n%Y", 
    date_breaks = "3 months", 
    minor_breaks = "1 month"
  ) +
  
  # Formatação Geral
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  labs(
    title = empty.dump(),
    x = empty.dump(),
    y = "Vazão (m³/s)"
  )

ggsave("figuras/7_final.png", width = 12, height = 5, dpi = 300)
