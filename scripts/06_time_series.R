# -----------------------------------------------------------------------------
# 6 - REGRESSÃO EM FUNÇÃO DE VARIÁVEIS INDEPENDENTES TEMPORAIS
# -----------------------------------------------------------------------------
# Carregamento dos pacotes
# -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)

# ----------------------------------------------------------------------------
# Carrega as bases de dados
# ----------------------------------------------------------------------------
vazoes <- read.csv("dados/83300200_Vazoes_Tratado.csv")
chuvas <- read.csv("dados/2749041_Chuvas_Tratado.csv")
meteoro <- read.csv("dados/dados_ituporanga_processados.csv")

# ----------------------------------------------------------------------------
# Timestamp das datas
# ----------------------------------------------------------------------------
vazoes <- vazoes %>% mutate(Data = as.Date(Data))
chuvas <- chuvas %>% mutate(Data = as.Date(Data))
meteoro <- meteoro %>% mutate(Data = as.Date(Data))

# ----------------------------------------------------------------------------
# Preparação dos dados
# ----------------------------------------------------------------------------
# 1. Transformar o seu dataframe (ajuste o nome da sua coluna de data)
vazoes_regressao <- vazoes %>%
  mutate(
    year = factor(year(Data)),
    month = factor(month(Data)),
    weekday = factor(wday(Data)),
    day_of_year = yday(Data)
  )

# 2. Executar a regressão múltipla
# Adicionamos as variáveis temporais como fatores
modelo_vazao <- lm(Vazao ~ year + month + weekday, data = vazoes_regressao)
summary(modelo_vazao)

forecast::checkresiduals(modelo_vazao)
ggsave("figuras/6_modelo_vazao_res.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# SUAVIZAÇÃO EXPONENCIAL
# -----------------------------------------------------------------------------
# Defina os conjuntos baseado no ano
treino <- vazoes %>% filter(year(Data) <= 2022)
teste  <- vazoes %>% filter(year(Data) > 2022)

# Verificação (Confirme quantos registros ficaram em cada grupo)
cat("Registros em treino:", nrow(treino), "\n")
cat("Registros em teste:", nrow(teste), "\n")

vazoes_ts = ts(vazoes$Vazao, frequency=365, start=c(1978,6))
autoplot(vazoes_ts)

treino$Data=ymd(treino$Data)

# -----------------------------------------------------------------------------
# Ajuste do modelo de suavização
# -----------------------------------------------------------------------------
plot_ses <- function() {
  plot(
    vazoes_ts,
    type = "s",
    lwd = 2.5,
    col = "gray",
    ylab = expression("Vazão (" * m^3 / s * ")"),
    xlab = empty.dump()
  )
  
  lines(fitted(SES1), col = "blue", lwd = 2, lty = 2)
  lines(fitted(SES2), col = "red", lwd = 2, lty = 3)
  lines(fitted(SES), col = "green", lwd = 2, lty = 4)
  
  legend(
    "topleft",
    lwd = c(1.5, 2, 2, 2),
    lty = c(1, 2, 3, 4),
    col = c("gray", "blue", "red", "green"),
    legend = c(
      "Original",
      expression(alpha == 0.1),
      expression(alpha == 0.9),
      expression(alpha == "Auto")
    )
  )
}

plot_ses()  # mostra no RStudio

png(
  "figuras/06_SES.png",
  width = 8,
  height = 5,
  units = "in",
  res = 300
)

plot_ses()  # salva no arquivo
dev.off()

# -----------------------------------------------------------------------------
# PREVISÃO
# -----------------------------------------------------------------------------
# Altera a frequência dos dados
# -----------------------------------------------------------------------------
vazoes_mensal <- vazoes %>%
  mutate(
    AnoMes = floor_date(Data, "month")
  ) %>%
  group_by(AnoMes) %>%
  summarise(
    Vazao = mean(Vazao, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Ajusta o split treino e teste
# -----------------------------------------------------------------------------
treino <- vazoes_mensal %>%
  filter(year(AnoMes) <= 2022)

teste <- vazoes_mensal %>%
  filter(year(AnoMes) > 2022)

# -----------------------------------------------------------------------------
# Cria as séries temporais de treino e teste
# -----------------------------------------------------------------------------
treino_ts <- ts(
  treino$Vazao,
  start = c(year(min(treino$AnoMes)),
            month(min(treino$AnoMes))),
  frequency = 12
)

teste_ts <- ts(
  teste$Vazao,
  start = c(year(min(teste$AnoMes)),
            month(min(teste$AnoMes))),
  frequency = 12
)

# -----------------------------------------------------------------------------
# Cria os modelos
# -----------------------------------------------------------------------------
SES <- ses(treino_ts, h = length(teste_ts))

HOLT <- holt(treino_ts, h = length(teste_ts))

HW_ad <- hw(
  treino_ts,
  seasonal = "additive",
  h = length(teste_ts)
)

HW_mult <- hw(
  treino_ts,
  seasonal = "multiplicative",
  h = length(teste_ts)
)

ETS_model <- ets(treino_ts)

ETS <- forecast(
  ETS_model,
  h = length(teste_ts)
)

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
comparacao <- data.frame(
  Data = teste$AnoMes,
  Observado = teste$Vazao,
  SES = as.numeric(SES$mean),
  HOLT = as.numeric(HOLT$mean),
  HW_ad = as.numeric(HW_ad$mean),
  HW_mult = as.numeric(HW_mult$mean),
  ETS = as.numeric(ETS$mean)
)

ggplot(comparacao, aes(x = Data)) +
  geom_line(
    aes(y = Observado, color = "Observado"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = SES, color = "SES"),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = HOLT, color = "HOLT"),
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  geom_line(
    aes(y = HW_ad, color = "HW aditivo"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = HW_mult, color = "HW multiplicativo"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = ETS, color = "ETS"),
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Observado" = "black",
      "SES" = "blue",
      "HOLT" = "red",
      "HW aditivo" = "darkgreen",
      "HW multiplicativo" = "orange",
      "ETS" = "purple"
    )
  ) +
  labs(
    x = empty.dump(),
    y = expression("Vazão média mensal (" * m^3 / s * ")"),
    color = "Modelo"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggsave("figuras/6_comparacao.png", width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Métricas
# -----------------------------------------------------------------------------
mae <- function(yreal, yprev) {
  mean(abs(yreal - yprev), na.rm = TRUE)
}

metricas <- data.frame(
  Modelo = c(
    "SES",
    "HOLT",
    "HW_ad",
    "HW_mult",
    "ETS"
  ),
  MAE = c(
    mae(comparacao$Observado, comparacao$SES),
    mae(comparacao$Observado, comparacao$HOLT),
    mae(comparacao$Observado, comparacao$HW_ad),
    mae(comparacao$Observado, comparacao$HW_mult),
    mae(comparacao$Observado, comparacao$ETS)
  )
)

metricas

rmse <- function(yreal, yprev) {
  sqrt(mean((yreal - yprev)^2, na.rm = TRUE))
}

metricas <- data.frame(
  Modelo = c(
    "SES",
    "HOLT",
    "HW_ad",
    "HW_mult",
    "ETS"
  ),
  MAE = c(
    mae(comparacao$Observado, comparacao$SES),
    mae(comparacao$Observado, comparacao$HOLT),
    mae(comparacao$Observado, comparacao$HW_ad),
    mae(comparacao$Observado, comparacao$HW_mult),
    mae(comparacao$Observado, comparacao$ETS)
  ),
  RMSE = c(
    rmse(comparacao$Observado, comparacao$SES),
    rmse(comparacao$Observado, comparacao$HOLT),
    rmse(comparacao$Observado, comparacao$HW_ad),
    rmse(comparacao$Observado, comparacao$HW_mult),
    rmse(comparacao$Observado, comparacao$ETS)
  )
)

metricas
