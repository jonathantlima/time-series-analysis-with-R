# ----------------------------------------------------------------------------
# 5 - REGRESSÃO LINEAR
# ----------------------------------------------------------------------------
# Carregamento dos pacotes
# ----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)

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
# Juntar ('merge') vazões e precipitação (dados ANA)
# ----------------------------------------------------------------------------
vazoes_chuvas <- inner_join(vazoes, chuvas, by = "Data")

# ----------------------------------------------------------------------------
# Regressão linear simples
# ----------------------------------------------------------------------------
modelo_linear_simples <- lm(Vazao ~ Chuva, data = vazoes_chuvas)
summary(modelo_linear_simples)

ggplot(vazoes_chuvas, aes(x = Chuva, y = Vazao)) +
  geom_point(color = "#00688B", alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Precipitação (mm)", y = "Vazão (m³/s)")

ggsave("figuras/5_lm.png", width = 8, height = 5, dpi = 300)

# ----------------------------------------------------------------------------
# Juntar ('merge') total
# ----------------------------------------------------------------------------
base_completa <- vazoes_chuvas %>% inner_join(meteoro, by = "Data")

# ----------------------------------------------------------------------------
# Regressão linear múltipla
# ----------------------------------------------------------------------------
options(scipen = 999)
modelo.multiplo <- lm(Vazao ~ Chuva + pressao_atmosferica + temperatura_media
                      + umidade_relativa_media + vel_vento_med,
                      data = base_completa)

summary(modelo.multiplo)

# ----------------------------------------------------------------------------
# Análise dos resíduos
# ----------------------------------------------------------------------------
forecast::checkresiduals(modelo_linear_simples)
ggsave("figuras/5_lm_residuals.png", width = 8, height = 5, dpi = 300)

forecast::checkresiduals(modelo.multiplo)
ggsave("figuras/5_mm_residuals.png", width = 8, height = 5, dpi = 300)
