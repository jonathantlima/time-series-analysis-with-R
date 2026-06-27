###############################################################
# Localização das estações pluviométricas próximas
###############################################################

library(readr)
library(dplyr)
library(sf)
library(ggplot2)

#--------------------------------------------------------------
# Coordenadas da estação fluviométrica
#--------------------------------------------------------------

fluvio <- tibble(
  
  codigo = "83300200",
  
  nome = "Rio do Sul",
  
  lon = -49.6438,
  
  lat = -27.2146
  
)

fluvio_sf <- st_as_sf(
  fluvio,
  coords = c("lon","lat"),
  crs = 4326
)

#--------------------------------------------------------------
# Cadastro das estações pluviométricas
#--------------------------------------------------------------

estacoes <- read_csv(
  "dados/Estacoes_Pluviometricas.csv",
  show_col_types = FALSE
)

# Espera-se as colunas:
#
# Codigo
# Nome
# Latitude
# Longitude

estacoes_sf <- st_as_sf(
  estacoes,
  coords = c("Longitude","Latitude"),
  crs = 4326
)

#--------------------------------------------------------------
# Distâncias
#--------------------------------------------------------------

distancias <- st_distance(
  estacoes_sf,
  fluvio_sf
)

estacoes$Distancia_km <-
  as.numeric(distancias)/1000

#--------------------------------------------------------------
# Seleciona as 10 mais próximas
#--------------------------------------------------------------

proximas <-
  estacoes |>
  arrange(Distancia_km) |>
  slice(1:10)

print(proximas)

#--------------------------------------------------------------
# Salva CSV
#--------------------------------------------------------------

write_csv(
  proximas,
  "resultados/estacoes_proximas.csv"
)

#--------------------------------------------------------------
# Mapa
#--------------------------------------------------------------

ggplot() +
  
  geom_sf(
    data = estacoes_sf,
    color = "gray70",
    size = 1
  ) +
  
  geom_sf(
    data = st_as_sf(
      proximas,
      coords = c("Longitude","Latitude"),
      crs = 4326
    ),
    color = "blue",
    size = 3
  ) +
  
  geom_sf(
    data = fluvio_sf,
    color = "red",
    size = 4
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Estações pluviométricas próximas",
    subtitle = "Estação fluviométrica 83300200"
  )

ggsave(
  "figuras/mapa_estacoes.png",
  width = 8,
  height = 6,
  dpi = 300
)