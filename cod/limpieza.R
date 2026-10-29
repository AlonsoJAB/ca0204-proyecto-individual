# ==========================================================================
# limpieza.R
# Proyecto individual — CA-0204
# Autor: Alonso José Alpízar Brenes
# Carnet: C20274
# Fecha: octubre 2026
#
# Descripción: carga los resultados del Maratón de Berlín 2017–2019, pasa los
# tiempos a minutos y aplica los filtros de limpieza. Guarda la base limpia en
# data/berlin_limpio.rds para usarla en metodologia.R.
#
# Datos: Miller, A. (2021). marathon-results. GitHub.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)
library(here)   # rutas relativas desde la raíz del proyecto

# Carga de datos -----------------------------------------------------------
berlin_2017 <- read_csv(here("data", "berlin_2017.csv"))
berlin_2018 <- read_csv(here("data", "berlin_2018.csv"))
berlin_2019 <- read_csv(here("data", "berlin_2019.csv"))

# Se unen los tres años en una sola base (123 797 corredores)
berlin <- bind_rows(berlin_2017, berlin_2018, berlin_2019)

# Tiempos a minutos --------------------------------------------------------
# as.numeric() pasa hh:mm:ss a segundos; se divide entre 60
berlin <- berlin |>
  mutate(across(c(time_full, time_half, starts_with("split_")),
                \(x) as.numeric(x) / 60))

# Revisión de faltantes ----------------------------------------------------
# 0 corredores sin tiempo final: la base solo incluye a quienes terminaron
sum(is.na(berlin$time_full))

# 271 corredores con al menos un parcial faltante
berlin |>
  filter(rowSums(across(starts_with("split_"), is.na)) > 0) |>
  nrow()

# Limpieza -----------------------------------------------------------------
# Se eliminan los corredores con algún parcial faltante (123 526)
berlin_limpio <- berlin |>
  filter(rowSums(across(starts_with("split_"), is.na)) == 0)

# Cortes de exclusión
# - Nivel "altamente entrenado" o superior (McKay et al., 2022):
#   hombres < 2:27:54 (147.9 min) y mujeres < 2:50:41 (170.7 min)
# - Tiempo límite oficial del Maratón de Berlín: 6 h 15 min (375 min)
corte_hombres <- 147.9
corte_mujeres <- 170.7
tiempo_limite <- 375

# Porcentaje de corredores de alto nivel por sexo (H: 0.34 %, M: 0.42 %)
berlin_limpio |>
  group_by(gender) |>
  summarise(pct_alto_nivel = mean(if_else(gender == "M",
                                          time_full < corte_hombres,
                                          time_full < corte_mujeres)) * 100)

# Corredores que superaron el tiempo límite (1 568)
sum(berlin_limpio$time_full > tiempo_limite)

# Se aplican los cortes (121 512 corredores)
berlin_limpio <- berlin_limpio |>
  filter(!(gender == "M" & time_full < corte_hombres),
         !(gender == "W" & time_full < corte_mujeres),
         time_full <= tiempo_limite)

# Resumen de la muestra final ----------------------------------------------
nrow(berlin_limpio)                                     # 121 512
sum(berlin_limpio$nationality == "CRC", na.rm = TRUE)   # 709 costarricenses

# Exportación --------------------------------------------------------------
saveRDS(berlin_limpio, here("data", "berlin_limpio.rds"))