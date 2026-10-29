# ==========================================================================
# metodologia.R
# Proyecto individual — CA-0204 (UCR)
# Autor: Alonso José Alpízar Brenes
# Fecha: octubre 2026
#
# Descripción: a partir de la base limpia, calcula el ritmo medio, el ritmo
# de salida (0–10 km) y de cierre (35 km–meta), el ritmo relativo (RP) y
# clasifica a cada corredor según su estrategia. Guarda el resultado en
# data/berlin_metodologia.rds para usarlo en el informe.
# ==========================================================================

# Paquetes -----------------------------------------------------------------
library(tidyverse)
library(here)

# Carga de datos -----------------------------------------------------------
# Base generada por limpieza.R (121 512 corredores)
berlin_limpio <- readRDS(here("data", "berlin_limpio.rds"))

# Ritmos en min/km ---------------------------------------------------------
# rmc: ritmo medio de carrera; salida: 0–10 km; cierre: 35 km–meta (7.195 km)
berlin_limpio <- berlin_limpio |>
  mutate(rmc          = time_full / 42.195,
         ritmo_salida = split_10k / 10,
         ritmo_cierre = (time_full - split_35k) / 7.195)

# Ritmo relativo (RP) ------------------------------------------------------
# Positivo = tramo más rápido que su ritmo medio; negativo = más lento
berlin_limpio <- berlin_limpio |>
  mutate(rp_salida = 100 * (rmc - ritmo_salida) / rmc,
         rp_cierre = 100 * (rmc - ritmo_cierre) / rmc)

# Clasificación de estrategias ---------------------------------------------
# clasificar(): recibe un ritmo relativo y devuelve la estrategia:
# "Por encima" (> +5 %), "Igual" (±5 %) o "Por debajo" (< −5 %)
clasificar <- function(rp) {
  case_when(rp >  5 ~ "Por encima",
            rp < -5 ~ "Por debajo",
            TRUE    ~ "Igual")
}

berlin_limpio <- berlin_limpio |>
  mutate(salida = factor(clasificar(rp_salida),
                         levels = c("Por debajo", "Igual", "Por encima")),
         cierre = factor(clasificar(rp_cierre),
                         levels = c("Por debajo", "Igual", "Por encima")))

# Nivel del corredor según su tiempo final ---------------------------------
berlin_limpio <- berlin_limpio |>
  mutate(nivel = cut(time_full, breaks = c(180, 240, 300, 360),
                     labels = c("3–4 h", "4–5 h", "5–6 h")))

# Resumen ------------------------------------------------------------------
berlin_limpio |> count(salida)
berlin_limpio |> count(cierre)

# Exportación --------------------------------------------------------------
saveRDS(berlin_limpio, here("data", "berlin_metodologia.rds"))