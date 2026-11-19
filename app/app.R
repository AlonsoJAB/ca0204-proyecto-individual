# ==========================================================================
# app/app.R
# Proyecto individual — CA-0204 (UCR)
# Autor: Alonso José Alpízar Brenes
# Fecha: noviembre 2026
#
# Descripción: aplicación Shiny con dos pestañas.
#   - Carrera virtual: una carrera representativa del Maratón de Berlín
#     2017–2019 (cada punto ≈ 95 corredores reales) que muestra en vivo quién
#     "choca con la pared" según cómo salió en los primeros 10 km. Al final
#     los porcentajes coinciden con los del informe y la presentación.
#   - Explorador de corredores: la carrera de un corredor real contra un
#     "fantasma" que corre todo el maratón a su ritmo medio.
#
# Cómo correrla: abrir Proyecto Individual.Rproj en RStudio y ejecutar
#   shiny::runApp("app")
# Usa los datos de data/ y la misma limpieza del informe (limpieza.R).
# La animación corre en el navegador (JavaScript), por eso se ve fluida.
# ==========================================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(here)

# Paleta "Berlín de día" (la misma de la presentación) -------------------
pal <- list(
  fondo   = "#F3F8FC",  # fondo de la página (cielo claro)
  panel   = "#FFFFFF",  # tarjetas y paneles
  borde   = "#D3E2EF",
  texto   = "#1E2B45",
  suave   = "#5B6B88",
  rapido  = "#E5484D",  # salieron más rápido (rojo del cordón)
  parejo  = "#2F80ED",  # salieron a su ritmo (línea azul del maratón)
  oro     = "#B07A1E",  # detalles (Puerta de Brandeburgo)
  violeta = "#3E9B5B",  # fantasma (verde Tiergarten)
  asfalto = "#4A5466",
  borde_pista = "#FFFFFF",
  parque  = "#BFE3C6",
  rio     = "#9CC8EE"
)
col_clase <- c("Por debajo" = "#9AA4B5", "Igual" = pal$parejo, "Por encima" = pal$rapido)
etq_clase <- c("Por debajo" = "Más lento que su promedio",
               "Igual"      = "A su ritmo (±5 %)",
               "Por encima" = "Más rápido que su promedio")

# 1. Datos y limpieza (igual que limpieza.R y metodologia.R) ----------------
a_min <- function(x) {
  p <- strsplit(x, ":", fixed = TRUE)
  vapply(p, function(v) if (length(v) == 3) sum(as.numeric(v) * c(60, 1, 1/60)) else NA_real_,
         numeric(1))
}

parciales <- c("split_5k", "split_10k", "split_15k", "split_20k",
               "split_25k", "split_30k", "split_35k", "split_40k")

clasif <- function(rp) {
  factor(case_when(rp > 5 ~ "Por encima", rp < -5 ~ "Por debajo", TRUE ~ "Igual"),
         levels = c("Por debajo", "Igual", "Por encima"))
}

berlin <- lapply(2017:2019, function(a) {
  read_csv(here("data", paste0("berlin_", a, ".csv")),
           col_types = cols(.default = "c"), show_col_types = FALSE)
}) |>
  bind_rows() |>
  mutate(across(c(time_full, time_half, all_of(parciales)), a_min)) |>
  filter(if_all(c(time_full, all_of(parciales)), ~ !is.na(.x))) |>
  filter(!(gender == "M" & time_full < 147.9),            # élite hombres
         !(gender %in% c("W", "F") & time_full < 170.7),  # élite mujeres
         time_full <= 375) |>                             # límite 6 h 15
  mutate(
    id      = row_number(),
    nombre  = paste(first_name, last_name),
    rmc     = time_full / 42.195,
    rp_sal  = 100 * (rmc - split_10k / 10) / rmc,
    rp_cie  = 100 * (rmc - (time_full - split_35k) / 7.195) / rmc,
    salida  = clasif(rp_sal),
    cierre  = clasif(rp_cie),
    nivel   = cut(time_full, c(180, 240, 300, 360),
                  labels = c("3–4 h", "4–5 h", "5–6 h")),
    caida   = 100 * ((time_full - time_half) - time_half) / time_half,
    pared   = !is.na(caida) & caida > 30
  )

# Para la simulación se dejan solo los corredores con parciales válidos: hay
# registros con parciales en 0:00:00 o fuera de orden (errores de la fuente),
# que harían aparecer corredores en medio de la ruta sin pasar por la salida.
tabla_t <- cbind(0, berlin$split_5k, berlin$split_10k, berlin$split_15k, berlin$split_20k,
                 berlin$split_25k, berlin$split_30k, berlin$split_35k, berlin$split_40k, berlin$time_full)
ritmos_tramo <- (tabla_t[, -1] - tabla_t[, -10]) /
  matrix(diff(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 42.195)), nrow(tabla_t), 9, byrow = TRUE)
berlin$parciales_ok <- apply(ritmos_tramo, 1, function(v) all(v >= 2.75 & v <= 20))
rm(tabla_t, ritmos_tramo)

# 2. Trazado aproximado del recorrido (lat, lon) ----------------------------
# Puntos de paso aproximados; el trazado se reescala a 42.195 km.
ruta <- tribble(
  ~lat,    ~lon,
  52.5141, 13.3640,  # salida: Straße des 17. Juni
  52.5145, 13.3501,  # Großer Stern
  52.5200, 13.3510,
  52.5250, 13.3420,  # Moabit
  52.5235, 13.3600,
  52.5240, 13.3700,
  52.5225, 13.3800,
  52.5240, 13.3880,
  52.5290, 13.4000,  # Torstraße
  52.5270, 13.4130,
  52.5170, 13.4400,  # Karl-Marx-Allee
  52.5180, 13.4220,
  52.5130, 13.4180,
  52.5070, 13.4250,
  52.4990, 13.4180,  # Kreuzberg
  52.4920, 13.4220,
  52.4870, 13.4250,  # Neukölln
  52.4890, 13.4080,
  52.4910, 13.3950,
  52.4920, 13.3750,
  52.4920, 13.3600,  # Schöneberg
  52.4830, 13.3500,
  52.4780, 13.3430,
  52.4700, 13.3300,
  52.4580, 13.3210,  # Steglitz
  52.4560, 13.2900,  # Dahlem
  52.4650, 13.2800,
  52.4850, 13.3000,
  52.4900, 13.3110,
  52.5000, 13.2950,
  52.5040, 13.3300,  # Kurfürstendamm
  52.5020, 13.3430,
  52.5060, 13.3650,
  52.5095, 13.3760,  # Potsdamer Platz
  52.5100, 13.3900,
  52.5130, 13.3900,
  52.5170, 13.3880,  # Unter den Linden
  52.5163, 13.3777,  # Brandenburger Tor
  52.5150, 13.3720   # meta
) |>
  mutate(x = (lon - 13.36) * cos(52.5 * pi / 180) * 111.32,   # km este-oeste
         y = (lat - 52.50) * 110.57)                          # km norte-sur

# Suavizar las esquinas del trazado (algoritmo de Chaikin)
chaikin <- function(x, y, veces = 3) {
  for (k in seq_len(veces)) {
    n <- length(x)
    qx <- 0.75 * x[-n] + 0.25 * x[-1]; rx <- 0.25 * x[-n] + 0.75 * x[-1]
    qy <- 0.75 * y[-n] + 0.25 * y[-1]; ry <- 0.25 * y[-n] + 0.75 * y[-1]
    x <- c(x[1], as.vector(rbind(qx, rx)), x[n])
    y <- c(y[1], as.vector(rbind(qy, ry)), y[n])
  }
  tibble(x = x, y = y)
}

a_xy <- function(df) {
  mutate(df, x = (lon - 13.36) * cos(52.5 * pi / 180) * 111.32,
         y = (lat - 52.50) * 110.57)
}

ruta <- chaikin(ruta$x, ruta$y) |>
  mutate(d = cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2))),
         km = d / max(d) * 42.195)

ruta_fina <- tibble(km = seq(0, 42.195, by = 0.05)) |>
  mutate(x = approx(ruta$km, ruta$x, km)$y,
         y = approx(ruta$km, ruta$y, km)$y)

posicion <- function(km) {
  tibble(x = approx(ruta_fina$km, ruta_fina$x, km, rule = 2)$y,
         y = approx(ruta_fina$km, ruta_fina$y, km, rule = 2)$y)
}

# Elementos de fondo (aproximados): río Spree y parques
spree <- tribble(
  ~lat, ~lon,
  52.5280, 13.2900, 52.5200, 13.3250, 52.5225, 13.3400, 52.5200, 13.3550,
  52.5215, 13.3700, 52.5190, 13.3790, 52.5205, 13.3920, 52.5175, 13.4020,
  52.5130, 13.4120, 52.5080, 13.4300, 52.5020, 13.4450, 52.4950, 13.4600
) |> a_xy()
spree <- chaikin(spree$x, spree$y, 3)

parques <- bind_rows(
  tribble(~lat, ~lon,
          52.5170, 13.3350, 52.5195, 13.3500, 52.5175, 13.3700, 52.5145, 13.3770,
          52.5095, 13.3720, 52.5085, 13.3500, 52.5110, 13.3370) |>
    mutate(parque = "Tiergarten"),
  tribble(~lat, ~lon,
          52.4800, 13.3900, 52.4785, 13.4100, 52.4680, 13.4100, 52.4660, 13.3900,
          52.4740, 13.3850) |>
    mutate(parque = "Tempelhofer Feld")
) |> a_xy()
etq_parques <- parques |> group_by(parque) |> summarise(x = mean(x), y = mean(y))

marcas_km <- posicion(seq(5, 40, 5)) |> mutate(etq = seq(5, 40, 5))

lugares <- tribble(
  ~km,  ~nombre,          ~dx,   ~dy,   ~hjust,
  6.5,  "Mitte",           0,     0.5,   0.5,
  10.0, "Friedrichshain",  0.2,   0.55,  0,
  15.5, "Kreuzberg",       0.35, -0.45,  0,
  23.5, "Schöneberg",     -0.45,  0,     1,
  28.5, "Steglitz",       -0.5,   0,     1,
  35.5, "Ku'damm",         0,    -0.6,   0.5
)
lugares <- bind_cols(lugares, posicion(lugares$km)) |> mutate(x = x + dx, y = y + dy)


km_parciales <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 42.195)
tiempos_de <- function(f) {
  c(0, f$split_5k, f$split_10k, f$split_15k, f$split_20k, f$split_25k,
    f$split_30k, f$split_35k, f$split_40k, f$time_full)
}

ritmo_txt <- function(v) {
  s <- as.integer(round(v * 60))
  sprintf("%d:%02d", s %/% 60L, s %% 60L)
}
reloj <- function(t) {
  s <- as.integer(round(t * 60))
  sprintf("%d:%02d:%02d", s %/% 3600L, (s %% 3600L) %/% 60L, s %% 60L)
}

# Kilómetro donde "muere" un corredor que chocó con la pared (2.ª mitad >30 %
# más lenta que la 1.ª): el primer tramo de 5 km después del km 20 cuyo ritmo
# es >30 % más lento que su ritmo de la 1.ª mitad (si no hay, el peor tramo).
# Kilómetro exacto (estimado) donde el corredor "choca con la pared": el punto
# donde su ritmo cruza 1.3 veces el ritmo de su primera mitad. Con parciales cada
# 5 km, se interpola el ritmo entre los puntos medios de cada tramo.
km_muerte <- function(df) {
  out <- rep(-1, nrow(df))
  i <- which(df$pared)
  if (length(i) == 0) return(out)
  d <- df[i, ]
  umbral <- 1.3 * d$time_half / 21.0975
  t <- cbind(d$split_15k, d$split_20k, d$split_25k, d$split_30k, d$split_35k, d$split_40k, d$time_full)
  largo <- diff(c(15, 20, 25, 30, 35, 40, 42.195))
  rit <- (t[, -1, drop = FALSE] - t[, -7, drop = FALSE]) / matrix(largo, nrow(t), 6, byrow = TRUE)
  medio <- c(17.5, 22.5, 27.5, 32.5, 37.5, 41.1)
  km <- vapply(seq_len(nrow(rit)), function(j) {
    r <- rit[j, ]; u <- umbral[j]
    k <- which(r[-1] > u)[1] + 1
    if (is.na(k)) return(medio[which.max(r[-1]) + 1])
    if (r[k - 1] >= u) return(medio[k - 1])
    medio[k - 1] + (u - r[k - 1]) / (r[k] - r[k - 1]) * (medio[k] - medio[k - 1])
  }, numeric(1))
  out[i] <- pmin(pmax(km, 20), 42)
  out
}

# Para los pocos corredores con parciales mal registrados (por ejemplo los 12
# que salieron lento y chocaron con la pared) se reconstruyen los parciales a
# partir de su media maratón y su tiempo total, a ritmo parejo en cada mitad.
reparar <- function(df) {
  h <- ifelse(is.na(df$time_half) | df$time_half <= 0, df$time_full / 2, df$time_half)
  f <- df$time_full
  en <- function(km) ifelse(km <= 21.0975, h * km / 21.0975, h + (f - h) * (km - 21.0975) / 21.0975)
  df$split_5k <- en(5);   df$split_10k <- en(10); df$split_15k <- en(15); df$split_20k <- en(20)
  df$split_25k <- en(25); df$split_30k <- en(30); df$split_35k <- en(35); df$split_40k <- en(40)
  df$time_half <- h
  df
}

# 3. La pista como SVG ---------------------------------------------------------
# Se dibuja una sola vez; el navegador solo mueve los puntos.
esc  <- 62                                   # píxeles por km
xmin <- min(ruta_fina$x) - 1.7; xmax <- max(ruta_fina$x) + 5.6
ymin <- min(ruta_fina$y) - 0.8; ymax <- max(ruta_fina$y) + 1.0
ancho <- round((xmax - xmin) * esc); alto <- round((ymax - ymin) * esc)
px <- function(x) (x - xmin) * esc
py <- function(y) (ymax - y) * esc
camino <- function(x, y, cerrar = FALSE) {
  paste0("M", paste(sprintf("%.1f,%.1f", px(x), py(y)), collapse = " L"),
         if (cerrar) " Z" else "")
}

# Zona de llegadas: los que terminan se ordenan ahí por orden de llegada
llegada <- list(x = ancho - 225, y = alto - 300, cols = 20, paso = 9)
zona_llegada <- function(id) {
  if (id != "pista1") return("")
  sprintf('<g><rect x="%.0f" y="%.0f" width="200" height="280" rx="14" fill="#FFFFFF" opacity="0.85"
           stroke="%s" stroke-opacity="0.5"/>
          <text x="%.0f" y="%.0f" class="llegadas">LLEGADAS</text>
          <text id="pista1-cuenta" x="%.0f" y="%.0f" class="llegadas-n">0</text></g>',
          llegada$x, llegada$y, pal$borde, llegada$x + 16, llegada$y + 26,
          llegada$x + 184, llegada$y + 26)
}
ruta_js_extra <- sprintf("window.LLEGADA = {x:%.0f, y:%.0f, cols:%d, paso:%d, filas:%d}; window.PISTA = {w:%d, h:%d};",
                         llegada$x + 15, llegada$y + 48, llegada$cols, llegada$paso, 24, ancho, alto)

svg_pista <- function(id) {
  d_ruta <- camino(ruta_fina$x, ruta_fina$y)
  inicio <- posicion(0)
  parques_svg <- lapply(split(parques, parques$parque), function(p)
    sprintf('<path d="%s" fill="%s" opacity="0.9"/>', camino(p$x, p$y, TRUE), pal$parque))
  etq_parq <- sprintf('<text x="%.1f" y="%.1f" class="parque">%s</text>',
                      px(etq_parques$x), py(etq_parques$y), etq_parques$parque)
  marcas <- sprintf(
    '<g><circle cx="%.1f" cy="%.1f" r="11" fill="%s" stroke="%s" stroke-width="1.5"/>
     <text x="%.1f" y="%.1f" class="km">%d</text></g>',
    px(marcas_km$x), py(marcas_km$y), pal$fondo, pal$oro,
    px(marcas_km$x), py(marcas_km$y) + 3.5, as.integer(marcas_km$etq))
  ancla <- c("0" = "start", "0.5" = "middle", "1" = "end")[as.character(lugares$hjust)]
  barrios <- sprintf('<text x="%.1f" y="%.1f" text-anchor="%s" class="barrio">%s</text>',
                     px(lugares$x), py(lugares$y) + 5, ancla, lugares$nombre)
  HTML(sprintf('
<svg id="%s" class="pista" viewBox="0 0 %d %d" preserveAspectRatio="xMidYMid meet">
  <defs>
    <radialGradient id="%s-bg" cx="55%%" cy="40%%" r="75%%">
      <stop offset="0%%" stop-color="#FFFFFF"/><stop offset="100%%" stop-color="#DCEEFA"/>
    </radialGradient>
    <filter id="%s-sombra" x="-10%%" y="-10%%" width="120%%" height="120%%">
      <feDropShadow dx="0" dy="6" stdDeviation="7" flood-color="#1E2B45" flood-opacity="0.25"/>
    </filter>
    <filter id="%s-brillo" x="-50%%" y="-50%%" width="200%%" height="200%%">
      <feGaussianBlur stdDeviation="2.2" result="b"/>
      <feMerge><feMergeNode in="b"/><feMergeNode in="SourceGraphic"/></feMerge>
    </filter>
    <pattern id="%s-cuadros" width="8" height="8" patternUnits="userSpaceOnUse">
      <rect width="8" height="8" fill="#fff"/><rect width="4" height="4" fill="#111"/>
      <rect x="4" y="4" width="4" height="4" fill="#111"/>
    </pattern>
  </defs>
  <rect width="100%%" height="100%%" rx="18" fill="url(#%s-bg)"/>
  %s %s
  <path d="%s" fill="none" stroke="%s" stroke-width="7" stroke-linecap="round" opacity="0.9"/>
  <text x="%.1f" y="%.1f" class="rio">Spree</text>
  <g filter="url(#%s-sombra)">
    <path d="%s" fill="none" stroke="%s" stroke-width="25" stroke-linecap="round" stroke-linejoin="round"/>
  </g>
  <path d="%s" fill="none" stroke="%s" stroke-width="21" stroke-linecap="round" stroke-linejoin="round"/>
  <path d="%s" fill="none" stroke="%s" stroke-width="1.4" stroke-dasharray="7 7" opacity="0.65"/>
  %s %s
  <rect x="%.1f" y="%.1f" width="18" height="18" fill="url(#%s-cuadros)" stroke="#fff" stroke-width="1.5" rx="2"/>
  <g transform="translate(%.1f,%.1f)">
    <text x="0" y="0" class="meta" fill="%s">SALIDA · META</text>
  </g>
  %s
  <g id="%s-puntos"></g>
</svg>',
               id, ancho, alto,
               id, id, id, id, id,
               paste(parques_svg, collapse = ""), paste(etq_parq, collapse = ""),
               camino(spree$x, spree$y), pal$rio,
               px(spree$x[5]) - 20, py(spree$y[5]) - 12,
               id, d_ruta, pal$borde_pista,
               d_ruta, pal$asfalto,
               d_ruta, "#FFFFFF",
               paste(marcas, collapse = ""), paste(barrios, collapse = ""),
               px(inicio$x) - 9, py(inicio$y) - 9, id,
               px(inicio$x) - 16, py(inicio$y) + 26, pal$oro,
               zona_llegada(id), id))
}

# Datos de la ruta para el navegador (km -> píxeles)
ruta_js <- sprintf("window.RUTA = {km:[%s], x:[%s], y:[%s]};",
                   paste(round(ruta_fina$km, 3), collapse = ","),
                   paste(round(px(ruta_fina$x), 1), collapse = ","),
                   paste(round(py(ruta_fina$y), 1), collapse = ","))

# 4. Animación en JavaScript ----------------------------------------------------
js <- r"---[
(function () {
  const KMS = [0, 5, 10, 15, 20, 25, 30, 35, 40, 42.195];
  function interp(xs, ys, x) {
    const n = xs.length;
    if (x <= xs[0]) return ys[0];
    if (x >= xs[n - 1]) return ys[n - 1];
    let lo = 0, hi = n - 1;
    while (hi - lo > 1) { const m = (lo + hi) >> 1; if (xs[m] <= x) lo = m; else hi = m; }
    const d = xs[hi] - xs[lo];
    return d > 0 ? ys[lo] + (x - xs[lo]) / d * (ys[hi] - ys[lo]) : ys[hi];
  }
  const kmEn = (tiempos, t) => interp(tiempos, KMS, t);
  // tabla rápida km -> (x, y) cada 10 m
  const PASO = 0.01, NT = Math.ceil(42.195 / PASO) + 1, TX = new Float32Array(NT), TY = new Float32Array(NT);
  for (let i = 0; i < NT; i++) { TX[i] = interp(RUTA.km, RUTA.x, i * PASO); TY[i] = interp(RUTA.km, RUTA.y, i * PASO); }
  const xy = km => { const i = Math.max(0, Math.min(NT - 1, Math.round(km / PASO))); return [TX[i], TY[i]]; };
  function reloj(t) {
    const s = Math.round(t * 60), h = Math.floor(s / 3600), m = Math.floor(s % 3600 / 60), ss = s % 60;
    return h + ":" + String(m).padStart(2, "0") + ":" + String(ss).padStart(2, "0");
  }
  const NS = "http://www.w3.org/2000/svg";

  // Motor de animación reutilizable
  function Animador(pref, alDibujar) {
    const a = { t: 0, maxT: 1, jugando: false, vel: 1, ultimo: null };
    const boton = document.getElementById(pref + "-play");
    const barra = document.getElementById(pref + "-barra");
    const relojEl = document.getElementById(pref + "-reloj");
    function dibujar() {
      relojEl.textContent = reloj(a.t);
      barra.value = a.t;
      barra.style.setProperty("--p", (100 * a.t / a.maxT) + "%");
      alDibujar(a.t);
    }
    function paso(ahora) {
      if (a.jugando) {
        if (a.ultimo !== null) a.t += (ahora - a.ultimo) / 1000 * (a.maxT / (pref === "c" ? 75 : 28)) * a.vel;
        a.ultimo = ahora;
        if (a.t >= a.maxT) { a.t = a.maxT; a.jugando = false; boton.innerHTML = "&#8635;"; }
        dibujar();
      }
      requestAnimationFrame(paso);
    }
    boton.addEventListener("click", () => {
      if (a.t >= a.maxT) a.t = 0;
      a.jugando = !a.jugando; a.ultimo = null;
      boton.innerHTML = a.jugando ? "&#10074;&#10074;" : "&#9654;";
      dibujar();
    });
    barra.addEventListener("input", () => { a.t = +barra.value; a.jugando = false; boton.innerHTML = "&#9654;"; dibujar(); });
    document.querySelectorAll("." + pref + "-vel").forEach(b => b.addEventListener("click", () => {
      a.vel = +b.dataset.v;
      document.querySelectorAll("." + pref + "-vel").forEach(o => o.classList.toggle("activo", o === b));
    }));
    a.reiniciar = function (maxT) {
      a.maxT = maxT; a.t = 0; a.jugando = false; barra.max = maxT;
      boton.innerHTML = "&#9654;"; dibujar();
    };
    requestAnimationFrame(paso);
    return a;
  }

  // ---------- Carrera virtual ----------
  // Los corredores de la carrera se dibujan en un <canvas> encima de la pista:
  // así se pueden soltar decenas de miles sin que se trabe.
  const COLC = { r: "#E5484D", p: "#2F80ED", l: "#9AA4B5" }, CRUZ = { r: "#E5484D", p: "#2F80ED", l: "#5B6B88" };
  let carrera = { corredores: [], jugando: false, caidas: [] };
  const BIN = 0.25, NB = Math.ceil(42.2 / BIN);
  const fmt = v => String(v).replace(/\B(?=(\d{3})+(?!\d))/g, " ");
  // colores en formato de píxel (ABGR) para dibujar miles de corredores rápido
  const PIX = { r: 0xFF4D48E5, p: 0xFFED802F, l: 0xFFB5A49A };
  let img = null, buf = null;
  function lienzo() {
    const cv = document.getElementById("pista1-canvas");
    const dpr = window.devicePixelRatio || 1, W = cv.clientWidth, H = cv.clientHeight;
    if (cv.width !== Math.round(W * dpr) || cv.height !== Math.round(H * dpr)) { cv.width = Math.round(W * dpr); cv.height = Math.round(H * dpr); img = null; }
    const ctx = cv.getContext("2d"), esc = W / PISTA.w;
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.clearRect(0, 0, cv.width, cv.height);
    ctx.setTransform(dpr * esc, 0, 0, dpr * esc, 0, 0);
    ctx.k = dpr * esc; ctx.cw = cv.width; ctx.ch = cv.height;
    return ctx;
  }
  // Se dibujan en orden aleatorio (los corredores vienen mezclados), así ningún
  // color tapa a los otros y la mezcla refleja cuántos hay de cada grupo.
  function pixeles(ctx, todos, rad) {
    if (!img || img.width !== ctx.cw || img.height !== ctx.ch) { img = ctx.createImageData(ctx.cw, ctx.ch); buf = new Uint32Array(img.data.buffer); }
    buf.fill(0);
    const k = ctx.k, d = Math.max(2, Math.round(2 * rad * k)), W = ctx.cw, H = ctx.ch;
    for (let i = 0; i < todos.length; i += 3) {
      const x0 = Math.round(todos[i] * k - d / 2), y0 = Math.round(todos[i + 1] * k - d / 2), col = todos[i + 2];
      if (x0 < 0 || y0 < 0 || x0 + d >= W || y0 + d >= H) continue;
      for (let yy = 0; yy < d; yy++) { const f = (y0 + yy) * W + x0; for (let xx = 0; xx < d; xx++) buf[f + xx] = col; }
    }
    ctx.putImageData(img, 0, 0);
  }
  function dibujarCarrera(t) {
    const C = carrera.corredores, N = C.length;
    if (!N) return;
    const ctx = lienzo(), muchos = N > 3000, rad = muchos ? 1.6 : N > 300 ? 2.7 : 4.2;
    const meta = { r: 0, p: 0, l: 0 }, n = { r: 0, p: 0, l: 0 }, caidos = { r: 0, p: 0, l: 0 };
    const capas = { r: [], p: [], l: [] }, cruces = [], llegadas = [], todos = [], zona = new Float32Array(NB);
    for (let i = 0; i < N; i++) {
      const c = C[i];
      let km = kmEn(c.t, t);
      const muerto = c.muere > 0 && km >= c.muere;
      if (muerto) km = c.muere;
      const llego = !muerto && t >= c.t[9];
      n[c.g] += c.w;
      if (muerto) {
        caidos[c.g] += c.w;
        zona[Math.min(NB - 1, Math.floor(c.muere / BIN))]++;
        if (!muchos) {
          const [x, y] = xy(km); cruces.push(x + c.dx, y + c.dy, c.g);
          if (!c.estabaMuerto && carrera.jugando) carrera.caidas.push({ x: x + c.dx, y: y + c.dy, ts: performance.now() });
        }
      } else if (llego) {
        meta[c.g] += c.w;
        if (c.puesto % carrera.porPunto === 0) llegadas.push(c.puesto / carrera.porPunto, c.g);
      } else {
        const [x, y] = xy(km); capas[c.g].push(x + c.dx, y + c.dy);
        if (muchos) todos.push(x + c.dx, y + c.dy, PIX[c.g]);
      }
      c.estabaMuerto = muerto;
    }
    // corredores en la pista
    ["l", "p", "r"].forEach(g => {
      const a = capas[g]; ctx.fillStyle = COLC[g];
      if (muchos) return;
      else {
        ctx.strokeStyle = "#FFFFFF"; ctx.lineWidth = 1;
        for (let k = 0; k < a.length; k += 2) { ctx.beginPath(); ctx.arc(a[k], a[k + 1], rad, 0, 6.2832); ctx.fill(); ctx.stroke(); }
      }
    });
    if (muchos) pixeles(ctx, todos, rad);
    // zona de la pared: franja roja alrededor de la ruta, más intensa donde chocan más
    if (muchos) {
      let pico = 0, total = 0;
      for (let b = 0; b < NB; b++) { total += zona[b]; if (zona[b] > zona[pico]) pico = b; }
      if (total > 0) {
        const off = carrera.off || (carrera.off = document.createElement("canvas"));
        if (off.width !== ctx.cw || off.height !== ctx.ch) { off.width = ctx.cw; off.height = ctx.ch; }
        const o = off.getContext("2d");
        o.setTransform(1, 0, 0, 1, 0, 0); o.clearRect(0, 0, off.width, off.height);
        o.setTransform(ctx.k, 0, 0, ctx.k, 0, 0); o.lineCap = "round"; o.lineJoin = "round";
        // primero lo más suave, al final lo más intenso (opaco, para que no se acumule)
        const orden = []; for (let b = 0; b < NB; b++) {
          const v = (zona[Math.max(0, b - 1)] + 2 * zona[b] + zona[Math.min(NB - 1, b + 1)]) / 4;
          if (v > 0) orden.push([b, Math.min(1, v / carrera.maxZona)]);
        }
        orden.sort((u, v) => u[1] - v[1]).forEach(([b, f]) => {
          const g = Math.round(255 - (255 - 72) * f), bl = Math.round(255 - (255 - 77) * f), r = Math.round(255 - (255 - 229) * f);
          o.strokeStyle = "rgb(" + r + "," + g + "," + bl + ")"; o.lineWidth = 28 + 22 * f;
          o.beginPath();
          for (let km = b * BIN; km <= (b + 1) * BIN + 1e-6; km += 0.05) { const [x, y] = xy(km); km === b * BIN ? o.moveTo(x, y) : o.lineTo(x, y); }
          o.stroke();
        });
        ctx.save(); ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.globalCompositeOperation = "destination-over"; ctx.globalAlpha = 0.6;
        ctx.drawImage(off, 0, 0); ctx.restore();
        carrera.etq = { km: (pico + 0.5) * BIN, total: total };
      }
    }
    // etiqueta de la zona con más choques
    if (muchos && carrera.etq) {
      const [x, y] = xy(carrera.etq.km), txt1 = "La pared: km " + carrera.etq.km.toFixed(1).replace(".0", ""),
            txt2 = fmt(carrera.etq.total) + " corredores chocaron";
      ctx.font = "bold 14px Outfit, Helvetica, Arial"; const w = Math.max(ctx.measureText(txt1).width, 150) + 20;
      const bx = x + 34, by = y - 30;
      ctx.fillStyle = "rgba(255,255,255,.95)"; ctx.strokeStyle = "#E5484D"; ctx.lineWidth = 1.5;
      ctx.beginPath(); ctx.roundRect ? ctx.roundRect(bx, by, w, 42, 9) : ctx.rect(bx, by, w, 42); ctx.fill(); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(x + 14, y); ctx.lineTo(bx, by + 21); ctx.stroke();
      ctx.fillStyle = "#E5484D"; ctx.textAlign = "left"; ctx.fillText(txt1, bx + 10, by + 18);
      ctx.fillStyle = "#5B6B88"; ctx.font = "11px Outfit, Helvetica, Arial"; ctx.fillText(txt2, bx + 10, by + 34);
    }
    // cruces de los que chocaron con la pared
    const tc = muchos ? 1.6 : N > 300 ? 3.2 : 4; ctx.lineWidth = muchos ? 0.9 : 2; ctx.lineCap = "round";
    ctx.globalAlpha = muchos ? 0.6 : 1;
    for (let k = 0; k < cruces.length; k += 3) {
      const x = cruces[k], y = cruces[k + 1]; ctx.strokeStyle = CRUZ[cruces[k + 2]];
      ctx.beginPath(); ctx.moveTo(x - tc, y - tc); ctx.lineTo(x + tc, y + tc); ctx.moveTo(x + tc, y - tc); ctx.lineTo(x - tc, y + tc); ctx.stroke();
    }
    ctx.globalAlpha = 1;
    // destello al caer
    const ahora = performance.now();
    carrera.caidas = carrera.caidas.filter(o => ahora - o.ts < 700);
    carrera.caidas.forEach(o => { const f = (ahora - o.ts) / 700; ctx.strokeStyle = "rgba(229,72,77," + (0.9 * (1 - f)) + ")";
      ctx.lineWidth = 2; ctx.beginPath(); ctx.arc(o.x, o.y, 5 + 15 * f, 0, 6.2832); ctx.stroke(); });
    // zona de llegadas: cada punto = porPunto corredores
    for (let k = 0; k < llegadas.length; k += 2) {
      const j = llegadas[k];
      const x = LLEGADA.x + (j % LLEGADA.cols) * LLEGADA.paso, y = LLEGADA.y + Math.floor(j / LLEGADA.cols) * LLEGADA.paso;
      ctx.fillStyle = COLC[llegadas[k + 1]]; ctx.beginPath(); ctx.arc(x, y, LLEGADA.r || 3.6, 0, 6.2832); ctx.fill();
    }
    if (carrera.porPunto * carrera.rep > 1) {
      ctx.fillStyle = "#5B6B88"; ctx.font = "11px Outfit, Helvetica, Arial"; ctx.textAlign = "left";
      ctx.fillText("cada punto = " + fmt(carrera.porPunto * carrera.rep) + " corredores", LLEGADA.x - 6, LLEGADA.y + 222);
    }
    const cuenta = document.getElementById("pista1-cuenta");
    if (cuenta) { cuenta.textContent = fmt(Math.round(meta.r + meta.p + meta.l)) + " / " + fmt(Math.round(n.r + n.p + n.l)); cuenta.style.fontSize = N > 99999 ? "8.5px" : N > 999 ? "10px" : ""; }
    // tarjetas de la pared (en vivo)
    ["l", "p", "r"].forEach(g => {
      const pc = 100 * caidos[g] / Math.max(1, n[g]);
      document.getElementById("w-" + g).textContent = (pc === 0 ? "0 %" : (pc < 1 ? pc.toFixed(2) : pc.toFixed(1)) + " %");
      document.getElementById("w-n-" + g).textContent = fmt(Math.round(caidos[g])) + " de " + fmt(Math.round(n[g])) + " corredores";
    });
  }

  // ---------- Explorador ----------
  let expl = null;
  function dibujarExplorador(t) {
    if (!expl) return;
    const kmR = kmEn(expl.t, t), kmF = Math.min(42.195, t / expl.rmc);
    const [xr, yr] = xy(kmR), [xf, yf] = xy(kmF);
    expl.real.setAttribute("cx", xr); expl.real.setAttribute("cy", yr);
    expl.fant.setAttribute("cx", xf); expl.fant.setAttribute("cy", yf);
    const dif = kmR - kmF;
    const dist = Math.abs(dif) < 1 ? Math.round(Math.abs(dif) * 1000) + " m" : Math.abs(dif).toFixed(1) + " km";
    document.getElementById("e-frase").innerHTML = t === 0
      ? "El <span class='c-v'>fantasma</span> corre todo el maratón a su ritmo medio"
      : (Math.abs(dif) < 0.01 ? "Van juntos" :
         "<span class='c-o'>" + expl.nombre + "</span> va " + dist + (dif > 0 ? " adelante" : " atrás") + " del <span class='c-v'>fantasma</span>");
  }

  $(function () {
    const anim1 = Animador("c", t => { carrera.jugando = anim1 && anim1.jugando; dibujarCarrera(t); });
    window.addEventListener("resize", () => dibujarCarrera(anim1.t));
    const anim2 = Animador("e", dibujarExplorador);

    Shiny.addCustomMessageHandler("carrera", function (m) {
      const N = m.g.length, muchos = N > 3000, disp = muchos ? 9 : 12;
      carrera.rep = m.rep || 1; const W = m.w || [];
      const C = new Array(N);
      for (let i = 0; i < N; i++) {
        const t = new Array(10); for (let k = 0; k < 10; k++) t[k] = m.t[i * 10 + k] / 60;
        C[i] = { g: m.g[i], t: t, muere: m.m[i], w: W[i] || 1, estabaMuerto: false,
                 dx: (Math.random() - 0.5) * disp, dy: (Math.random() - 0.5) * disp, puesto: -1 };
      }
      // puesto de llegada (solo entre los que terminan sin caer)
      const fin = []; for (let i = 0; i < N; i++) if (!(C[i].muere > 0)) fin.push(i);
      fin.sort((a, b) => C[a].t[9] - C[b].t[9]).forEach((i, k) => C[i].puesto = k);
      carrera.corredores = C; carrera.caidas = [];
      carrera.porPunto = Math.max(1, Math.ceil(fin.length / (LLEGADA.cols * LLEGADA.filas)));
      if (carrera.rep > 1) { carrera.porPunto = 1; LLEGADA.cols = 34; LLEGADA.paso = 5.4; LLEGADA.r = 2.2; LLEGADA.x0 = LLEGADA.x0 || LLEGADA.x; LLEGADA.x = LLEGADA.x0 - 7; } else { LLEGADA.cols = 20; LLEGADA.paso = 9; LLEGADA.r = 3.6; if (LLEGADA.x0) LLEGADA.x = LLEGADA.x0; }
      const z = new Float32Array(NB); C.forEach(c => { if (c.muere > 0) z[Math.min(NB - 1, Math.floor(c.muere / BIN))]++; });
      carrera.maxZona = Math.max(1, ...z); carrera.etq = null;
      anim1.reiniciar(m.maxT);
    });

    Shiny.addCustomMessageHandler("explorador", function (m) {
      const g = document.getElementById("pista2-puntos");
      g.innerHTML = "";
      const fant = document.createElementNS(NS, "circle");
      fant.setAttribute("r", 9); fant.setAttribute("class", "punto-fantasma");
      const real = document.createElementNS(NS, "circle");
      real.setAttribute("r", 9); real.setAttribute("class", "punto-real");
      g.appendChild(fant); g.appendChild(real);
      expl = { t: m.t, rmc: m.rmc, nombre: m.nombre, real: real, fant: fant };
      anim2.reiniciar(m.maxT);
    });
  });
})();
]---"

css <- sprintf(r"---[
@import url('https://fonts.googleapis.com/css2?family=Outfit:wght@400;600;800&display=swap');
body { background:linear-gradient(180deg,#EAF4FC 0%%,%1$s 320px); background-attachment:fixed; min-height:100vh; color:%2$s; font-family:'Outfit','Helvetica Neue',Arial,sans-serif; }
h4, label, .control-label { color:%2$s; }
.navbar-default { background:#FFFFFF; border:none; border-bottom:5px solid transparent;
  border-image: repeating-linear-gradient(90deg,%7$s 0 16px,#FFFFFF 16px 32px) 5; box-shadow:0 2px 10px rgba(30,43,69,.08); }
.navbar-default .navbar-brand { color:%2$s !important; font-weight:800; letter-spacing:.3px; }
.navbar-default .navbar-brand span { color:%8$s; }
.navbar-default .navbar-nav>li>a { color:%5$s !important; font-weight:600; }
.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover,
.navbar-default .navbar-nav>.active>a:focus { background:transparent !important; color:%8$s !important;
  box-shadow: inset 0 -3px 0 %8$s; }
.well { background:rgba(255,255,255,.92); border:1px solid %3$s; border-top:4px solid %8$s; border-radius:16px; box-shadow:0 4px 14px rgba(30,43,69,.08); }
.tarjeta { background:rgba(255,255,255,.92); box-shadow:0 4px 14px rgba(30,43,69,.08); border:1px solid %3$s; border-radius:16px; padding:14px 18px; margin-bottom:14px; }
.lienzo { background:rgba(255,255,255,.92); box-shadow:0 6px 20px rgba(30,43,69,.1); border:1px solid %3$s; border-radius:20px; padding:16px; }
.pista { width:100%%; height:auto; display:block; }
.pista text.km { fill:%4$s; font-size:10px; font-weight:800; text-anchor:middle; }
.pista text.barrio { fill:%5$s; font-size:15px; font-style:italic; }
.pista text.parque { fill:#3E9B5B; font-size:12px; font-style:italic; text-anchor:middle; }
.pista text.rio { fill:#2F6FB3; font-size:12px; font-style:italic; }
.pista text.meta { fill:%4$s; font-size:12px; font-weight:800; text-anchor:end; letter-spacing:1.5px; paint-order:stroke; stroke:#FFFFFF; stroke-width:4px; }
.punto-r { fill:%7$s; stroke:#FFFFFF; stroke-width:1; }
.punto-p { fill:%8$s; stroke:#FFFFFF; stroke-width:1; }
.punto-l { fill:#9AA4B5; stroke:#FFFFFF; stroke-width:1; }
.cruz-l { stroke:#5B6B88; stroke-width:2.4; stroke-linecap:round; opacity:.85; }
.cruz-r { stroke:%7$s; stroke-width:2.4; stroke-linecap:round; opacity:.85; }
.cruz-p { stroke:%8$s; stroke-width:2.4; stroke-linecap:round; opacity:.85; }
.pista text.llegadas { fill:%4$s; font-size:13px; font-weight:800; letter-spacing:2px; }
.pista text.llegadas-n { fill:%2$s; font-size:13px; font-weight:800; text-anchor:end; }
.onda { fill:none; stroke:%7$s; stroke-width:2; transform-box:fill-box; transform-origin:center;
  animation:onda .7s ease-out forwards; }
@keyframes onda { from { transform:scale(1); opacity:.9; } to { transform:scale(4); opacity:0; } }
.punto-real { fill:%4$s; stroke:#fff; stroke-width:2.5; filter:drop-shadow(0 2px 4px rgba(30,43,69,.4)); }
.punto-fantasma { fill:%9$s; fill-opacity:.55; stroke:%9$s; stroke-width:2; stroke-dasharray:3 3; }
.c-r { color:%7$s; } .c-p { color:%8$s; } .c-v { color:%9$s; } .c-o { color:%4$s; }
.reloj { font-size:44px; font-weight:800; letter-spacing:1px; font-variant-numeric:tabular-nums; line-height:1; }
.frase { color:%5$s; font-size:17px; margin-top:6px; min-height:24px; }
.chico { color:%5$s; font-size:13px; }
.grande { font-size:34px; font-weight:800; line-height:1.1; }
.controles { display:flex; align-items:center; gap:14px; margin-top:12px; }
.play { width:48px; height:48px; border-radius:50%%; border:none; font-size:18px; color:#FFFFFF;
  background:%8$s; box-shadow:0 6px 18px rgba(47,128,237,.3); flex:none; }
.play:hover { filter:brightness(1.1); }
input.barra { -webkit-appearance:none; appearance:none; flex:1; height:6px; border-radius:3px;
  background:linear-gradient(90deg,%8$s var(--p,0%%),%3$s var(--p,0%%)); outline:none; }
input.barra::-webkit-slider-thumb { -webkit-appearance:none; width:16px; height:16px; border-radius:50%%; background:#fff; border:2px solid %8$s; cursor:pointer; }
input.barra::-moz-range-thumb { width:16px; height:16px; border-radius:50%%; background:#fff; border:2px solid %8$s; }
.vel { background:transparent; border:1px solid %3$s; color:%5$s; border-radius:20px; padding:4px 10px; font-weight:600; }
.vel.activo { border-color:%8$s; color:%8$s; background:#EAF2FD; }
.btn-lindo { border:none; border-radius:12px; font-weight:700; color:#FFFFFF !important; padding:9px 16px;
  background:%8$s; box-shadow:0 4px 12px rgba(47,128,237,.25); }
.btn-lindo:hover, .btn-lindo:focus { filter:brightness(1.08); }
.fila-grupo { margin:10px 0 4px; display:flex; justify-content:space-between; font-weight:700; }
.pista-mini { height:8px; background:%3$s; border-radius:4px; overflow:hidden; }
.pista-mini div { height:100%%; border-radius:4px; transition:width .1s linear; }
.irs--shiny .irs-bar { background:%8$s; border-color:%8$s; }
.irs--shiny .irs-single { background:%8$s; color:#FFFFFF; }
.irs--shiny .irs-line { background:%3$s; border-color:%3$s; }
.irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text { color:%5$s; background:transparent; }
.radio-inline, .radio label { color:%2$s; }
.selectize-input, .selectize-dropdown { background:#FFFFFF !important; color:%2$s !important; border-color:%3$s !important; }
.selectize-input input { color:%2$s !important; }
.selectize-dropdown .active { background:%3$s !important; color:%2$s !important; }
.leyenda { display:flex; gap:18px; justify-content:center; margin-top:10px; color:%5$s; }
.leyenda i { display:inline-block; width:12px; height:12px; border-radius:50%%; margin-right:6px; vertical-align:-1px; }
.recalculating { opacity:1 !important; }
.pista-wrap { display:flex; justify-content:center; }
.pista-in { position:relative; width:min(100%%, calc((100vh - 200px) * %10$s)); }
.pista-in canvas { position:absolute; left:0; top:0; width:100%%; height:100%%; pointer-events:none; }
.controles-carrera .btn-lindo { padding:7px 12px; font-size:14px; }
.controles-carrera .irs { margin-top:-4px; }
.pared-tit { margin:0 0 6px !important; font-size:15px !important; }
.pared-card { padding:7px 11px !important; margin-bottom:7px !important; }
.pared-card .num { font-size:30px !important; min-width:74px !important; }
.btn-todos { background:#1E2B45 !important; box-shadow:0 4px 12px rgba(30,43,69,.25) !important; }
.pared-tit { font-weight:800; font-size:16px; margin:2px 0 10px; }
.pared-card { display:grid; grid-template-columns:auto 1fr; column-gap:12px; align-items:center;
  padding:9px 12px !important; margin-bottom:9px !important; }
.pared-card .etq { grid-column:1 / 3; font-weight:800; font-size:14px; }
.pared-card .num { grid-row:2 / 4; font-size:34px; font-weight:800; line-height:1; font-variant-numeric:tabular-nums; min-width:84px; }
.pared-card .chico { font-size:11.5px; line-height:1.25; }
.pared-card .real { grid-column:1 / 3; margin-top:5px; font-size:11px; color:%5$s; border-top:1px solid %3$s; padding-top:5px; line-height:1.3; }
.controles-carrera { padding:10px 14px !important; }
.controles-carrera .form-group { margin-bottom:6px; }
/* que la carrera entre en una pantalla */
.navbar { margin-bottom:12px; }
.lienzo.carrera { padding:10px 14px; }
.lienzo.carrera .reloj { font-size:34px; }
.lienzo.carrera .frase { font-size:14px; margin-top:2px; min-height:20px; }
.lienzo.carrera .pista { width:100%%; height:auto; display:block; }
.lienzo.carrera .controles { margin-top:8px; }
.lienzo.carrera .leyenda { margin-top:6px; font-size:13px; }
.well { padding:14px 16px; margin-bottom:12px; }
.tarjeta { margin-bottom:12px; }
.pista text.km { fill:%2$s; }
hr { border-color:%3$s; }
]---", pal$fondo, pal$texto, pal$borde, pal$oro, pal$suave, pal$panel,
pal$rapido, pal$parejo, pal$violeta, sprintf("%.4f", ancho / alto))

controles <- function(pref) {
  div(class = "controles",
      tags$button(id = paste0(pref, "-play"), class = "play", HTML("&#9654;")),
      tags$input(id = paste0(pref, "-barra"), class = "barra", type = "range",
                 min = 0, max = 1, step = 0.1, value = 0),
      tags$button(class = paste("vel activo", paste0(pref, "-vel")), `data-v` = 1, "1×"),
      tags$button(class = paste("vel", paste0(pref, "-vel")), `data-v` = 2, "2×"),
      tags$button(class = paste("vel", paste0(pref, "-vel")), `data-v` = 4, "4×"))
}


tarjeta_pared <- function(g, titulo, color) {
  div(class = "tarjeta pared-card", style = sprintf("border-left:5px solid %s;", color),
      div(class = "etq", titulo),
      div(class = "num", id = paste0("w-", g), style = sprintf("color:%s;", color), "0 %"),
      div(class = "chico", "chocó con la pared"),
      div(class = "chico", id = paste0("w-n-", g), ""),
      div(class = "real", uiOutput(paste0("real_", g), inline = TRUE)))
}

# 5. Interfaz ------------------------------------------------------------------
ui <- navbarPage(
  title = HTML("Maratón de Berlín <span>2017–2019</span>"),
  header = tags$head(tags$style(HTML(css)), tags$script(HTML(ruta_js)), tags$script(HTML(ruta_js_extra)), tags$script(HTML(js))),
  
  tabPanel("Carrera virtual",
           fluidRow(
             column(3,
                    div(class = "pared-tit", "¿Quién choca con la pared?"),
                    tarjeta_pared("l", "Salieron más lento", "#5B6B88"),
                    tarjeta_pared("p", "Salieron a su ritmo", pal$parejo),
                    tarjeta_pared("r", "Salieron más rápido", pal$rapido)
             ),
             column(9,
                    div(class = "lienzo carrera",
                        div(style = "text-align:center;",
                            div(class = "reloj", id = "c-reloj", "0:00:00")),
                        div(class = "pista-wrap",
                            div(class = "pista-in",
                                HTML(as.character(svg_pista("pista1"))),
                                tags$canvas(id = "pista1-canvas"))),
                        controles("c"),
                        div(class = "leyenda",
                            span(tags$i(style = sprintf("background:%s", pal$rapido)), "Salieron más rápido"),
                            span(tags$i(style = sprintf("background:%s", pal$parejo)), "A su ritmo (±5 %)"),
                            span(tags$i(style = "background:#9AA4B5"), "Salieron más lento")))
             )
           )
  ),
  
  tabPanel("Explorador de corredores",
           fluidRow(
             column(3,
                    wellPanel(
                      h4(style = "font-weight:800;", "Elegí un corredor"),
                      actionButton("azar", "Corredor al azar", icon = icon("dice"), class = "btn-lindo"),
                      br(), br(),
                      selectizeInput("buscar", "o buscalo por nombre", choices = NULL,
                                     options = list(placeholder = "Escribí un nombre…"))
                    ),
                    uiOutput("ficha")
             ),
             column(6,
                    div(class = "lienzo",
                        div(style = "text-align:center;",
                            div(class = "reloj", id = "e-reloj", "0:00:00"),
                            div(class = "frase", id = "e-frase", "")),
                        HTML(as.character(svg_pista("pista2"))),
                        controles("e"),
                        div(class = "leyenda",
                            span(tags$i(style = sprintf("background:%s", pal$oro)), textOutput("e_nombre", inline = TRUE)),
                            span(tags$i(style = sprintf("background:%s;opacity:.6", pal$violeta)), "Fantasma a ritmo parejo")))
             ),
             column(3,
                    div(class = "lienzo", plotOutput("ritmos", height = "520px")))
           )
  )
)

# 6. Servidor ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ---------- Carrera virtual ----------
  # Carrera representativa: cada punto representa ~95 corredores reales.
  # Se eligen corredores reales en cada grupo (cómo salieron × si chocaron con
  # la pared), en proporción a su tamaño, para que los porcentajes de la
  # carrera coincidan con los de todos los datos.
  por_punto <- 95
  # Carrera representativa: cada punto ≈ 95 corredores reales. Se eligen puntos
  # en cada grupo (cómo salieron × si chocaron con la pared) y cada punto lleva un
  # "peso" = corredores que representa. Así los porcentajes de la carrera son
  # exactamente los de todos los datos (los mismos de la presentación).
  grupo <- reactive({
    berlin |>
      group_by(salida, pared) |>
      group_modify(function(df, k) {
        n <- max(1, round(nrow(df) / por_punto))
        ok <- df |> filter(parciales_ok) |> arrange(time_full)
        fuente <- if (nrow(ok) >= 1) ok else reparar(arrange(df, time_full))
        idx <- ceiling(((seq_len(n) - 1) + runif(n)) / n * nrow(fuente))   # repartidos en todos los tiempos
        mutate(fuente[idx, ], peso = nrow(df) / n)
      }) |>
      ungroup() |>
      slice_sample(prop = 1)
  })

  observe({
    g <- grupo()
    tiempos <- cbind(0, g$split_5k, g$split_10k, g$split_15k, g$split_20k, g$split_25k,
                     g$split_30k, g$split_35k, g$split_40k, g$time_full)
    session$sendCustomMessage("carrera", list(
      maxT = max(g$time_full) + 3, rep = por_punto, w = g$peso,
      g = unname(c("Por debajo" = "l", "Igual" = "p", "Por encima" = "r")[as.character(g$salida)]),
      t = as.integer(round(as.vector(t(tiempos)) * 60)),    # segundos, 10 por corredor
      m = km_muerte(g)))
  })

  resumen <- reactive({
    berlin |>
      group_by(salida) |>
      summarise(n = n(), chocaron = sum(pared), pared = 100 * mean(pared), .groups = "drop")
  })

  real_txt <- function(clase) {
    renderUI({
      r <- resumen() |> filter(salida == clase)
      HTML(sprintf("Con todos los datos: <b>%s</b> (%s de %s)",
                   ifelse(r$pared < 1, sprintf("%.2f %%", r$pared), sprintf("%.1f %%", r$pared)),
                   format(r$chocaron, big.mark = " "), format(r$n, big.mark = " ")))
    })
  }
  output$real_l <- real_txt("Por debajo")
  output$real_p <- real_txt("Igual")
  output$real_r <- real_txt("Por encima")

  # ---------- Explorador ----------
  updateSelectizeInput(session, "buscar", server = TRUE, selected = character(0),
                       choices = setNames(berlin$id, paste0(berlin$nombre, " (", berlin$nationality,
                                                            ", ", berlin$year, ")")))
  # Empieza con Anita Eggert, el ejemplo de la presentación
  anita <- berlin$id[berlin$nombre == "Anita Eggert" & berlin$year == "2017"]
  elegido <- reactiveVal(if (length(anita) == 1) anita else sample(berlin$id, 1))
  observeEvent(input$azar, elegido(sample(berlin$id, 1)))
  observeEvent(input$buscar, if (nzchar(input$buscar)) elegido(as.integer(input$buscar)))
  corredor <- reactive(berlin |> filter(id == elegido()))
  
  observe({
    c1 <- corredor()
    session$sendCustomMessage("explorador", list(
      t = tiempos_de(c1), rmc = c1$rmc, nombre = c1$nombre,
      maxT = max(c1$time_full, 42.195 * c1$rmc) + 3))
  })
  
  output$e_nombre <- renderText(corredor()$nombre)
  
  output$ficha <- renderUI({
    c1 <- corredor()
    nivel <- ifelse(is.na(c1$nivel), ifelse(c1$time_full < 180, "menos de 3 h", "más de 6 h"),
                    as.character(c1$nivel))
    fila <- function(etq, valor, color = pal$texto) {
      div(style = "display:flex;justify-content:space-between;margin:5px 0;",
          span(class = "chico", etq), span(style = sprintf("color:%s;font-weight:700;", color), valor))
    }
    div(class = "tarjeta",
        div(style = sprintf("font-size:24px;font-weight:800;color:%s;", pal$oro), c1$nombre),
        div(class = "chico", sprintf("%s · Berlín %s · nivel %s", c1$nationality, c1$year, nivel)),
        hr(style = "margin:10px 0;"),
        fila("Tiempo final", reloj(c1$time_full)),
        fila("Ritmo medio", paste(ritmo_txt(c1$rmc), "/km")),
        fila("Salida (0–10 km)", sprintf("%+.1f %%", c1$rp_sal), col_clase[as.character(c1$salida)]),
        div(class = "chico", style = "text-align:right;", etq_clase[as.character(c1$salida)]),
        fila("Cierre (35 km–meta)", sprintf("%+.1f %%", c1$rp_cie), col_clase[as.character(c1$cierre)]),
        div(class = "chico", style = "text-align:right;", etq_clase[as.character(c1$cierre)]),
        fila("Caída 2.ª mitad",
             ifelse(is.na(c1$caida), "sin dato", sprintf("%+.1f %%", c1$caida)),
             ifelse(isTRUE(c1$pared), pal$rapido, pal$texto)),
        div(class = "chico", style = "text-align:right;",
            ifelse(is.na(c1$caida), "", ifelse(c1$pared, "chocó con la pared", "no chocó con la pared")))
    )
  })
  
  output$ritmos <- renderPlot({
    c1 <- corredor()
    tiempos <- tiempos_de(c1)
    tramos <- tibble(km = (head(km_parciales, -1) + tail(km_parciales, -1)) / 2,
                     ritmo = diff(tiempos) / diff(km_parciales))
    ggplot(tramos, aes(km, ritmo)) +
      annotate("rect", xmin = 0, xmax = 10, ymin = -Inf, ymax = Inf, fill = pal$parejo, alpha = .07) +
      annotate("rect", xmin = 35, xmax = 42.195, ymin = -Inf, ymax = Inf, fill = pal$parejo, alpha = .07) +
      geom_hline(yintercept = c1$rmc, colour = pal$violeta, linetype = "dashed", linewidth = 0.8) +
      annotate("text", x = 1, y = c1$rmc, label = "ritmo medio", colour = pal$violeta,
               vjust = 1.7, hjust = 0, fontface = "bold", size = 4.5) +
      geom_line(colour = pal$oro, linewidth = 1.4) +
      geom_point(colour = pal$oro, fill = pal$panel, shape = 21, size = 3, stroke = 1.4) +
      scale_y_continuous(labels = ritmo_txt) +
      scale_x_continuous(breaks = seq(0, 40, 10)) +
      labs(x = "Kilómetro", y = "Ritmo (min/km) · arriba = más lento", title = "Ritmo cada 5 km") +
      theme_minimal(base_size = 14) +
      theme(plot.background = element_rect(fill = pal$panel, colour = NA),
            panel.background = element_rect(fill = pal$panel, colour = NA),
            panel.grid.major = element_line(colour = pal$borde),
            panel.grid.minor = element_blank(),
            axis.text = element_text(colour = pal$suave),
            axis.title = element_text(colour = pal$suave),
            plot.title = element_text(face = "bold", colour = pal$texto))
  }, bg = pal$panel)

}

shinyApp(ui, server)
