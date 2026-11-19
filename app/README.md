# app/

Aplicación Shiny del proyecto: **Maratón de Berlín 2017–2019**.

| Archivo | Qué es |
|:--|:--|
| `app.R` | Toda la aplicación (interfaz, servidor y la animación en JavaScript). Lee los CSV de `../data/` y aplica la misma limpieza del informe. |

## Pestañas

- **Carrera virtual:** una carrera representativa en la ruta del maratón. Cada punto es un corredor real y representa a unos 95 corredores. Los colores indican cómo salió en los primeros 10 km (rojo: más rápido que su ritmo medio; azul: a su ritmo, ±5 %; gris: más lento). Quien choca con la pared cae con una ✕ en el kilómetro estimado donde se derrumbó. A la izquierda se va llenando "¿Quién choca con la pared?", que al final coincide con los porcentajes del informe y la presentación (1.2 %, 0.02 % y 12.4 %).
- **Explorador de corredores:** busca un corredor por nombre (o uno al azar) y lo hace correr contra un "fantasma" a su ritmo medio, con su ficha y su ritmo cada 5 km.

## Cómo correrla

1. Abrir `Proyecto Individual.Rproj` en RStudio.
2. En la consola:
   ```r
   shiny::runApp("app")
   ```
3. Darle play a la carrera (velocidades 1×, 2× y 4×).

Paquetes: `shiny`, `dplyr`, `readr`, `ggplot2` y `here` (ver `config/paquetes.txt`).
