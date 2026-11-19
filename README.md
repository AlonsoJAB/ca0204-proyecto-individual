# ¿Cuál es la mejor estrategia para correr el Maratón de Berlín?

Proyecto individual del curso CA-0204 Herramientas de Ciencia de Datos I (UCR, II-2026).
Autor: Alonso José Alpízar Brenes

Análisis de las estrategias de ritmo en el Maratón de Berlín 2017–2019 (121 512 corredores). Se compara el ritmo de cada corredor en los primeros 10 km (salida) y en los últimos 7.195 km (cierre) con su propio ritmo medio total, según su nivel, y se estudia quién "choca con la pared" (segunda mitad más de 30 % más lenta que la primera).

## Estructura

| Carpeta | Contenido |
|:--|:--|
| `cod/` | Código del análisis: limpieza, metodología e informe |
| `presentacion/` | Presentación en Quarto (revealjs), su estilo e imágenes |
| `app/` | Aplicación Shiny (carrera virtual y explorador de corredores) |
| `data/` | Datos crudos del Maratón de Berlín 2017–2019 (Miller, 2021) |
| `res/` | Informe final en PDF |
| `info/` | Rúbrica del proyecto |
| `config/` | Paquetes de R necesarios |

Cada carpeta tiene su propio `README.md` con el detalle de sus archivos.

## Cómo reproducirlo

1. Abrir `Proyecto Individual.Rproj` en RStudio (todas las rutas usan `here()` desde la raíz del proyecto).
2. Instalar los paquetes de `config/paquetes.txt`:
   ```r
   install.packages(c("tidyverse", "here", "knitr", "rmarkdown", "shiny"))
   ```
3. Correr `cod/limpieza.R` y después `cod/metodologia.R` (generan los `.rds` en `data/`).
4. **Informe:** renderizar `cod/Individual_Alpizar_Alonso_HerramientasDatosI_2026.qmd` y mover el PDF a `res/`.
5. **Presentación:** renderizar `presentacion/presentacion.qmd` (necesita internet para las fórmulas y la letra).
6. **Aplicación:** `shiny::runApp("app")`.

Los archivos que generan Quarto y los scripts (`.html`, `_files/`, `.tex`, `.log`, `.rds`) no se suben al repositorio (ver `.gitignore`).

## Fuente de datos

Miller, A. (2021). *marathon-results* [Conjunto de datos]. GitHub. https://github.com/AndrewMillerOnline/marathon-results
