# ¿Cuál es la mejor estrategia para correr un maratón?

Proyecto individual del curso CA-0204 Herramientas de Ciencia de Datos I (UCR, II-2026).
Autor: Alonso José Alpízar Brenes

Análisis de las estrategias de ritmo en el Maratón de Berlín 2017–2019: se compara el ritmo de cada corredor en los primeros 10 km y en los últimos 7.195 km con su ritmo medio de carrera.

## Estructura

- `cod/`: scripts de R y el informe en Quarto
  - `limpieza.R`: carga y limpieza de los datos
  - `metodologia.R`: cálculo de ritmos y clasificación de estrategias
  - `Individual_Alpizar_Alonso_HerramientasDatosI_2026.qmd`: informe en Quarto
- `res/`: informe en Quarto
- `data/`: resultados del Maratón de Berlín 2017–2019 (Miller, 2021)
- `info/`: documentos de apoyo
- `config/`: configuración del proyecto

## Cómo reproducirlo

1. Abrir `Proyecto Individual.Rproj` en RStudio.
2. Instalar los paquetes: `install.packages(c("tidyverse", "here"))`.
3. Correr `cod/limpieza.R` y después `cod/metodologia.R`.
4. Renderizar `cod/Individual_Alpizar_Alonso_HerramientasDatosI_2026.qmd`.

## Fuente de datos

Miller, A. (2021). *marathon-results* [Conjunto de datos]. GitHub. https://github.com/AndrewMillerOnline/marathon-results
