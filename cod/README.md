# cod/

Código del análisis y del informe. Se corre desde la raíz del proyecto (abriendo `Proyecto Individual.Rproj`).

| Archivo | Qué hace |
|:--|:--|
| `limpieza.R` | Carga los tres CSV de `data/`, pasa los tiempos a minutos y aplica los filtros (parciales faltantes, corredores élite y más de 6 h 15 min). Guarda `data/berlin_limpio.rds`. |
| `metodologia.R` | Calcula el ritmo medio total (RMT), los ritmos de salida y cierre, el ritmo relativo (RP), la clasificación de estrategias, el nivel y la caída en la segunda mitad. Guarda `data/berlin_metodologia.rds`. |
| `Individual_Alpizar_Alonso_HerramientasDatosI_2026.qmd` | Informe en Quarto (PDF). El PDF final está en `res/`. |

Orden: `limpieza.R` → `metodologia.R` → informe.

La presentación está en `presentacion/` y la aplicación Shiny en `app/`.
