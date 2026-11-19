# data/

Resultados del Maratón de Berlín 2017, 2018 y 2019 (123 797 corredores en total).

| Archivo | Contenido |
|:--|:--|
| `berlin_2017.csv`, `berlin_2018.csv`, `berlin_2019.csv` | Datos crudos: nombre, nacionalidad, sexo, parciales cada 5 km, media maratón y tiempo total de cada corredor. |
| `berlin_limpio.rds` | Generado por `cod/limpieza.R` (no se sube al repositorio). |
| `berlin_metodologia.rds` | Generado por `cod/metodologia.R` (no se sube al repositorio). |

Variables que usa el análisis: `gender` (sexo, para el corte de élite), `split_10k`, `split_35k`, `time_half` y `time_full`.

Fuente: Miller, A. (2021). *marathon-results* [Conjunto de datos]. GitHub. https://github.com/AndrewMillerOnline/marathon-results
