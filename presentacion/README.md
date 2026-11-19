# presentacion/

Presentación del proyecto en Quarto (revealjs): **¿Cuál es la mejor estrategia para correr el Maratón de Berlín?**

| Archivo | Qué es |
|:--|:--|
| `presentacion.qmd` | Código de la presentación. Lee los CSV de `../data/`, aplica la misma limpieza del informe y calcula todos los números y gráficos al renderizar. |
| `estilo.scss` | Estilo "Berlín de día": colores, letra (Outfit), tarjetas y espaciado. |
| `img/portada.png` | Fondo pixel art de la portada (Puerta de Brandeburgo, Torre de TV y la línea azul del maratón). |
| `img/fondo.png` | Versión aclarada del fondo, para el resto de las diapositivas. |

## Diapositivas

1. Portada
2. Objetivo y datos (limpieza y variables usadas)
3. Método: ritmo medio total (RMT), ritmo de cada tramo y ritmo relativo, con un ejemplo real
4. Clasificación (±5 %) y definición de chocar con la pared (caída > 30 %)
5. Resultado 1: casi la mitad sale demasiado rápido
6. Resultado 2: correr parejo da el mejor tiempo
7. Resultado 3: comparando corredores de nivel parecido
8. Resultado 4: ¿quién choca con la pared?
9. Conclusiones
10. Cierre

## Cómo renderizarla

1. Abrir `Proyecto Individual.Rproj` en RStudio.
2. Abrir `presentacion/presentacion.qmd` y apretar **Render**.
3. Se genera `presentacion.html` (no se sube al repositorio). Para presentar, abrirlo en el navegador y apretar `F` para pantalla completa.

Necesita internet al abrirla, para las fórmulas (MathJax) y la letra.
