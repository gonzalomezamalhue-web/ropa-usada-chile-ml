# Dependencias del proyecto (ejecutar una sola vez)
paquetes <- c(
  # manejo de datos
  "dplyr", "readr", "readxl", "tidyr", "tibble", "forcats", "stringr", "data.table",
  # gráficos
  "ggplot2", "scales",
  # modelado (tidymodels, como en el curso)
  "rsample", "parsnip", "tune", "yardstick", "recipes", "rpart", "ranger",
  # segmentación
  "tidyclust", "cluster"
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)

cat("Paquetes listos. Versión de R:", R.version.string, "\n")
