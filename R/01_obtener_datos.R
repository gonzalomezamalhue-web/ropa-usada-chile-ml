# =============================================================================
# TIG Ciencia de Datos - UNAB Primavera 2026
# Script 01: Obtención de datos
# -----------------------------------------------------------------------------
# Fuente: Servicio Nacional de Aduanas de Chile
#         "Base de datos Operaciones de Ingreso" (archivos anuales)
#         https://www.aduana.cl/base-de-datos-operaciones-de-ingreso/aduana/2024-11-12/122745.html
# Período: enero 2024 - octubre 2026
# Objetivo: quedarse solo con la partida arancelaria 6309 (ropa usada,
#           "artículos de prendería") a partir de los archivos completos.
#
# NOTA: este paso solo es necesario para reconstruir los datos desde cero.
#       El repositorio ya incluye los archivos filtrados en datos/brutos/.
# =============================================================================

# install.packages("data.table")
library(data.table)

dir.create("datos/brutos", recursive = TRUE, showWarnings = FALSE)
dir.create("datos/originales", recursive = TRUE, showWarnings = FALSE)
base_url <- "https://www.aduana.cl/aduana/site/docs/20241112/20241112122745/"

# -----------------------------------------------------------------------------
# 1. Descargar archivos originales (cada uno pesa entre 115 y 150 MB)
# -----------------------------------------------------------------------------
# 2025 viene comprimido en .rar: descárguelo desde la página de Aduanas,
# descomprímalo y deje "Ingresos2025.csv" en datos/originales/.
archivos <- c(
  "2024" = "ingresos_2024.csv",
  "2025" = "Ingresos2025.csv",        # descomprimir manualmente desde ingresos2025.rar
  "2026" = "ingreso_da_10_2026.csv"   # enero-octubre 2026
)

for (anio in c("2024", "2026")) {
  destino <- file.path("datos/originales", archivos[[anio]])
  if (!file.exists(destino)) {
    options(timeout = 1800)
    download.file(paste0(base_url, archivos[[anio]]), destino, mode = "wb")
  }
}

dicc <- "datos/brutos/tablas_de_codigos_v2_0.xlsx"
if (!file.exists(dicc)) {
  download.file(paste0(base_url, "tablas_de_codigos_v2_0.xlsx"), dicc, mode = "wb")
}

# -----------------------------------------------------------------------------
# 2. Leer cada archivo completo y filtrar la partida 6309
# -----------------------------------------------------------------------------
# - Se lee todo como texto para no perder ceros a la izquierda en los códigos.
# - Excel no sirve aquí: los archivos superan su límite de 1.048.576 filas.
for (anio in names(archivos)) {
  ruta <- file.path("datos/originales", archivos[[anio]])
  if (!file.exists(ruta)) {
    message("Falta ", ruta, " (ver instrucciones arriba). Se omite ", anio)
    next
  }
  datos <- data.table::fread(ruta, sep = ";", colClasses = "character",
                             encoding = "Latin-1")
  ropa <- datos[startsWith(gsub("\\D", "", ITEM_SA), "6309")]
  salida <- file.path("datos/brutos", paste0("ropa_usada_6309_", anio, ".csv"))
  data.table::fwrite(ropa, salida, sep = ";")
  cat(anio, ": ", nrow(datos), " filas totales -> ", nrow(ropa), " de ropa usada\n", sep = "")
}
