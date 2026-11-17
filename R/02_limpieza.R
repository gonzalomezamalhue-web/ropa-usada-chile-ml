# =============================================================================
# TIG Ciencia de Datos - UNAB Primavera 2026
# Script 02: Limpieza y preparación de datos
# -----------------------------------------------------------------------------
# Entrada : datos/brutos/ropa_usada_6309_AAAA.csv (salida del script 01)
#           datos/brutos/tablas_de_codigos_v2_0.xlsx (diccionario de Aduanas)
# Salida  : datos/procesados/ropa_usada_limpia.csv
# =============================================================================

# install.packages(c("dplyr", "readr", "readxl", "stringr", "tidyr"))
library(dplyr)
source("R/00_funciones.R")

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dicc <- "datos/brutos/tablas_de_codigos_v2_0.xlsx"

# -----------------------------------------------------------------------------
# 1. Leer y unir los tres años
# -----------------------------------------------------------------------------
# Los archivos de cada año traen diferencias de formato:
#  - 2024 escribe los enteros como "2024,00" y la columna lleva tilde
#    (CANTIDAD_MERCANCÍA).
# Por eso se lee todo como texto y se normaliza antes de unir.
leer_anio <- function(ruta) {
  d <- readr::read_delim(ruta, delim = ";", col_types = readr::cols(.default = "c"),
                         locale = readr::locale(encoding = "UTF-8"),
                         show_col_types = FALSE)
  names(d) <- iconv(names(d), to = "ASCII//TRANSLIT")   # quita tildes
  names(d) <- gsub("[^A-Z_]", "", toupper(names(d)))
  d
}

archivos <- list.files("datos/brutos", pattern = "^ropa_usada_6309_\\d{4}\\.csv$",
                       full.names = TRUE)
bruto <- archivos |>
  lapply(leer_anio) |>
  bind_rows()

cat("Registros brutos (partida 6309):", nrow(bruto), "\n")

# -----------------------------------------------------------------------------
# 2. Tipos de dato
# -----------------------------------------------------------------------------
codigos <- c("PERIODO", "MES", "COD_ADUANA_TRAMITACION", "COD_TIPO_OPERACION",
             "COD_PAIS_ORIGEN", "COD_PAIS_ADQUISICION", "COD_REGIMEN_IMPORTACION",
             "COD_PUERTO_EMBARQUE", "COD_PUERTO_DESEMBARQUE",
             "COD_VIA_TRANSPORTE", "CL_COMPRA", "COD_UNIDAD_MEDIDA")

datos <- bruto |>
  mutate(
    across(all_of(codigos), ~ as.character(as.integer(a_numero(.x)))),
    CIF_US             = a_numero(CIF_US),
    AD_VALOREM_US      = a_numero(AD_VALOREM_US),
    CANTIDAD_MERCANCIA = a_numero(CANTIDAD_MERCANCIA),
    ITEM_SA            = gsub("\\D", "", ITEM_SA)
  )

# -----------------------------------------------------------------------------
# 3. Diccionario: traducir códigos a nombres
# -----------------------------------------------------------------------------
aduanas <- leer_tabla_codigos(dicc, 2) |> select(COD_ADUANA_TRAMITACION, aduana = NOMBRE_ADUANA)
tipos   <- leer_tabla_codigos(dicc, 3) |> select(COD_TIPO_OPERACION, tipo_operacion = NOMBRE_TIPO_OPERACION)
paises  <- leer_tabla_codigos(dicc, 4) |> select(COD_PAIS_ORIGEN = COD_PAIS, pais_origen = NOMBRE_PAIS,
                                                continente = NOMBRE_CONTINENTE)
vias    <- leer_tabla_codigos(dicc, 7) |> select(COD_VIA_TRANSPORTE, via = NOMBRE_VIA_TRANSPORTE)
clausulas <- leer_tabla_codigos(dicc, 13) |> select(CL_COMPRA, clausula = SIGLA_CLAUSULA)

datos <- datos |>
  left_join(aduanas,   by = "COD_ADUANA_TRAMITACION") |>
  left_join(tipos,     by = "COD_TIPO_OPERACION") |>
  left_join(paises,    by = "COD_PAIS_ORIGEN") |>
  left_join(vias,      by = "COD_VIA_TRANSPORTE") |>
  left_join(clausulas, by = "CL_COMPRA")

# -----------------------------------------------------------------------------
# 4. Filtros, cada uno con su justificación (se registra cuántas filas elimina)
# -----------------------------------------------------------------------------
registro <- tibble::tibble(paso = "Registros brutos", filas = nrow(datos))
anotar <- function(df, paso) {
  force(df)   # evalúa primero los pasos anteriores del pipe
  registro <<-bind_rows(registro, tibble::tibble(paso = paso, filas = nrow(df)))
  df
}

# Lista oficial de Aduanas de tipos de operación que corresponden a importación
tipos_importacion <- c(101, 102, 103, 104, 105, 113, 115, 116, 119, 120, 121, 122,
                       123, 129, 130, 134, 142, 151, 152, 165, 171, 179, 180)

limpio <- datos |>
  # (0) Verificar que el código arancelario sea realmente 6309 (un filtro de
  #     texto simple también captura filas cuyo valor CIF empieza con "6309").
  filter(startsWith(ITEM_SA, "6309")) |>
  anotar("(0) Código arancelario 6309 verificado") |>
  # (a) Solo importaciones (excluye almacenes particulares, que no son
  #     importaciones definitivas y podrían duplicar operaciones).
  filter(as.integer(COD_TIPO_OPERACION) %in% tipos_importacion) |>
  anotar("(a) Solo importaciones") |>
  # (b) Excluir courier y vía postal: son encomiendas pequeñas, no fardos
  #     comerciales, y distorsionan el precio por kilo (hasta USD 400+/kg).
  filter(!COD_VIA_TRANSPORTE %in% c("5", "11"),
         !COD_TIPO_OPERACION %in% c("122", "123", "171")) |>
  anotar("(b) Sin courier ni vía postal") |>
  # (c) Valores válidos: cantidad en kilos netos y montos positivos.
  filter(COD_UNIDAD_MEDIDA == "6",
         !is.na(CIF_US), CIF_US > 0,
         !is.na(CANTIDAD_MERCANCIA), CANTIDAD_MERCANCIA > 0) |>
  anotar("(c) Kilos y CIF positivos") |>
  # (d) Embarques comerciales: al menos 100 kg (un fardo típico pesa 45-55 kg).
  filter(CANTIDAD_MERCANCIA >= 100) |>
  anotar("(d) Embarques de 100 kg o más") |>
  mutate(usd_kg = CIF_US / CANTIDAD_MERCANCIA)

# (e) Valores extremos del precio por kilo: regla de Tukey sobre el logaritmo
#     (el precio por kilo es muy asimétrico, por eso se usa log).
q <- quantile(log(limpio$usd_kg), c(0.25, 0.75))
lim_inf <- exp(q[1] - 1.5 * diff(q))
lim_sup <- exp(q[2] + 1.5 * diff(q))

limpio <- limpio |>
  filter(usd_kg >= lim_inf, usd_kg <= lim_sup) |>
  anotar(sprintf("(e) Precio entre USD %.2f y %.2f por kg", lim_inf, lim_sup))

# -----------------------------------------------------------------------------
# 5. Variables finales para el análisis
# -----------------------------------------------------------------------------
# IMPORTANTE (fuga de información): AD_VALOREM_US es el 6% del valor CIF, es
# decir, se calcula A PARTIR del precio. Si se usara como predictor, el modelo
# "haría trampa". Se excluye del modelado.
limpio <- limpio |>
  mutate(
    anio        = as.integer(PERIODO),
    mes         = as.integer(MES),
    subpartida  = ITEM_SA,
    kg          = CANTIDAD_MERCANCIA,
    log_kg      = log(kg),
    log_usd_kg  = log(usd_kg),
    aduana      = coalesce(aduana, "Otra"),
    via         = coalesce(via, "Otra"),
    pais_origen = coalesce(pais_origen, "Desconocido"),
    origen_no_precisado = COD_PAIS_ORIGEN == "904",
    triangulado = COD_PAIS_ORIGEN != COD_PAIS_ADQUISICION   # se compra en un país distinto al de origen
  ) |>
  select(anio, mes, aduana, via, pais_origen, continente, origen_no_precisado,
         triangulado, clausula, subpartida, kg, log_kg, CIF_US, usd_kg, log_usd_kg)

readr::write_csv(limpio, "datos/procesados/ropa_usada_limpia.csv")
readr::write_csv(registro, "datos/procesados/registro_limpieza.csv")

print(registro, n = Inf)
cat("\nRegistros finales:", nrow(limpio), "\n")
