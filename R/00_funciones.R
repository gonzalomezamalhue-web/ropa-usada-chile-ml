# =============================================================================
# Funciones auxiliares del proyecto
# =============================================================================

# Lee una hoja del diccionario de Aduanas (tablas_de_codigos_v2_0.xlsx).
# Cada hoja trae filas de título antes del encabezado real, así que se busca
# la fila que contiene un nombre de columna que empieza con "COD_" o "CL_".
leer_tabla_codigos <- function(archivo, hoja) {
  bruto <- readxl::read_excel(archivo, sheet = hoja, col_names = FALSE,
                              .name_repair = "minimal")
  es_encabezado <- apply(bruto, 1, function(fila)
    any(grepl("^(COD_|CL_COMPRA|MONEDA)", fila, ignore.case = FALSE)))
  fila_enc <- which(es_encabezado)[1]
  datos <- bruto[(fila_enc + 1):nrow(bruto), ]
  nombres <- as.character(unlist(bruto[fila_enc, ]))
  nombres[is.na(nombres) | nombres == ""] <- paste0("vacia_", seq_along(nombres))[is.na(nombres) | nombres == ""]
  names(datos) <- nombres
  datos <- datos[, !grepl("^vacia_", names(datos))]
  datos <- datos[!is.na(datos[[1]]), ]
  datos[[1]] <- as.character(as.integer(as.numeric(datos[[1]])))
  datos
}

# Convierte texto con coma decimal ("2024,00", "5112,5") a número.
a_numero <- function(x) as.numeric(gsub(",", ".", x, fixed = TRUE))
