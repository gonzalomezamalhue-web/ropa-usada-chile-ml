# =============================================================================
# TIG Ciencia de Datos - UNAB Primavera 2026
# Script 03: Análisis exploratorio de datos (EDA)
# -----------------------------------------------------------------------------
# Entrada : datos/procesados/ropa_usada_limpia.csv
# Salida  : figuras/*.png y resultados/eda_*.csv
# =============================================================================

library(dplyr)
library(ggplot2)

dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

datos <- readr::read_csv("datos/procesados/ropa_usada_limpia.csv", show_col_types = FALSE) |>
  mutate(subpartida = as.character(subpartida))

tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.minor = element_blank())
color <- "#2F6690"

guardar <- function(grafico, nombre, ancho = 8, alto = 5) {
  ggsave(file.path("figuras", nombre), grafico, width = ancho, height = alto, dpi = 150, bg = "white")
}

# -----------------------------------------------------------------------------
# 1. Estadística descriptiva general
# -----------------------------------------------------------------------------
descriptiva <- datos |>
  summarise(
    registros      = n(),
    toneladas      = sum(kg) / 1000,
    cif_millones   = sum(CIF_US) / 1e6,
    usd_kg_min     = min(usd_kg),
    usd_kg_p25     = quantile(usd_kg, 0.25),
    usd_kg_mediana = median(usd_kg),
    usd_kg_media   = mean(usd_kg),
    usd_kg_p75     = quantile(usd_kg, 0.75),
    usd_kg_max     = max(usd_kg),
    kg_mediana     = median(kg)
  )
readr::write_csv(descriptiva, "resultados/eda_descriptiva.csv")
print(descriptiva)

# Resumen por categoría: n, toneladas, mediana y precio ponderado por kilo
resumen_por <- function(variable) {
  datos |>
    group_by(.data[[variable]]) |>
    summarise(registros = n(),
              toneladas = round(sum(kg) / 1000),
              mediana_usd_kg = round(median(usd_kg), 2),
              usd_kg_ponderado = round(sum(CIF_US) / sum(kg), 2),
              .groups = "drop") |>
    arrange(desc(toneladas))
}
for (v in c("anio", "aduana", "via", "subpartida", "pais_origen", "clausula")) {
  readr::write_csv(resumen_por(v), paste0("resultados/eda_por_", v, ".csv"))
}

# -----------------------------------------------------------------------------
# 2. Distribución de la variable objetivo (precio por kilo)
# -----------------------------------------------------------------------------
g1 <- ggplot(datos, aes(usd_kg)) +
  geom_histogram(bins = 60, fill = color) +
  geom_vline(xintercept = median(datos$usd_kg), linetype = "dashed") +
  labs(title = "El precio por kilo es muy asimétrico",
       subtitle = sprintf("Mediana: USD %.2f/kg (línea punteada). La mayoría bajo USD 2/kg, con una cola larga.",
                          median(datos$usd_kg)),
       x = "USD por kilo (CIF)", y = "Número de importaciones") +
  tema
guardar(g1, "01_distribucion_usd_kg.png")

g2 <- ggplot(datos, aes(usd_kg)) +
  geom_histogram(bins = 50, fill = color) +
  scale_x_log10(labels = scales::label_dollar(accuracy = 0.01)) +
  labs(title = "En escala logarítmica la distribución se vuelve casi simétrica",
       subtitle = "Por eso los modelos predicen log(USD/kg)",
       x = "USD por kilo (escala log)", y = "Número de importaciones") +
  tema
guardar(g2, "02_distribucion_log_usd_kg.png")

# -----------------------------------------------------------------------------
# 3. Precio por kilo según aduana de ingreso
# -----------------------------------------------------------------------------
aduanas_principales <- datos |> count(aduana) |> filter(n >= 30) |> pull(aduana)
g3 <- datos |>
  filter(aduana %in% aduanas_principales) |>
  mutate(aduana = forcats::fct_reorder(aduana, usd_kg, median)) |>
  ggplot(aes(aduana, usd_kg)) +
  geom_boxplot(fill = color, alpha = 0.6, outlier.alpha = 0.2) +
  scale_y_log10(labels = scales::label_dollar(accuracy = 0.1)) +
  coord_flip() +
  labs(title = "La aduana de ingreso marca grandes diferencias de precio",
       subtitle = "Iquique, San Antonio y Arica concentran la ropa más barata por kilo",
       x = NULL, y = "USD por kilo (escala log)") +
  tema
guardar(g3, "03_usd_kg_por_aduana.png")

# -----------------------------------------------------------------------------
# 4. Precio por kilo según país de origen (10 países con más toneladas)
# -----------------------------------------------------------------------------
top_paises <- datos |> group_by(pais_origen) |> summarise(t = sum(kg)) |>
  slice_max(t, n = 10) |> pull(pais_origen)
g4 <- datos |>
  filter(pais_origen %in% top_paises) |>
  mutate(pais_origen = stringr::str_trunc(pais_origen, 28),
         pais_origen = forcats::fct_reorder(pais_origen, usd_kg, median)) |>
  ggplot(aes(pais_origen, usd_kg)) +
  geom_boxplot(fill = color, alpha = 0.6, outlier.alpha = 0.2) +
  scale_y_log10(labels = scales::label_dollar(accuracy = 0.1)) +
  coord_flip() +
  labs(title = "El país de origen también separa precios",
       subtitle = "10 orígenes con más toneladas importadas",
       x = NULL, y = "USD por kilo (escala log)") +
  tema
guardar(g4, "04_usd_kg_por_pais.png")

# -----------------------------------------------------------------------------
# 5. Subpartida arancelaria
# -----------------------------------------------------------------------------
g5 <- datos |>
  mutate(subpartida = forcats::fct_reorder(subpartida, usd_kg, median)) |>
  ggplot(aes(subpartida, usd_kg)) +
  geom_boxplot(fill = color, alpha = 0.6, outlier.alpha = 0.2) +
  scale_y_log10(labels = scales::label_dollar(accuracy = 0.1)) +
  coord_flip() +
  labs(title = "Precio por kilo según subpartida de ropa usada",
       x = "Subpartida (código arancelario)", y = "USD por kilo (escala log)") +
  tema
guardar(g5, "05_usd_kg_por_subpartida.png")

# -----------------------------------------------------------------------------
# 6. Tamaño del embarque vs precio por kilo (economías de escala)
# -----------------------------------------------------------------------------
correlacion <- cor(datos$log_kg, datos$log_usd_kg)
g6 <- ggplot(datos, aes(kg, usd_kg)) +
  geom_point(alpha = 0.15, color = color) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10(labels = scales::label_dollar(accuracy = 0.1)) +
  labs(title = "A mayor tamaño del embarque, menor precio por kilo",
       subtitle = sprintf("Correlación entre log(kg) y log(USD/kg): %.2f", correlacion),
       x = "Kilos del embarque (escala log)", y = "USD por kilo (escala log)") +
  tema
guardar(g6, "06_kg_vs_usd_kg.png")

# -----------------------------------------------------------------------------
# 7. Evolución mensual
# -----------------------------------------------------------------------------
mensual <- datos |>
  mutate(fecha = as.Date(sprintf("%d-%02d-01", anio, mes))) |>
  group_by(fecha) |>
  summarise(toneladas = sum(kg) / 1000,
            usd_kg_ponderado = sum(CIF_US) / sum(kg), .groups = "drop")
readr::write_csv(mensual, "resultados/eda_mensual.csv")

g7 <- ggplot(mensual, aes(fecha, toneladas)) +
  geom_col(fill = color) +
  labs(title = "Toneladas de ropa usada importadas por mes",
       subtitle = "Enero 2024 - octubre 2026 (embarques comerciales)",
       x = NULL, y = "Toneladas") +
  tema
guardar(g7, "07_toneladas_mensuales.png")

g8 <- ggplot(mensual, aes(fecha, usd_kg_ponderado)) +
  geom_line(color = color, linewidth = 1) +
  geom_point(color = color) +
  labs(title = "Precio promedio ponderado por kilo, mes a mes",
       x = NULL, y = "USD por kilo") +
  tema
guardar(g8, "08_usd_kg_mensual.png")

# -----------------------------------------------------------------------------
# 8. Datos faltantes y calidad
# -----------------------------------------------------------------------------
calidad <- tibble::tibble(
  variable = names(datos),
  faltantes = sapply(datos, function(x) sum(is.na(x))),
  pct_faltantes = round(100 * faltantes / nrow(datos), 1)
)
calidad <- bind_rows(calidad, tibble::tibble(
  variable = "origen_no_precisado (país 904)",
  faltantes = sum(datos$origen_no_precisado),
  pct_faltantes = round(100 * mean(datos$origen_no_precisado), 1)
))
readr::write_csv(calidad, "resultados/eda_calidad.csv")
print(calidad)
