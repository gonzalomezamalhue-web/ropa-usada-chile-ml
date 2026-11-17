# =============================================================================
# TIG Ciencia de Datos - UNAB Primavera 2026
# Script 05: Segmentación no supervisada (K-means)
# -----------------------------------------------------------------------------
# Pregunta complementaria: ¿qué tipos de importaciones de ropa usada existen
# según su tamaño (kg) y su precio por kilo?
# Se usa K-means como en el Laboratorio 08 (tidyclust, engine "stats").
# =============================================================================

# install.packages(c("dplyr", "readr", "ggplot2", "tidyclust", "cluster"))
library(dplyr)
library(ggplot2)

datos <- readr::read_csv("datos/procesados/ropa_usada_limpia.csv", show_col_types = FALSE)

# K-means usa distancias: las variables se estandarizan (media 0, desv. 1)
# para que ninguna domine por su escala.
segmentacion <- datos |>
  mutate(z_log_kg = as.numeric(scale(log_kg)),
         z_log_usd_kg = as.numeric(scale(log_usd_kg)))

# -----------------------------------------------------------------------------
# 1. Elegir el número de grupos: codo (inercia) y silueta
# -----------------------------------------------------------------------------
X <- segmentacion |> select(z_log_kg, z_log_usd_kg)
distancias <- dist(X)

eleccion_k <- lapply(2:6, function(k) {
  set.seed(123)
  km <- stats::kmeans(X, centers = k, nstart = 25)
  tibble::tibble(
    k = k,
    inercia = km$tot.withinss,
    silueta = mean(cluster::silhouette(km$cluster, distancias)[, "sil_width"])
  )
}) |> bind_rows()
print(eleccion_k)
readr::write_csv(eleccion_k, "resultados/kmeans_eleccion_k.csv")
# Resultado: k = 4 tiene la mayor silueta promedio.

# -----------------------------------------------------------------------------
# 2. Modelo final con tidyclust
# -----------------------------------------------------------------------------
set.seed(123)
ajuste_kmeans <-
  tidyclust::k_means(num_clusters = 4) |>
  parsnip::set_engine("stats", nstart = 25) |>
  parsnip::fit(~ z_log_kg + z_log_usd_kg, data = segmentacion)

segmentacion <- segmentacion |>
  bind_cols(tidyclust::extract_cluster_assignment(ajuste_kmeans))

# -----------------------------------------------------------------------------
# 3. Perfil de cada segmento (para ponerle nombre)
# -----------------------------------------------------------------------------
perfil <- segmentacion |>
  group_by(.cluster) |>
  summarise(registros = n(),
            kg_mediana = median(kg),
            usd_kg_mediana = round(median(usd_kg), 2),
            toneladas = round(sum(kg) / 1000),
            pct_toneladas = round(100 * sum(kg) / sum(segmentacion$kg), 1),
            .groups = "drop") |>
  mutate(segmento = case_when(
    kg_mediana >= 5000 & usd_kg_mediana < 1 ~ "Grandes y baratos (granel)",
    kg_mediana >= 5000                      ~ "Grandes y caros",
    usd_kg_mediana < 1                      ~ "Pequeños y baratos",
    TRUE                                    ~ "Pequeños y caros (selección)"
  ))
print(perfil)
readr::write_csv(perfil, "resultados/kmeans_perfil_segmentos.csv")

segmentacion <- segmentacion |> left_join(select(perfil, .cluster, segmento), by = ".cluster")

# Composición por aduana: ¿qué tipo de importación entra por cada puerto?
por_aduana <- segmentacion |>
  filter(aduana %in% c("Iquique", "San Antonio", "Valparaíso", "Talcahuano", "Arica")) |>
  count(aduana, segmento) |>
  group_by(aduana) |>
  mutate(pct = round(100 * n / sum(n), 1))
readr::write_csv(por_aduana, "resultados/kmeans_segmentos_por_aduana.csv")

# -----------------------------------------------------------------------------
# 4. Gráfico de los segmentos
# -----------------------------------------------------------------------------
g <- ggplot(segmentacion, aes(kg, usd_kg, color = segmento)) +
  geom_point(alpha = 0.35, size = 1.2) +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10(labels = scales::label_dollar(accuracy = 0.1)) +
  scale_color_manual(values = c("#2F6690", "#D1495B", "#EDAE49", "#00798C")) +
  labs(title = "Cuatro tipos de importaciones de ropa usada (K-means, k = 4)",
       x = "Kilos del embarque (escala log)", y = "USD por kilo (escala log)",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave("figuras/11_segmentos_kmeans.png", g, width = 8, height = 6, dpi = 150, bg = "white")
