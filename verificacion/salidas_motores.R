# Genera las mismas salidas de 04 y 05 usando rpart/ranger/kmeans directamente
suppressMessages({library(dplyr); library(ggplot2)})
source("verificacion/modelado_motores.R")  # deja objetos: res, bosque_f, pru, g_mejor, cp_mejor
comp <- tibble::tibble(modelo = c("Línea base (mediana)","Regresión lineal","Árbol de decisión","Bosque aleatorio"),
                       rmse = res[,"rmse"], mae = res[,"mae"], rsq = res[,"rsq"], mae_usd_kg = res[,"mae_usd"]) |> arrange(rmse)
readr::write_csv(comp, "resultados/comparacion_modelos.csv")
imp <- tibble::enframe(bosque_f$variable.importance, name="variable", value="importancia") |> arrange(desc(importancia))
readr::write_csv(imp, "resultados/importancia_variables.csv")
g_imp <- ggplot(imp, aes(reorder(variable, importancia), importancia)) + geom_col(fill="#2F6690") + coord_flip() +
  labs(title="Qué variables usa más el bosque aleatorio",
       subtitle="Importancia por permutación: cuánto empeora el modelo si se desordena la variable", x=NULL, y="Importancia") +
  theme_minimal(base_size=12)
ggsave("figuras/09_importancia_variables.png", g_imp, width=8, height=5, dpi=150, bg="white")
pp <- pru |> mutate(.pred = predict(bosque_f, pru)$predictions)
g_pred <- ggplot(pp, aes(exp(log_usd_kg), exp(.pred))) + geom_point(alpha=0.25, color="#2F6690") + geom_abline(linetype="dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(title="Bosque aleatorio: precio real vs predicho (conjunto de prueba)", subtitle="La línea punteada es la predicción perfecta",
       x="USD/kg real (escala log)", y="USD/kg predicho (escala log)") + theme_minimal(base_size=12)
ggsave("figuras/10_real_vs_predicho.png", g_pred, width=7, height=6, dpi=150, bg="white")

# K-means
d <- readr::read_csv("datos/procesados/ropa_usada_limpia.csv", show_col_types=FALSE) |>
  mutate(z_log_kg=as.numeric(scale(log_kg)), z_log_usd_kg=as.numeric(scale(log_usd_kg)))
X <- d |> select(z_log_kg, z_log_usd_kg); D <- dist(X)
ek <- lapply(2:6, function(k){ set.seed(123); km<-kmeans(X,k,nstart=25)
  tibble::tibble(k=k, inercia=km$tot.withinss, silueta=mean(cluster::silhouette(km$cluster,D)[,"sil_width"]))}) |> bind_rows()
readr::write_csv(ek, "resultados/kmeans_eleccion_k.csv"); print(ek)
set.seed(123); km <- kmeans(X, 4, nstart=25); d$.cluster <- factor(paste0("Cluster_", km$cluster))
perfil <- d |> group_by(.cluster) |> summarise(registros=n(), kg_mediana=median(kg), usd_kg_mediana=round(median(usd_kg),2),
  toneladas=round(sum(kg)/1000), pct_toneladas=round(100*sum(kg)/sum(d$kg),1), .groups="drop") |>
  mutate(segmento=case_when(kg_mediana>=5000 & usd_kg_mediana<1 ~ "Grandes y baratos (granel)", kg_mediana>=5000 ~ "Grandes y caros",
    usd_kg_mediana<1 ~ "Pequeños y baratos", TRUE ~ "Pequeños y caros (selección)"))
print(perfil); readr::write_csv(perfil, "resultados/kmeans_perfil_segmentos.csv")
d <- left_join(d, select(perfil,.cluster,segmento), by=".cluster")
pa <- d |> filter(aduana %in% c("Iquique","San Antonio","Valparaíso","Talcahuano","Arica")) |> count(aduana, segmento) |>
  group_by(aduana) |> mutate(pct=round(100*n/sum(n),1))
readr::write_csv(pa, "resultados/kmeans_segmentos_por_aduana.csv")
g <- ggplot(d, aes(kg, usd_kg, color=segmento)) + geom_point(alpha=0.35, size=1.2) +
  scale_x_log10(labels=scales::label_comma()) + scale_y_log10(labels=scales::label_dollar(accuracy=0.1)) +
  scale_color_manual(values=c("#2F6690","#D1495B","#EDAE49","#00798C")) +
  labs(title="Cuatro tipos de importaciones de ropa usada (K-means, k = 4)", x="Kilos del embarque (escala log)", y="USD por kilo (escala log)", color=NULL) +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
ggsave("figuras/11_segmentos_kmeans.png", g, width=8, height=6, dpi=150, bg="white")
