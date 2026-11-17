# =============================================================================
# TIG Ciencia de Datos - UNAB Primavera 2026
# Script 04: Modelado supervisado
# -----------------------------------------------------------------------------
# Pregunta: ¿se puede predecir el precio por kilo (USD CIF/kg) de una
#           importación de ropa usada a partir de la información de su
#           declaración de ingreso?
# Variable objetivo: log(USD/kg). Se usa log porque el precio por kilo es muy
#                    asimétrico (ver EDA, figuras 01 y 02).
# Modelos: línea base (mediana), regresión lineal, árbol de decisión y bosque
#          aleatorio. Partición 60 / 20 / 20 (entrenamiento / validación / prueba)
#          como en la clase 11.
# =============================================================================

# install.packages(c("dplyr", "readr", "forcats", "ggplot2", "rsample",
#                    "parsnip", "tune", "yardstick", "rpart", "ranger"))
library(dplyr)
library(ggplot2)

dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Datos y selección de variables
# -----------------------------------------------------------------------------
# Predictores: solo información disponible en la declaración ANTES de conocer
# el precio. Se EXCLUYEN CIF_US y AD_VALOREM_US porque se calculan a partir
# del valor de la mercadería (fuga de información).
datos <- readr::read_csv("datos/procesados/ropa_usada_limpia.csv", show_col_types = FALSE) |>
  mutate(
    subpartida  = factor(subpartida),
    # Categorías con pocas observaciones se agrupan para que el modelo no
    # aprenda de casos aislados.
    aduana      = factor(forcats::fct_lump_min(aduana, 30, other_level = "Otra")),
    via         = factor(forcats::fct_lump_min(via, 30, other_level = "Otra")),
    pais_origen = factor(forcats::fct_lump_n(pais_origen, 12, other_level = "Otros")),
    continente  = factor(continente),
    clausula    = factor(forcats::fct_lump_min(coalesce(clausula, "Sin dato"), 30, other_level = "Otra")),
    origen_no_precisado = factor(origen_no_precisado),
    triangulado = factor(triangulado)
  )

formula_modelo <- log_usd_kg ~ anio + mes + aduana + via + pais_origen + continente +
  origen_no_precisado + triangulado + clausula + subpartida + log_kg

# -----------------------------------------------------------------------------
# 2. Partición 60 / 20 / 20
# -----------------------------------------------------------------------------
set.seed(123)
particion <- rsample::initial_validation_split(
  datos,
  prop = c(0.6, 0.2),
  strata = log_usd_kg
)
entrenamiento <- rsample::training(particion)
validacion    <- rsample::validation(particion)
prueba        <- rsample::testing(particion)
muestras      <- rsample::validation_set(particion)

cat("Entrenamiento:", nrow(entrenamiento), " Validación:", nrow(validacion),
    " Prueba:", nrow(prueba), "\n")

metricas <- yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)

# -----------------------------------------------------------------------------
# 3. Especificación de modelos
# -----------------------------------------------------------------------------
modelo_lineal <-
  parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode("regression")

modelo_arbol <-
  parsnip::decision_tree(
    cost_complexity = tune::tune(),   # se elige en validación
    tree_depth = 30,
    min_n = 20
  ) |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode("regression")

modelo_bosque <-
  parsnip::rand_forest(
    mtry  = tune::tune(),             # se eligen en validación
    min_n = tune::tune(),
    trees = 500
  ) |>
  parsnip::set_engine("ranger", importance = "permutation") |>
  parsnip::set_mode("regression")

# -----------------------------------------------------------------------------
# 4. Selección de hiperparámetros con el conjunto de validación
# -----------------------------------------------------------------------------
set.seed(123)
resultados_arbol <- tune::tune_grid(
  modelo_arbol,
  preprocessor = formula_modelo,
  resamples = muestras,
  grid = tibble::tibble(cost_complexity = c(0.0001, 0.001, 0.005, 0.01, 0.05)),
  metrics = metricas
)

set.seed(123)
resultados_bosque <- tune::tune_grid(
  modelo_bosque,
  preprocessor = formula_modelo,
  resamples = muestras,
  grid = tidyr::expand_grid(mtry = c(2, 4, 6, 8), min_n = c(5, 10, 20)),
  metrics = metricas
)

tune::collect_metrics(resultados_arbol)  |> readr::write_csv("resultados/validacion_arbol.csv")
tune::collect_metrics(resultados_bosque) |> readr::write_csv("resultados/validacion_bosque.csv")

mejor_arbol  <- tune::select_best(resultados_arbol,  metric = "rmse")
mejor_bosque <- tune::select_best(resultados_bosque, metric = "rmse")
print(mejor_arbol); print(mejor_bosque)

# -----------------------------------------------------------------------------
# 5. Ajuste final (entrenamiento + validación) y evaluación ÚNICA en prueba
# -----------------------------------------------------------------------------
entrenamiento_validacion <- bind_rows(entrenamiento, validacion)

ajuste_lineal <- modelo_lineal |>
  parsnip::fit(formula_modelo, data = entrenamiento_validacion)

ajuste_arbol <- modelo_arbol |>
  tune::finalize_model(mejor_arbol) |>
  parsnip::fit(formula_modelo, data = entrenamiento_validacion)

set.seed(123)
ajuste_bosque <- modelo_bosque |>
  tune::finalize_model(mejor_bosque) |>
  parsnip::fit(formula_modelo, data = entrenamiento_validacion)

# Línea base: predecir siempre la mediana (lo mínimo que un modelo debe superar)
mediana_base <- median(entrenamiento_validacion$log_usd_kg)

predicciones <- bind_rows(
  prueba |> mutate(.pred = mediana_base, modelo = "Línea base (mediana)"),
  prueba |> bind_cols(predict(ajuste_lineal, new_data = prueba)) |> mutate(modelo = "Regresión lineal"),
  prueba |> bind_cols(predict(ajuste_arbol,  new_data = prueba)) |> mutate(modelo = "Árbol de decisión"),
  prueba |> bind_cols(predict(ajuste_bosque, new_data = prueba)) |> mutate(modelo = "Bosque aleatorio")
)

# Métricas en escala log (RMSE, MAE, R2) y error en dólares por kilo
comparacion <- predicciones |>
  group_by(modelo) |>
  metricas(truth = log_usd_kg, estimate = .pred) |>
  select(modelo, .metric, .estimate) |>
  tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>
  left_join(
    predicciones |>
      mutate(real_usd = exp(log_usd_kg), pred_usd = exp(.pred)) |>
      group_by(modelo) |>
      yardstick::mae(truth = real_usd, estimate = pred_usd) |>
      select(modelo, mae_usd_kg = .estimate),
    by = "modelo"
  ) |>
  arrange(rmse)

print(comparacion)
readr::write_csv(comparacion, "resultados/comparacion_modelos.csv")

# -----------------------------------------------------------------------------
# 6. Interpretación: importancia de variables del bosque
# -----------------------------------------------------------------------------
importancia <- tibble::enframe(ajuste_bosque$fit$variable.importance,
                               name = "variable", value = "importancia") |>
  arrange(desc(importancia))
readr::write_csv(importancia, "resultados/importancia_variables.csv")

g_imp <- ggplot(importancia, aes(reorder(variable, importancia), importancia)) +
  geom_col(fill = "#2F6690") +
  coord_flip() +
  labs(title = "Qué variables usa más el bosque aleatorio",
       subtitle = "Importancia por permutación: cuánto empeora el modelo si se desordena la variable",
       x = NULL, y = "Importancia") +
  theme_minimal(base_size = 12)
ggsave("figuras/09_importancia_variables.png", g_imp, width = 8, height = 5, dpi = 150, bg = "white")

g_pred <- predicciones |>
  filter(modelo == "Bosque aleatorio") |>
  ggplot(aes(exp(log_usd_kg), exp(.pred))) +
  geom_point(alpha = 0.25, color = "#2F6690") +
  geom_abline(linetype = "dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Bosque aleatorio: precio real vs predicho (conjunto de prueba)",
       subtitle = "La línea punteada es la predicción perfecta",
       x = "USD/kg real (escala log)", y = "USD/kg predicho (escala log)") +
  theme_minimal(base_size = 12)
ggsave("figuras/10_real_vs_predicho.png", g_pred, width = 7, height = 6, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# 7. Chequeo de robustez temporal: entrenar con 2024-2025 y probar en 2026
# -----------------------------------------------------------------------------
# Simula el uso real: predecir importaciones futuras con datos del pasado.
pasado <- filter(datos, anio <= 2025)
futuro <- filter(datos, anio == 2026)

set.seed(123)
ajuste_temporal <- modelo_bosque |>
  tune::finalize_model(mejor_bosque) |>
  parsnip::fit(formula_modelo, data = pasado)

temporal <- futuro |>
  bind_cols(predict(ajuste_temporal, new_data = futuro)) |>
  metricas(truth = log_usd_kg, estimate = .pred)
print(temporal)
readr::write_csv(temporal, "resultados/chequeo_temporal_2026.csv")
