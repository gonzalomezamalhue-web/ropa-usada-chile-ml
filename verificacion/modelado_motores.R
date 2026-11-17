# Versión de verificación del modelado usando directamente los motores
# (rpart, ranger) que parsnip usa por debajo. Sirve para comprobar cifras.
suppressMessages({library(dplyr)})
set.seed(123)

datos <- readr::read_csv("datos/procesados/ropa_usada_limpia.csv", show_col_types = FALSE) |>
  mutate(
    subpartida = factor(subpartida),
    aduana = factor(forcats::fct_lump_min(aduana, 30, other_level = "Otra")),
    via = factor(forcats::fct_lump_min(via, 30, other_level = "Otra")),
    pais_origen = factor(forcats::fct_lump_n(pais_origen, 12, other_level = "Otros")),
    continente = factor(continente),
    clausula = factor(forcats::fct_lump_min(coalesce(clausula, "Sin dato"), 30, other_level = "Otra")),
    origen_no_precisado = factor(origen_no_precisado),
    triangulado = factor(triangulado)
  )

formula <- log_usd_kg ~ anio + mes + aduana + via + pais_origen + continente +
  origen_no_precisado + triangulado + clausula + subpartida + log_kg

set.seed(123)
particion <- rsample::initial_validation_split(datos, prop = c(0.6, 0.2), strata = log_usd_kg)
ent <- rsample::training(particion); val <- rsample::validation(particion); pru <- rsample::testing(particion)
cat("n:", nrow(ent), nrow(val), nrow(pru), "\n")

met <- function(y, p) c(rmse = sqrt(mean((y - p)^2)), mae = mean(abs(y - p)),
                        rsq = cor(y, p)^2, mae_usd = mean(abs(exp(y) - exp(p))))

# Línea base: mediana del entrenamiento
base <- median(ent$log_usd_kg)

# Lineal
lin <- lm(formula, data = ent)

# Árbol: elegir cost_complexity en validación
cps <- c(0.0001, 0.001, 0.005, 0.01, 0.05)
res_arbol <- sapply(cps, function(cp) {
  m <- rpart::rpart(formula, data = ent, cp = cp, maxdepth = 30, minsplit = 20)
  met(val$log_usd_kg, predict(m, val))["rmse"]
})
print(data.frame(cp = cps, rmse_val = res_arbol))
cp_mejor <- cps[which.min(res_arbol)]

# Bosque: elegir mtry y min_n en validación
grilla <- expand.grid(mtry = c(2, 4, 6, 8), min_n = c(5, 10, 20))
res_bosque <- apply(grilla, 1, function(g) {
  set.seed(123)
  m <- ranger::ranger(formula, data = ent, num.trees = 500, mtry = g["mtry"], min.node.size = g["min_n"])
  met(val$log_usd_kg, predict(m, val)$predictions)["rmse"]
})
grilla$rmse_val <- res_bosque; print(grilla)
g_mejor <- grilla[which.min(res_bosque), ]

# Reajuste final con entrenamiento + validación y evaluación única en prueba
ent_val <- bind_rows(ent, val)
lin_f <- lm(formula, data = ent_val)
arbol_f <- rpart::rpart(formula, data = ent_val, cp = cp_mejor, maxdepth = 30, minsplit = 20)
set.seed(123)
bosque_f <- ranger::ranger(formula, data = ent_val, num.trees = 500, mtry = g_mejor$mtry,
                           min.node.size = g_mejor$min_n, importance = "permutation")

res <- rbind(
  base   = met(pru$log_usd_kg, rep(median(ent_val$log_usd_kg), nrow(pru))),
  lineal = met(pru$log_usd_kg, predict(lin_f, pru)),
  arbol  = met(pru$log_usd_kg, predict(arbol_f, pru)),
  bosque = met(pru$log_usd_kg, predict(bosque_f, pru)$predictions)
)
print(round(res, 3))
print(sort(bosque_f$variable.importance, decreasing = TRUE))
cat("cp:", cp_mejor, " mtry/min_n:", g_mejor$mtry, g_mejor$min_n, "\n")

# Chequeo temporal: entrenar 2024-2025, probar 2026
tr <- filter(datos, anio <= 2025); te <- filter(datos, anio == 2026)
set.seed(123)
bt <- ranger::ranger(formula, data = tr, num.trees = 500, mtry = g_mejor$mtry, min.node.size = g_mejor$min_n)
print(round(rbind(base_t = met(te$log_usd_kg, rep(median(tr$log_usd_kg), nrow(te))),
                  bosque_t = met(te$log_usd_kg, predict(bt, te)$predictions)), 3))
