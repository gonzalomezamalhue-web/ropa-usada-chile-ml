# Precio por kilo de la ropa usada importada en Chile

Trabajo Integrado Grupal (TIG) · **Ciencia de Datos** · Universidad Andrés Bello · Primavera 2026
Profesor: Felipe Cárcamo Jiménez · Integrantes: **Gonzalo Meza Malhue** y **Cristian Tapia**

## Pregunta de investigación

¿Qué factores explican el precio por kilo (USD CIF/kg) de la ropa usada que importa Chile, y es posible predecirlo con la información disponible en la declaración de importación?

## Datos

| Elemento | Detalle |
|---|---|
| Institución | Servicio Nacional de Aduanas de Chile |
| Conjunto | [Base de datos Operaciones de Ingreso](https://www.aduana.cl/base-de-datos-operaciones-de-ingreso/aduana/2024-11-12/122745.html) + diccionario `tablas_de_codigos_v2_0.xlsx` |
| Período | Enero 2024 – octubre 2026 |
| Filtro | Partida arancelaria 6309 (artículos de prendería / ropa usada) |
| Unidad de observación | Ítem de declaración de importación (agregado por Aduanas) |
| Registros | 7.281 brutos → 6.103 importaciones comerciales tras la limpieza |

Los archivos originales de Aduanas pesan entre 115 y 150 MB cada uno, así que **no se incluyen**. El repositorio trae los registros ya filtrados de la partida 6309 en `datos/brutos/`, y `R/01_obtener_datos.R` documenta cómo reconstruirlos desde la fuente.

## Estructura

```
├── R/
│   ├── 00_funciones.R          # lectura del diccionario de Aduanas
│   ├── 01_obtener_datos.R      # (opcional) descarga y filtro de la partida 6309
│   ├── 02_limpieza.R           # limpieza y registro de cada filtro
│   ├── 03_eda.R                # análisis exploratorio, figuras 01-08
│   ├── 04_modelado.R           # línea base, lineal, árbol, bosque; partición 60/20/20
│   └── 05_segmentacion.R       # K-means (tidyclust)
├── datos/
│   ├── brutos/                 # partida 6309 por año + diccionario de códigos
│   └── procesados/             # datos limpios y registro de limpieza
├── figuras/                    # gráficos generados
├── resultados/                 # tablas de resultados (CSV)
├── verificacion/               # verificación de cifras con los motores rpart/ranger
└── instalar_paquetes.R         # dependencias
```

## Cómo ejecutar

Requisitos: R 4.3 o superior y RStudio.

1. Descargue o clone este repositorio.
2. En RStudio: **Session → Set Working Directory → Choose Directory…** y elija la carpeta del repositorio.
3. Instale las dependencias (solo la primera vez):
   ```r
   source("instalar_paquetes.R")
   ```
4. Ejecute los scripts en orden:
   ```r
   source("R/02_limpieza.R",     encoding = "UTF-8")
   source("R/03_eda.R",          encoding = "UTF-8")
   source("R/04_modelado.R",     encoding = "UTF-8")
   source("R/05_segmentacion.R", encoding = "UTF-8")
   ```

El script `01_obtener_datos.R` solo hace falta para reconstruir los datos desde los archivos originales de Aduanas.

## Metodología

- **Variable objetivo:** log(USD CIF / kilo neto).
- **Predictores:** año, mes, aduana, vía de transporte, país de origen, continente, origen no precisado, triangulación, cláusula de compraventa, subpartida y log(kg).
- **Fuga de información evitada:** se excluyen `CIF_US` y `AD_VALOREM_US` (este último es el 6% del CIF).
- **Partición:** 60% entrenamiento / 20% validación / 20% prueba (`rsample::initial_validation_split`, semilla 123). Los hiperparámetros se eligen en validación y la prueba se usa una sola vez.
- **Chequeo temporal:** entrenar con 2024–2025 y evaluar en 2026.

## Resultados principales

| Modelo | RMSE (log) | MAE (log) | R² | Error medio (USD/kg) |
|---|---|---|---|---|
| **Bosque aleatorio** | **0,596** | **0,413** | **0,577** | **0,68** |
| Árbol de decisión | 0,679 | 0,479 | 0,461 | 0,78 |
| Regresión lineal | 0,722 | 0,569 | 0,378 | 0,91 |
| Línea base (mediana) | 0,921 | 0,747 | — | 1,16 |

- El bosque aleatorio reduce el error un 35% frente a la línea base y mantiene su desempeño al predecir 2026 (R² 0,578).
- Variables más importantes: tamaño del embarque, aduana, país de origen, subpartida y cláusula.
- K-means (k = 4, silueta 0,42): el 73% de las toneladas corresponde a embarques grandes y baratos (granel).

## Limitaciones

- Los datos no informan la calidad del fardo, que es una determinante clave del precio.
- El 26% de los registros tiene país de origen "no precisado".
- Los resultados son asociaciones predictivas, no efectos causales.

## Uso de inteligencia artificial

Se utilizó Claude (Anthropic) como apoyo para evaluar fuentes de datos, filtrar los archivos de Aduanas, redactar y depurar el código R y estructurar el informe. Las decisiones metodológicas fueron revisadas por el equipo y los scripts se verificaron ejecutándolos de forma independiente.

## Licencia y datos

El código se publica bajo licencia MIT (ver `LICENSE`). Los datos provienen del portal de datos abiertos del Servicio Nacional de Aduanas de Chile y se redistribuyen filtrados con fines académicos, citando la fuente.
