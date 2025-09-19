# Este archivo prueba la versión 2 de la función generar_universo_simpson()

test_that("La salida final tiene la nueva estructura y tipos de datos", {
  df <- generar_universo_simpson(n_empresas_total = 1000, umbral_antiguedad_meses = 1)
  
  testthat::expect_s3_class(df, "data.frame")
  columnas_esperadas <- c(
    "id_empresa", "cultura_pago_real", "riesgo_deuda", "antiguedad_deuda_cat",
    "prob_predictiva_modelo", "tipo_contacto_BPS", "regularizacion_observada"
  )
  testthat::expect_named(df, columnas_esperadas, ignore.order = TRUE)
  
  # Verificar tipos
  testthat::expect_s3_class(df$cultura_pago_real, "factor")
  testthat::expect_equal(nlevels(df$cultura_pago_real), 3)
  testthat::expect_s3_class(df$tipo_contacto_BPS, "factor")
  testthat::expect_equal(nlevels(df$tipo_contacto_BPS), 3)
  testthat::expect_type(df$regularizacion_observada, "integer")
})

test_that("El filtrado de la población objetivo funciona correctamente", {
  n_total <- 20000
  df <- generar_universo_simpson(n_empresas_total = n_total)
  
  # El número de filas final debe ser menor que el total inicial
  testthat::expect_lt(nrow(df), n_total)
  # Y no debe ser cero (sanity check)
  testthat::expect_gt(nrow(df), 0)
})

test_that("La paradoja de Simpson se manifiesta con los nuevos tratamientos", {
  # Usamos un N grande para que los promedios sean estables
  df <- generar_universo_simpson(n_empresas_total = 200000, semilla = 42)
  
  # --- Test a nivel agregado ---
  tasas_agregadas <- df |>
    dplyr::group_by(tipo_contacto_BPS) |>
    dplyr::summarise(tasa_reg = mean(regularizacion_observada, na.rm = TRUE), .groups = "drop")
  
  # FIX: Extracción robusta de los valores para evitar errores de longitud
  tasa_ausencia_agg <- tasas_agregadas |>
    dplyr::filter(tipo_contacto_BPS == "Ausencia de Contacto") |>
    dplyr::pull(tasa_reg)
  tasa_involucrado_agg <- tasas_agregadas |>
    dplyr::filter(tipo_contacto_BPS == "Contacto Involucrado") |>
    dplyr::pull(tasa_reg)
  
  # Pre-condición: Asegurarse de que ambos valores fueron encontrados antes de comparar
  testthat::expect_length(tasa_ausencia_agg, 1)
  testthat::expect_length(tasa_involucrado_agg, 1)
  
  # A nivel agregado, "Ausencia de Contacto" parece ser la mejor estrategia
  testthat::expect_gt(tasa_ausencia_agg, tasa_involucrado_agg)
  
  
  # --- Test a nivel de subgrupo (Cultura Baja) ---
  tasas_cultura_baja <- df |>
    dplyr::filter(cultura_pago_real == "Baja") |>
    dplyr::group_by(tipo_contacto_BPS) |>
    dplyr::summarise(tasa_reg = mean(regularizacion_observada, na.rm = TRUE), .groups = "drop")
  
  # FIX: Extracción robusta
  tasa_ausencia_baja <- tasas_cultura_baja |>
    dplyr::filter(tipo_contacto_BPS == "Ausencia de Contacto") |>
    dplyr::pull(tasa_reg)
  tasa_involucrado_baja <- tasas_cultura_baja |>
    dplyr::filter(tipo_contacto_BPS == "Contacto Involucrado") |>
    dplyr::pull(tasa_reg)
  
  # Pre-condición:
  testthat::expect_length(tasa_ausencia_baja, 1)
  testthat::expect_length(tasa_involucrado_baja, 1)
  
  # En este subgrupo, la relación se invierte: "Contacto Involucrado" es la mejor.
  testthat::expect_gt(tasa_involucrado_baja, tasa_ausencia_baja)
})