# Este archivo prueba la función generar_universo_simpson()

test_that("La salida tiene la estructura y dimensiones correctas", {
  n_filas <- 100
  df <- generar_universo_simpson(n_empresas = n_filas, semilla = 1)
  
  # Es un tibble/data.frame?
  testthat::expect_s3_class(df, "data.frame")
  # Tiene las filas correctas?
  testthat::expect_equal(nrow(df), n_filas)
  # Tiene los nombres de columna esperados?
  columnas_esperadas <- c(
    "id_empresa", "cultura_pago_real", "riesgo_deuda", "antiguedad_deuda_cat",
    "prob_predictiva_modelo", "decision_juicio_BPS", "regularizacion_observada"
  )
  testthat::expect_named(df, columnas_esperadas)
  
  # Tienen las columnas los tipos correctos?
  testthat::expect_type(df$id_empresa, "integer")
  testthat::expect_s3_class(df$cultura_pago_real, "factor")
  testthat::expect_type(df$riesgo_deuda, "double")
  testthat::expect_s3_class(df$antiguedad_deuda_cat, "factor")
  testthat::expect_type(df$prob_predictiva_modelo, "double")
  testthat::expect_s3_class(df$decision_juicio_BPS, "factor")
  testthat::expect_type(df$regularizacion_observada, "integer")
})

test_that("La semilla garantiza la reproducibilidad", {
  df1 <- generar_universo_simpson(n_empresas = 50, semilla = 123)
  df2 <- generar_universo_simpson(n_empresas = 50, semilla = 123)
  df3 <- generar_universo_simpson(n_empresas = 50, semilla = 999)
  
  # Dos llamadas con la misma semilla deben ser idénticas
  testthat::expect_identical(df1, df2)
  # Una llamada con semilla diferente no debe ser idéntica
  testthat::expect_false(identical(df1, df3))
})

test_that("La paradoja de Simpson se manifiesta con parámetros por defecto", {
  df <- generar_universo_simpson(n_empresas = 20000, semilla = 42) # N grande para estabilidad
  
  # Calcular ATE "naive" (agregado)
  tasas_agregadas <- df |>
    dplyr::group_by(decision_juicio_BPS) |>
    dplyr::summarise(tasa_reg = mean(regularizacion_observada))
  
  tasa_juicio <- tasas_agregadas$tasa_reg[tasas_agregadas$decision_juicio_BPS == "Juicio"]
  tasa_no_juicio <- tasas_agregadas$tasa_reg[tasas_agregadas$decision_juicio_BPS == "No Juicio"]
  ate_naive <- tasa_juicio - tasa_no_juicio
  
  # A nivel agregado, el juicio parece PERJUDICIAL
  testthat::expect_lt(ate_naive, 0)
  
  # Calcular ATE para el subgrupo de CULTURA BAJA
  tasas_cultura_baja <- df |>
    dplyr::filter(cultura_pago_real == "Baja") |>
    dplyr::group_by(decision_juicio_BPS) |>
    dplyr::summarise(tasa_reg = mean(regularizacion_observada))
  
  tasa_juicio_baja <- tasas_cultura_baja$tasa_reg[tasas_cultura_baja$decision_juicio_BPS == "Juicio"]
  tasa_no_juicio_baja <- tasas_cultura_baja$tasa_reg[tasas_cultura_baja$decision_juicio_BPS == "No Juicio"]
  ate_cultura_baja <- tasa_juicio_baja - tasa_no_juicio_baja
  
  # Dentro del grupo de cultura baja, el juicio es BENEFICIOSO
  testthat::expect_gt(ate_cultura_baja, 0)
})