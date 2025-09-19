# Este archivo prueba la versión 2 de la función resumir_simulacion()

test_that("La función resume correctamente la nueva estructura de datos", {
  # Crear un tibble de prueba falso pero conocido, con 3 simulaciones
  datos_test <- tibble::tibble(
    id_simulacion = 1:3,
    # Datos para 'agg' y 'ausencia'
    tasa_ausencia_de_contacto_agg = c(0.8, 0.9, 1.0),
    # Datos para 'baja' y 'ligero'
    tasa_contacto_ligero_baja = c(0.2, 0.3, NA),
    # Rellenar el resto para que las columnas existan
    tasa_contacto_ligero_agg = 1, tasa_contacto_involucrado_agg = 1,
    tasa_ausencia_de_contacto_baja = 1, tasa_contacto_involucrado_baja = 1,
    tasa_ausencia_de_contacto_media = 1, tasa_contacto_ligero_media = 1,
    tasa_contacto_involucrado_media = 1, tasa_ausencia_de_contacto_alta = 1,
    tasa_contacto_ligero_alta = 1, tasa_contacto_involucrado_alta = 1
  )
  
  resumen <- resumir_simulacion(datos_test)
  
  # 1. Verificar la estructura de salida
  testthat::expect_s3_class(resumen, "data.frame")
  testthat::expect_equal(nrow(resumen), 12) # 4 grupos x 3 contactos
  testthat::expect_equal(ncol(resumen), 7)
  testthat::expect_named(resumen, c(
    "grupo_cultura", "tipo_contacto", "media", "desv_est",
    "n_sims", "ci_inferior", "ci_superior"
  ))
  
  # 2. Verificar cálculos para un grupo específico
  resumen_agg_ausencia <- resumen |>
    dplyr::filter(grupo_cultura == "Agregado", tipo_contacto == "Ausencia de Contacto")
  
  testthat::expect_equal(resumen_agg_ausencia$n_sims, 3)
  testthat::expect_equal(resumen_agg_ausencia$media, 0.9)
  testthat::expect_equal(resumen_agg_ausencia$desv_est, 0.1)
  
  # 3. Verificar manejo de NAs
  resumen_baja_ligero <- resumen |>
    dplyr::filter(grupo_cultura == "Cultura Baja", tipo_contacto == "Contacto Ligero")
  
  testthat::expect_equal(resumen_baja_ligero$n_sims, 2)
  testthat::expect_equal(resumen_baja_ligero$media, 0.25)
  testthat::expect_equal(round(resumen_baja_ligero$desv_est, 4), 0.0707)
})