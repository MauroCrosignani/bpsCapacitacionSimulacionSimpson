# Este archivo prueba la versión 2 de la función ejecutar_simulacion_mc()

test_that("La salida tiene la nueva estructura y dimensiones correctas", {
  n_sim <- 3
  # Usar n_empresas_total suficientemente grande para asegurar que la población
  # objetivo no sea vacía.
  resultados <- ejecutar_simulacion_mc(N_simulaciones = n_sim, n_empresas_total = 10000)
  
  # Verificar dimensiones
  testthat::expect_s3_class(resultados, "data.frame")
  testthat::expect_equal(nrow(resultados), n_sim)
  testthat::expect_equal(ncol(resultados), 13)
  
  # Verificar nombres de columnas
  nombres_esperados <- c(
    # Agregado
    "tasa_ausencia_de_contacto_agg", "tasa_contacto_ligero_agg", "tasa_contacto_involucrado_agg",
    # Cultura Baja
    "tasa_ausencia_de_contacto_baja", "tasa_contacto_ligero_baja", "tasa_contacto_involucrado_baja",
    # Cultura Media
    "tasa_ausencia_de_contacto_media", "tasa_contacto_ligero_media", "tasa_contacto_involucrado_media",
    # Cultura Alta
    "tasa_ausencia_de_contacto_alta", "tasa_contacto_ligero_alta", "tasa_contacto_involucrado_alta",
    # ID
    "id_simulacion"
  )
  testthat::expect_named(resultados, nombres_esperados, ignore.order = TRUE)
  
  # Verificar tipos de datos
  columnas_tasas <- setdiff(names(resultados), "id_simulacion")
  for (col in columnas_tasas) {
    testthat::expect_type(resultados[[col]], "double")
  }
  testthat::expect_type(resultados$id_simulacion, "integer")
})