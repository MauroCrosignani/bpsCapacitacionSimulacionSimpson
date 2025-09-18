# Este archivo prueba la función ejecutar_simulacion_mc()

test_that("La salida tiene la estructura y dimensiones correctas", {
  n_sim <- 5
  # Usamos un n_empresas pequeño para que la prueba sea rápida
  resultados <- ejecutar_simulacion_mc(N_simulaciones = n_sim, n_empresas = 100)
  
  # Es un tibble/data.frame?
  testthat::expect_s3_class(resultados, "data.frame")
  
  # Tiene las filas correctas?
  # Podrían ser menos si alguna simulación fallara, por lo que probamos <=
  testthat::expect_lte(nrow(resultados), n_sim)
  # Para este caso de prueba simple, esperamos que todas funcionen
  testthat::expect_equal(nrow(resultados), n_sim)
  
  
  # Tiene los nombres de columna esperados?
  columnas_esperadas <- c(
    "id_simulacion", "ate_naive",
    "ate_ajustado_cultura_baja", "ate_ajustado_cultura_alta"
  )
  testthat::expect_named(resultados, columnas_esperadas)
  
  # Tienen las columnas los tipos correctos?
  testthat::expect_type(resultados$id_simulacion, "integer")
  testthat::expect_type(resultados$ate_naive, "double")
  testthat::expect_type(resultados$ate_ajustado_cultura_baja, "double")
  testthat::expect_type(resultados$ate_ajustado_cultura_alta, "double")
})

test_that("La función maneja correctamente los argumentos pasados con ...", {
  # Esta prueba no verifica la mecánica interna de `...`, sino una consecuencia
  # observable de su uso. Si pasamos un `n_empresas` muy pequeño, es posible
  # que no se asignen casos a todos los grupos de tratamiento, generando NAs.
  # Esto confirma que el parámetro se está utilizando.
  # Suprimimos advertencias de `glm` que pueden surgir con pocos datos.
  suppressWarnings({
    # Con n_empresas = 10, es muy probable que algún subgrupo no tenga
    # asignaciones a Juicio/No Juicio, resultando en NA para el ATE.
    resultados <- ejecutar_simulacion_mc(N_simulaciones = 10, n_empresas = 10)
  })
  
  # Verificamos que al menos un NA fue generado, lo que implica que el
  # pequeño `n_empresas` tuvo un efecto.
  hay_nas <- any(is.na(resultados$ate_naive)) ||
    any(is.na(resultados$ate_ajustado_cultura_baja)) ||
    any(is.na(resultados$ate_ajustado_cultura_alta))
  
  testthat::expect_true(hay_nas)
})