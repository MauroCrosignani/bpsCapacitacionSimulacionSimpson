# Este archivo prueba la función resumir_simulacion()

test_that("Los cálculos de resumen son correctos para datos conocidos", {
  datos_test <- tibble::tibble(
    ate_naive = c(1, 2, 3),
    ate_ajustado_cultura_baja = c(10, 20, 30),
    ate_ajustado_cultura_alta = c(0, 0, 0)
  )
  
  resumen <- resumir_simulacion(datos_test)
  
  # Verificar estructura
  testthat::expect_equal(nrow(resumen), 3)
  testthat::expect_named(resumen, c("metrica", "media", "desv_est", "n_sims", "ci_inferior", "ci_superior"))
  
  # Verificar cálculos para ate_naive
  resumen_naive <- dplyr::filter(resumen, metrica == "ate_naive")
  testthat::expect_equal(resumen_naive$media, 2)
  testthat::expect_equal(resumen_naive$desv_est, 1)
  testthat::expect_equal(resumen_naive$n_sims, 3)
  
  # Verificar cálculos para ate_ajustado_cultura_baja
  resumen_baja <- dplyr::filter(resumen, metrica == "ate_ajustado_cultura_baja")
  testthat::expect_equal(resumen_baja$media, 20)
  testthat::expect_equal(resumen_baja$desv_est, 10)
  
  # Verificar cálculos para ate_ajustado_cultura_alta
  resumen_alta <- dplyr::filter(resumen, metrica == "ate_ajustado_cultura_alta")
  testthat::expect_equal(resumen_alta$media, 0)
  # FIX: sd(c(0,0,0)) es 0, no NA. Corregimos la expectativa del test.
  testthat::expect_equal(resumen_alta$desv_est, 0)
})


test_that("La función maneja NAs correctamente", {
  datos_test_na <- tibble::tibble(
    ate_naive = c(1, 3, NA, 5),
    ate_ajustado_cultura_baja = c(10, 20, NA, NA),
    ate_ajustado_cultura_alta = c(NA, NA, NA, NA)
  )
  
  resumen <- resumir_simulacion(datos_test_na)
  
  # Verificar cálculos para ate_naive con NAs
  resumen_naive <- dplyr::filter(resumen, metrica == "ate_naive")
  testthat::expect_equal(resumen_naive$n_sims, 3)
  testthat::expect_equal(resumen_naive$media, 3) # (1+3+5)/3
  testthat::expect_equal(resumen_naive$desv_est, 2) # sd(c(1,3,5))
  
  # Verificar cálculos para ate_ajustado_cultura_baja con NAs
  resumen_baja <- dplyr::filter(resumen, metrica == "ate_ajustado_cultura_baja")
  testthat::expect_equal(resumen_baja$n_sims, 2)
  testthat::expect_equal(resumen_baja$media, 15)
})


test_that("La función arroja un error si faltan columnas", {
  datos_invalidos <- data.frame(a = 1:3, b = 4:6)
  
  testthat::expect_error(
    resumir_simulacion(datos_invalidos),
    regexp = "all\\(columnas_requeridas %in% names\\(datos_simulacion\\)\\) is not TRUE"
  )
})

test_that("La función arroja un error con nivel de confianza inválido", {
  datos_test <- tibble::tibble(
    ate_naive = 1, ate_ajustado_cultura_baja = 1, ate_ajustado_cultura_alta = 1
  )
  testthat::expect_error(resumir_simulacion(datos_test, nivel_confianza = 1.1))
  testthat::expect_error(resumir_simulacion(datos_test, nivel_confianza = 0))
})