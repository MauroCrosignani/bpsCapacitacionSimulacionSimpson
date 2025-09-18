# Este archivo prueba la función graficar_paradoja_simpson()

test_that("La función devuelve un objeto ggplot con las capas correctas", {
  # Crear un data.frame de resumen de prueba basado en el dput
  datos_resumen_test <- structure(
    list(
      metrica = c("ate_ajustado_cultura_alta", "ate_ajustado_cultura_baja", "ate_naive"),
      media = c(-0.32, 0.14, -0.38),
      desv_est = c(0.013, 0.172, 0.013),
      n_sims = c(100L, 98L, 100L),
      ci_inferior = c(-0.33, 0.11, -0.39),
      ci_superior = c(-0.32, 0.17, -0.38)
    ),
    row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")
  )
  
  # Llamar a la función bajo prueba
  p <- graficar_paradoja_simpson(datos_resumen_test)
  
  # 1. Verificar que el resultado es un objeto ggplot
  testthat::expect_s3_class(p, "ggplot")
  
  # 2. Verificar que el número de capas es el esperado (4 en este caso)
  testthat::expect_length(p$layers, 4)
  
  # 3. Verificar la clase de la geometría de cada capa en orden
  # Capa 1: geom_vline
  testthat::expect_true(inherits(p$layers[[1]]$geom, "GeomVline"))
  # Capa 2: geom_col
  testthat::expect_true(inherits(p$layers[[2]]$geom, "GeomCol"))
  # Capa 3: geom_errorbarh
  testthat::expect_true(inherits(p$layers[[3]]$geom, "GeomErrorbarh"))
  # Capa 4: geom_text
  testthat::expect_true(inherits(p$layers[[4]]$geom, "GeomText"))
})

test_that("La función arroja un error si faltan columnas", {
  datos_invalidos <- data.frame(metrica = "a", media = 1) # Faltan ci_inferior/superior
  
  testthat::expect_error(
    graficar_paradoja_simpson(datos_invalidos)
  )
})