# Este archivo prueba la versión 2 de la función graficar_paradoja_simpson()

test_that("La función devuelve un ggplot facetado con las capas correctas", {
  # Crear un data.frame de resumen de prueba en formato tidy
  datos_resumen_test <- structure(
    list(
      grupo_cultura = structure(c(1L, 1L, 2L, 2L),
                                levels = c("Agregado", "Cultura Baja"), class = "factor"
      ),
      tipo_contacto = structure(c(1L, 2L, 1L, 2L),
                                levels = c("Ausencia de Contacto", "Contacto Ligero"), class = "factor"
      ),
      media = c(0.8, 0.7, 0.2, 0.4),
      desv_est = c(0.02, 0.03, 0.05, 0.06),
      n_sims = c(100L, 100L, 100L, 100L),
      ci_inferior = c(0.78, 0.67, 0.15, 0.34),
      ci_superior = c(0.82, 0.73, 0.25, 0.46)
    ),
    row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")
  )
  
  # Llamar a la función bajo prueba
  p <- graficar_paradoja_simpson(datos_resumen_test)
  
  # 1. Verificar que el resultado es un objeto ggplot
  testthat::expect_s3_class(p, "ggplot")
  
  # 2. Prueba Clave: Verificar que el gráfico es facetado
  testthat::expect_s3_class(p$facet, "FacetWrap")
  
  # 3. Verificar la presencia de las capas geométricas esperadas
  geoms_presentes <- sapply(p$layers, function(layer) class(layer$geom)[1])
  # FIX: Actualizar la expectativa de "GeomErrorbarh" a "GeomErrorbar"
  geoms_esperadas <- c("GeomVline", "GeomCol", "GeomErrorbar", "GeomText")
  # Usar expect_setequal para ignorar el orden
  testthat::expect_setequal(geoms_presentes, geoms_esperadas)
})