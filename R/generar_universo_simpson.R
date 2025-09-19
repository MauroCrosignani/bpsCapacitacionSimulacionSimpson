#' Genera un Universo de Datos Simulado para la Paradoja de Simpson (Versión 2)
#'
#' @description
#' Esta función implementa un Proceso de Generación de Datos (DGP) avanzado para
#' ilustrar la Paradoja de Simpson en un contexto de negocio realista para el BPS.
#' Primero, genera una población grande de empresas y luego la filtra para
#' obtener una "población objetivo" específica (ej. antigüedad de deuda mínima,
#' sin contacto previo). Sobre esta población, asigna tratamientos multivaluados
#' ("Ausencia de Contacto", "Contacto Ligero", "Contacto Involucrado") de forma
#' sesgada, basándose en un modelo predictivo de alta precisión, para demostrar
#' cómo las conclusiones a nivel agregado pueden ser engañosas.
#'
#' @param n_empresas_total Número total de empresas a generar en la población inicial,
#'   antes del filtrado.
#' @param semilla Un número entero para fijar la semilla aleatoria y garantizar la
#'   reproducibilidad.
#' @param umbral_antiguedad_meses La antigüedad mínima de la deuda (en meses) para que
#'   una empresa sea incluida en la población objetivo.
#' @param modo_depuracion Booleano. Si es `TRUE`, imprime mensajes de diagnóstico
#'   intermedios durante la generación de datos.
#' @param p_baja Probabilidad de que una empresa en la población objetivo tenga
#'   una "Baja" cultura de pago.
#' @param p_media Probabilidad de que una empresa en la población objetivo tenga
#'   una "Media" cultura de pago. La probabilidad de "Alta" se calcula como
#'   `1 - p_baja - p_media`.
#'
#' @return Un `tibble` con la **población objetivo ya filtrada** y las siguientes
#'   columnas: `id_empresa`, `cultura_pago_real`, `riesgo_deuda`,
#'   `antiguedad_deuda_cat`, `prob_predictiva_modelo`, `tipo_contacto_BPS`,
#'   `regularizacion_observada`.
#'
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   universo_objetivo <- generar_universo_simpson(n_empresas_total = 10000)
#'   head(universo_objetivo)
#' })
generar_universo_simpson <- function(
    n_empresas_total = 100000,
    semilla = 42,
    umbral_antiguedad_meses = 12,
    modo_depuracion = FALSE,
    p_baja = 0.2,
    p_media = 0.5) {
  
  # --- 1. Validación de Entradas ---
  stopifnot(
    is.numeric(n_empresas_total), n_empresas_total > 0,
    is.numeric(umbral_antiguedad_meses), umbral_antiguedad_meses > 0,
    is.numeric(p_baja), p_baja > 0, p_baja < 1,
    is.numeric(p_media), p_media > 0, p_media < 1,
    (p_baja + p_media) < 1
  )
  
  # --- 2. Inicialización ---
  set.seed(semilla)
  
  # --- 3. Proceso de Generación de Datos (DGP) ---
  poblacion_inicial <- tibble::tibble(id_empresa = 1:n_empresas_total) |>
    dplyr::mutate(
      antiguedad_deuda_meses = runif(n_empresas_total, min = 1, max = 60),
      fue_contactada_previamente = rbinom(n_empresas_total, size = 1, prob = 0.7)
    )
  if (modo_depuracion) message(paste("Paso 1-2: Generadas", nrow(poblacion_inicial), "empresas iniciales."))
  
  poblacion_objetivo <- poblacion_inicial |>
    dplyr::filter(
      .data$antiguedad_deuda_meses > umbral_antiguedad_meses,
      .data$fue_contactada_previamente == 0
    )
  if (modo_depuracion) message(paste("Paso 3: Filtrada la población objetivo a", nrow(poblacion_objetivo), "empresas."))
  
  universo_final <- poblacion_objetivo |>
    dplyr::mutate(
      cultura_pago_real = sample(
        c("Baja", "Media", "Alta"), size = dplyr::n(), replace = TRUE,
        prob = c(p_baja, p_media, 1 - p_baja - p_media)
      ) |> factor(levels = c("Baja", "Media", "Alta")),
      riesgo_deuda = rnorm(dplyr::n(), mean = 2 - as.integer(.data$cultura_pago_real), sd = 0.5),
      antiguedad_deuda_cat = dplyr::ntile(.data$antiguedad_deuda_meses, 2) |>
        factor(levels = c(1, 2), labels = c("Antigua", "Muy Antigua"))
    ) |>
    dplyr::mutate(
      # FIX 2: Aumentamos el efecto de la cultura de pago para forzar la paradoja
      logit_baseline = -2.5 + (as.integer(.data$cultura_pago_real) * 2.5) - (.data$riesgo_deuda * 0.5),
      efecto_ligero = 0.8,
      efecto_involucrado = 1.8 + dplyr::if_else(.data$cultura_pago_real == "Baja", 1.2, 0),
      Y_ausencia = rbinom(dplyr::n(), 1, plogis(.data$logit_baseline)),
      Y_ligero = rbinom(dplyr::n(), 1, plogis(.data$logit_baseline + .data$efecto_ligero)),
      Y_involucrado = rbinom(dplyr::n(), 1, plogis(.data$logit_baseline + .data$efecto_involucrado))
    ) |>
    dplyr::mutate(
      prob_predictiva_modelo = {
        prob_pred <- tryCatch({
          # FIX 1: Usar pick() para ser compatible con dplyr >= 1.1.0
          modelo_predictivo <- glm(Y_ausencia ~ riesgo_deuda + antiguedad_deuda_cat, data = dplyr::pick(dplyr::everything()), family = "binomial")
          predict(modelo_predictivo, type = "response")
        }, error = function(e) {
          warning("GLM falló: ", e$message)
          rep(NA_real_, dplyr::n())
        })
        ruido <- rnorm(dplyr::n(), mean = 0, sd = 0.03)
        pmin(1, pmax(0, prob_pred + ruido))
      }
    ) |>
    dplyr::mutate(
      tipo_contacto_BPS = dplyr::case_when(
        is.na(.data$prob_predictiva_modelo) ~ "Contacto Involucrado",
        .data$prob_predictiva_modelo > 0.8 ~ "Ausencia de Contacto",
        .data$prob_predictiva_modelo > 0.5 ~ "Contacto Ligero",
        TRUE ~ "Contacto Involucrado"
      )
    ) |>
    dplyr::mutate(
      regularizacion_observada = dplyr::case_when(
        .data$tipo_contacto_BPS == "Ausencia de Contacto" ~ .data$Y_ausencia,
        .data$tipo_contacto_BPS == "Contacto Ligero" ~ .data$Y_ligero,
        .data$tipo_contacto_BPS == "Contacto Involucrado" ~ .data$Y_involucrado
      ),
      tipo_contacto_BPS = factor(.data$tipo_contacto_BPS, levels = c("Ausencia de Contacto", "Contacto Ligero", "Contacto Involucrado"))
    ) |>
    dplyr::select(
      id_empresa, cultura_pago_real, riesgo_deuda, antiguedad_deuda_cat,
      prob_predictiva_modelo, tipo_contacto_BPS, regularizacion_observada
    )
  
  if (modo_depuracion) message(paste("Paso 8: Asignados tratamientos:\n", paste(capture.output(table(universo_final$tipo_contacto_BPS)), collapse = "\n")))
  
  return(universo_final)
}