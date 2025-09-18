#' Resume los Resultados de una Simulación Monte Carlo
#'
#' @description
#' Esta función toma el data.frame de resultados generado por
#' `ejecutar_simulacion_mc()` y calcula estadísticas descriptivas clave para
#' cada métrica (ATE naive, ATE ajustados). El resultado es una tabla de
#' resumen concisa, ideal para informes y visualizaciones.
#'
#' @param datos_simulacion Un `tibble` o `data.frame` que contiene los resultados
#'   de la simulación, con columnas como `ate_naive`, `ate_ajustado_cultura_baja`,
#'   y `ate_ajustado_cultura_alta`.
#' @param nivel_confianza El nivel de confianza para el cálculo de los
#'   intervalos (por ejemplo, 0.95 para un 95% de confianza).
#'
#' @return Un `tibble` de resumen con una fila por cada métrica y las
#'   siguientes columnas: `metrica`, `media`, `desv_est`, `n_sims`,
#'   `ci_inferior`, `ci_superior`.
#'
#' @importFrom stats qnorm sd
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   # 1. Ejecutar una simulación de ejemplo
#'   resultados_mc <- ejecutar_simulacion_mc(N_simulaciones = 100, n_empresas = 500)
#'   # 2. Resumir los resultados
#'   resumen <- resumir_simulacion(resultados_mc)
#'   print(resumen)
#' })
resumir_simulacion <- function(datos_simulacion, nivel_confianza = 0.95) {
  # --- 1. Validación de Entradas ---
  columnas_requeridas <- c("ate_naive", "ate_ajustado_cultura_baja", "ate_ajustado_cultura_alta")
  stopifnot(
    is.data.frame(datos_simulacion),
    all(columnas_requeridas %in% names(datos_simulacion)),
    is.numeric(nivel_confianza), nivel_confianza > 0, nivel_confianza < 1
  )
  
  # --- 2. Transformación y Resumen ---
  z_score <- stats::qnorm(1 - (1 - nivel_confianza) / 2)
  
  resumen_df <- datos_simulacion |>
    # Paso A: Pivotar a formato largo
    tidyr::pivot_longer(
      cols = dplyr::all_of(columnas_requeridas),
      names_to = "metrica",
      values_to = "valor"
    ) |>
    # Paso B: Agrupar por métrica
    dplyr::group_by(.data$metrica) |>
    # Paso C y D: Resumir y calcular IC
    dplyr::summarise(
      media = mean(.data$valor, na.rm = TRUE),
      desv_est = stats::sd(.data$valor, na.rm = TRUE),
      n_sims = sum(!is.na(.data$valor)),
      .groups = "drop" # Desagrupar después del summarise
    ) |>
    dplyr::mutate(
      error_estandar = .data$desv_est / sqrt(.data$n_sims),
      ci_inferior = .data$media - z_score * .data$error_estandar,
      ci_superior = .data$media + z_score * .data$error_estandar
    ) |>
    # Reordenar columnas para mayor claridad
    dplyr::select(
      "metrica", "media", "desv_est", "n_sims", "ci_inferior", "ci_superior"
    )
  
  return(resumen_df)
}