#' Resume los Resultados de una Simulación Monte Carlo (Versión 2)
#'
#' @description
#' Esta función toma el data.frame ancho de resultados generado por la nueva
#' versión de `ejecutar_simulacion_mc()` y lo transforma en un formato largo y
#' tidy. Calcula estadísticas descriptivas clave (media, sd, IC) para cada
#' una de las 12 combinaciones de métricas (tipo de contacto x subgrupo de cultura).
#'
#' @param datos_simulacion Un `tibble` o `data.frame` que contiene los resultados
#'   de la simulación, con 12 columnas de tasas como `tasa_ausencia_de_contacto_agg`.
#' @param nivel_confianza El nivel de confianza para el cálculo de los
#'   intervalos (por ejemplo, 0.95 para un 95% de confianza).
#'
#' @return Un `tibble` de resumen largo y tidy con una fila por cada métrica/subgrupo
#'   y las siguientes columnas: `grupo_cultura`, `tipo_contacto`, `media`,
#'   `desv_est`, `n_sims`, `ci_inferior`, `ci_superior`.
#'
#' @importFrom stats qnorm sd
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   # 1. Ejecutar una simulación de ejemplo
#'   resultados_mc <- ejecutar_simulacion_mc(
#'     N_simulaciones = 10,
#'     n_empresas_total = 5000
#'   )
#'   # 2. Resumir los resultados en formato tidy
#'   resumen <- resumir_simulacion(resultados_mc)
#'   print(resumen)
#' })
resumir_simulacion <- function(datos_simulacion, nivel_confianza = 0.95) {
  # --- 1. Validación de Entradas ---
  columnas_requeridas <- c(
    "tasa_ausencia_de_contacto_agg", "tasa_contacto_ligero_agg", "tasa_contacto_involucrado_agg",
    "tasa_ausencia_de_contacto_baja", "tasa_contacto_ligero_baja", "tasa_contacto_involucrado_baja",
    "tasa_ausencia_de_contacto_media", "tasa_contacto_ligero_media", "tasa_contacto_involucrado_media",
    "tasa_ausencia_de_contacto_alta", "tasa_contacto_ligero_alta", "tasa_contacto_involucrado_alta"
  )
  stopifnot(
    is.data.frame(datos_simulacion),
    all(columnas_requeridas %in% names(datos_simulacion)),
    is.numeric(nivel_confianza), nivel_confianza > 0, nivel_confianza < 1
  )
  
  # --- 2. Transformación y Resumen ---
  z_score <- stats::qnorm(1 - (1 - nivel_confianza) / 2)
  
  resumen_df <- datos_simulacion |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("tasa_"),
      names_to = c(".value", "grupo_cultura"),
      names_pattern = "tasa_(.*)_(.*)"
    ) |>
    tidyr::pivot_longer(
      cols = c("ausencia_de_contacto", "contacto_ligero", "contacto_involucrado"),
      names_to = "tipo_contacto",
      values_to = "tasa_reg"
    ) |>
    dplyr::group_by(grupo_cultura, tipo_contacto) |>
    dplyr::summarise(
      media = mean(tasa_reg, na.rm = TRUE),
      desv_est = stats::sd(tasa_reg, na.rm = TRUE),
      n_sims = sum(!is.na(tasa_reg)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      error_estandar = desv_est / sqrt(n_sims),
      ci_inferior = media - z_score * error_estandar,
      ci_superior = media + z_score * error_estandar
    ) |>
    dplyr::mutate(
      grupo_cultura = dplyr::recode_factor(grupo_cultura,
                                           "agg" = "Agregado", "baja" = "Cultura Baja",
                                           "media" = "Cultura Media", "alta" = "Cultura Alta"
      ),
      tipo_contacto = dplyr::recode_factor(tipo_contacto,
                                           "ausencia_de_contacto" = "Ausencia de Contacto",
                                           "contacto_ligero" = "Contacto Ligero",
                                           "contacto_involucrado" = "Contacto Involucrado"
      )
    ) |>
    dplyr::select(
      grupo_cultura, tipo_contacto, media, desv_est,
      n_sims, ci_inferior, ci_superior
    )
  
  return(resumen_df)
}