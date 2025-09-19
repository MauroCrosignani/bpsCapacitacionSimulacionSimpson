#' Ejecuta una Simulación Monte Carlo de la Paradoja de Simpson (Versión 2)
#'
#' @description
#' Esta función orquesta una simulación Monte Carlo utilizando la nueva versión
#' del DGP (`generar_universo_simpson`). Llama repetidamente a la función para
#' generar múltiples universos de datos y calcula las tasas de regularización
#' brutas para cada combinación de tipo de contacto y subgrupo de cultura de pago.
#'
#' @param N_simulaciones El número de veces que se repetirá la simulación.
#' @param ... Argumentos adicionales que se pasarán directamente a
#'   `generar_universo_simpson()`, como `n_empresas_total`.
#'
#' @return Un `tibble` donde cada fila corresponde a una simulación exitosa,
#'   con 13 columnas:
#'   - `id_simulacion`: Identificador numérico de la simulación.
#'   - `tasa_ausencia_de_contacto_agg`, `tasa_contacto_ligero_agg`, `tasa_contacto_involucrado_agg`: Tasas a nivel agregado.
#'   - `tasa_ausencia_de_contacto_baja`, `tasa_contacto_ligero_baja`, `tasa_contacto_involucrado_baja`: Tasas para cultura "Baja".
#'   - `tasa_ausencia_de_contacto_media`, `tasa_contacto_ligero_media`, `tasa_contacto_involucrado_media`: Tasas para cultura "Media".
#'   - `tasa_ausencia_de_contacto_alta`, `tasa_contacto_ligero_alta`, `tasa_contacto_involucrado_alta`: Tasas para cultura "Alta".
#'
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   resultados <- ejecutar_simulacion_mc(N_simulaciones = 10, n_empresas_total = 5000)
#'   print(head(resultados))
#' })
ejecutar_simulacion_mc <- function(N_simulaciones = 1000, ...) {
  # --- 1. Validación de Entradas ---
  stopifnot(
    is.numeric(N_simulaciones),
    N_simulaciones > 0,
    N_simulaciones == round(N_simulaciones)
  )
  
  # --- 2. Definición de la Lógica de una Simulación ---
  
  # Función auxiliar robusta para calcular tasas
  calcular_tasas <- function(df) {
    niveles_tratamiento <- c("Ausencia de Contacto", "Contacto Ligero", "Contacto Involucrado")
    
    tasas <- df |>
      dplyr::group_by(tipo_contacto_BPS) |>
      dplyr::summarise(tasa_reg = mean(regularizacion_observada, na.rm = TRUE), .groups = "drop") |>
      tidyr::complete(tipo_contacto_BPS = niveles_tratamiento) |>
      tidyr::pivot_wider(names_from = tipo_contacto_BPS, values_from = tasa_reg)
    
    names(tasas) <- paste0("tasa_", tolower(gsub(" ", "_", names(tasas))))
    return(tasas)
  }
  
  una_simulacion <- function(id, ...) {
    universo <- generar_universo_simpson(semilla = id, ...)
    
    tasas_agg <- calcular_tasas(universo) |>
      dplyr::rename_with(~ paste0(., "_agg"))
    
    tasas_baja <- calcular_tasas(dplyr::filter(universo, cultura_pago_real == "Baja")) |>
      dplyr::rename_with(~ paste0(., "_baja"))
    
    tasas_media <- calcular_tasas(dplyr::filter(universo, cultura_pago_real == "Media")) |>
      dplyr::rename_with(~ paste0(., "_media"))
    
    tasas_alta <- calcular_tasas(dplyr::filter(universo, cultura_pago_real == "Alta")) |>
      dplyr::rename_with(~ paste0(., "_alta"))
    
    dplyr::bind_cols(
      tasas_agg, tasas_baja, tasas_media, tasas_alta
    ) |>
      dplyr::mutate(id_simulacion = id)
  }
  
  # --- 3. Ejecución Robusta de la Simulación ---
  una_simulacion_robusta <- purrr::possibly(una_simulacion, otherwise = NULL, quiet = FALSE)
  
  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "Simulando [:bar] :percent [ETA: :eta]", total = N_simulaciones, width = 60
    )
    tick_progress <- function() pb$tick()
  } else {
    tick_progress <- function() {}
  }
  
  message(paste("Iniciando simulación Monte Carlo con", N_simulaciones, "iteraciones..."))
  
  resultados <- purrr::map_dfr(1:N_simulaciones, function(id) {
    res <- una_simulacion_robusta(id, ...)
    tick_progress()
    return(res)
  })
  
  message("Simulación completada.")
  return(resultados)
}