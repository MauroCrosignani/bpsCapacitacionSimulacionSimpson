#' Ejecuta una Simulación Monte Carlo de la Paradoja de Simpson
#'
#' @description
#' Esta función orquesta una simulación Monte Carlo. Llama repetidamente a la
#' función `generar_universo_simpson()` para generar múltiples universos de datos
#' y calcula métricas clave para cada uno. El objetivo es evaluar la
#' distribución de los estimadores del Efecto de Tratamiento Promedio (ATE)
#' y demostrar la consistencia de la paradoja.
#'
#' @param N_simulaciones El número de veces que se repetirá la simulación (el
#'   número de universos a generar).
#' @param ... Argumentos adicionales que se pasarán directamente a
#'   `generar_universo_simpson()`. Esto permite configurar parámetros como
#'   `n_empresas`, `p_cultura_alta`, etc., para toda la simulación.
#'
#' @return Un `tibble` donde cada fila corresponde a una simulación exitosa,
#'   con las siguientes columnas:
#'   - `id_simulacion`: Identificador numérico de la simulación.
#'   - `ate_naive`: El ATE "ingenuo" calculado a nivel agregado.
#'   - `ate_ajustado_cultura_baja`: El ATE calculado para el subgrupo de "Baja" cultura de pago.
#'   - `ate_ajustado_cultura_alta`: El ATE calculado para el subgrupo de "Alta" cultura de pago.
#'
#' @export
#'
#' @examples
#' # Ejecuta una simulación pequeña con 10 iteraciones y 500 empresas por universo
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   resultados <- ejecutar_simulacion_mc(N_simulaciones = 10, n_empresas = 500)
#'   print(resultados)
#'   # Calcula el ATE naive promedio a lo largo de todas las simulaciones
#'   mean(resultados$ate_naive)
#' })
ejecutar_simulacion_mc <- function(N_simulaciones = 1000, ...) {
  # --- 1. Validación de Entradas ---
  stopifnot(
    is.numeric(N_simulaciones),
    N_simulaciones > 0,
    N_simulaciones == round(N_simulaciones) # Asegurar que es entero
  )
  
  # --- 2. Definición de la Lógica de una Simulación ---
  # Función auxiliar que se ejecutará para cada iteración
  una_simulacion <- function(id, ...) {
    universo <- generar_universo_simpson(semilla = id, ...)
    
    calcular_ate <- function(df) {
      if (nrow(df) == 0) return(NA_real_)
      
      tasas <- df |>
        # FIX: Usar .data[["col"]] para programación robusta y evitar warnings
        dplyr::group_by(.data[["decision_juicio_BPS"]]) |>
        dplyr::summarise(tasa_reg = mean(.data[["regularizacion_observada"]], na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = "decision_juicio_BPS", values_from = "tasa_reg")
      
      tasa_juicio <- if ("Juicio" %in% names(tasas)) tasas$Juicio else NA_real_
      tasa_no_juicio <- if ("No Juicio" %in% names(tasas)) tasas$`No Juicio` else NA_real_
      
      return(tasa_juicio - tasa_no_juicio)
    }
    
    ate_naive <- calcular_ate(universo)
    ate_baja <- calcular_ate(dplyr::filter(universo, .data$cultura_pago_real == "Baja"))
    ate_alta <- calcular_ate(dplyr::filter(universo, .data$cultura_pago_real == "Alta"))
    
    tibble::tibble(
      id_simulacion = id,
      ate_naive = ate_naive,
      ate_ajustado_cultura_baja = ate_baja,
      ate_ajustado_cultura_alta = ate_alta
    )
  }
  
  una_simulacion_robusta <- purrr::possibly(una_simulacion, otherwise = NULL, quiet = FALSE)
  
  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = "Simulando [:bar] :percent [ETA: :eta]",
      total = N_simulaciones,
      width = 60
    )
    # FIX: La función tick_progress no necesita argumentos
    tick_progress <- function() {
      pb$tick()
    }
  } else {
    tick_progress <- function() {} # Función vacía si {progress} no está
  }
  
  message(paste("Iniciando simulación Monte Carlo con", N_simulaciones, "iteraciones..."))
  
  resultados <- purrr::map_dfr(1:N_simulaciones, function(id) {
    res <- una_simulacion_robusta(id, ...)
    tick_progress() # La llamada ahora coincide con la definición
    return(res)
  })
  
  message("Simulación completada.")
  return(resultados)
}