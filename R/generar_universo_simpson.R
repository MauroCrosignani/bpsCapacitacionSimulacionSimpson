#' Genera un Universo de Datos para Demostrar la Paradoja de Simpson
#'
#' @description
#' Esta función genera un data.frame (tibble) sintético que ilustra la
#' Paradoja de Simpson en un contexto simulado de gestión de cobranzas.
#' Crea un escenario donde una variable confusora no observada (`cultura_pago_real`)
#' invierte la conclusión sobre la efectividad de una intervención (`decision_juicio_BPS`)
#' en un resultado (`regularizacion_observada`).
#'
#' @param n_empresas Número total de empresas a simular.
#' @param semilla Un número entero para fijar la semilla aleatoria y garantizar la
#'   reproducibilidad.
#' @param modo_depuracion Booleano. Si es `TRUE`, imprime mensajes de diagnóstico
#'   intermedios durante la generación de datos.
#' @param p_cultura_alta La probabilidad de que una empresa tenga una "Alta"
#'   cultura de pago.
#' @param efecto_juicio_cultura_baja El efecto causal real (en la escala log-odds)
#'   de iniciar un juicio para empresas con "Baja" cultura de pago. Un valor
#'   positivo significa que el juicio aumenta la probabilidad de regularización.
#' @param efecto_juicio_cultura_alta El efecto causal real (en la escala log-odds)
#'   de iniciar un juicio para empresas con "Alta" cultura de pago. Un valor
#'   negativo significa que el juicio disminuye la probabilidad de regularización.
#'
#' @return Un `tibble` con `n_empresas` filas y las siguientes columnas:
#'   `id_empresa`, `cultura_pago_real`, `riesgo_deuda`, `antiguedad_deuda_cat`,
#'   `prob_predictiva_modelo`, `decision_juicio_BPS`, `regularizacion_observada`.
#'
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   universo_simulado <- generar_universo_simpson(n_empresas = 1000, semilla = 123)
#'   head(universo_simulado)
#' })
generar_universo_simpson <- function(
    n_empresas = 10000,
    semilla = 42,
    modo_depuracion = FALSE,
    p_cultura_alta = 0.5,
    efecto_juicio_cultura_baja = 1.5,
    efecto_juicio_cultura_alta = -1.0) {
  # --- 1. Validación de Entradas ---
  stopifnot(
    is.numeric(n_empresas), n_empresas > 0,
    is.numeric(semilla),
    is.logical(modo_depuracion),
    is.numeric(p_cultura_alta), p_cultura_alta >= 0, p_cultura_alta <= 1
  )
  
  # --- 2. Inicialización ---
  set.seed(semilla)
  
  datos_base <- tibble::tibble(id_empresa = 1:n_empresas)
  
  if (modo_depuracion) message(paste("Paso 0: Creado tibble inicial con", n_empresas, "empresas."))
  
  # --- 3. Proceso de Generación de Datos (DGP) ---
  universo <- datos_base |>
    # Paso 1: Generar `cultura_pago_real` (U - El Confounder)
    dplyr::mutate(
      cultura_pago_real = rbinom(n = n_empresas, size = 1, prob = p_cultura_alta)
    ) |>
    # Paso 2: Generar Covariables Observables (X)
    dplyr::mutate(
      # Riesgo menor para cultura alta
      riesgo_deuda = rnorm(n = n_empresas, mean = 0.8 - cultura_pago_real * 0.5, sd = 0.2),
      # Antigüedad más corta para cultura alta
      antiguedad_deuda_cat = sample(
        c("Corta", "Media", "Larga"),
        size = n_empresas,
        replace = TRUE,
        prob = if (cultura_pago_real[1] == 1) c(0.6, 0.3, 0.1) else c(0.1, 0.3, 0.6)
      )
    ) |>
    # Paso 3: Definir Resultados Potenciales (Y(0), Y(1))
    dplyr::mutate(
      # Baseline log-odds de regularizar depende de cultura y riesgo
      logit_baseline = -1.5 + cultura_pago_real * 2.5 - riesgo_deuda * 1.2,
      
      # Log-odds con y sin juicio (el efecto del juicio depende de la cultura)
      logit_prob_y0 = logit_baseline, # Sin juicio
      logit_prob_y1 = logit_baseline + dplyr::if_else(cultura_pago_real == 1,
                                                      efecto_juicio_cultura_alta,
                                                      efecto_juicio_cultura_baja
      ), # Con juicio
      
      # Probabilidades
      prob_y0 = plogis(logit_prob_y0),
      prob_y1 = plogis(logit_prob_y1),
      
      # Resultados potenciales (realizaciones)
      Y0 = rbinom(n = n_empresas, size = 1, prob = prob_y0),
      Y1 = rbinom(n = n_empresas, size = 1, prob = prob_y1)
    )
  
  if (modo_depuracion) {
    ate_real_baja <- mean(universo$Y1[universo$cultura_pago_real == 0] - universo$Y0[universo$cultura_pago_real == 0])
    ate_real_alta <- mean(universo$Y1[universo$cultura_pago_real == 1] - universo$Y0[universo$cultura_pago_real == 1])
    message(paste("Paso 3: ATE real (Y1-Y0) en Cultura Baja:", round(ate_real_baja, 3)))
    message(paste("Paso 3: ATE real (Y1-Y0) en Cultura Alta:", round(ate_real_alta, 3)))
  }
  
  # Paso 4: Simular el Modelo Predictivo del BPS (P)
  # El modelo solo ve las X para predecir lo que pasaría sin intervención (Y0)
  modelo_predictivo <- glm(
    Y0 ~ riesgo_deuda + antiguedad_deuda_cat,
    data = universo,
    family = "binomial"
  )
  
  prob_pred_sin_ruido <- predict(modelo_predictivo, type = "response")
  ruido <- rnorm(n = n_empresas, mean = 0, sd = 0.03)
  prob_pred_con_ruido <- prob_pred_sin_ruido + ruido
  # Asegurar que la probabilidad esté en el rango [0, 1]
  prob_predictiva_modelo <- pmin(1, pmax(0, prob_pred_con_ruido))
  
  universo <- universo |>
    dplyr::mutate(prob_predictiva_modelo = prob_predictiva_modelo)
  
  if (modo_depuracion) {
    cor_pred <- cor(universo$prob_predictiva_modelo, universo$Y0)
    message(paste("Paso 4: Correlación del modelo predictivo con Y0:", round(cor_pred, 3)))
  }
  
  # Continuación del pipeline
  universo <- universo |>
    # Paso 5: Asignar Tratamiento (`decision_juicio_BPS` - D)
    dplyr::mutate(
      decision_juicio_BPS = dplyr::if_else(prob_predictiva_modelo > 0.7, 0, 1)
    ) |>
    # Paso 6: Generar Resultado Observado (Y)
    dplyr::mutate(
      regularizacion_observada = dplyr::if_else(
        decision_juicio_BPS == 1, Y1, Y0
      )
    )
  
  if (modo_depuracion) {
    n_juicio <- sum(universo$decision_juicio_BPS)
    message(paste("Paso 5: Asignados a juicio", n_juicio, "empresas."))
  }
  
  # --- 4. Formateo Final y Retorno ---
  universo_final <- universo |>
    dplyr::mutate(
      cultura_pago_real = factor(cultura_pago_real, levels = c(0, 1), labels = c("Baja", "Alta")),
      antiguedad_deuda_cat = factor(antiguedad_deuda_cat, levels = c("Corta", "Media", "Larga")),
      decision_juicio_BPS = factor(decision_juicio_BPS, levels = c(0, 1), labels = c("No Juicio", "Juicio"))
    ) |>
    dplyr::select(
      id_empresa, cultura_pago_real, riesgo_deuda, antiguedad_deuda_cat,
      prob_predictiva_modelo, decision_juicio_BPS, regularizacion_observada
    )
  
  return(universo_final)
}