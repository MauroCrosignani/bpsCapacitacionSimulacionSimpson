#' Genera una Visualización Facetada de la Paradoja de Simpson (Versión 2)
#'
#' @description
#' Esta función toma los resultados resumidos y tidy de la simulación Monte Carlo
#' y genera un gráfico de barras facetado. El gráfico está diseñado para
#' comparar de forma clara y directa el efecto de los diferentes tipos de contacto
#' a través de los distintos subgrupos de cultura de pago, revelando así la
#' Paradoja de Simpson en un formato visualmente intuitivo.
#'
#' @param datos_resumen Un `tibble` o `data.frame` en formato largo y tidy,
#'   usualmente la salida de `resumir_simulacion()`.
#' @param titulo El título principal del gráfico.
#' @param subtitulo El subtítulo del gráfico, que puede usarse para añadir contexto.
#'
#' @return Un objeto de clase `ggplot` que puede ser impreso o modificado
#'   posteriormente.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' try({ # try() para evitar errores en CRAN si los paquetes no están
#'   # 1. Ejecutar y resumir una simulación
#'   resultados_mc <- ejecutar_simulacion_mc(
#'     N_simulaciones = 10,
#'     n_empresas_total = 10000
#'   )
#'   resumen <- resumir_simulacion(resultados_mc)
#'
#'   # 2. Generar el gráfico facetado
#'   grafico_paradoja <- graficar_paradoja_simpson(resumen)
#'   print(grafico_paradoja)
#' })
graficar_paradoja_simpson <- function(
    datos_resumen,
    titulo = "La Estrategia 'Óptima' Cambia Según el Segmento de Cliente",
    subtitulo = "La tasa de regularización muestra que la mejor intervención no es la misma para todos") {
  # --- 1. Validación de Entradas ---
  columnas_requeridas <- c("grupo_cultura", "tipo_contacto", "media", "ci_inferior", "ci_superior")
  stopifnot(
    is.data.frame(datos_resumen),
    all(columnas_requeridas %in% names(datos_resumen))
  )
  
  # --- 2. Construcción del Gráfico ggplot Facetado ---
  plot <- ggplot2::ggplot(
    datos_resumen,
    ggplot2::aes(x = media, y = tipo_contacto, fill = tipo_contacto)
  ) +
    ggplot2::facet_wrap(~grupo_cultura, ncol = 2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::geom_col(width = 0.7) +
    # FIX: Usar geom_errorbar con orientation = "y" en lugar de la obsoleta geom_errorbarh
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_inferior, xmax = ci_superior),
      width = 0.25, linewidth = 0.5, color = "gray20", orientation = "y"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(media, accuracy = 0.1)),
      hjust = -0.15,
      color = "black",
      size = 3.5
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Ausencia de Contacto" = "#F8766D",
        "Contacto Ligero" = "#00BA38",
        "Contacto Involucrado" = "#619CFF"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Tasa de Regularización Promedio (Estimación de la Simulación)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      plot.title = ggplot2::element_text(face = "bold", size = 18),
      plot.subtitle = ggplot2::element_text(color = "gray30", margin = ggplot2::margin(b = 15)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.text.y = ggplot2::element_text(size = 11)
    )
  
  return(plot)
}