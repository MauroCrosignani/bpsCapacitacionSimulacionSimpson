#' Genera una Visualización de la Paradoja de Simpson
#'
#' @description
#' Esta función toma los resultados resumidos de la simulación Monte Carlo y
#' genera un gráfico de barras con barras de error. El gráfico está diseñado
#' para ilustrar de manera clara y persuasiva la Paradoja de Simpson,
#' mostrando cómo el efecto de tratamiento agregado (naive) puede ser engañoso
#' en comparación con los efectos segmentados.
#'
#' @param datos_resumen Un `tibble` o `data.frame` que contiene los resultados
#'   resumidos, usualmente la salida de `resumir_simulacion()`.
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
#'   resumen <- resumir_simulacion(
#'     ejecutar_simulacion_mc(N_simulaciones = 100, n_empresas = 1000)
#'   )
#'   # 2. Generar el gráfico
#'   grafico_paradoja <- graficar_paradoja_simpson(resumen)
#'   print(grafico_paradoja)
#' })
graficar_paradoja_simpson <- function(
    datos_resumen,
    titulo = "El Juicio Parece Contraproducente, Pero es Efectivo para el Grupo Correcto",
    subtitulo = "Comparación del Efecto de Tratamiento Promedio (ATE) Agregado vs. Segmentado") {
  # --- 1. Validación de Entradas ---
  columnas_requeridas <- c("metrica", "media", "ci_inferior", "ci_superior")
  stopifnot(
    is.data.frame(datos_resumen),
    all(columnas_requeridas %in% names(datos_resumen))
  )
  
  # --- 2. Preparación de Datos para Graficar ---
  datos_preparados <- datos_resumen |>
    dplyr::mutate(
      # Crear etiquetas claras y ordenarlas para el gráfico
      metrica_etiquetada = dplyr::case_when(
        .data$metrica == "ate_naive" ~ "Resultado General (Agregado)",
        .data$metrica == "ate_ajustado_cultura_baja" ~ "Efecto en Empresas de Cultura Baja",
        .data$metrica == "ate_ajustado_cultura_alta" ~ "Efecto en Empresas de Cultura Alta",
        TRUE ~ .data$metrica
      ),
      # Convertir a factor para controlar el orden en el eje Y
      metrica_etiquetada = factor(.data$metrica_etiquetada, levels = c(
        "Efecto en Empresas de Cultura Alta",
        "Efecto en Empresas de Cultura Baja",
        "Resultado General (Agregado)"
      ))
    )
  
  # --- 3. Construcción del Gráfico ggplot ---
  plot <- ggplot2::ggplot(
    datos_preparados,
    ggplot2::aes(x = .data$media, y = .data$metrica_etiquetada, fill = .data$metrica_etiquetada)
  ) +
    # Línea vertical en x=0 para referencia de efecto nulo
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    # Barras principales que muestran el ATE promedio
    ggplot2::geom_col(width = 0.6) +
    # Barras de error horizontales para el intervalo de confianza
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$ci_inferior, xmax = .data$ci_superior),
      height = 0.2, linewidth = 0.6, color = "gray20"
    ) +
    # Etiqueta de texto con el valor de la media
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$media, 2)),
      hjust = dplyr::if_else(datos_preparados$media >= 0, -0.2, 1.2), # Posición del texto
      color = "black",
      size = 3.5
    ) +
    # --- 4. Estética y Diseño Ejecutivo ---
    ggplot2::scale_fill_manual(
      values = c(
        "Resultado General (Agregado)" = "gray60",
        "Efecto en Empresas de Cultura Baja" = "#00BFC4", # Verde azulado (positivo)
        "Efecto en Empresas de Cultura Alta" = "#F8766D" # Rojo coral (negativo)
      )
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Efecto Promedio Estimado en la Tasa de Regularización (ATE)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      plot.subtitle = ggplot2::element_text(color = "gray30", margin = ggplot2::margin(b = 15)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10))
    )
  
  return(plot)
}