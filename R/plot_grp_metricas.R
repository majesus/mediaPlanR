
#' @encoding UTF-8
#' @title Visualización de GRPs y métricas relacionadas por soporte
#' @description Genera un gráfico de puntos para comparar soportes publicitarios
#' según GRPs y coste total, contactos y coste/GRP. El gráfico muestra la relación entre el coste por GRP,
#' los contactos totales y el coste total de cada soporte, utilizando un sistema de
#' burbujas con colores distintivos para cada soporte.
#'
#' @param audiencias Vector numérico con las audiencias de cada soporte
#' @param inserciones Vector numérico del número de inserciones por soporte
#' @param precios Vector numérico con el precio por inserción de cada soporte
#' @param nombres Character vector con los nombres de los soportes
#' @param pob_total Tamaño de la población objetivo
#' @param titulo Character. Título del gráfico (opcional)
#'
#' @return Un objeto ggplot2 que representa el gráfico de burbujas
#'
#' @examples
#' # Ejemplo básico con tres soportes
#' plot_grp_metricas(
#'   audiencias = c(300000, 400000, 200000),
#'   inserciones = c(3, 2, 4),
#'   precios = c(1000, 1500, 800),
#'   nombres = c("Marca", "As", "20 Minutos"),
#'   pob_total = 1000000,
#'   titulo = "Análisis de Soportes Deportivos"
#' )
#'
#' @import ggplot2
#' @import ggrepel
#' @importFrom scales comma
#' @export

plot_grp_metricas <- function(audiencias, inserciones, precios, nombres,
                             pob_total, titulo = "Comparación de Soportes Publicitarios") {

  if (!all(sapply(list(audiencias, inserciones, precios), is.numeric))) {
    stop("audiencias, inserciones y precios deben ser vectores numéricos")
  }
  if (length(unique(c(length(audiencias), length(inserciones),
                      length(precios), length(nombres)))) != 1) {
    stop("Todos los vectores de entrada deben tener la misma longitud")
  }

  df <- data.frame(
    nombre = nombres,
    contactos = audiencias * inserciones,
    cgrp = (inserciones * precios) / ((audiencias * inserciones / pob_total) * 100),
    coste = inserciones * precios
  )

  ggplot(df, aes(x = .data$cgrp, y = .data$contactos,
                 color = .data$nombre, label = .data$nombre)) +
    geom_point(alpha = 0.7, stroke = 1, color = "black", shape = 21, aes(fill = .data$nombre)) +
    ggrepel::geom_text_repel(aes(size = .data$coste * .1), box.padding = 1, max.overlaps = Inf) +
    scale_size_continuous(range = c(5, 20)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_viridis_d(option = "turbo") + # re-exportada por ggplot2 (viridisLite)
    labs(
      title = titulo,
      x = "C/GRP",
      y = "Contactos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      plot.background = element_rect(fill = "white", color = NA),
      text = element_text(family = "sans")
    )
}


