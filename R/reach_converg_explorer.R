
# Correccion en la funcion calculate_coverage
calculate_coverage <- function(alpha, beta, n) {
  coverage <- numeric(n)
  for(i in 1:n) {
    if (alpha > 0 && beta + i > 0) {
      p_zero <- beta(alpha, beta + i) / beta(alpha, beta)
      coverage[i] <- 1 - p_zero
    } else {
      coverage[i] <- NA
    }
  }
  return(coverage)
}

calculate_incremental <- function(coverage) {
  c(coverage[1], diff(coverage))
}

check_convergencia <- function(incrementales, threshold) {
  incrementales <= threshold
}

calculate_contact_distribution <- function(alpha, beta, n, max_contacts) {
  # Validar max_contacts
  if (is.na(max_contacts) || max_contacts < 1) {
    return(numeric(0))  # Retorna vector vacio si no es valido
  }

  dist <- numeric(max_contacts)
  for(k in 1:max_contacts) {
    if (alpha + k > 0 && beta + n - k > 0) {
      numerador <- choose(n, k) * beta(alpha + k, beta + n - k)
      denominador <- beta(alpha, beta)
      dist[k] <- numerador / denominador
    } else {
      dist[k] <- NA
    }
  }
  return(dist)
}

#' @encoding UTF-8
#' @title Explorador de Convergencia de la Cobertura
#' @description Aplicacion Shiny para el analisis de la convergencia de la cobertura.
#'
#' @details
#' La aplicacion permite:
#' \itemize{
#'   \item Configurar un plan de medios aplicando el modelo Beta-Binomial
#'   \item Analizar la evolucion de la cobertura acumulada e incremental
#'   \item Analizar la distribucion de contactos (y acumulada)
#'   \item Visualizar distribuciones mediante graficos de lineas
#'   \item Calcular estadisticas relevantes de la audiencia
#' }
#'
#' @section Parametros de Configuracion:
#' \itemize{
#'   \item Tamano de poblacion
#'   \item Parametros de forma de la distribucion Beta-Binomial
#'   \item Maximo numero de contactos a mostrar
#'   \item Umbral de convergencia
#' }
#'
#' @return Lanza una aplicacion Shiny interactiva (efecto secundario); no
#' devuelve ningun valor util para el flujo de un script.
#'
#' @examples
#' if (interactive()) {
#'   run_reach_converg_explorer()
#' }
#'
#' @import shiny
#' @import bslib
#' @importFrom graphics plot.new text
#' @importFrom utils tail
#' @export
run_reach_converg_explorer <- function() {

  ui <- bslib::page_sidebar(
    title = "Analisis de Convergencia - Modelo Beta Binomial",
    sidebar = sidebar(
      numericInput("poblacion", "Poblacion objetivo:", value = 1000000, min = 1000, max = 100000000),
      numericInput("alpha", "Alpha:", value = 0.5, min = 0.1, max = 10, step = 0.1),
      numericInput("beta", "Beta:", value = 1.5, min = 0.1, max = 10, step = 0.1),
      numericInput("n_inserciones", "Numero de inserciones:", value = 30, min = 10, max = 100),
      numericInput("max_contacts", "Maximo numero de contactos a mostrar:", value = 10, min = 1, max = 30),
      numericInput("threshold", "Umbral de convergencia:", value = 0.01, min = 0.001, max = 0.1, step = 0.001),
      actionButton("calcular", "Calcular", class = "btn-primary"),
      hr(),
      helpText("Ajuste los parametros y presione 'Calcular' para ver los resultados")
    ),
    layout_columns(
      card(card_header("Convergencia de Cobertura"), plotOutput("convergencia_plot")),
      card(card_header("Cobertura Incremental"), plotOutput("incremental_plot"))
    ),
    layout_columns(
      card(card_header("Distribucion de Contactos"), plotOutput("dist_contactos_plot")),
      card(card_header("Distribucion Acumulada de Contactos"), plotOutput("dist_acumulada_plot"))
    ),
    card(
      card_header("Resumen del Plan"),
      tableOutput("resumen_table")
    )
  )

  server <- function(input, output, session) {
    observe({
      updateNumericInput(session, "max_contacts", max = input$n_inserciones)
    })

    datos_calculados <- eventReactive(input$calcular, {
      coverage <- calculate_coverage(input$alpha, input$beta, input$n_inserciones)
      incremental <- calculate_incremental(coverage)
      convergencia <- check_convergencia(incremental, input$threshold)
      punto_convergencia <- which(incremental <= input$threshold)[1]

      dist_contactos <- calculate_contact_distribution(input$alpha, input$beta, input$n_inserciones, input$max_contacts)
      dist_acumulada <- rev(cumsum(rev(dist_contactos)))

      list(
        coverage = coverage,
        incremental = incremental,
        convergencia = convergencia,
        dist_contactos = dist_contactos,
        dist_acumulada = dist_acumulada,
        punto_convergencia = punto_convergencia
      )
    })

    output$convergencia_plot <- renderPlot({
      req(datos_calculados())
      datos <- data.frame(
        insercion = 1:length(datos_calculados()$coverage),
        cobertura = datos_calculados()$coverage,
        absolutos = datos_calculados()$coverage * input$poblacion
      )

      p <- ggplot(datos, aes(x = .data$insercion, y = .data$cobertura)) +
        geom_line(color = "blue") +
        geom_point() +
        labs(x = "Numero de inserciones", y = "Cobertura acumulada",
             title = "Evolucion de la cobertura") +
        theme_minimal() +
        scale_y_continuous(
          labels = scales::percent,
          sec.axis = sec_axis(~.*input$poblacion, name = "Personas alcanzadas",
                              labels = scales::comma)
        )

      # Anadir marcador del punto de convergencia si existe
      if (!is.na(datos_calculados()$punto_convergencia)) {
        p <- p +
          geom_vline(xintercept = datos_calculados()$punto_convergencia,
                     color = "darkred",
                     linetype = "longdash") +
          annotate("text",
                   x = datos_calculados()$punto_convergencia,
                   y = max(datos$cobertura),
                   label = paste("Converg.:",
                                 datos_calculados()$punto_convergencia),
                   hjust = -0.1,
                   color = "darkred")
      }

      p
    })

    output$incremental_plot <- renderPlot({
      req(datos_calculados())
      datos <- data.frame(
        insercion = 1:length(datos_calculados()$incremental),
        incremental = datos_calculados()$incremental,
        absolutos = datos_calculados()$incremental * input$poblacion
      )

      p <- ggplot(datos, aes(x = .data$insercion, y = .data$incremental)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        geom_hline(yintercept = input$threshold, color = "red", linetype = "dashed") +
        labs(x = "Numero de inserciones",
             y = "Cobertura incremental",
             title = "Cobertura incremental por insercion") +
        theme_minimal() +
        scale_y_continuous(
          labels = scales::percent,
          sec.axis = sec_axis(~.*input$poblacion,
                              name = "Personas alcanzadas (incremento)",
                              labels = scales::comma)
        )

      # Anadir marcador del punto de convergencia si existe
      if (!is.na(datos_calculados()$punto_convergencia)) {
        p <- p +
          geom_vline(xintercept = datos_calculados()$punto_convergencia,
                     color = "darkred",
                     linetype = "longdash") +
          annotate("text",
                   x = datos_calculados()$punto_convergencia,
                   y = max(datos$incremental),
                   label = paste("Converg.:",
                                 datos_calculados()$punto_convergencia),
                   hjust = -0.1,
                   color = "darkred")
      }

      p
    })

    output$dist_contactos_plot <- renderPlot({
      req(datos_calculados())
      req(input$max_contacts > 0)  # Asegurar que max_contacts es valido

      datos <- data.frame(
        contactos = 1:length(datos_calculados()$dist_contactos),
        probabilidad = datos_calculados()$dist_contactos,
        absolutos = datos_calculados()$dist_contactos * input$poblacion
      )

      if (nrow(datos) == 0 || all(is.na(datos$probabilidad))) {
        # Mostrar mensaje de error en lugar de grafico vacio
        plot.new()
        text(0.5, 0.5, "Por favor, introduce un numero valido\nde contactos a mostrar",
             cex = 1.2, col = "red", adj = 0.5)
      } else {
        ggplot(datos, aes(x = .data$contactos, y = .data$probabilidad)) +
          geom_bar(stat = "identity", fill = "lightgreen") +
          labs(x = "Numero de contactos", y = "Probabilidad", title = "Distribucion de contactos") +
          theme_minimal() +
          scale_y_continuous(
            labels = scales::percent,
            sec.axis = sec_axis(~.*input$poblacion, name = "Numero de personas", labels = scales::comma)
          ) +
          scale_x_continuous(breaks = 1:input$max_contacts)
      }
    })

    output$dist_acumulada_plot <- renderPlot({
      req(datos_calculados())
      req(input$max_contacts > 0)  # Asegurar que max_contacts es valido

      datos <- data.frame(
        contactos = 1:length(datos_calculados()$dist_acumulada),
        probabilidad = datos_calculados()$dist_acumulada,
        absolutos = datos_calculados()$dist_acumulada * input$poblacion
      )

      if (nrow(datos) == 0 || all(is.na(datos$probabilidad))) {
        # Mostrar mensaje de error en lugar de grafico vacio
        plot.new()
        text(0.5, 0.5, "Por favor, introduce un numero valido\nde contactos a mostrar",
             cex = 1.2, col = "red", adj = 0.5)
      } else {
        ggplot(datos, aes(x = .data$contactos, y = .data$probabilidad)) +
          geom_bar(stat = "identity", fill = "orange") +
          labs(x = "Numero de contactos", y = "Probabilidad acumulada",
               title = "Distribucion acumulada (al menos X contactos)") +
          theme_minimal() +
          scale_y_continuous(
            labels = scales::percent,
            sec.axis = sec_axis(~.*input$poblacion, name = "Numero de personas", labels = scales::comma)
          ) +
          scale_x_continuous(breaks = 1:input$max_contacts)
      }
    })

    output$resumen_table <- renderTable({
      req(datos_calculados())

      cobertura_final <- tail(datos_calculados()$coverage, 1)
      ultimo_incremento <- tail(datos_calculados()$incremental, 1)
      prob_1_contacto <- datos_calculados()$dist_contactos[1]
      prob_2_mas_contactos <- sum(datos_calculados()$dist_contactos[2:length(datos_calculados()$dist_contactos)], na.rm = TRUE)

      data.frame(
        Metrica = c("Alpha", "Beta", "Cobertura final", "Ultimo incremento", "1 contacto exacto", "2 o mas contactos"),
        Porcentaje = c(
          sprintf("%.2f", input$alpha),
          sprintf("%.2f", input$beta),
          sprintf("%.1f%%", cobertura_final * 100),
          sprintf("%.2f%%", ultimo_incremento * 100),
          sprintf("%.1f%%", prob_1_contacto * 100),
          sprintf("%.1f%%", prob_2_mas_contactos * 100)
        ),
        Personas = c(
          "---",
          "---",
          format(round(cobertura_final * input$poblacion), big.mark = ","),
          format(round(ultimo_incremento * input$poblacion), big.mark = ","),
          format(round(prob_1_contacto * input$poblacion), big.mark = ","),
          format(round(prob_2_mas_contactos * input$poblacion), big.mark = ",")
        )
      )
    })
  }

  # Lanzamos la aplicacion Shiny
  shinyApp(ui = ui, server = server)
}
