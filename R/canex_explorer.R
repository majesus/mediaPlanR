#' @encoding UTF-8
#' @title Explorador del Modelo CANEX
#' @description Aplicación Shiny para configurar una pauta multivehículo,
#' calcular su distribución de contactos mediante el modelo CANEX
#' (\code{\link{calc_canex}}) y visualizar la distribución de frecuencias y
#' la cobertura acumulada.
#'
#' @details
#' La aplicación permite:
#' \itemize{
#'   \item Configurar la población objetivo y el número de vehículos (2 a 5)
#'   \item Introducir, para cada vehículo, el número de inserciones (k) y las
#'   coberturas tras la primera y segunda inserción (R1, R2)
#'   \item Introducir la matriz de duplicaciones observadas entre cada par de
#'   vehículos
#'   \item Cargar un ejemplo predefinido de referencia
#'   \item Visualizar la distribución de frecuencias y la cobertura acumulada
#' }
#'
#' @return Lanza una aplicacion Shiny interactiva (efecto secundario); no
#' devuelve ningun valor util para el flujo de un script.
#'
#' @examples
#' if (interactive()) {
#'   run_canex_explorer()
#' }
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @export
run_canex_explorer <- function() {

  ui <- bslib::page_sidebar(
    title = "Explorador del Modelo CANEX",
    sidebar = sidebar(
      width = 380,
      numericInput("poblacion_canex", "Población objetivo:", value = 1000000, min = 1, step = 1000),
      numericInput("n_vehiculos_canex", "Número de vehículos:", value = 2, min = 2, max = 5, step = 1),
      uiOutput("vehiculos_ui"),
      uiOutput("duplicaciones_ui"),
      actionButton("calcular_canex", "Calcular distribución", class = "btn-primary"),
      actionButton("ejemplo_canex", "Cargar ejemplo", class = "btn-secondary"),
      hr(),
      helpText("k: inserciones. R1: cobertura tras 1 inserción. R2: cobertura tras 2 inserciones. ",
               "p(i,j): proporción de población expuesta simultáneamente a los vehículos i y j.")
    ),
    card(
      card_header("Resumen"),
      tableOutput("resumen_canex")
    ),
    layout_columns(
      card(card_header("Distribución de frecuencias (K = k)"), plotOutput("plot_prob_canex")),
      card(card_header("Cobertura acumulada (K ≥ k)"), plotOutput("plot_cum_canex"))
    ),
    card(
      card_header("Distribución de contactos detallada"),
      tableOutput("tabla_canex")
    )
  )

  server <- function(input, output, session) {

    rv <- reactiveValues(
      n = 2,
      vehiculos = data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502)),
      duplicaciones = matrix(c(1, 0.0157, 0.0157, 1), nrow = 2)
    )

    # Redimensionar el estado (vehiculos/duplicaciones) cuando cambia n,
    # conservando los valores ya introducidos para los vehiculos comunes.
    observeEvent(input$n_vehiculos_canex, {
      n_new <- input$n_vehiculos_canex
      if (is.na(n_new) || n_new < 2 || n_new > 5 || n_new == rv$n) return()

      n_common <- min(n_new, rv$n)
      k_new <- rep(1, n_new); R1_new <- rep(0.1, n_new); R2_new <- rep(0.15, n_new)
      k_new[seq_len(n_common)] <- rv$vehiculos$k[seq_len(n_common)]
      R1_new[seq_len(n_common)] <- rv$vehiculos$R1[seq_len(n_common)]
      R2_new[seq_len(n_common)] <- rv$vehiculos$R2[seq_len(n_common)]

      dup_new <- matrix(0, n_new, n_new)
      diag(dup_new) <- 1
      dup_new[seq_len(n_common), seq_len(n_common)] <- rv$duplicaciones[seq_len(n_common), seq_len(n_common)]

      rv$n <- n_new
      rv$vehiculos <- data.frame(k = k_new, R1 = R1_new, R2 = R2_new)
      rv$duplicaciones <- dup_new
    })

    observeEvent(input$ejemplo_canex, {
      updateNumericInput(session, "poblacion_canex", value = 1000000)
      updateNumericInput(session, "n_vehiculos_canex", value = 2)
      rv$n <- 2
      rv$vehiculos <- data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502))
      rv$duplicaciones <- matrix(c(1, 0.0157, 0.0157, 1), nrow = 2)
    })

    output$vehiculos_ui <- renderUI({
      n <- rv$n
      tagList(
        h5("Datos por vehículo"),
        lapply(seq_len(n), function(i) {
          fluidRow(
            column(4, numericInput(paste0("veh_k_", i), paste0("V", i, ": k"),
                                    value = rv$vehiculos$k[i], min = 1, step = 1)),
            column(4, numericInput(paste0("veh_R1_", i), "R1",
                                    value = rv$vehiculos$R1[i], min = 0.0001, max = 1, step = 0.0001)),
            column(4, numericInput(paste0("veh_R2_", i), "R2",
                                    value = rv$vehiculos$R2[i], min = 0.0001, max = 1, step = 0.0001))
          )
        })
      )
    })

    output$duplicaciones_ui <- renderUI({
      n <- rv$n
      ancho_col <- max(3, floor(12 / max(n - 1, 1)))
      tagList(
        h5("Matriz de duplicaciones p(i,j)"),
        lapply(seq_len(n - 1), function(i) {
          fluidRow(
            lapply((i + 1):n, function(j) {
              column(
                ancho_col,
                numericInput(paste0("dup_", i, "_", j), paste0("p(", i, ",", j, ")"),
                             value = rv$duplicaciones[i, j], min = 0, max = 1, step = 0.0001)
              )
            })
          )
        })
      )
    })

    input_or_na <- function(id) {
      val <- input[[id]]
      if (is.null(val)) NA_real_ else val
    }

    resultado <- eventReactive(input$calcular_canex, {
      n <- rv$n
      k_vals <- vapply(seq_len(n), function(i) input_or_na(paste0("veh_k_", i)), numeric(1))
      R1_vals <- vapply(seq_len(n), function(i) input_or_na(paste0("veh_R1_", i)), numeric(1))
      R2_vals <- vapply(seq_len(n), function(i) input_or_na(paste0("veh_R2_", i)), numeric(1))
      vehicles_data <- data.frame(k = k_vals, R1 = R1_vals, R2 = R2_vals)

      dup <- matrix(1, n, n)
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          val <- input_or_na(paste0("dup_", i, "_", j))
          dup[i, j] <- val
          dup[j, i] <- val
        }
      }

      poblacion <- input$poblacion_canex

      tryCatch(
        calc_canex(vehicles_data, dup, poblacion),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )
    })

    output$resumen_canex <- renderTable({
      res <- resultado()
      req(res)
      data.frame(
        Métrica = c("Cobertura total (1+)", "OTS medio (entre alcanzados)", "Probabilidad de 0 contactos"),
        Valor = c(
          sprintf("%.2f%% (%s personas)", res$total_reach * 100,
                  format(res$total_reach_people, big.mark = ",", scientific = FALSE)),
          sprintf("%.2f", res$stats$avg_contacts),
          sprintf("%.2f%%", res$stats$zero_contacts_prob * 100)
        )
      )
    })

    output$tabla_canex <- renderTable({
      res <- resultado()
      req(res)
      data.frame(
        k = res$distribution$contacts,
        `Prob. (K=k)` = sprintf("%.2f%%", res$distribution$percentage),
        `Personas (K=k)` = format(round(res$distribution$people), big.mark = ",", scientific = FALSE),
        `Prob. (K≥k)` = sprintf("%.2f%%", res$cumulative$percentage),
        `Personas (K≥k)` = format(round(res$cumulative$people), big.mark = ",", scientific = FALSE),
        check.names = FALSE
      )
    })

    output$plot_prob_canex <- renderPlot({
      res <- resultado()
      req(res)
      ggplot(res$distribution, aes(x = factor(.data$contacts), y = .data$percentage)) +
        geom_col(fill = "#0984e3") +
        labs(x = "k contactos", y = "% de población", title = NULL) +
        theme_minimal()
    })

    output$plot_cum_canex <- renderPlot({
      res <- resultado()
      req(res)
      ggplot(res$cumulative, aes(x = factor(.data$min_contacts), y = .data$percentage, group = 1)) +
        geom_line(color = "#ff7a00", linewidth = 1) +
        geom_point(color = "#ff7a00", size = 2) +
        labs(x = "k contactos (mínimo)", y = "% de población acumulado", title = NULL) +
        theme_minimal()
    })
  }

  shinyApp(ui = ui, server = server)
}
