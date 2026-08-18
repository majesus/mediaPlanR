# Funcion de masa de probabilidad Beta-Binomial: P(X = k) = choose(n,k) *
# B(k+alpha, n-k+beta) / B(alpha, beta). Funcion interna, solo para uso de
# run_beta_binomial_explorer(); ver calc_beta_binomial() para la version
# exportada y documentada del modelo.
#' @noRd
dbetabinom <- function(x, n, alpha, beta) {
  choose(n, x) * beta(x + alpha, n - x + beta) / beta(alpha, beta)
}

#' @encoding UTF-8
#' @title Explorador de la Distribucion Beta-Binomial
#' @description Aplicacion Shiny para visualizar la funcion de masa de
#' probabilidad de la distribucion beta-binomial y sus estadisticos
#' (media, varianza, moda) a partir de los parametros P, A1, A2 y n.
#'
#' @return Lanza una aplicacion Shiny interactiva (efecto secundario); no
#' devuelve ningun valor util para el flujo de un script.
#'
#' @examples
#' if (interactive()) {
#'   run_beta_binomial_explorer()
#' }
#'
#' @import shiny
#' @import bslib
#' @export
run_beta_binomial_explorer <- function() {

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    titlePanel("Explorador de la Distribucion Beta-Binomial"),

    layout_sidebar(
      sidebar = sidebar(
        h4("Parametros"),
        sliderInput("P", "Parametro P:", min = 1, max = 1000000, value = 1000000, step = 1000),
        sliderInput("A1", "Parametro A1:", min = 1, max = 1000000, value = 500000, step = 1000),
        sliderInput("A2", "Parametro A2:", min = 1, max = 1000000, value = 550000, step = 1000),
        sliderInput("n", "Numero de ensayos (n):", min = 1, max = 100, value = 20),
        actionButton("explain", "Explicar calculos", class = "btn-primary")
      ),

      card(
        card_header("Funcion de Masa de Probabilidad Beta-Binomial"),
        plotOutput("betaBinomPlot")
      ),

      card(
        card_header("Estadisticas de la Distribucion"),
        tableOutput("stats")
      ),

      card(
        card_header("Formula de la Distribucion Beta-Binomial"),
        withMathJax(
          "$$P(X = k) = \\binom{n}{k} \\frac{B(k+\\alpha, n-k+\\beta)}{B(\\alpha, \\beta)}$$"
        ),
        "Donde:",
        tags$ul(
          tags$li("n es el numero de ensayos"),
          tags$li("k es el numero de exitos"),
          tags$li("alpha y beta son los parametros de la distribucion beta"),
          tags$li("B(.,.) es la funcion beta")
        )
      ),

      card(
        card_header("Explicacion Detallada"),
        verbatimTextOutput("explanation")
      )
    )
  )

  server <- function(input, output, session) {

    params <- reactive({
      R1 <- input$A1 / input$P
      R2 <- input$A2 / input$P
      alpha <- (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2)
      beta <- (alpha * (1 - R1)) / R1

      # Comprobacion de valores validos
      if (is.nan(alpha) || is.nan(beta) || alpha <= 0 || beta <= 0) {
        return(list(valid = FALSE, alpha = NA, beta = NA))
      }

      list(valid = TRUE, alpha = alpha, beta = beta)
    })

    output$betaBinomPlot <- renderPlot({
      p <- params()

      # Validar si los parametros son validos antes de generar el grafico
      validate(
        need(p$valid, "Los parametros calculados no son validos. Es posible que el valor de A2 sea demasiado grande, causando que los calculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
      )

      x <- 0:input$n
      y <- dbetabinom(x, input$n, p$alpha, p$beta)

      ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_col(fill = "steelblue", alpha = 0.7) +
        labs(x = "Numero de exitos", y = "Probabilidad",
             title = paste("Distribucion Beta-Binomial (n =", input$n,
                           ", alpha =", p$alpha, ", beta =", p$beta, ")")) +
        theme_minimal()
    })

    output$stats <- renderTable({
      p <- params()

      # Validar si los parametros son validos antes de generar la tabla
      validate(
        need(p$valid, "Los parametros calculados no son validos. Es posible que el valor de A2 sea demasiado grande, causando que los calculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
      )

      n <- input$n
      alpha <- p$alpha
      beta <- p$beta

      mean <- n * alpha / (alpha + beta)
      variance <- (n * alpha * beta * (alpha + beta + n)) / ((alpha + beta)^2 * (alpha + beta + 1))
      mode <- floor((n + 1) * alpha / (alpha + beta) - 1)

      data.frame(
        Estadistica = c("Media", "Varianza", "Moda"),
        Valor = c(round(mean, 4), round(variance, 4), mode)
      )
    })

    output$explanation <- renderPrint({
      p <- params()

      # Validar si los parametros son validos antes de generar la explicacion
      validate(
        need(p$valid, "Los parametros calculados no son validos. Es posible que el valor de A2 sea demasiado grande, causando que los calculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
      )

      # La validacion anterior detendra la ejecucion aqui si no es valida, no se ejecutara nada despues

      # Si el usuario no ha solicitado la explicacion, mostrar un mensaje de espera
      if (!input$explain) {
        cat("A la espera de que se ejecute el analisis. Por favor, haga clic en el boton 'Explicar calculos' para ver la explicacion detallada.")
        return()  # Termina aqui si no se ha solicitado la explicacion
      }

      # Continuar solo si la validacion paso y el usuario ha solicitado la explicacion
      n <- input$n
      alpha <- p$alpha
      beta <- p$beta

      cat("Explicacion detallada del modelo beta-binomial:\n\n")
      cat(sprintf("Numero de ensayos (n): %d\n", n))
      cat(sprintf("Parametro Alpha: %.2f\n", alpha))
      cat(sprintf("Parametro Beta: %.2f\n\n", beta))

      cat("La distribucion beta-binomial es una generalizacion de la distribucion binomial donde la probabilidad de exito en cada ensayo no es fija, sino que sigue una distribucion beta.\n\n")

      cat("Interpretacion de los resultados:\n")
      mean <- n * alpha / (alpha + beta)
      variance <- (n * alpha * beta * (alpha + beta + n)) / ((alpha + beta)^2 * (alpha + beta + 1))
      mode <- floor((n + 1) * alpha / (alpha + beta) - 1)

      cat(sprintf("1. El valor esperado (media) de esta distribucion beta-binomial es %.4f\n", mean))
      cat("   Esto representa el numero promedio de exitos esperados.\n\n")
      cat(sprintf("2. La varianza de esta distribucion beta-binomial es %.4f\n", variance))
      cat("   La varianza mide la dispersion y es util para modelar sobredispersion.\n\n")
      cat(sprintf("3. La moda (valor mas probable) de esta distribucion es %d\n", mode))
      cat("   Este es el numero de exitos mas probable en un solo experimento.\n\n")

      cat("La distribucion beta-binomial es mas dispersa que la binomial, lo que la hace util para modelar situaciones con mayor variabilidad o sobredispersion.\n")

      if (alpha == beta) {
        cat("\nComo alpha = beta, la distribucion es simetrica alrededor de n/2.\n")
      } else if (alpha > beta) {
        cat("\nComo alpha > beta, la distribucion esta sesgada hacia la derecha.\n")
      } else {
        cat("\nComo alpha < beta, la distribucion esta sesgada hacia la izquierda.\n")
      }

    })
  }
  shinyApp(ui = ui, server = server)
}
