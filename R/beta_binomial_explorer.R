# Función de masa de probabilidad Beta-Binomial: P(X = k) = choose(n,k) *
# B(k+alpha, n-k+beta) / B(alpha, beta). Función interna, solo para uso de
# run_beta_binomial_explorer(); ver calc_beta_binomial() para la versión
# exportada y documentada del modelo.
#' @noRd
dbetabinom <- function(x, n, alpha, beta) {
  choose(n, x) * beta(x + alpha, n - x + beta) / beta(alpha, beta)
}

#' @encoding UTF-8
#' @title Explorador de la Distribución Beta-Binomial
#' @description Aplicación Shiny para visualizar la función de masa de
#' probabilidad de la distribución beta-binomial y sus estadísticos
#' (media, varianza, moda) a partir de los parámetros P, A1, A2 y n.
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

    titlePanel("Explorador de la Distribución Beta-Binomial"),

    layout_sidebar(
      sidebar = sidebar(
        h4("Parámetros"),
        sliderInput("P", "Parámetro P:", min = 1, max = 1000000, value = 1000000, step = 1000),
        sliderInput("A1", "Parámetro A1:", min = 1, max = 1000000, value = 500000, step = 1000),
        sliderInput("A2", "Parámetro A2:", min = 1, max = 1000000, value = 550000, step = 1000),
        sliderInput("n", "Número de ensayos (n):", min = 1, max = 100, value = 20),
        actionButton("explain", "Explicar cálculos", class = "btn-primary")
      ),

      card(
        card_header("Función de Masa de Probabilidad Beta-Binomial"),
        plotOutput("betaBinomPlot")
      ),

      card(
        card_header("Estadísticas de la Distribución"),
        tableOutput("stats")
      ),

      card(
        card_header("Fórmula de la Distribución Beta-Binomial"),
        withMathJax(
          "$$P(X = k) = \\binom{n}{k} \\frac{B(k+\\alpha, n-k+\\beta)}{B(\\alpha, \\beta)}$$"
        ),
        "Donde:",
        tags$ul(
          tags$li("n es el número de ensayos"),
          tags$li("k es el número de éxitos"),
          tags$li("α y β son los parámetros de la distribución beta"),
          tags$li("B(·,·) es la función beta")
        )
      ),

      card(
        card_header("Explicación Detallada"),
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

      # Comprobación de valores válidos
      if (is.nan(alpha) || is.nan(beta) || alpha <= 0 || beta <= 0) {
        return(list(valid = FALSE, alpha = NA, beta = NA))
      }

      list(valid = TRUE, alpha = alpha, beta = beta)
    })

    output$betaBinomPlot <- renderPlot({
      p <- params()

      # Validar si los parámetros son válidos antes de generar el gráfico
      validate(
        need(p$valid, "Los parámetros calculados no son válidos. Es posible que el valor de A2 sea demasiado grande, causando que los cálculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
      )

      x <- 0:input$n
      y <- dbetabinom(x, input$n, p$alpha, p$beta)

      ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_col(fill = "steelblue", alpha = 0.7) +
        labs(x = "Número de éxitos", y = "Probabilidad",
             title = paste("Distribución Beta-Binomial (n =", input$n,
                           ", α =", p$alpha, ", β =", p$beta, ")")) +
        theme_minimal()
    })

    output$stats <- renderTable({
      p <- params()

      # Validar si los parámetros son válidos antes de generar la tabla
      validate(
        need(p$valid, "Los parámetros calculados no son válidos. Es posible que el valor de A2 sea demasiado grande, causando que los cálculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
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

      # Validar si los parámetros son válidos antes de generar la explicación
      validate(
        need(p$valid, "Los parámetros calculados no son válidos. Es posible que el valor de A2 sea demasiado grande, causando que los cálculos generen valores infinitos o NaNs. Por favor, ajusta los valores de A1, A2 o P.")
      )

      # La validación anterior detendrá la ejecución aquí si no es válida, no se ejecutará nada después

      # Si el usuario no ha solicitado la explicación, mostrar un mensaje de espera
      if (!input$explain) {
        cat("A la espera de que se ejecute el análisis. Por favor, haga clic en el botón 'Explicar cálculos' para ver la explicación detallada.")
        return()  # Termina aquí si no se ha solicitado la explicación
      }

      # Continuar solo si la validación pasó y el usuario ha solicitado la explicación
      n <- input$n
      alpha <- p$alpha
      beta <- p$beta

      cat("Explicación detallada del modelo beta-binomial:\n\n")
      cat(sprintf("Número de ensayos (n): %d\n", n))
      cat(sprintf("Parámetro Alpha (α): %.2f\n", alpha))
      cat(sprintf("Parámetro Beta (β): %.2f\n\n", beta))

      cat("La distribución beta-binomial es una generalización de la distribución binomial donde la probabilidad de éxito en cada ensayo no es fija, sino que sigue una distribución beta.\n\n")

      cat("Interpretación de los resultados:\n")
      mean <- n * alpha / (alpha + beta)
      variance <- (n * alpha * beta * (alpha + beta + n)) / ((alpha + beta)^2 * (alpha + beta + 1))
      mode <- floor((n + 1) * alpha / (alpha + beta) - 1)

      cat(sprintf("1. El valor esperado (media) de esta distribución beta-binomial es %.4f\n", mean))
      cat("   Esto representa el número promedio de éxitos esperados.\n\n")
      cat(sprintf("2. La varianza de esta distribución beta-binomial es %.4f\n", variance))
      cat("   La varianza mide la dispersión y es útil para modelar sobredispersión.\n\n")
      cat(sprintf("3. La moda (valor más probable) de esta distribución es %d\n", mode))
      cat("   Este es el número de éxitos más probable en un solo experimento.\n\n")

      cat("La distribución beta-binomial es más dispersa que la binomial, lo que la hace útil para modelar situaciones con mayor variabilidad o sobredispersión.\n")

      if (alpha == beta) {
        cat("\nComo α = β, la distribución es simétrica alrededor de n/2.\n")
      } else if (alpha > beta) {
        cat("\nComo α > β, la distribución está sesgada hacia la derecha.\n")
      } else {
        cat("\nComo α < β, la distribución está sesgada hacia la izquierda.\n")
      }

    })
  }
  shinyApp(ui = ui, server = server)
}
