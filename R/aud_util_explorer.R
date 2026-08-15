
#' @encoding UTF-8
#' @title Explorador de Audiencia Útil
#' @description Aplicación Shiny para el análisis de audiencias brutas y útiles
#' con diferentes criterios demográficos.
#'
#' @details
#' La aplicación permite:
#' \itemize{
#'   \item Configurar una audiencia bruta con distribuciones de sexo, edad y nivel socioeconómico
#'   \item Analizar la audiencia útil mediante filtros demográficos
#'   \item Visualizar distribuciones mediante gráficos de barras
#'   \item Calcular estadísticas relevantes de la audiencia
#' }
#'
#' @section Parámetros de Configuración:
#' \itemize{
#'   \item Tamaño de audiencia
#'   \item Distribución por sexo (porcentajes)
#'   \item Grupos de edad seleccionables
#'   \item Niveles socioeconómicos (porcentajes)
#' }
#'
#' @return Lanza una aplicacion Shiny interactiva (efecto secundario); no
#' devuelve ningun valor util para el flujo de un script.
#'
#' @examples
#' if (interactive()) {
#'   run_aud_util_explorer()
#' }
#'
#' @import shiny
#' @import bslib
#' @import dplyr
#' @export
run_aud_util_explorer <- function() {

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    titlePanel("Explorador de la Audiencia Útil"),

    layout_sidebar(
      sidebar = sidebar(
        width = 350,

        # Panel de configuración
        card(
          card_header("Configuración"),

          numericInput("n_registros", "Tamaño de Audiencia:",
                       value = 1000000, min = 1000, max = 10000000),

          # Distribución por Sexo
          h4("Distribución por Sexo"),
          fluidRow(
            column(6, numericInput("prop_mujer", "Mujeres (%)",
                                   value = 60, min = 0, max = 100)),
            column(6, numericInput("prop_hombre", "Hombres (%)",
                                   value = 40, min = 0, max = 100))
          ),

          # Distribución por Edad (modificado para usar porcentajes)
          h4("Distribución por Edad"),
          fluidRow(
            column(6,
                   numericInput("prop_14_19", "14-19 %", value = 10, min = 0, max = 100),
                   numericInput("prop_20_24", "20-24 %", value = 15, min = 0, max = 100),
                   numericInput("prop_25_34", "25-34 %", value = 20, min = 0, max = 100),
                   numericInput("prop_35_44", "35-44 %", value = 15, min = 0, max = 100)
            ),
            column(6,
                   numericInput("prop_45_54", "45-54 %", value = 15, min = 0, max = 100),
                   numericInput("prop_55_64", "55-64 %", value = 10, min = 0, max = 100),
                   numericInput("prop_65_74", "65-74 %", value = 10, min = 0, max = 100),
                   numericInput("prop_75_plus", "75+ %", value = 5, min = 0, max = 100)
            )
          ),

          # Distribución por NSE
          h4("Distribución por NSE"),
          fluidRow(
            column(6,
                   numericInput("prop_ia1", "IA1 %", value = 10, min = 0, max = 100),
                   numericInput("prop_ia2", "IA2 %", value = 15, min = 0, max = 100),
                   numericInput("prop_ib", "IB %", value = 30, min = 0, max = 100),
                   numericInput("prop_ic", "IC %", value = 20, min = 0, max = 100)
            ),
            column(6,
                   numericInput("prop_id", "ID %", value = 15, min = 0, max = 100),
                   numericInput("prop_ie1", "IE1 %", value = 5, min = 0, max = 100),
                   numericInput("prop_ie2", "IE2 %", value = 5, min = 0, max = 100)
            )
          )
        ),

        # Panel de filtros
        card(
          card_header("Filtros para Audiencia Útil"),

          radioButtons("method", "Método:",
                       choices = c("Directo" = "direct",
                                   "Muestreo" = "sampling"),
                       selected = "direct"),

          conditionalPanel(
            condition = "input.method == 'sampling'",
            sliderInput("sample_size", "Tamaño de Muestra (%):",
                        min = 1, max = 100, value = 10, step = 1)
          ),

          selectInput("sexo_filter", "Filtrar por Sexo:",
                      choices = c("MUJER", "HOMBRE"),
                      multiple = TRUE),

          selectInput("edad_filter", "Filtrar por Edad:",
                      choices = c("14-19", "20-24", "25-34", "35-44",
                                  "45-54", "55-64", "65-74", "75+"),
                      multiple = TRUE),

          selectInput("nse_filter", "Filtrar por NSE:",
                      choices = c("IA1", "IA2", "IB", "IC", "ID", "IE1", "IE2"),
                      multiple = TRUE)
        )
      ),

      # Contenido principal con acordeones
      accordion(
        accordion_panel(
          "Audiencia Bruta",
          layout_column_wrap(
            width = 1/3,
            card(plotOutput("bruta_sexo", height = "300px")),
            card(plotOutput("bruta_edad", height = "300px")),
            card(plotOutput("bruta_nse", height = "300px"))
          )
        ),
        accordion_panel(
          "Audiencia Útil",
          layout_column_wrap(
            width = 1/3,
            card(plotOutput("util_sexo", height = "300px")),
            card(plotOutput("util_edad", height = "300px")),
            card(plotOutput("util_nse", height = "300px"))
          ),
          card(
            card_header("Estadísticas"),
            tableOutput("stats_table")
          )
        ),
        multiple = TRUE
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    # Datos reactivos para la audiencia bruta
    audiencia_bruta <- reactive({
      # Validar inputs
      validate(
        need(input$prop_mujer + input$prop_hombre == 100,
             "Las proporciones de sexo deben sumar 100%"),
        need(sum(c(input$prop_ia1, input$prop_ia2, input$prop_ib,
                   input$prop_ic, input$prop_id, input$prop_ie1,
                   input$prop_ie2)) == 100,
             "Las proporciones de NSE deben sumar 100%"),
        need(sum(c(input$prop_14_19, input$prop_20_24, input$prop_25_34,
                   input$prop_35_44, input$prop_45_54, input$prop_55_64,
                   input$prop_65_74, input$prop_75_plus)) == 100,
             "Las proporciones de edad deben sumar 100%")
      )

      set.seed(123)

      data.frame(
        sexo = sample(c("MUJER", "HOMBRE"),
                      input$n_registros,
                      prob = c(input$prop_mujer/100, input$prop_hombre/100),
                      replace = TRUE),
        grupo_edad = sample(
          c("14-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"),
          input$n_registros,
          prob = c(input$prop_14_19, input$prop_20_24, input$prop_25_34,
                   input$prop_35_44, input$prop_45_54, input$prop_55_64,
                   input$prop_65_74, input$prop_75_plus)/100,
          replace = TRUE
        ),
        nivel_socioeconomico = sample(
          c("IA1", "IA2", "IB", "IC", "ID", "IE1", "IE2"),
          input$n_registros,
          prob = c(input$prop_ia1, input$prop_ia2, input$prop_ib,
                   input$prop_ic, input$prop_id, input$prop_ie1,
                   input$prop_ie2)/100,
          replace = TRUE
        )
      )
    })

    # Audiencia útil
    audiencia_util <- reactive({
      req(audiencia_bruta())

      # Validar que haya al menos un filtro seleccionado
      req(length(input$sexo_filter) > 0 ||
            length(input$edad_filter) > 0 ||
            length(input$nse_filter) > 0,
          "Seleccione al menos un criterio de filtrado")

      datos_filtrados <- audiencia_bruta()

      if (length(input$sexo_filter) > 0)
        datos_filtrados <- datos_filtrados[datos_filtrados$sexo %in% input$sexo_filter,]

      if (length(input$edad_filter) > 0)
        datos_filtrados <- datos_filtrados[datos_filtrados$grupo_edad %in% input$edad_filter,]

      if (length(input$nse_filter) > 0)
        datos_filtrados <- datos_filtrados[datos_filtrados$nivel_socioeconomico %in% input$nse_filter,]

      if (input$method == "sampling") {
        sample_size <- floor(nrow(datos_filtrados) * (input$sample_size/100))
        datos_filtrados <- datos_filtrados[sample(nrow(datos_filtrados),
                                                  min(sample_size, nrow(datos_filtrados))),]
      }

      datos_filtrados
    })

    # Función genérica para crear gráficos de barras
    crear_grafico_barras <- function(data, variable, titulo) {
      ggplot(data, aes(x = .data[[variable]], fill = .data[[variable]])) +
        geom_bar(aes(y = after_stat(count)/nrow(data))) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
        ) +
        labs(title = titulo, y = "Proporción", x = NULL)
    }

    # Gráficos de audiencia bruta
    output$bruta_sexo <- renderPlot({
      req(audiencia_bruta())
      crear_grafico_barras(audiencia_bruta(), "sexo", "Por Sexo")
    })

    output$bruta_edad <- renderPlot({
      req(audiencia_bruta())
      crear_grafico_barras(audiencia_bruta(), "grupo_edad", "Por Edad")
    })

    output$bruta_nse <- renderPlot({
      req(audiencia_bruta())
      crear_grafico_barras(audiencia_bruta(), "nivel_socioeconomico", "Por NSE")
    })

    # Gráficos de audiencia útil
    output$util_sexo <- renderPlot({
      req(audiencia_util())
      crear_grafico_barras(audiencia_util(), "sexo", "Por Sexo")
    })

    output$util_edad <- renderPlot({
      req(audiencia_util())
      crear_grafico_barras(audiencia_util(), "grupo_edad", "Por Edad")
    })

    output$util_nse <- renderPlot({
      req(audiencia_util())
      crear_grafico_barras(audiencia_util(), "nivel_socioeconomico", "Por NSE")
    })

    # Estadísticas
    output$stats_table <- renderTable({
      req(audiencia_util())
      data.frame(
        Métrica = c(
          "Audiencia Bruta",
          "Audiencia Útil",
          "Tasa de Conversión",
          "Método de Cálculo"
        ),
        Valor = c(
          format(nrow(audiencia_bruta()), big.mark = ","),
          format(nrow(audiencia_util()), big.mark = ","),
          sprintf("%.1f%%", 100 * nrow(audiencia_util()) / nrow(audiencia_bruta())),
          ifelse(input$method == "direct", "Directo",
                 sprintf("Muestreo (%d%%)", input$sample_size))
        )
      )
    })

    # Observadores para validación
    observe({
      if (input$prop_mujer + input$prop_hombre != 100) {
        showNotification("Las proporciones de sexo deben sumar 100%", type = "warning")
      }

      suma_nse <- sum(c(input$prop_ia1, input$prop_ia2, input$prop_ib,
                        input$prop_ic, input$prop_id, input$prop_ie1,
                        input$prop_ie2))
      if (suma_nse != 100) {
        showNotification("Las proporciones de NSE deben sumar 100%", type = "warning")
      }

      suma_edad <- sum(c(input$prop_14_19, input$prop_20_24, input$prop_25_34,
                         input$prop_35_44, input$prop_45_54, input$prop_55_64,
                         input$prop_65_74, input$prop_75_plus))
      if (suma_edad != 100) {
        showNotification("Las proporciones de edad deben sumar 100%", type = "warning")
      }
    })
  }

  # Ejecutar la aplicación Shiny
  shinyApp(ui = ui, server = server)
}
