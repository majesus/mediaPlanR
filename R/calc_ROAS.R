#' @encoding UTF-8
#' @title Calcular ROAS (Retorno sobre la Inversión Publicitaria)
#' @description Implementación del cálculo de ROAS para campañas de marketing,
#' considerando el embudo de conversión y efectos de recomendación (word-of-mouth).
#'
#' @details La función calcula dos tipos de ROAS:
#' \itemize{
#'   \item ROAS Bruto: Ingresos totales por ventas / Inversión publicitaria
#'   \item ROAS Neto: Beneficio (margen) total / Inversión publicitaria
#' }
#'
#' @param audiencia_efectiva Numérico. Número total de personas alcanzadas por la campaña
#' @param tasa_visita Numérico. Porcentaje de la audiencia que visita la web (0-1). Si es NULL, no se usa en cálculos.
#' @param tasa_prueba Numérico. Porcentaje de visitantes que prueban el producto (0-1). Si es NULL, no se usa en cálculos.
#' @param tasa_conversion Numérico. Porcentaje que se convierte en cliente regular (0-1). Si es NULL, no se usa en cálculos.
#' @param tasa_wom Numérico. Nuevos clientes generados por cada cliente actual (0-1). Si es NULL, no se usa en cálculos.
#' @param unidades_mes Numérico. Unidades compradas por cliente al mes
#' @param precio_unidad Numérico. Precio de venta por unidad
#' @param margen_unidad Numérico. Beneficio neto por unidad vendida
#' @param vida_cliente Numérico. Meses que permanece activo un cliente
#' @param periodo_analisis Numérico. Meses a considerar en el análisis
#' @param inversion Numérico. Inversión total en publicidad
#' @param variaciones Lista con nombre de la variable y valor numérico.
#' Escenarios para análisis de sensibilidad.
#' @param imprimir_resultados Lógico. Si TRUE, muestra resultados detallados
#' @param roas_objetivo Numérico. ROAS objetivo a alcanzar; si se indica, la
#' función calcula la audiencia necesaria para lograrlo (opcional)
#' @param tipo_roas_objetivo Carácter. Tipo de ROAS objetivo ("bruto" o "neto")
#'
#' @examples
#' # ROAS a partir de una audiencia efectiva
#' calcular_roas(
#'   audiencia_efectiva = 51000,
#'   precio_unidad = 2.50,
#'   margen_unidad = 1.14,
#'   inversion = 45000
#' )
#'
#' # Audiencia necesaria para un ROAS objetivo
#' calcular_roas(
#'   inversion = 45000,
#'   precio_unidad = 2.85,
#'   margen_unidad = 1.14,
#'   roas_objetivo = 5,
#'   tipo_roas_objetivo = "bruto"
#' )
#'
#' @import ggplot2
#' @export
calcular_roas <- function(audiencia_efectiva = NULL,
                          tasa_visita = NULL,
                          tasa_prueba = NULL,
                          tasa_conversion = NULL,
                          tasa_wom = NULL,
                          unidades_mes = 8,
                          precio_unidad = 2.50,
                          margen_unidad = 1.14,
                          vida_cliente = 12,
                          periodo_analisis = 12,
                          inversion,
                          variaciones = NULL,
                          imprimir_resultados = TRUE,
                          roas_objetivo = NULL,
                          tipo_roas_objetivo = "bruto") {

  # Validaciones
  if (is.null(audiencia_efectiva) && is.null(roas_objetivo)) {
    stop("Debe especificar 'audiencia_efectiva' o 'roas_objetivo'")
  }
  if (!is.null(roas_objetivo) && (roas_objetivo <= 0)) {
    stop("'roas_objetivo' debe ser un valor positivo")
  }
  if (!is.null(variaciones) && !all(unlist(lapply(variaciones, is.numeric)))) {
    stop("Todos los valores en 'variaciones' deben ser numéricos")
  }

  if (!is.null(tasa_visita) && (tasa_visita < 0 || tasa_visita > 1)) {
    stop("La 'tasa_visita' debe estar entre 0 y 1")
  }
  if (!is.null(tasa_prueba) && (tasa_prueba < 0 || tasa_prueba > 1)) {
    stop("La 'tasa_prueba' debe estar entre 0 y 1")
  }
  if (!is.null(tasa_conversion) && (tasa_conversion < 0 || tasa_conversion > 1)) {
    stop("La 'tasa_conversion' debe estar entre 0 y 1")
  }
  if (!is.null(tasa_wom) && tasa_wom < 0) {
    stop("La 'tasa_wom' debe ser mayor o igual a 0")
  }
  if (unidades_mes <=0 || precio_unidad <=0 || margen_unidad <=0 || vida_cliente <=0 || periodo_analisis <=0 || inversion <=0){
    stop ("unidades_mes, precio_unidad, margen_unidad, vida_cliente, periodo_analisis, e inversion deben ser valores mayores que 0.")
  }

  # Funciones internas
  calcular_metricas <- function(audiencia) {
    # 1. Cálculos del embudo de conversión

    # Modificar el cálculo de visitas si tasa_visita es NULL
    if (is.null(tasa_visita)) {
      visitas <- audiencia
    } else {
      visitas <- audiencia * tasa_visita
    }

    # Modificar el cálculo de pruebas si tasa_prueba es NULL
    if (is.null(tasa_prueba)) {
      pruebas <- visitas
    } else {
      pruebas <- visitas * tasa_prueba
    }

    # Modificar el cálculo de clientes_directos si tasa_conversion es NULL
    if (is.null(tasa_conversion)) {
      clientes_directos <- pruebas
    } else {
      clientes_directos <- pruebas * tasa_conversion
    }

    # Modificar el cálculo de clientes_wom si tasa_wom es NULL
    if (is.null(tasa_wom)) {
      clientes_wom <- 0
    } else {
      clientes_wom <- clientes_directos * tasa_wom
    }

    clientes_totales <- clientes_directos + clientes_wom

    # 2. Métricas financieras
    periodo_efectivo <- min(periodo_analisis, vida_cliente)
    ventas_totales <- clientes_totales * unidades_mes * precio_unidad * periodo_efectivo
    beneficio_total <- clientes_totales * unidades_mes * margen_unidad * periodo_efectivo

    # 3. Cálculo de ROAS y métricas relacionadas
    roas_bruto <- ventas_totales / inversion
    roas_neto <- beneficio_total / inversion
    roi <- (beneficio_total - inversion) / inversion * 100
    cpa <- inversion / clientes_totales

    return(list(
      metricas = list(
        roas_bruto = roas_bruto,
        roas_neto = roas_neto,
        roi = roi,
        cpa = cpa,
        ventas_totales = ventas_totales,
        beneficio_total = beneficio_total
      ),
      embudo = list(
        audiencia = audiencia,
        visitas = visitas,
        pruebas = pruebas,
        clientes_directos = clientes_directos,
        clientes_wom = clientes_wom,
        clientes_totales = clientes_totales
      )
    ))
  }

  crear_tabla_resultados <- function(resultados) {
    cat("\n=== ANÁLISIS DE CAMPAÑA DE MARKETING ===\n\n")

    # 1. Métricas del Embudo de Conversión
    cat("EMBUDO DE CONVERSIÓN:\n")
    cat(sprintf("%-25s %12s %15s\n", "Etapa", "Cantidad", "Ratio"))
    cat(paste(rep("-", 55), collapse = ""), "\n")
    cat(sprintf("%-25s %12.0f %15s\n", "Audiencia Alcanzada",
                round(resultados$embudo$audiencia), "100%"))

    # Incluir condicionales en la impresión de resultados
    if (!is.null(tasa_visita)) {
      cat(sprintf("%-25s %12.0f %15.1f%%\n", "Visitas Web/RRSS",
                  round(resultados$embudo$visitas),
                  100 * resultados$embudo$visitas/resultados$embudo$audiencia))
    }

    if (!is.null(tasa_prueba)) {
      cat(sprintf("%-25s %12.0f %15.1f%%\n", "Pruebas de Producto",
                  round(resultados$embudo$pruebas),
                  100 * resultados$embudo$pruebas/resultados$embudo$visitas))
    }

    if (!is.null(tasa_conversion)) {
      cat(sprintf("%-25s %12.0f %15.1f%%\n", "Clientes Directos",
                  round(resultados$embudo$clientes_directos),
                  100 * resultados$embudo$clientes_directos/resultados$embudo$pruebas))
    }

    if (!is.null(tasa_wom)) {
      cat(sprintf("%-25s %12.0f %15.1f%%\n", "Clientes por WOM",
                  round(resultados$embudo$clientes_wom),
                  100 * resultados$embudo$clientes_wom/resultados$embudo$clientes_directos))
    }

    cat(sprintf("%-25s %12.0f\n", "CLIENTES TOTALES",
                round(resultados$embudo$clientes_totales)))

    # 2. Métricas Financieras
    cat("\nMÉTRICAS FINANCIERAS:\n")
    cat(sprintf("%-25s %12s\n", "Métrica", "Valor"))
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat(sprintf("%-25s %12.2fx\n", "ROAS Bruto", resultados$metricas$roas_bruto))
    cat(sprintf("%-25s %12.2fx\n", "ROAS Neto", resultados$metricas$roas_neto))
    cat(sprintf("%-25s %12.2f%%\n", "ROI", resultados$metricas$roi))
    cat(sprintf("%-25s %12.2f€\n", "CPA", resultados$metricas$cpa))
    cat(sprintf("%-25s %12.2f€\n", "Ventas Totales", resultados$metricas$ventas_totales))
    cat(sprintf("%-25s %12.2f€\n", "Beneficio Total", resultados$metricas$beneficio_total))

    # 3. Explicación de Resultados
    cat("\nEXPLICACIÓN DE RESULTADOS:\n")
    cat("------------------------\n")
    cat(sprintf("1. Por cada 1€ invertido en publicidad:\n"))
    cat(sprintf("   - Se generan %.2f€ en ventas (ROAS Bruto)\n", resultados$metricas$roas_bruto))
    cat(sprintf("   - Se obtienen %.2f€ en beneficios (ROAS Neto)\n", resultados$metricas$roas_neto))
    cat(sprintf("2. El coste de adquisición por cliente (CPA) es %.2f€\n", resultados$metricas$cpa))
    cat(sprintf("3. La inversión publicitaria produce un ROI del %.1f%%\n", resultados$metricas$roi))

    if (!is.null(tasa_wom)) {
      cat(sprintf("4. Cada cliente directo genera %.1f clientes adicionales por WOM\n",
                  resultados$embudo$clientes_wom / resultados$embudo$clientes_directos))
    }

    cat(sprintf("5. Del total de la audiencia alcanzada (%.0f):\n", resultados$embudo$audiencia))
    if (!is.null(tasa_visita)) {
      cat(sprintf("   - %.1f%% visita la web\n", 100 * tasa_visita))
    }
    if (!is.null(tasa_prueba)) {
      cat(sprintf("   - %.1f%% de los visitantes prueba el producto\n", 100 * tasa_prueba))
    }
    if (!is.null(tasa_conversion)) {
      cat(sprintf("   - %.1f%% de los que prueban se convierten en clientes\n", 100 * tasa_conversion))
    }

    # 4. Gráfico del embudo de conversión

    # Crear data frame para ggplot, excluyendo etapas no utilizadas
    embudo_df <- data.frame(
      Etapa = c("Audiencia"),
      Cantidad = c(resultados$embudo$audiencia)
    )

    if (!is.null(tasa_visita)) {
      embudo_df <- rbind(embudo_df, data.frame(Etapa = "Visitas", Cantidad = resultados$embudo$visitas))
    }
    if (!is.null(tasa_prueba)) {
      embudo_df <- rbind(embudo_df, data.frame(Etapa = "Pruebas", Cantidad = resultados$embudo$pruebas))
    }
    if (!is.null(tasa_conversion)) {
      embudo_df <- rbind(embudo_df, data.frame(Etapa = "Clientes Directos", Cantidad = resultados$embudo$clientes_directos))
    }
    if (!is.null(tasa_wom)) {
      embudo_df <- rbind(embudo_df, data.frame(Etapa = "Clientes WOM", Cantidad = resultados$embudo$clientes_wom))
    }

    # Asegurarse de que el orden de los factores refleje el orden deseado en el gráfico
    orden_etapas <- c("Audiencia", "Visitas", "Pruebas", "Clientes Directos", "Clientes WOM")
    embudo_df$Etapa <- factor(embudo_df$Etapa, levels = orden_etapas, ordered = TRUE)

    plot_embudo <- ggplot(embudo_df, aes(x = .data$Etapa, y = .data$Cantidad, fill = .data$Etapa)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(.data$Cantidad)), vjust = -0.5) +
      labs(title = "Embudo de Conversión", x = "Etapa", y = "Cantidad") +
      theme_minimal() +
      guides(fill = "none")

    print(plot_embudo)
  }

  # Inicialización de resultados
  resultados <- NULL

  # Análisis con audiencia efectiva
  if (!is.null(audiencia_efectiva)) {
    resultados_base <- calcular_metricas(audiencia_efectiva)
    if (imprimir_resultados) {
      crear_tabla_resultados(resultados_base)
    }

    resultados <- data.frame(
      ROAS_Bruto = resultados_base$metricas$roas_bruto,
      ROAS_Neto = resultados_base$metricas$roas_neto,
      ROI = resultados_base$metricas$roi,
      CPA = resultados_base$metricas$cpa,
      Ventas_Totales = resultados_base$metricas$ventas_totales,
      Beneficio_Total = resultados_base$metricas$beneficio_total,
      Clientes_Totales = resultados_base$embudo$clientes_totales
    )
  }

  # Cálculo de audiencia necesaria para ROAS objetivo
  if (!is.null(roas_objetivo)) {
    # Validar tipo de ROAS
    tipo_roas_objetivo <- tolower(tipo_roas_objetivo)
    if (!tipo_roas_objetivo %in% c("bruto", "neto")) {
      stop("tipo_roas_objetivo debe ser 'bruto' o 'neto'")
    }

    # Cálculos para ROAS objetivo
    ingresos_necesarios <- roas_objetivo * inversion
    periodo_efectivo <- min(periodo_analisis, vida_cliente)

    # El valor por unidad depende del tipo de ROAS
    unidad_valor <- if(tipo_roas_objetivo == "bruto") precio_unidad else margen_unidad

    clientes_necesarios <- ingresos_necesarios / (unidades_mes * unidad_valor * periodo_efectivo)

    # Ajustar el cálculo de clientes_directos_necesarios si tasa_wom es NULL
    if (is.null(tasa_wom)) {
      clientes_directos_necesarios <- clientes_necesarios
    } else {
      clientes_directos_necesarios <- clientes_necesarios / (1 + tasa_wom)
    }

    # Ajustar el cálculo de pruebas_necesarias si tasa_conversion es NULL
    if (is.null(tasa_conversion)) {
      pruebas_necesarias <- clientes_directos_necesarios
    } else {
      pruebas_necesarias <- clientes_directos_necesarios / tasa_conversion
    }

    # Ajustar el cálculo de visitas_necesarias si tasa_prueba es NULL
    if (is.null(tasa_prueba)) {
      visitas_necesarias <- pruebas_necesarias
    } else {
      visitas_necesarias <- pruebas_necesarias / tasa_prueba
    }

    # Ajustar el cálculo de la audiencia_necesaria si tasa_visita es NULL
    if (is.null(tasa_visita)) {
      audiencia_necesaria <- visitas_necesarias
    } else {
      audiencia_necesaria <- visitas_necesarias / tasa_visita
    }

    # Imprimir resultados si es necesario
    if (imprimir_resultados) {
      cat(sprintf("\nREQUERIMIENTOS PARA ROAS %s OBJETIVO %.2fx:\n",
                  toupper(tipo_roas_objetivo), roas_objetivo))
      cat("----------------------------------------\n")
      cat(sprintf("Audiencia necesaria: %.0f personas\n", round(audiencia_necesaria)))
      cat(sprintf("Visitas necesarias: %.0f\n", round(visitas_necesarias)))
      cat(sprintf("Pruebas necesarias: %.0f\n", round(pruebas_necesarias)))
      cat(sprintf("Clientes directos necesarios: %.0f\n", round(clientes_directos_necesarios)))
      cat(sprintf("Clientes totales necesarios: %.0f\n", round(clientes_necesarios)))

      # Verificación
      verificacion <- calcular_metricas(audiencia_necesaria)
      cat("\nVERIFICACIÓN:\n")
      if (tipo_roas_objetivo == "bruto") {
        cat(sprintf("ROAS bruto calculado: %.2fx\n", verificacion$metricas$roas_bruto))
        cat(sprintf("(ROAS neto equivalente: %.2fx)\n", verificacion$metricas$roas_neto))
      } else {
        cat(sprintf("ROAS neto calculado: %.2fx\n", verificacion$metricas$roas_neto))
        cat(sprintf("(ROAS bruto equivalente: %.2fx)\n", verificacion$metricas$roas_bruto))
      }
    }

    # Crear o actualizar el data frame de resultados
    if (is.null(resultados)) {
      resultados <- data.frame(
        Tipo_ROAS = tipo_roas_objetivo,
        ROAS_Objetivo = roas_objetivo,
        Audiencia_Necesaria = audiencia_necesaria,
        Visitas_Necesarias = visitas_necesarias,
        Pruebas_Necesarias = pruebas_necesarias,
        Clientes_Directos_Necesarios = clientes_directos_necesarios,
        Clientes_Totales_Necesarios = clientes_necesarios
      )
    } else {
      resultados$Tipo_ROAS_Objetivo <- tipo_roas_objetivo
      resultados$ROAS_Objetivo <- roas_objetivo
      resultados$Audiencia_Necesaria <- audiencia_necesaria
      resultados$Visitas_Necesarias <- visitas_necesarias
      resultados$Pruebas_Necesarias <- pruebas_necesarias
      resultados$Clientes_Directos_Necesarios <- clientes_directos_necesarios
      resultados$Clientes_Totales_Necesarios <- clientes_necesarios
    }
  }

  # Análisis de sensibilidad si se proporcionan variaciones
  if (!is.null(variaciones)) {
    # Crear un data frame para almacenar los resultados del análisis de sensibilidad
    resultados_sensibilidad <- data.frame()

    # Entorno local de esta llamada a calcular_roas(): las variables que se
    # modifican temporalmente a continuación (tasa_visita, tasa_prueba, ...)
    # viven aqui, nunca en el entorno global del usuario.
    env <- environment()

    # Iterar sobre cada variable especificada en 'variaciones'
    for (variable in names(variaciones)) {
      valor_base <- get(variable, envir = env)

      # Iterar sobre cada variación para la variable actual
      for (i in seq_along(variaciones[[variable]])) {

        # Modificar la variable (solo en el entorno local de la función)
        assign(variable, variaciones[[variable]][i], envir = env)

        # Calcular las métricas con la variable modificada
        resultados_escenario <- calcular_metricas(audiencia_efectiva)

        # Preparar los datos para agregar a 'resultados_sensibilidad'
        df_escenario <- data.frame(
          Variable = variable,
          Valor = variaciones[[variable]][i],
          ROAS_Bruto = resultados_escenario$metricas$roas_bruto,
          ROAS_Neto = resultados_escenario$metricas$roas_neto,
          ROI = resultados_escenario$metricas$roi,
          CPA = resultados_escenario$metricas$cpa,
          Ventas_Totales = resultados_escenario$metricas$ventas_totales,
          Beneficio_Total = resultados_escenario$metricas$beneficio_total,
          Clientes_Totales = resultados_escenario$embudo$clientes_totales
        )

        # Agregar los resultados del escenario actual a 'resultados_sensibilidad'
        resultados_sensibilidad <- rbind(resultados_sensibilidad, df_escenario)
      }

      # Restaurar el valor original de la variable en el MISMO entorno local
      # (nunca en .GlobalEnv, para no sobrescribir variables del usuario)
      assign(variable, valor_base, envir = env)
    }

    # Imprimir el análisis de sensibilidad si se solicita
    if (imprimir_resultados) {
      cat("\nANÁLISIS DE SENSIBILIDAD:\n")
      print(resultados_sensibilidad)
    }

    # Combinar los resultados de sensibilidad con los resultados principales
    if (is.null(resultados)) {
      resultados <- resultados_sensibilidad
    } else {
      resultados <- list(Resultados_Base = resultados, Analisis_Sensibilidad = resultados_sensibilidad)
    }
  }

  # Asegurar que siempre devolvemos algo
  if (is.null(resultados)) {
    resultados <- data.frame()
  }

  invisible(resultados)
}
