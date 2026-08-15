#' @encoding UTF-8
#' @title Optimización de planes de medios con restricciones mediante procesamiento por lotes
#' @description Implementa un algoritmo de optimización para encontrar la combinación óptima de soportes publicitarios
#' que maximiza la cobertura para una Frecuencia Efectiva Mínima (FEM) determinada, respetando restricciones presupuestarias.
#' El algoritmo utiliza un enfoque de procesamiento por lotes (batches) y permite elegir entre el modelo de Sainsbury o el Binomial
#' para el cálculo de coberturas y distribución de contactos. Permite la exclusión de soportes específicos y maneja restricciones
#' presupuestarias con tolerancia configurable. Adicionalmente, permite trabajar con audiencias brutas o audiencias útiles
#' (considerando índices de utilidad).
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param soportes_df Data frame con columnas: soportes (character), audiencias (numeric), tarifas (numeric).
#'        Si se usa audiencia útil, debe incluir también indices_utilidad (numeric)
#' @param FEM Frecuencia Efectiva Mínima requerida (número entero positivo)
#' @param objetivo_cobertura Cobertura objetivo a alcanzar (porcentaje)
#' @param presupuesto_max Presupuesto máximo disponible
#' @param poblacion_total Tamaño de la población objetivo (por defecto 47000000)
#' @param tam_batch Tamaño de los lotes para procesamiento (por defecto 5)
#' @param soportes_vetados Vector de caracteres con nombres de soportes a excluir (opcional)
#' @param modelo Modelo a utilizar para el cálculo de cobertura: "sainsbury" o "binomial" (por defecto "sainsbury")
#' @param usar_audiencia_util Logical indicando si usar audiencia útil (TRUE) o bruta (FALSE, por defecto)
#' @param tol Tolerancia para comparación de números flotantes.
#' @param tolerancia_presupuesto_tipo Tipo de tolerancia para el presupuesto: "porcentaje" o "absoluto" (por defecto "porcentaje").
#' @param tolerancia_cobertura_tipo Tipo de tolerancia para la cobertura: "porcentaje", "absoluto" o "personas" (por defecto "porcentaje").
#' @param tolerancia_presupuesto Valor de la tolerancia para el presupuesto (por defecto 0.10 = 10% si tolerancia_presupuesto_tipo es "porcentaje").
#' @param tolerancia_cobertura Valor de la tolerancia para la cobertura (por defecto 0.05 = 5% si tolerancia_cobertura_tipo es "porcentaje").
#'
#' @details
#' El algoritmo de optimización sigue los siguientes pasos:
#' \enumerate{
#'   \item Preprocesamiento de datos:
#'     \itemize{
#'       \item Filtrado de soportes vetados
#'       \item Cálculo de audiencias útiles si corresponde (audiencia * índice_utilidad)
#'       \item Cálculo de ratios de eficiencia (audiencia/tarifa o audiencia útil/tarifa)
#'       \item Ordenación por eficiencia
#'     }
#'   \item Procesamiento por lotes:
#'     \itemize{
#'       \item División en batches para optimizar el proceso
#'       \item Evaluación de combinaciones que cumplen FEM mínima
#'       \item Verificación de restricción presupuestaria
#'       \item Cálculo de cobertura mediante el modelo seleccionado
#'     }
#'   \item Optimización iterativa:
#'     \itemize{
#'       \item Búsqueda de mejores soluciones en cada batch
#'       \item Actualización progresiva de la mejor solución
#'       \item Criterio de parada por objetivo alcanzado
#'     }
#' }
#'
#' Los modelos disponibles son:
#' \itemize{
#'   \item Sainsbury: Considera la duplicación entre soportes como el producto de las probabilidades individuales
#'   \item Binomial: Asume una probabilidad media de exposición para todos los soportes
#' }
#'
#' @return Una lista conteniendo:
#' \itemize{
#'   \item exito: Logical indicando si se encontró solución factible
#'   \item cobertura_alcanzada: Porcentaje de cobertura logrado
#'   \item coste_total: Coste total del plan
#'   \item soportes_seleccionados: Data frame con los soportes del plan óptimo. Si se usa audiencia útil,
#'         incluye columnas adicionales para audiencia útil e índices de utilidad
#'   \item plan_completo: Vector binario indicando soportes seleccionados
#'   \item objetivo_alcanzado: Logical indicando si se alcanzó el objetivo
#'   \item distribucion: Lista con distribución de contactos y acumulada
#'   \item soportes_vetados: Vector de soportes excluidos encontrados
#'   \item soportes_no_encontrados: Vector de soportes vetados no localizados
#' }
#'
#' @examples
#' # Ejemplo con audiencia bruta y modelo Sainsbury, tolerancia de presupuesto
#' # absoluta y de cobertura en personas, a partir de un CSV externo
#' \dontrun{
#' datos <- readr::read_csv(file = "data.csv", show_col_types = FALSE)
#'
#' resultado_bruto <- optimize_media_sb(
#'   soportes_df = datos,
#'   FEM = 2,
#'   objetivo_cobertura = 50,
#'   presupuesto_max = 100000,
#'   modelo = "sainsbury",
#'   usar_audiencia_util = FALSE,
#'   tolerancia_presupuesto_tipo = "absoluto",
#'   tolerancia_presupuesto = 5000,  # 5000€ de tolerancia
#'   tolerancia_cobertura_tipo = "personas",
#'   tolerancia_cobertura = 10000, # 10000 personas de tolerancia
#'   poblacion_total = 47000000
#' )
#' }
#'
#' # Ejemplo con audiencia útil y modelo Binomial, tolerancia de presupuesto y cobertura en porcentajes
#' datos_util <- data.frame(
#'   soportes = c("Medio1", "Medio2", "Medio3"),
#'   audiencias = c(1000000, 800000, 600000),
#'   tarifas = c(50000, 40000, 30000),
#'   indices_utilidad = c(1.2, 1.1, 0.9)
#' )
#'
#' resultado_util <- optimize_media_sb(
#'   soportes_df = datos_util,
#'   FEM = 2,
#'   objetivo_cobertura = 50,
#'   presupuesto_max = 100000,
#'   modelo = "binomial",
#'   usar_audiencia_util = TRUE,
#'   tolerancia_presupuesto_tipo = "porcentaje",
#'   tolerancia_presupuesto = 0.15, # 15%
#'   tolerancia_cobertura_tipo = "porcentaje",
#'   tolerancia_cobertura = 0.1 # 10%
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para el modelo de Sainsbury
#' \code{\link{calc_binomial}} para el modelo Binomial
#' @importFrom utils txtProgressBar setTxtProgressBar
optimize_media_sb <- function(soportes_df,
                              FEM,
                              objetivo_cobertura,
                              presupuesto_max,
                              poblacion_total = 47000000,
                              tam_batch = 5,
                              soportes_vetados = NULL,
                              modelo = c("sainsbury", "binomial"),
                              usar_audiencia_util = FALSE,
                              tol = 1e-10,
                              tolerancia_presupuesto_tipo = "porcentaje",
                              tolerancia_cobertura_tipo = "porcentaje",
                              tolerancia_presupuesto = 0.10,
                              tolerancia_cobertura = 0.05) {
  # Validar modelo
  modelo <- match.arg(modelo)

  # Validar tipos de tolerancia
  tolerancia_presupuesto_tipo <-
    match.arg(tolerancia_presupuesto_tipo, c("porcentaje", "absoluto"))
  tolerancia_cobertura_tipo <-
    match.arg(tolerancia_cobertura_tipo, c("porcentaje", "absoluto", "personas"))

  cat("\nIniciando optimización...\n")

  # Validación para audiencia útil
  if (usar_audiencia_util &&
      !"indices_utilidad" %in% colnames(soportes_df)) {
    stop("Se solicitó usar audiencia útil pero no se encuentra la columna 'indices_utilidad'")
  }

  # Gestión de soportes vetados
  soportes_no_encontrados <- character(0)
  if (!is.null(soportes_vetados)) {
    soportes_vetados <- unique(trimws(soportes_vetados))
    for (soporte in soportes_vetados) {
      if (!soporte %in% soportes_df$soportes) {
        soportes_no_encontrados <- c(soportes_no_encontrados, soporte)
        soportes_vetados <- soportes_vetados[soportes_vetados != soporte]
      }
    }
    if (length(soportes_no_encontrados) > 0) {
      cat("\nAVISO: Los siguientes soportes vetados no fueron encontrados:\n")
      cat(paste("-", soportes_no_encontrados, collapse = "\n"))
      cat("\n")
    }
    # Filtrar soportes vetados
    soportes_df <-
      soportes_df[!soportes_df$soportes %in% soportes_vetados, ]
  }

  # Verificar que quedan suficientes soportes
  if (nrow(soportes_df) < FEM) {
    stop("No hay suficientes soportes disponibles para alcanzar la FEM requerida")
  }

  # Preparar datos
  if (usar_audiencia_util) {
    soportes_df$audiencias_calculo <-
      soportes_df$audiencias * soportes_df$indices_utilidad
  } else {
    soportes_df$audiencias_calculo <- soportes_df$audiencias
  }

  soportes_df$eficiencia <-
    soportes_df$audiencias_calculo / soportes_df$tarifas
  soportes_ordenados <- soportes_df[order(-soportes_df$eficiencia), ]
  n_soportes <- nrow(soportes_ordenados)

  # Crear batches basados en el número de soportes restantes
  n_batches <- ceiling((n_soportes - FEM + 1) / tam_batch)

  # Función para evaluar un batch de combinaciones
  evaluar_batch <- function(batch_combinaciones) {
    resultados <- list()
    costes <- sapply(1:nrow(batch_combinaciones), function(i) {
      sum(soportes_ordenados$tarifas[batch_combinaciones[i, ] == 1])
    })

    # Calcular la tolerancia de presupuesto en valor absoluto
    if (tolerancia_presupuesto_tipo == "porcentaje") {
      tolerancia_presupuesto_abs <- presupuesto_max * tolerancia_presupuesto
    } else {
      tolerancia_presupuesto_abs <- tolerancia_presupuesto
    }

    # Filtrar primero por presupuesto
    combinaciones_validas <-
      which(costes <= presupuesto_max + tolerancia_presupuesto_abs)

    if (length(combinaciones_validas) == 0) {
      return(NULL)
    }

    # Evaluar solo las combinaciones que cumplen presupuesto
    for (i in combinaciones_validas) {
      comb <- batch_combinaciones[i, ]
      if (sum(comb) >= FEM) {
        audiencias_sel <- soportes_ordenados$audiencias_calculo[comb == 1]
        resultado <- if (modelo == "sainsbury") {
          calc_sainsbury(audiencias_sel, poblacion_total)
        } else {
          calc_binomial(audiencias_sel, poblacion_total)
        }
        cobertura <- resultado$acumulada$porcentaje[FEM]

        resultados[[length(resultados) + 1]] <- list(
          combinacion = comb,
          cobertura = cobertura,
          coste = costes[i]
        )
      }
    }
    return(resultados)
  }

  # Inicializar mejor solución
  mejor_combinacion <- NULL
  mejor_cobertura <- 0
  mejor_coste <- Inf

  cat("\nProcesando por batches...\n")
  pb <- txtProgressBar(min = 0, max = n_batches, style = 3)

  # Generar y evaluar batches de combinaciones
  for (batch_idx in 1:n_batches) {
    setTxtProgressBar(pb, batch_idx)

    # Calcular índices para este batch
    inicio <- FEM + (batch_idx - 1) * tam_batch
    fin <- min(inicio + tam_batch - 1, n_soportes)

    # Generar todas las combinaciones posibles para este batch
    batch_base <- rep(0, n_soportes)
    batch_base[1:FEM] <- 1  # Asegurar FEM mínimo

    # Generar variaciones para este batch
    combinaciones_batch <-
      matrix(0, nrow = 2 ^ (fin - inicio + 1), ncol = n_soportes)
    combinaciones_batch[, 1:FEM] <- 1  # Asegurar FEM mínimo

    for (i in 1:(2 ^ (fin - inicio + 1))) {
      bits <- as.integer(intToBits(i - 1)[1:(fin - inicio + 1)])
      combinaciones_batch[i, inicio:fin] <- bits
    }

    # Evaluar batch
    resultados_batch <- evaluar_batch(combinaciones_batch)

    if (!is.null(resultados_batch)) {
      for (resultado in resultados_batch) {
        if (resultado$cobertura > mejor_cobertura) {
          mejor_combinacion <- resultado$combinacion
          mejor_cobertura <- resultado$cobertura
          mejor_coste <- resultado$coste

          cat(sprintf(
            "\nBatch %d/%d - ¡Nueva mejor solución!\n",
            batch_idx,
            n_batches
          ))
          cat(sprintf(
            "Cobertura: %.2f%%, Coste: %.2f€\n",
            mejor_cobertura,
            mejor_coste
          ))

          if (mejor_cobertura >= objetivo_cobertura) {
            cat("\n¡Objetivo alcanzado! Finalizando búsqueda...\n")
            break
          }
        }
      }
    }

    if (mejor_cobertura >= objetivo_cobertura)
      break
  }

  close(pb)

  # Verificar si encontramos una solución
  if (is.null(mejor_combinacion)) {
    cat("\nNO SE HA ENCONTRADO SOLUCIÓN FACTIBLE\n")
    cat("===================================\n")
    cat(sprintf("- Presupuesto máximo: %.2f€\n", presupuesto_max))
    cat(sprintf("- FEM requerida: %d impactos\n", FEM))
    cat(sprintf("- Objetivo de cobertura: %.2f%%\n", objetivo_cobertura))

    # Mostrar el soporte más barato como referencia
    soporte_min <-
      soportes_ordenados[which.min(soportes_ordenados$tarifas), ]
    cat(sprintf(
      "\nNota: El soporte más económico cuesta %.2f€ (%s)\n",
      soporte_min$tarifas,
      soporte_min$soportes
    ))

    return(
      list(
        exito = FALSE,
        mensaje = "No se encontró solución factible con las restricciones dadas",
        presupuesto_max = presupuesto_max,
        FEM_requerida = FEM,
        objetivo_cobertura = objetivo_cobertura,
        soportes_vetados = soportes_vetados,
        soportes_no_encontrados = soportes_no_encontrados,
        soporte_mas_economico = list(
          nombre = soporte_min$soportes,
          tarifa = soporte_min$tarifas
        )
      )
    )
  }

  # Preparar resultado
  soportes_seleccionados <-
    soportes_ordenados[mejor_combinacion == 1, ]

  # Calcular distribución final usando el modelo seleccionado
  distribucion_final <- if (modelo == "sainsbury") {
    calc_sainsbury(soportes_seleccionados$audiencias_calculo, poblacion_total)
  } else {
    calc_binomial(soportes_seleccionados$audiencias_calculo, poblacion_total)
  }

  # Preparar output
  # Preparar output
  cat("\nPLAN DE MEDIOS OPTIMIZADO\n")
  cat("========================\n")
  cat(sprintf("Modelo utilizado: %s\n", modelo))
  cat(sprintf(
    "Tipo de audiencia: %s\n",
    ifelse(usar_audiencia_util, "Audiencia útil", "Audiencia bruta")
  ))

  if (!is.null(soportes_vetados) &&
      (
        length(soportes_vetados) > 0 || length(soportes_no_encontrados) > 0
      )) {
    cat("\nSoportes excluidos del análisis:\n")
    if (length(soportes_vetados) > 0) {
      cat("- Excluidos encontrados en la base:\n")
      cat(paste("  *", soportes_vetados, collapse = "\n"))
    }
    if (length(soportes_no_encontrados) > 0) {
      cat("\n- Solicitados pero no encontrados en la base:\n")
      cat(paste("  *", soportes_no_encontrados, collapse = "\n"))
    }
    cat("\n")
  }

  cat("\nDISTRIBUCIÓN DE CONTACTOS:\n")
  cat("------------------------\n")
  cat("\nDistribución normal:\n")
  for (i in 1:length(distribucion_final$distribucion$personas)) {
    cat(
      sprintf(
        "%d contactos: %.2f%% (%.0f personas)\n",
        i - 1,
        # i-1 porque empezamos desde 0 contactos
        distribucion_final$distribucion$porcentaje[i],
        distribucion_final$distribucion$personas[i]
      )
    )
  }

  cat("\nDistribución acumulada:\n")
  for (i in 1:length(distribucion_final$acumulada$personas)) {
    cat(
      sprintf(
        "%d+ contactos: %.2f%% (%.0f personas)\n",
        i,
        distribucion_final$acumulada$porcentaje[i],
        distribucion_final$acumulada$personas[i]
      )
    )
  }

  cat("\nRESUMEN ECONÓMICO:\n")
  cat("----------------\n")
  cat(sprintf("Coste total: %.2f€\n", mejor_coste))
  cat(sprintf(
    "Coste por impacto: %.2f€\n",
    mejor_coste / sum(distribucion_final$distribucion$personas)
  ))

  cat("\nSOPORTES SELECCIONADOS:\n")
  cat("--------------------\n")

  # Preparar tabla de resultados según tipo de audiencia
  if (usar_audiencia_util) {
    tabla_plan <- soportes_seleccionados[
      order(-soportes_seleccionados$eficiencia),
      c("soportes",
        "audiencias",
        "audiencias_calculo",
        "indices_utilidad",
        "tarifas",
        "eficiencia")
    ]
    names(tabla_plan)[names(tabla_plan) == "audiencias_calculo"] <-
      "audiencia_util"
  } else {
    tabla_plan <- soportes_seleccionados[
      order(-soportes_seleccionados$eficiencia),
      c("soportes", "audiencias", "tarifas", "eficiencia")
    ]
  }

  # ... (Código anterior, hasta la impresión de la tabla de soportes seleccionados)

  print(tabla_plan)

  cat("\nEVALUACIÓN DE OBJETIVOS:\n")
  cat("=====================\n")

  # Evaluación del presupuesto
  # Calcular la tolerancia de presupuesto en valor absoluto
  if (tolerancia_presupuesto_tipo == "porcentaje") {
    tolerancia_presupuesto_abs <- presupuesto_max * tolerancia_presupuesto
  } else {
    tolerancia_presupuesto_abs <- tolerancia_presupuesto
  }

  presupuesto_usado_pct <-
    (mejor_coste / presupuesto_max) * 100
  cat(sprintf("\n1. PRESUPUESTO:\n"))
  cat(sprintf("   - Disponible: %.2f€\n", presupuesto_max))
  cat(sprintf(
    "   - Utilizado: %.2f€ (%.1f%%)\n",
    mejor_coste,
    presupuesto_usado_pct
  ))
  # Usar tolerancia absoluta en la comparación
  if (abs(mejor_coste - presupuesto_max) <= tolerancia_presupuesto_abs) {
    cat("   ✓ Se ha cumplido la restricción presupuestaria\n")
  } else {
    cat(
      sprintf(
        "   ⚠ Se ha excedido el presupuesto en %.2f€\n",
        presupuesto_max - mejor_coste
      )
    )
  }

  # Evaluación de la cobertura
  cat(sprintf("\n2. COBERTURA:\n"))

  # Imprimir el objetivo de cobertura en el formato correcto
  if (tolerancia_cobertura_tipo == "personas") {
    cat(sprintf("   - Objetivo: %.0f personas\n", objetivo_cobertura))
  } else {
    cat(sprintf("   - Objetivo: %.2f%%\n", objetivo_cobertura))
  }
  cat(sprintf("   - Alcanzada: %.2f%%\n", mejor_cobertura))

  # Calcular la diferencia en personas o porcentaje según el tipo de tolerancia
  if (tolerancia_cobertura_tipo == "personas") {
    diferencia_cobertura <- round(objetivo_cobertura - (mejor_cobertura / 100 * poblacion_total), 0)

    # Usar la diferencia y la tolerancia para evaluar el cumplimiento
    if (objetivo_cobertura - tolerancia_cobertura <= round(mejor_cobertura / 100 * poblacion_total, 0) &&
        round(mejor_cobertura / 100 * poblacion_total, 0) <= objetivo_cobertura + tolerancia_cobertura) {
      cat("   ✓ Se ha alcanzado el objetivo de cobertura\n")
    } else {
      cat(
        sprintf(
          "   ⚠ No se ha alcanzado el objetivo. Diferencia: %.0f personas\n",
          diferencia_cobertura
        )
      )
    }
  }
  else
  {
    diferencia_cobertura <- round(objetivo_cobertura - mejor_cobertura, 8) # Diferencia en porcentaje

    # Usar la diferencia y la tolerancia para evaluar el cumplimiento
    if (abs(diferencia_cobertura) <= tolerancia_cobertura * if (tolerancia_cobertura_tipo == "porcentaje") objetivo_cobertura else 1) {
      cat("   ✓ Se ha alcanzado el objetivo de cobertura\n")
    } else {
      cat(
        sprintf(
          "   ⚠ No se ha alcanzado el objetivo. Diferencia: %.8f%%\n",
          diferencia_cobertura
        )
      )
    }
  }

  # Evaluación de la FEM
  contactos_FEM <- distribucion_final$acumulada$porcentaje[FEM]
  cat(sprintf("\n3. FRECUENCIA EFECTIVA MÍNIMA (FEM):\n"))
  cat(sprintf("   - FEM requerida: %d contactos\n", FEM))
  cat(sprintf(
    "   - Población con %d+ contactos: %.2f%%\n",
    FEM,
    contactos_FEM
  ))

  # Resumen general del plan
  # Resumen general del plan
  cat("\nRESUMEN GENERAL:\n")
  cat("---------------\n")

  # Calcular la tolerancia de presupuesto en valor absoluto
  if (tolerancia_presupuesto_tipo == "porcentaje") {
    tolerancia_presupuesto_abs <- presupuesto_max * tolerancia_presupuesto
  } else {
    tolerancia_presupuesto_abs <- tolerancia_presupuesto
  }

  # Evaluar si se ha alcanzado el objetivo de cobertura según el tipo de tolerancia
  if (tolerancia_cobertura_tipo == "personas") {
    cobertura_cumplida <- (objetivo_cobertura - tolerancia_cobertura <= round(mejor_cobertura / 100 * poblacion_total, 0) &&
                             round(mejor_cobertura / 100 * poblacion_total, 0) <= objetivo_cobertura + tolerancia_cobertura)
  } else {
    cobertura_cumplida <- (abs(diferencia_cobertura) <= tolerancia_cobertura * if (tolerancia_cobertura_tipo == "porcentaje") objetivo_cobertura else 1)
  }

  # Evaluar el cumplimiento de los objetivos
  if (mejor_coste <= presupuesto_max + tolerancia_presupuesto_abs && cobertura_cumplida) {
    cat("✓ PLAN ÓPTIMO: Se han cumplido todos los objetivos\n")
  } else {
    if (mejor_coste <= presupuesto_max + tolerancia_presupuesto_abs) {
      cat(
        "⚠ PLAN SUBÓPTIMO: Se ajusta al presupuesto pero no alcanza la cobertura deseada\n"
      )
      cat("   Recomendaciones:\n")
      cat("   - Considerar aumentar el presupuesto\n")
      cat("   - Revisar soportes vetados\n")
      cat("   - Evaluar otros soportes alternativos\n")
    } else if (cobertura_cumplida) {
      cat(
        "⚠ PLAN SUBÓPTIMO: Alcanza la cobertura pero excede el presupuesto\n"
      )
      cat("   Recomendaciones:\n")
      cat("   - Aumentar el presupuesto disponible\n")
      cat("   - Buscar soportes más eficientes\n")
    } else {
      cat("⚠ PLAN NO VIABLE: No cumple ni presupuesto ni cobertura\n")
      cat("   Recomendaciones:\n")
      cat("   - Revisar objetivos de cobertura\n")
      cat("   - Aumentar presupuesto\n")
      cat("   - Considerar otros soportes\n")
    }
  }

  # Modificar el return para incluir la nueva información

  return(
    list(
      exito = TRUE,
      cobertura_alcanzada = mejor_cobertura,
      coste_total = mejor_coste,
      soportes_seleccionados = tabla_plan,
      plan_completo = mejor_combinacion,
      objetivo_alcanzado = abs(diferencia_cobertura) < tolerancia_cobertura,
      presupuesto_cumplido = abs(mejor_coste - presupuesto_max) < tolerancia_presupuesto_abs,
      evaluacion = list(
        presupuesto = list(
          disponible = presupuesto_max,
          utilizado = mejor_coste,
          porcentaje_uso = presupuesto_usado_pct,
          cumplido = abs(mejor_coste - presupuesto_max) < tolerancia_presupuesto_abs,
          tolerancia_tipo = tolerancia_presupuesto_tipo,
          tolerancia_valor = tolerancia_presupuesto
        ),
        cobertura = list(
          objetivo = objetivo_cobertura,
          alcanzada = mejor_cobertura,
          diferencia = diferencia_cobertura,
          cumplido = abs(diferencia_cobertura) < tolerancia_cobertura,
          tolerancia_tipo = tolerancia_cobertura_tipo,
          tolerancia_valor = tolerancia_cobertura
        ),
        FEM = list(
          requerida = FEM,
          poblacion_alcanzada = contactos_FEM
        )
      ),
      distribucion = distribucion_final,
      soportes_vetados = soportes_vetados,
      soportes_no_encontrados = soportes_no_encontrados,
      tipo_audiencia = ifelse(usar_audiencia_util, "util", "bruta"),
      modelo_usado = modelo
    )
  )
}
