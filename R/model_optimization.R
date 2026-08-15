

#' @encoding UTF-8
#' @title Optimización de distribución de contactos mediante modelo Beta-Binomial
#' @description Esta función optimiza la distribución de contactos publicitarios y calcula
#' los coeficientes de duplicación (R1 y R2) utilizando la distribución Beta-Binomial.
#' El proceso busca la mejor combinación de parámetros alpha y beta y número de inserciones que satisfaga
#' los criterios de cobertura efectiva y frecuencia efectiva (FE) especificados por el usuario.
#'
#' @param Pob Tamaño de la población
#' @param FE Frecuencia efectiva (FE, número objetivo de impactos por persona)
#' @param cob_efectiva Número objetivo de personas a alcanzar con FE contactos
#' @param A1 Audiencia tras la primera inserción
#' @param max_inserciones Número de inserciones máximo a considerar (default: 5)
#' @param tolerancia Margen de error permitido en las soluciones (default: 0.05)
#' @param step_A Incremento para búsqueda del parámetro alpha (default: 0.1)
#' @param step_B Incremento para búsqueda del parámetro beta (default: 0.1)
#' @param batch_size Tamaño del lote para procesamiento (default: 1000000)
#' @param min_soluciones Número mínimo de soluciones para parar (default: 10)
#' @param error_aceptable Error aceptable como proporción (default: 0.01)
#'
#' @return Una lista con los siguientes componentes:
#' \itemize{
#'   \item mejores_combinaciones: Data frame con todas las combinaciones válidas de
#'         parámetros, incluyendo:
#'         \itemize{
#'           \item n: Número de inserciones
#'           \item x: Frecuencia efectiva
#'           \item alpha: Parámetro alpha del modelo
#'           \item beta: Parámetro beta del modelo
#'           \item R1: Proporción de personas alcanzadas tras la primera inserción
#'           \item R2: Proporción de personas alcanzadas tras la segunda inserción
#'           \item prob: Probabilidad asociada
#'           \item distancia_objetivo: Diferencia absoluta con respecto al objetivo
#'         }
#'   \item mejores_combinaciones_top_10: Las 10 mejores combinaciones según criterios
#'   \item data: Data frame con la distribución de contactos final
#'   \item alpha: Valor óptimo seleccionado para alpha
#'   \item beta: Valor óptimo seleccionado para beta
#'   \item n_optimo: Número óptimo de inserciones
#' }
#'
#' @details
#' La función realiza los siguientes pasos:
#' \enumerate{
#'   \item Validación de parámetros de entrada y dependencias
#'   \item Cálculo de valores objetivo normalizados y tolerancias
#'   \item Generación de combinaciones de parámetros (alpha, beta, n)
#'   \item Cálculo de distribuciones Beta-Binomiales por lotes
#'   \item Filtrado de resultados según criterios especificados
#'   \item Cálculo de coeficientes R1 y R2 para soluciones válidas
#'   \item Selección de mejor solución y generación de distribución final
#' }
#'
#' @note
#' Los parámetros alpha y beta controlan la forma de la distribución Beta-Binomial:
#' \itemize{
#'   \item alpha: Controla la asimetría hacia valores altos de probabilidad
#'   \item beta: Controla la asimetría hacia valores bajos de probabilidad
#'   \item La combinación de ambos determina la dispersión y forma final
#' }
#'
#' La función utiliza procesamiento por lotes para optimizar el uso de memoria
#' y proporciona información de progreso durante la ejecución.
#'
#' @import ggplot2
#' @import extraDistr
#' @importFrom stats optimize
#'
#' @examples
#' \donttest{
#' # Ejemplo 1: Caso básico con población pequeña
#' resultado1 <- optimizar_d(
#'   Pob = 100000,          # Población de 100 mil
#'   FE = 3,                # Frecuencia efectiva de 3
#'   cob_efectiva = 59000,  # Objetivo: 59% de cobertura
#'   A1 = 50000,            # Primera audiencia: 50%
#'   max_inserciones = 5    # Máximo 5 inserciones
#' )
#'
#' # Examinar resultados
#' print(head(resultado1$mejores_combinaciones))
#' print(resultado1$data)
#'
#' # Ejemplo 2: Caso con mayor precisión
#' resultado2 <- optimizar_d(
#'   Pob = 1000000,
#'   FE = 4,
#'   cob_efectiva = 600000,
#'   A1 = 450000,
#'   max_inserciones = 8,
#'   tolerancia = 0.03,     # Menor tolerancia
#'   step_A = 0.01,         # Pasos más pequeños
#'   step_B = 0.01,
#'   min_soluciones = 20    # Más soluciones requeridas
#' )
#' }
#'
#' @seealso
#' \code{\link{calc_R1_R2}} para detalles sobre el cálculo de coeficientes de duplicación
#'
#' @references
#' Leckenby, J. D., & Boyd, M. M. (1984). An improved beta binomial reach/frequency model for magazines.
#' Current Issues and Research in Advertising, 7(1), 1-24.
#'
#' @importFrom utils head
#' @export

optimizar_d <- function(Pob,
                        FE,
                        cob_efectiva,
                        A1,
                        max_inserciones,
                        tolerancia = 0.05,
                        step_A = 0.1,
                        step_B = 0.1,
                        batch_size = 1000000,
                        min_soluciones = 10,     # Número mínimo de soluciones para parar
                        error_aceptable = 0.01) { # Error aceptable como proporción (0.01 = 1%)

  #___________________________________#

  # [Validaciones iniciales...]
  if (!is.numeric(Pob) || !is.numeric(FE) || !is.numeric(cob_efectiva) || !is.numeric(max_inserciones)) {
    stop("Todos los parámetros deben ser numéricos.")
  }
  if (Pob <= 0 || cob_efectiva <= 0) {
    stop("Pob y cob_efectiva deben ser positivos.")
  }
  if (FE <= 0) {
    stop("'FE' no puede ser igual o menor que 0.")
  }
  if (FE > max_inserciones) {
    stop("'FE' no puede ser superior a 'max_inserciones'.")
  }
  if (cob_efectiva > Pob) {
    stop("El valor objetivo no puede ser mayor que la población.")
  }
  if (error_aceptable <= 0 || error_aceptable >= 1) {
    stop("error_aceptable debe estar entre 0 y 1")
  }
  if (min_soluciones <= 0) {
    stop("min_soluciones debe ser mayor que 0")
  }

  # Cálculo de la tolerancia y R1_objetivo
  cob_efectiva_norm <- cob_efectiva / Pob
  tolerancia <- cob_efectiva_norm * tolerancia
  R1_objetivo <- A1 / Pob

  # Definición de rangos
  rangos_n <- seq(FE, max_inserciones, 1)
  rangos_prob1 <- seq(0.01, 100, step_A)
  rangos_prob2 <- seq(0.01, 100, step_B)

  # Información inicial
  total_combinaciones <- length(rangos_n) * length(rangos_prob1) * length(rangos_prob2)

  if (!isTRUE(getOption('knitr.in.progress'))) {
    cat(sprintf("\nTotal de combinaciones a probar: %d", total_combinaciones))
    cat(sprintf("\n- Valores n: %d (de %d a %d)", length(rangos_n), min(rangos_n), max(rangos_n)))
    cat(sprintf("\n- Valores alpha: %d (de %.2f a %.2f, step %.3f)", length(rangos_prob1), min(rangos_prob1), max(rangos_prob1), step_A))
    cat(sprintf("\n- Valores beta: %d (de %.2f a %.2f, step %.3f)", length(rangos_prob2), min(rangos_prob2), max(rangos_prob2), step_B))
    cat(sprintf("\n- Criterios de parada: %d soluciones con error < %.1f%%\n\n", min_soluciones, error_aceptable * 100))
  }

  # Variables para progreso
  contador <- 0
  ultima_actualizacion <- Sys.time()
  mejores_combinaciones <- data.frame()
  mejor_prob <- Inf
  encontrada_solucion <- FALSE

  # Procesar por lotes para cada n
  for (n in rangos_n) {
    for (i in seq(1, length(rangos_prob1), by=batch_size)) {
      alphas_batch <- rangos_prob1[i:min(i+batch_size-1, length(rangos_prob1))]

      for (j in seq(1, length(rangos_prob2), by=batch_size)) {
        if (encontrada_solucion) break

        betas_batch <- rangos_prob2[j:min(j+batch_size-1, length(rangos_prob2))]

        # Crear lote
        batch <- expand.grid(
          n = n,
          x = FE,
          alpha = alphas_batch,
          beta = betas_batch
        )

        # Procesar lote: llamada unica y vectorizada a dbbinom en vez de una
        # llamada por combinacion (evaluar miles de combinaciones con un
        # mapply que reevalua dbbinom() una a una es innecesariamente lento).
        probs <- extraDistr::dbbinom(x = batch$x, size = batch$n, alpha = batch$alpha, beta = batch$beta)

        contador <- contador + nrow(batch)
        if (difftime(Sys.time(), ultima_actualizacion, units = "secs") > 0.1 &&
            !isTRUE(getOption('knitr.in.progress'))) {
          mejor_prob <- min(mejor_prob, min(abs(cob_efectiva_norm - probs)))

          cat(sprintf("\rProgreso: %.2f%% | n=%d, lote de %d combinaciones, mejor_diff=%.6f",
                      contador/total_combinaciones*100,
                      n, nrow(batch), mejor_prob))

          ultima_actualizacion <- Sys.time()
        }

        # Filtrar resultados del lote
        indices <- which(abs(cob_efectiva_norm - probs) <= tolerancia)
        if (length(indices) > 0) {
          batch_seleccionado <- batch[indices, ]
          resultados <- mapply(function(alpha, beta) {
            tryCatch({
              res <- calc_R1_R2(alpha, beta)
              c(R1 = res$R1, R2 = res$R2)
            }, error = function(e) {
              c(R1 = NA, R2 = NA)
            })
          }, batch_seleccionado$alpha, batch_seleccionado$beta, SIMPLIFY = TRUE)

          resultados_df <- as.data.frame(t(resultados))
          names(resultados_df) <- c("R1", "R2")

          batch_seleccionado <- cbind(batch_seleccionado, resultados_df)
          batch_seleccionado$prob <- probs[indices] * Pob
          batch_seleccionado$distancia_objetivo <- abs(cob_efectiva - batch_seleccionado$prob)

          mejores_combinaciones <- rbind(mejores_combinaciones, batch_seleccionado)

          # Verificar criterio de parada temprana
          if (nrow(mejores_combinaciones) > 0) {
            temp_filtradas <- mejores_combinaciones[
              abs(mejores_combinaciones$R1 - R1_objetivo) <= tolerancia,
            ]

            if (nrow(temp_filtradas) >= min_soluciones &&
                min(abs(temp_filtradas$prob - cob_efectiva)) < cob_efectiva * error_aceptable) {
              cat(sprintf("\n\nEncontradas %d soluciones con error menor al %.1f%%. Parando búsqueda.",
                          nrow(temp_filtradas), error_aceptable * 100))
              encontrada_solucion <- TRUE
              break
            }
          }
        }

        # Liberar memoria
        rm(batch, probs)
        gc()
      }
      if (encontrada_solucion) break
    }
    if (encontrada_solucion) break
  }

  cat("\n\nFiltrando resultados finales...\n")

  if (nrow(mejores_combinaciones) == 0) {
    cat(">>> No se han encontrado soluciones que cumplan los criterios.\n")
    return(NULL)
  }

  mejores_combinaciones <- mejores_combinaciones[order(mejores_combinaciones$distancia_objetivo),]
  mejores_combinaciones <- mejores_combinaciones[!is.na(mejores_combinaciones$R1),]

  R1_filtradas <- which(abs(mejores_combinaciones$R1 - R1_objetivo) <= tolerancia)
  if (length(R1_filtradas) == 0) {
    cat(">>> No se encontraron soluciones que cumplan el criterio de R1.\n")
    return(NULL)
  }

  mejores_combinaciones <- mejores_combinaciones[R1_filtradas,]

  # Procesar mejor solución
  principal <- mejores_combinaciones[1,]
  alpha <- principal$alpha
  beta <- principal$beta
  n_optimo <- principal$n

  if (!isTRUE(getOption('knitr.in.progress'))) {
    cat("\nMejor solución encontrada:\n")
    cat(sprintf("n = %d, α = %.3f, β = %.3f\n", n_optimo, alpha, beta))
    cat(sprintf("R1 = %.4f (objetivo: %.4f)\n", principal$R1, R1_objetivo))
    cat(sprintf("Cobertura = %.0f (objetivo: %.0f)\n", principal$prob, cob_efectiva))
  }

  # Calcular distribución final
  distribucion <- extraDistr::dbbinom(0:n_optimo, size = n_optimo, alpha = alpha, beta = beta)
  acumuladas <- sapply(1:n_optimo, function(k) sum(distribucion[(k + 1):(n_optimo + 1)]))

  data <- data.frame(
    inserciones = 1:n_optimo,
    d_probabilidad = distribucion[2:(n_optimo + 1)],
    dc_acumulada = acumuladas
  )

  data_ls <- list(
    mejores_combinaciones = mejores_combinaciones,
    mejores_combinaciones_top_10 = head(mejores_combinaciones, 10),
    data = head(data, n_optimo),
    alpha = alpha,
    beta = beta,
    n_optimo = n_optimo
  )

  imprimir_resultados(data_ls)

  return(invisible(data_ls))
}

#______________________________________________________

#' @encoding UTF-8
#' @title Optimización de distribución de contactos acumulada mediante modelo Beta-Binomial
#' @description Esta función optimiza la distribución de contactos publicitarios y calcula
#' los coeficientes de duplicación (R1 y R2) utilizando la distribución Beta-Binomial.
#' El proceso busca la mejor combinación de parámetros alpha y beta y número de inserciones que satisfaga
#' los criterios de cobertura efectiva y frecuencia efectiva mínima (FEM) especificados por el usuario.
#' La función calcula la cobertura acumulada para individuos que han visto el anuncio FEM o más veces.
#'
#' @param Pob Tamaño de la población
#' @param FEM Frecuencia efectiva mínima (FEM, número mínimo de impactos por persona)
#' @param cob_efectiva Número objetivo de personas a alcanzar con FEM o más contactos
#' @param A1 Audiencia tras la primera inserción
#' @param max_inserciones Número de inserciones máximo a considerar
#' @param tolerancia Margen de error permitido en las soluciones (default: 0.05)
#' @param step_A Incremento para búsqueda del parámetro alpha (default: 0.1)
#' @param step_B Incremento para búsqueda del parámetro beta (default: 0.1)
#' @param batch_size Tamaño del lote para procesamiento (default: 1000000)
#' @param min_soluciones Número mínimo de soluciones para parar (default: 10)
#' @param error_aceptable Error aceptable como proporción (default: 0.01)
#'
#' @return Una lista con los siguientes componentes:
#' \itemize{
#'   \item mejores_combinaciones: Data frame con todas las combinaciones válidas de
#'         parámetros, incluyendo:
#'         \itemize{
#'           \item n: Número de inserciones
#'           \item x: Frecuencia efectiva mínima
#'           \item alpha: Parámetro alpha del modelo
#'           \item beta: Parámetro beta del modelo
#'           \item R1: Proporción de personas alcanzadas tras la primera inserción
#'           \item R2: Proporción de personas alcanzadas tras la segunda inserción
#'           \item prob: Probabilidad acumulada (FEM o más contactos)
#'           \item distancia_objetivo: Diferencia absoluta con respecto al objetivo
#'         }
#'   \item mejores_combinaciones_top_10: Las 10 mejores combinaciones según criterios
#'   \item data: Data frame con la distribución de contactos final, incluyendo:
#'         \itemize{
#'           \item inserciones: Número de inserciones
#'           \item d_probabilidad: Distribución de probabilidad
#'           \item dc_acumulada: Distribución acumulada
#'         }
#'   \item alpha: Valor óptimo seleccionado para alpha
#'   \item beta: Valor óptimo seleccionado para beta
#'   \item n_optimo: Número óptimo de inserciones
#' }
#'
#' @details
#' La función realiza los siguientes pasos:
#' \enumerate{
#'   \item Validación de parámetros de entrada y dependencias
#'   \item Cálculo de valores objetivo normalizados y tolerancias
#'   \item Generación de combinaciones de parámetros (alpha, beta, n)
#'   \item Cálculo de distribuciones Beta-Binomiales por lotes
#'   \item Suma acumulada de probabilidades para k mayor o igual que FEM
#'   \item Filtrado de resultados según criterios especificados
#'   \item Cálculo de coeficientes R1 y R2 para soluciones válidas
#'   \item Selección de mejor solución y generación de distribución final
#' }
#'
#' @note
#' Esta función difiere de optimizar_d en que:
#' \itemize{
#'   \item Utiliza FEM en lugar de FE
#'   \item Calcula coberturas acumuladas (personas que ven el anuncio FEM o más veces)
#'   \item La optimización considera la suma de probabilidades para k mayor o igual que FEM
#' }
#'
#' @import ggplot2
#' @import extraDistr
#' @importFrom stats optimize
#'
#' @examples
#' \donttest{
#' # Ejemplo 1: Optimización para cobertura acumulada
#' resultado1 <- optimizar_dc(
#'   Pob = 1000000,         # Población de 1 millón
#'   FEM = 3,               # Frecuencia efectiva mínima de 3
#'   cob_efectiva = 600000, # Objetivo: 600,000 personas con 3+ contactos
#'   A1 = 450000,           # Primera audiencia: 450,000
#'   max_inserciones = 8    # Máximo 8 inserciones
#' )
#'
#' # Ejemplo 2: Caso con mayor precisión
#' resultado2 <- optimizar_dc(
#'   Pob = 500000,
#'   FEM = 4,
#'   cob_efectiva = 250000,
#'   A1 = 200000,
#'   max_inserciones = 10,
#'   tolerancia = 0.03,     # Menor tolerancia
#'   step_A = 0.05,         # Pasos más pequeños
#'   step_B = 0.05,
#'   min_soluciones = 15    # Más soluciones requeridas
#' )
#'
#' # Comparar probabilidades acumuladas vs objetivo
#' print(data.frame(
#'   Objetivo = resultado2$cob_efectiva,
#'   Logrado = resultado2$mejores_combinaciones[1, "prob"],
#'   Error = abs(resultado2$mejores_combinaciones[1, "distancia_objetivo"])
#' ))
#' }
#'
#' @seealso
#' \code{\link{optimizar_d}} para optimización con frecuencia efectiva exacta
#' \code{\link{calc_R1_R2}} para detalles sobre el cálculo de coeficientes de duplicación
#'
#' @references
#' Leckenby, J. D., & Boyd, M. M. (1984). An improved beta binomial reach/frequency model for magazines.
#' Current Issues and Research in Advertising, 7(1), 1-24.
#'
#' @export

optimizar_dc <- function(Pob,
                         FEM,
                         cob_efectiva,
                         A1,
                         max_inserciones,
                         tolerancia = 0.05,
                         step_A = 0.1,
                         step_B = 0.1,
                         batch_size = 1000000,
                         min_soluciones = 10,     # Número mínimo de soluciones para parar
                         error_aceptable = 0.01) { # Error aceptable como proporción (0.01 = 1%)

  #___________________________________#

  # [Validaciones iniciales...]
  if (!is.numeric(Pob) || !is.numeric(FEM) || !is.numeric(cob_efectiva) || !is.numeric(max_inserciones)) {
    stop("Todos los parámetros deben ser numéricos.")
  }
  if (Pob <= 0 || cob_efectiva <= 0) {
    stop("Pob y cob_efectiva deben ser positivos.")
  }
  if (FEM <= 0) {
    stop("'FEM' no puede ser igual o menor que 0.")
  }
  if (FEM > max_inserciones) {
    stop("'FEM' no puede ser superior a 'max_inserciones'.")
  }
  if (cob_efectiva > Pob) {
    stop("El valor objetivo no puede ser mayor que la población.")
  }
  if (error_aceptable <= 0 || error_aceptable >= 1) {
    stop("error_aceptable debe estar entre 0 y 1")
  }
  if (min_soluciones <= 0) {
    stop("min_soluciones debe ser mayor que 0")
  }

  # Cálculo de la tolerancia y R1_objetivo
  cob_efectiva_norm <- cob_efectiva / Pob
  tolerancia <- cob_efectiva_norm * tolerancia
  R1_objetivo <- A1 / Pob

  # Definición de rangos
  rangos_n <- seq(FEM, max_inserciones, 1)
  rangos_prob1 <- seq(0.01, 100, step_A)
  rangos_prob2 <- seq(0.01, 100, step_B)

  # Información inicial
  total_combinaciones <- length(rangos_n) * length(rangos_prob1) * length(rangos_prob2)

  if (!isTRUE(getOption('knitr.in.progress'))) {
    cat(sprintf("\nTotal de combinaciones a probar: %d", total_combinaciones))
    cat(sprintf("\n- Valores n: %d (de %d a %d)", length(rangos_n), min(rangos_n), max(rangos_n)))
    cat(sprintf("\n- Valores alpha: %d (de %.2f a %.2f, step %.3f)", length(rangos_prob1), min(rangos_prob1), max(rangos_prob1), step_A))
    cat(sprintf("\n- Valores beta: %d (de %.2f a %.2f, step %.3f)", length(rangos_prob2), min(rangos_prob2), max(rangos_prob2), step_B))
    cat(sprintf("\n- Criterios de parada: %d soluciones con error < %.1f%%\n\n", min_soluciones, error_aceptable * 100))
  }

  # Variables para progreso
  contador <- 0
  ultima_actualizacion <- Sys.time()
  mejores_combinaciones <- data.frame()
  mejor_prob <- Inf
  encontrada_solucion <- FALSE

  # Procesar por lotes para cada n
  for (n in rangos_n) {
    for (i in seq(1, length(rangos_prob1), by=batch_size)) {
      alphas_batch <- rangos_prob1[i:min(i+batch_size-1, length(rangos_prob1))]

      for (j in seq(1, length(rangos_prob2), by=batch_size)) {
        if (encontrada_solucion) break

        betas_batch <- rangos_prob2[j:min(j+batch_size-1, length(rangos_prob2))]

        # Crear lote
        batch <- expand.grid(
          n = n,
          x = FEM,
          alpha = alphas_batch,
          beta = betas_batch
        )

        # Procesar lote: para cada combinacion (alpha, beta) del lote se
        # necesita la probabilidad acumulada P(X >= FEM); se calcula con una
        # unica llamada vectorizada a dbbinom en vez de una llamada (y una
        # suma) por cada combinacion.
        x_vals <- FEM:n
        n_rows <- nrow(batch)
        probs_all <- extraDistr::dbbinom(
          x = rep(x_vals, times = n_rows),
          size = n,
          alpha = rep(batch$alpha, each = length(x_vals)),
          beta = rep(batch$beta, each = length(x_vals))
        )
        probs <- colSums(matrix(probs_all, nrow = length(x_vals), ncol = n_rows))

        contador <- contador + n_rows
        if (difftime(Sys.time(), ultima_actualizacion, units = "secs") > 0.1 &&
            !isTRUE(getOption('knitr.in.progress'))) {
          mejor_prob <- min(mejor_prob, min(abs(cob_efectiva_norm - probs)))

          cat(sprintf("\rProgreso: %.2f%% | n=%d, lote de %d combinaciones, mejor_diff=%.6f",
                      contador/total_combinaciones*100,
                      n, n_rows, mejor_prob))

          ultima_actualizacion <- Sys.time()
        }

        # Filtrar resultados del lote
        indices <- which(abs(cob_efectiva_norm - probs) <= tolerancia)
        if (length(indices) > 0) {
          batch_seleccionado <- batch[indices, ]
          resultados <- mapply(function(alpha, beta) {
            tryCatch({
              res <- calc_R1_R2(alpha, beta)
              c(R1 = res$R1, R2 = res$R2)
            }, error = function(e) {
              c(R1 = NA, R2 = NA)
            })
          }, batch_seleccionado$alpha, batch_seleccionado$beta, SIMPLIFY = TRUE)

          resultados_df <- as.data.frame(t(resultados))
          names(resultados_df) <- c("R1", "R2")

          batch_seleccionado <- cbind(batch_seleccionado, resultados_df)
          batch_seleccionado$prob <- probs[indices] * Pob
          batch_seleccionado$distancia_objetivo <- abs(cob_efectiva - batch_seleccionado$prob)

          mejores_combinaciones <- rbind(mejores_combinaciones, batch_seleccionado)

          # Verificar criterio de parada temprana
          if (nrow(mejores_combinaciones) > 0) {
            temp_filtradas <- mejores_combinaciones[
              abs(mejores_combinaciones$R1 - R1_objetivo) <= tolerancia,
            ]

            if (nrow(temp_filtradas) >= min_soluciones &&
                min(abs(temp_filtradas$prob - cob_efectiva)) < cob_efectiva * error_aceptable) {
              cat(sprintf("\n\nEncontradas %d soluciones con error menor al %.1f%%. Parando búsqueda.",
                          nrow(temp_filtradas), error_aceptable * 100))
              encontrada_solucion <- TRUE
              break
            }
          }
        }

        # Liberar memoria
        rm(batch, probs)
        gc()
      }
      if (encontrada_solucion) break
    }
    if (encontrada_solucion) break
  }

  cat("\n\nFiltrando resultados finales...\n")

  if (nrow(mejores_combinaciones) == 0) {
    cat(">>> No se han encontrado soluciones que cumplan los criterios.\n")
    return(NULL)
  }

  mejores_combinaciones <- mejores_combinaciones[order(mejores_combinaciones$distancia_objetivo),]
  mejores_combinaciones <- mejores_combinaciones[!is.na(mejores_combinaciones$R1),]

  R1_filtradas <- which(abs(mejores_combinaciones$R1 - R1_objetivo) <= tolerancia)
  if (length(R1_filtradas) == 0) {
    cat(">>> No se encontraron soluciones que cumplan el criterio de R1.\n")
    return(NULL)
  }

  mejores_combinaciones <- mejores_combinaciones[R1_filtradas,]

  # Procesar mejor solución
  # Procesar mejor solución
  principal <- mejores_combinaciones[1,]
  alpha <- principal$alpha
  beta <- principal$beta
  n_optimo <- principal$n

  if (!isTRUE(getOption('knitr.in.progress'))) {
    cat("\nMejor solución encontrada:\n")
    cat(sprintf("n = %d, α = %.3f, β = %.3f\n", n_optimo, alpha, beta))
    cat(sprintf("R1 = %.4f (objetivo: %.4f)\n", principal$R1, R1_objetivo))
    cat(sprintf("Cobertura = %.0f (objetivo: %.0f)\n", principal$prob, cob_efectiva))
  }

  # Calcular distribución final
  distribucion <- extraDistr::dbbinom(0:n_optimo, size = n_optimo, alpha = alpha, beta = beta)
  acumuladas <- sapply(1:n_optimo, function(k) sum(distribucion[(k + 1):(n_optimo + 1)]))

  data <- data.frame(
    inserciones = 1:n_optimo,
    d_probabilidad = distribucion[2:(n_optimo + 1)],
    dc_acumulada = acumuladas
  )

  data_ls <- list(
    mejores_combinaciones = mejores_combinaciones,
    mejores_combinaciones_top_10 = head(mejores_combinaciones, 10),
    data = head(data, n_optimo),
    alpha = alpha,
    beta = beta,
    n_optimo = n_optimo
  )

  imprimir_resultados(data_ls)

  return(invisible(data_ls))
}



