#' @encoding UTF-8
#' @title Cálculo del Modelo Morgensztem Beta Binomial Distribution (MBBD)
#' @description Implementa el modelo MBBD para calcular la distribución de contactos
#' de un plan de medios. Combina la estimación de cobertura de Morgensztem con la
#' distribución beta binomial para ajustar la distribución de contactos.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param insertions Vector numérico. Número de inserciones para cada soporte (ni)
#' @param audiences Vector numérico. Audiencia de cada soporte en personas (Ai)
#' @param RM Entero. Estimación de cobertura según Morgensztem en personas
#' @param universe Entero. Tamaño del universo objetivo en personas
#' @param A0 Numérico. Valor inicial del parámetro A (entre 0 y 10)
#' @param precision Numérico. Criterio de convergencia en personas. Por defecto 100
#' @param max_iter Entero. Número máximo de iteraciones permitidas. Por defecto 100
#' @param adj_factor Numérico. Factor de ajuste para el parámetro A. Por defecto 0.01
#'
#' @details
#' El modelo MBBD ajusta iterativamente los parámetros de una distribución beta binomial
#' hasta que su cobertura coincide con la estimada por el método de Morgensztem:
#' \enumerate{
#'   \item Calcula B0 inicial según la fórmula:
#'     \itemize{
#'       \item B0 = A0 * (SUMATORIO ni - SUMATORIO niAi) / (SUMATORIO niAi)
#'     }
#'   \item Ajusta iterativamente los parámetros hasta que las coberturas convergen:
#'     \itemize{
#'       \item Si RM mayor que BBD: aumenta A
#'       \item Si RM menor que BBD: disminuye A
#'       \item Recalcula B en cada iteración
#'     }
#' }
#'
#' El modelo asume:
#' \itemize{
#'   \item Los parámetros A y B deben estar entre 0 y 10
#'   \item La cobertura BBD se calcula como 1 - P(K=0)
#'   \item La convergencia se alcanza cuando |BBD - RM| menor o igual que precision
#' }
#'
#' @return Una lista de clase "MBBD" conteniendo:
#' \itemize{
#'   \item parameters: Lista con parámetros finales:
#'     \itemize{
#'       \item AF: Parámetro A final
#'       \item BF: Parámetro B final
#'       \item N: Total de inserciones
#'       \item universe: Tamaño del universo
#'       \item iterations: Número de iteraciones realizadas
#'       \item converged: Indicador de convergencia
#'     }
#'   \item coverage: Lista con coberturas:
#'     \itemize{
#'       \item RM: Cobertura de Morgensztem
#'       \item BBD: Cobertura Beta Binomial
#'     }
#'   \item contact_distribution: Vector con probabilidades de 0 a N contactos
#'   \item iteration_history: Data frame con historial de iteraciones
#' }
#'
#' @examples
#' # Ejemplo básico
#' insertions <- c(5, 7, 4)
#' audiences <- c(500000, 550000, 600000)
#' RM <- 550000
#' universe <- 1000000
#' resultado <- calc_MBBD(insertions, audiences, RM, universe, A0 = 0.1)
#'
#' # Examinar resultados
#' print(resultado)
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_sainsbury}} para estimaciones el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_metheringham}} para estimaciones con el modelo de Metheringham

#' @importFrom extraDistr dbbinom
#' @importFrom stats complete.cases
calc_MBBD <- function(insertions, audiences, RM, universe, A0,
                      precision = 100,
                      max_iter = 100,
                      adj_factor = 0.01) {

  # Input validation
  m <- length(insertions)
  if(!is.numeric(m) || m <= 0 || m != round(m)) {
    stop("m must be a positive integer")
  }
  if(length(insertions) != m || !all(insertions > 0)) {
    stop("insertions must be a vector of positive numbers with length m")
  }
  if(length(audiences) != m || !all(audiences > 0)) {
    stop("audiences must be a vector of positive integers")
  }
  if(!is.numeric(RM) || RM < 0 || RM > universe) {
    stop("RM must be a positive number less than or equal to universe size")
  }
  if(!is.numeric(universe) || universe <= 0) {
    stop("universe must be a positive number")
  }
  if(any(audiences > universe)) {
    stop("audiences cannot be larger than universe size")
  }
  if(A0 <= 0 || A0 > 2000) {
    stop("A0 must be between 0 and 2000")
  }

  # Convert to proportions for internal calculations
  audience_props <- audiences / universe

  # Initial calculations
  sum_ni <- sum(insertions)
  sum_ni_Ai <- sum(insertions * audience_props)

  # Calculate initial B0
  initial_B0 <- A0 * (sum_ni - sum_ni_Ai) / sum_ni_Ai

  # Initialize variables
  iter <- 0
  current_A <- A0
  current_B <- initial_B0
  difference <- Inf

  # Store history
  history <- data.frame(
    iteration = numeric(),
    A = numeric(),
    B = numeric(),
    coverage_bbd = numeric(),
    difference = numeric()
  )

  # Main iteration loop
  while(abs(difference) > precision && iter < max_iter) {
    # Calculate BBD coverage
    p_zero <- extraDistr::dbbinom(0, size = sum_ni, alpha = current_A, beta = current_B)
    coverage_bbd <- (1 - p_zero) * universe

    # Update difference
    difference <- coverage_bbd - RM

    # Store iteration
    history <- rbind(history, data.frame(
      iteration = iter,
      A = current_A,
      B = current_B,
      coverage_bbd = coverage_bbd,
      difference = difference
    ))

    # Update parameters
    if(difference > 0) {  # Si BBD > RM, DISMINUIR A
      current_A <- current_A * (1 - adj_factor)
    } else {  # Si BBD < RM, AUMENTAR A
      current_A <- current_A * (1 + adj_factor)
    }

    # Recalculate B
    current_B <- current_A * (sum_ni - sum_ni_Ai) / sum_ni_Ai

    iter <- iter + 1
  }

  # Final parameters
  AF <- current_A
  BF <- current_B

  # Calculate final distribution
  N <- sum_ni
  contact_distribution <- numeric(N + 1)
  for(k in 0:N) {
    contact_distribution[k + 1] <- extraDistr::dbbinom(k, size = N, alpha = AF, beta = BF)
  }

  # Return results
  result <- list(
    parameters = list(
      AF = AF,
      BF = BF,
      A0 = A0,
      initial_B0 = initial_B0,
      N = N,
      m = m,
      universe = universe,
      iterations = iter,
      converged = abs(difference) <= precision
    ),
    coverage = list(
      RM = RM,
      BBD = coverage_bbd
    ),
    contact_distribution = contact_distribution,
    iteration_history = history
  )

  class(result) <- c("MBBD", "list")
  return(result)
}

#' Imprime un objeto MBBD
#'
#' @description Método para imprimir los resultados de un modelo MBBD
#' @param x Objeto de clase "MBBD"
#' @param ... Argumentos adicionales pasados a print
#' @export
#' @method print MBBD
print.MBBD <- function(x, ...) {
  # Función auxiliar para formatear números grandes
  format_number <- function(x) format(x, big.mark = ",", scientific = FALSE)

  # Cabecera
  cat("\n\033[1mResultados del Modelo MBBD\033[0m")
  cat("\n===============================\n")

  # Información del universo
  cat("\n\033[1mUNIVERSO Y SOPORTES:\033[0m")
  cat("\n---------------------")
  cat(sprintf("\nUniverso = %s personas", format_number(x$parameters$universe)))
  cat(sprintf("\nSoportes = %d", x$parameters$m))  # Añadir m a los parámetros
  cat(sprintf("\nTotal inserciones = %d", x$parameters$N))

  # Parámetros
  cat("\n\n\033[1mPARÁMETROS BETA BINOMIAL:\033[0m")
  cat("\n-------------------------")
  cat(sprintf("\nA inicial (A0) = %.4f", x$parameters$A0))  # Añadir A0 a los parámetros
  cat(sprintf("\nB inicial (B0) = %.4f", x$parameters$initial_B0))
  cat(sprintf("\nA final (AF) = %.4f", x$parameters$AF))
  cat(sprintf("\nB final (BF) = %.4f", x$parameters$BF))

  # Coberturas
  cat("\n\n\033[1mCOBERTURAS:\033[0m")
  cat("\n-----------")
  cat(sprintf("\nMorgensztem (RM) = %s personas (%.2f%%)",
              format_number(x$coverage$RM),
              100*x$coverage$RM/x$parameters$universe))
  cat(sprintf("\nBeta Binomial   = %s personas (%.2f%%)",
              format_number(x$coverage$BBD),
              100*x$coverage$BBD/x$parameters$universe))
  cat(sprintf("\nDiferencia      = %s personas (%.2f%%)",
              format_number(abs(x$coverage$BBD - x$coverage$RM)),
              100*abs(x$coverage$BBD - x$coverage$RM)/x$parameters$universe))

  # Convergencia
  cat("\n\n\033[1mCONVERGENCIA:\033[0m")
  cat("\n-------------")
  cat(sprintf("\nIteraciones realizadas = %d", x$parameters$iterations))
  cat(sprintf("\nConvergencia alcanzada = %s",
              ifelse(x$parameters$converged, "\033[32mSí\033[0m", "\033[31mNo\033[0m")))

  # Distribución de contactos
  cat("\n\n\033[1mDISTRIBUCIÓN DE CONTACTOS:\033[0m")
  cat("\n-------------------------\n")
  dist_table <- data.frame(
    'Nº Contactos' = 0:x$parameters$N,
    'Probabilidad (%)' = sprintf("%.2f%%", x$contact_distribution * 100),
    'Acumulado (%)' = sprintf("%.2f%%", cumsum(x$contact_distribution) * 100)
  )
  print(dist_table)

  # Estadísticas de la distribución
  cat("\n\033[1mESTADÍSTICAS DE CONTACTOS:\033[0m")
  cat("\n--------------------------")
  contactos <- 0:x$parameters$N
  media <- sum(contactos * x$contact_distribution)
  var <- sum((contactos - media)^2 * x$contact_distribution)
  cat(sprintf("\nMedia de contactos = %.2f", media))
  cat(sprintf("\nDesviación típica = %.2f", sqrt(var)))
  cat(sprintf("\nModa = %d", which.max(x$contact_distribution) - 1))
  cat("\n\n")
}
