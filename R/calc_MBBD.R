#' @encoding UTF-8
#' @title Fit a Beta-Binomial distribution to an external reach estimate
#' @description Fits one Beta-Binomial contact distribution while preserving
#' the insertion-weighted mean exposure probability and reproducing an external
#' schedule-reach estimate. This is a univariate mean-zero calibration; it is
#' not the Morgensztern Sequential Aggregation Distribution (MSAD).
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param insertions Vector numerico. Numero de inserciones para cada soporte (ni)
#' @param audiences Vector numerico. Audiencia de cada soporte en personas (Ai)
#' @param RM Numerico. External schedule reach in people. It may come from
#' Morgensztern or any other independently justified reach estimator.
#' @param universe Entero. Tamano del universo objetivo en personas
#' @param A0 Numerico. Valor inicial historico del parametro A. Se conserva
#' por compatibilidad y para informar B0; la calibracion v2 no depende del
#' punto inicial.
#' @param precision Numerico. Criterio de convergencia en personas. Por defecto 100
#' @param max_iter Entero. Numero maximo de iteraciones permitidas. Por defecto 100
#' @param adj_factor Argumento heredado, conservado por compatibilidad. La
#' calibracion v2 usa busqueda de raices y no pasos multiplicativos.
#'
#' @details
#' El modelo conserva la probabilidad media ponderada por inserciones y calibra
#' la concentracion de una distribucion Beta-Binomial hasta que su cobertura
#' coincide con el reach externo suministrado:
#' \enumerate{
#'   \item Calcula la probabilidad media ponderada y B0:
#'     \itemize{
#'       \item B0 = A0 * (SUMATORIO ni - SUMATORIO niAi) / (SUMATORIO niAi)
#'     }
#'   \item Determina el intervalo teorico factible, desde el limite polarizado
#'   hasta el limite binomial independiente.
#'   \item Resuelve BBD(A) - RM = 0 mediante \code{uniroot()} en la escala
#'   logaritmica de la concentracion.
#'   \item Recalcula cobertura y distribucion con exactamente los mismos
#'   parametros finales.
#' }
#'
#' El modelo asume:
#' \itemize{
#'   \item A y B son positivos y mantienen constante A/(A+B)
#'   \item La cobertura BBD se calcula como 1 - P(K=0)
#'   \item La convergencia se alcanza cuando |BBD - RM| menor o igual que precision
#' }
#'
#' @return A list of class `bbd_reach_fit` and legacy class `MBBD` containing:
#' \itemize{
#'   \item parameters: Lista con parametros finales:
#'     \itemize{
#'       \item AF: Parametro A final
#'       \item BF: Parametro B final
#'       \item N: Total de inserciones
#'       \item universe: Tamano del universo
#'       \item iterations: Numero de iteraciones realizadas
#'       \item converged: Indicador de convergencia
#'     }
#'   \item coverage: Lista con coberturas:
#'     \itemize{
#'       \item RM: Reach externo utilizado como restriccion
#'       \item BBD: Cobertura Beta Binomial
#'     }
#'   \item contact_distribution: Vector con probabilidades de 0 a N contactos
#'   \item iteration_history: Data frame con historial de iteraciones
#' }
#'
#' @examples
#' # Ejemplo basico
#' insertions <- c(5, 7, 4)
#' audiences <- c(500000, 550000, 600000)
#' RM <- 550000
#' universe <- 1000000
#' resultado <- fit_bbd_to_reach(insertions, audiences, RM, universe, A0 = 0.1)
#'
#' # Examinar resultados
#' print(resultado)
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_sainsbury}} para estimaciones el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_metheringham}} para estimaciones con el modelo de Metheringham

#' @importFrom extraDistr dbbinom
fit_bbd_to_reach <- function(insertions, audiences, RM, universe, A0,
                      precision = 100,
                      max_iter = 100,
                      adj_factor = 0.01) {

  if (!is.numeric(insertions) || !length(insertions) || anyNA(insertions) ||
      any(!is.finite(insertions)) ||
      any(insertions < 1 | insertions != round(insertions))) {
    stop("insertions must contain positive finite integers.", call. = FALSE)
  }
  m <- length(insertions)
  if (!is.numeric(audiences) || length(audiences) != m || anyNA(audiences) ||
      any(!is.finite(audiences)) || any(audiences <= 0)) {
    stop("audiences must contain one positive finite value per vehicle.",
         call. = FALSE)
  }
  if (!is.numeric(universe) || length(universe) != 1L ||
      !is.finite(universe) || universe <= 0) {
    stop("universe must be one positive finite number.", call. = FALSE)
  }
  if (any(audiences > universe)) {
    stop("audiences cannot exceed universe.", call. = FALSE)
  }
  if (!is.numeric(RM) || length(RM) != 1L || !is.finite(RM) ||
      RM <= 0 || RM > universe) {
    stop("RM must be one positive finite number no greater than universe.",
         call. = FALSE)
  }
  if (!is.numeric(A0) || length(A0) != 1L || !is.finite(A0) ||
      A0 <= 0 || A0 > 2000) {
    stop("A0 must be one finite number in (0, 2000].", call. = FALSE)
  }
  if (!is.numeric(precision) || length(precision) != 1L ||
      !is.finite(precision) || precision <= 0) {
    stop("precision must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(max_iter) || length(max_iter) != 1L ||
      !is.finite(max_iter) || max_iter < 1 || max_iter != round(max_iter)) {
    stop("max_iter must be one positive integer.", call. = FALSE)
  }

  # MBBD preserves the insertion-weighted mean exposure probability and
  # calibrates the concentration of the Beta mixing distribution to RM.
  audience_props <- audiences / universe
  sum_ni <- sum(insertions)
  mean_probability <- sum(insertions * audience_props) / sum_ni
  initial_B0 <- A0 * (1 - mean_probability) / mean_probability

  coverage_for_log_concentration <- function(log_concentration) {
    concentration <- exp(log_concentration)
    alpha <- concentration * mean_probability
    beta <- concentration * (1 - mean_probability)
    (1 - extraDistr::dbbinom(0, size = sum_ni, alpha = alpha, beta = beta)) * universe
  }

  lower_log <- -30
  upper_log <- 30
  feasible_min <- mean_probability * universe
  feasible_max <- (1 - (1 - mean_probability)^sum_ni) * universe
  if (RM < feasible_min - precision || RM > feasible_max + precision) {
    stop(sprintf("RM is outside the feasible Beta-Binomial interval [%.0f, %.0f]",
                 feasible_min, feasible_max), call. = FALSE)
  }

  history <- data.frame(
    iteration = integer(), A = numeric(), B = numeric(),
    coverage_bbd = numeric(), difference = numeric()
  )
  eval_count <- 0L
  objective <- function(log_concentration) {
    eval_count <<- eval_count + 1L
    concentration <- exp(log_concentration)
    a <- concentration * mean_probability
    b <- concentration * (1 - mean_probability)
    coverage <- coverage_for_log_concentration(log_concentration)
    history <<- rbind(history, data.frame(
      iteration = eval_count, A = a, B = b,
      coverage_bbd = coverage, difference = coverage - RM
    ))
    coverage - RM
  }

  N <- sum_ni
  if (abs(feasible_min - RM) <= precision) {
    AF <- 0
    BF <- 0
    contact_distribution <- numeric(N + 1L)
    contact_distribution[c(1L, N + 1L)] <-
      c(1 - mean_probability, mean_probability)
    coverage_bbd <- feasible_min
    iter <- 0L
    fit_type <- "polarized_limit"
  } else if (abs(feasible_max - RM) <= precision) {
    AF <- Inf
    BF <- Inf
    contact_distribution <- stats::dbinom(
      0:N, size = N, prob = mean_probability
    )
    coverage_bbd <- feasible_max
    iter <- 0L
    fit_type <- "binomial_limit"
  } else {
    root <- stats::uniroot(objective, c(lower_log, upper_log),
                           tol = max(.Machine$double.eps^0.5,
                                     precision / universe),
                           maxiter = max_iter)$root
    concentration <- exp(root)
    AF <- concentration * mean_probability
    BF <- concentration * (1 - mean_probability)
    coverage_bbd <- coverage_for_log_concentration(root)
    iter <- eval_count
    contact_distribution <- extraDistr::dbbinom(
      0:N, size = N, alpha = AF, beta = BF
    )
    fit_type <- "beta_binomial"
  }
  difference <- coverage_bbd - RM

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
      converged = abs(difference) <= precision,
      fit_type = fit_type,
      mean_probability = mean_probability,
      feasible_coverage = c(min = feasible_min, max = feasible_max)
    ),
    coverage = list(
      RM = RM,
      BBD = coverage_bbd,
      difference = difference
    ),
    contact_distribution = contact_distribution,
    iteration_history = history
  )

  class(result) <- c("bbd_reach_fit", "MBBD", "list")
  return(result)
}

#' Legacy name for a Beta-Binomial reach calibration
#'
#' `calc_MBBD()` is retained for compatibility. The historical name suggested
#' a full Morgensztern sequential model, although the function only calibrates
#' one Beta-Binomial distribution to an externally supplied reach. Use
#' [fit_bbd_to_reach()] for that operation or [calc_msad()] for the sequential
#' aggregation model described by Kim (2005).
#'
#' @inheritParams fit_bbd_to_reach
#' @return The value returned by [fit_bbd_to_reach()].
#' @export
calc_MBBD <- function(insertions, audiences, RM, universe, A0,
                      precision = 100, max_iter = 100,
                      adj_factor = 0.01) {
  fit_bbd_to_reach(
    insertions = insertions,
    audiences = audiences,
    RM = RM,
    universe = universe,
    A0 = A0,
    precision = precision,
    max_iter = max_iter,
    adj_factor = adj_factor
  )
}

#' Print a Beta-Binomial reach fit
#'
#' @description Prints the results of a Beta-Binomial distribution fitted to
#' an external reach estimate.
#' @param x Object inheriting from `bbd_reach_fit` or legacy class `MBBD`.
#' @param ... Argumentos adicionales pasados a print
#' @export
#' @method print bbd_reach_fit
print.bbd_reach_fit <- function(x, ...) {
  # Funcion auxiliar para formatear numeros grandes
  format_number <- function(x) format(x, big.mark = ",", scientific = FALSE)

  # Cabecera
  cat("\n\033[1mBeta-Binomial fit to external reach\033[0m")
  cat("\n===============================\n")

  # Informacion del universo
  cat("\n\033[1mUNIVERSO Y SOPORTES:\033[0m")
  cat("\n---------------------")
  cat(sprintf("\nUniverso = %s personas", format_number(x$parameters$universe)))
  cat(sprintf("\nSoportes = %d", x$parameters$m))  # Anadir m a los parametros
  cat(sprintf("\nTotal inserciones = %d", x$parameters$N))

  # Parametros
  cat("\n\n\033[1mPARAMETROS BETA BINOMIAL:\033[0m")
  cat("\n-------------------------")
  cat(sprintf("\nA inicial (A0) = %.4f", x$parameters$A0))  # Anadir A0 a los parametros
  cat(sprintf("\nB inicial (B0) = %.4f", x$parameters$initial_B0))
  cat(sprintf("\nA final (AF) = %.4f", x$parameters$AF))
  cat(sprintf("\nB final (BF) = %.4f", x$parameters$BF))

  # Coberturas
  cat("\n\n\033[1mCOBERTURAS:\033[0m")
  cat("\n-----------")
  cat(sprintf("\nExternal reach (RM) = %s personas (%.2f%%)",
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
              ifelse(x$parameters$converged, "\033[32mSi\033[0m", "\033[31mNo\033[0m")))

  # Distribucion de contactos
  cat("\n\n\033[1mDISTRIBUCION DE CONTACTOS:\033[0m")
  cat("\n-------------------------\n")
  dist_table <- data.frame(
    'No Contactos' = 0:x$parameters$N,
    'Probabilidad (%)' = sprintf("%.2f%%", x$contact_distribution * 100),
    'Acumulado (%)' = sprintf("%.2f%%", cumsum(x$contact_distribution) * 100)
  )
  print(dist_table)

  # Estadisticas de la distribucion
  cat("\n\033[1mESTADISTICAS DE CONTACTOS:\033[0m")
  cat("\n--------------------------")
  contactos <- 0:x$parameters$N
  media <- sum(contactos * x$contact_distribution)
  var <- sum((contactos - media)^2 * x$contact_distribution)
  cat(sprintf("\nMedia de contactos = %.2f", media))
  cat(sprintf("\nDesviacion tipica = %.2f", sqrt(var)))
  cat(sprintf("\nModa = %d", which.max(x$contact_distribution) - 1))
  cat("\n\n")
  invisible(x)
}

#' @export
print.MBBD <- function(x, ...) {
  print.bbd_reach_fit(x, ...)
}
