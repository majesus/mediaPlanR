#' @encoding UTF-8
#' @title Fit a Beta-Binomial distribution to an external reach estimate
#' @description Fits one Beta-Binomial exposure distribution while preserving
#' the insertion-weighted mean exposure probability and reproducing an external
#' schedule-reach estimate. This is a univariate mean-zero calibration; it is
#' not the Morgensztern Sequential Aggregation Distribution (MSAD).
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param insertions Numeric vector. Number of insertions for each vehicle (ni)
#' @param audiences Numeric vector. Audience of each vehicle in people (Ai)
#' @param RM Numeric. External schedule reach in people. It may come from
#' Morgensztern or any other independently justified reach estimator.
#' @param universe Integer. Target universe size in people
#' @param A0 Numeric. Historical initial value for parameter A. Kept for
#' compatibility and to inform B0; the v2 calibration does not depend on the
#' starting point.
#' @param precision Numeric. Convergence criterion in people. Defaults to 100
#' @param max_iter Integer. Maximum number of iterations allowed. Defaults to 100
#' @param adj_factor Legacy argument, kept for compatibility. The v2
#' calibration uses root finding rather than multiplicative steps.
#'
#' @details
#' The model preserves the insertion-weighted mean probability and calibrates
#' the concentration of a Beta-Binomial distribution until its coverage matches
#' the supplied external reach:
#' \enumerate{
#'   \item Computes the weighted mean probability and B0:
#'     \itemize{
#'       \item B0 = A0 * (SUM ni - SUM niAi) / (SUM niAi)
#'     }
#'   \item Determines the feasible theoretical interval, from the polarized
#'   limit to the independent binomial limit.
#'   \item Solves BBD(A) - RM = 0 via \code{uniroot()} on the log scale of the
#'   concentration.
#'   \item Recomputes coverage and the distribution with exactly the same
#'   final parameters.
#' }
#'
#' The model assumes:
#' \itemize{
#'   \item A and B are positive and keep A/(A+B) constant
#'   \item BBD coverage is computed as 1 - P(K=0)
#'   \item Convergence is reached when |BBD - RM| is at most precision
#' }
#'
#' @return A list of class `bbd_reach_fit` containing:
#' \itemize{
#'   \item parameters: List with the final parameters:
#'     \itemize{
#'       \item AF: Final A parameter
#'       \item BF: Final B parameter
#'       \item N: Total insertions
#'       \item universe: Universe size
#'       \item iterations: Number of iterations performed
#'       \item converged: Convergence indicator
#'     }
#'   \item coverage: List with coverage figures:
#'     \itemize{
#'       \item RM: External reach used as the constraint
#'       \item BBD: Beta-Binomial coverage
#'     }
#'   \item contact_distribution: Vector with the probabilities from 0 to N exposures
#'   \item iteration_history: Data frame with the iteration history
#' }
#'
#' @examples
#' # Basic example
#' insertions <- c(5, 7, 4)
#' audiences <- c(500000, 550000, 600000)
#' RM <- 550000
#' universe <- 1000000
#' result <- fit_bbd_to_reach(insertions, audiences, RM, universe, A0 = 0.1)
#'
#' # Inspect the results
#' print(result)
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial distribution
#' \code{\link{calc_sainsbury}} for the Sainsbury model
#' \code{\link{calc_binomial}} for the Binomial model
#' \code{\link{calc_metheringham}} for the Metheringham model

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

  # This calibration preserves the insertion-weighted mean exposure
  # probability and calibrates the concentration of the Beta mixing
  # distribution to RM.
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

  class(result) <- c("bbd_reach_fit", "list")
  return(result)
}

#' @export
print.bbd_reach_fit <- function(x, ...) {
  # Helper to format large numbers
  format_number <- function(x) format(x, big.mark = ",", scientific = FALSE)

  # Header
  cat("\nBeta-Binomial fit to external reach")
  cat("\n===============================\n")

  # Universe information
  cat("\nUNIVERSE AND VEHICLES:")
  cat("\n---------------------")
  cat(sprintf("\nUniverse = %s people", format_number(x$parameters$universe)))
  cat(sprintf("\nVehicles = %d", x$parameters$m))
  cat(sprintf("\nTotal insertions = %d", x$parameters$N))

  # Parameters
  cat("\n\nBETA-BINOMIAL PARAMETERS:")
  cat("\n-------------------------")
  cat(sprintf("\nInitial A (A0) = %.4f", x$parameters$A0))
  cat(sprintf("\nInitial B (B0) = %.4f", x$parameters$initial_B0))
  cat(sprintf("\nFinal A (AF) = %.4f", x$parameters$AF))
  cat(sprintf("\nFinal B (BF) = %.4f", x$parameters$BF))

  # Coverage
  cat("\n\nCOVERAGE:")
  cat("\n-----------")
  cat(sprintf("\nExternal reach (RM) = %s people (%.2f%%)",
              format_number(x$coverage$RM),
              100*x$coverage$RM/x$parameters$universe))
  cat(sprintf("\nBeta-Binomial   = %s people (%.2f%%)",
              format_number(x$coverage$BBD),
              100*x$coverage$BBD/x$parameters$universe))
  cat(sprintf("\nDifference      = %s people (%.2f%%)",
              format_number(abs(x$coverage$BBD - x$coverage$RM)),
              100*abs(x$coverage$BBD - x$coverage$RM)/x$parameters$universe))

  # Convergence
  cat("\n\nCONVERGENCE:")
  cat("\n-------------")
  cat(sprintf("\nIterations performed = %d", x$parameters$iterations))
  cat(sprintf("\nConverged = %s",
              ifelse(x$parameters$converged, "Yes", "No")))

  # Exposure distribution
  cat("\n\nEXPOSURE DISTRIBUTION:")
  cat("\n-------------------------\n")
  dist_table <- data.frame(
    'Exposures' = 0:x$parameters$N,
    'Probability (%)' = sprintf("%.2f%%", x$contact_distribution * 100),
    'Cumulative (%)' = sprintf("%.2f%%", cumsum(x$contact_distribution) * 100)
  )
  print(dist_table)

  # Distribution statistics
  cat("\nEXPOSURE STATISTICS:")
  cat("\n--------------------")
  contacts <- 0:x$parameters$N
  mean_contacts <- sum(contacts * x$contact_distribution)
  var_contacts <- sum((contacts - mean_contacts)^2 * x$contact_distribution)
  cat(sprintf("\nMean exposures = %.2f", mean_contacts))
  cat(sprintf("\nStandard deviation = %.2f", sqrt(var_contacts)))
  cat(sprintf("\nMode = %d", which.max(x$contact_distribution) - 1))
  cat("\n\n")
  invisible(x)
}
