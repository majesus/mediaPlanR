#' Calibrate a Beta-Binomial effective-reach model
#'
#' The first-insertion reach fixes the Beta mean. The function then calibrates
#' only its concentration, avoiding the million-point two-dimensional grids
#' used by the historical functions.
#'
#' @param first_reach Reach after one insertion, as a proportion.
#' @param target_reach Target probability for exactly `frequency` exposures or
#'   at least `frequency` exposures.
#' @param frequency Positive exposure threshold.
#' @param max_insertions Maximum number of insertions considered.
#' @param type `exact` or `at_least`.
#' @param tolerance Desired absolute probability error.
#'
#' @return A `bbd_calibration` object with calibrated parameters, full
#' distribution and diagnostics.
#' @export
calibrate_bbd <- function(first_reach, target_reach, frequency,
                          max_insertions, type = c("at_least", "exact"),
                          tolerance = 1e-6) {
  type <- match.arg(type)
  scalar_probability <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x >= 1) {
      stop(name, " must be one number strictly between zero and one", call. = FALSE)
    }
  }
  scalar_probability(first_reach, "first_reach")
  scalar_probability(target_reach, "target_reach")
  if (!is.numeric(frequency) || length(frequency) != 1L || frequency < 1 ||
      frequency != round(frequency) || !is.numeric(max_insertions) ||
      length(max_insertions) != 1L || max_insertions < frequency ||
      max_insertions != round(max_insertions)) {
    stop("frequency and max_insertions must be compatible positive integers", call. = FALSE)
  }

  probability_at <- function(log_concentration, n) {
    concentration <- exp(log_concentration)
    alpha <- first_reach * concentration
    beta <- (1 - first_reach) * concentration
    distribution <- extraDistr::dbbinom(0:n, size = n, alpha = alpha, beta = beta)
    if (type == "exact") distribution[frequency + 1L] else
      sum(distribution[(frequency + 1L):(n + 1L)])
  }

  candidates <- lapply(frequency:max_insertions, function(n) {
    grid <- seq(-15, 15, length.out = 301L)
    errors <- vapply(grid, function(x) abs(probability_at(x, n) - target_reach), numeric(1))
    best <- which.min(errors)
    lower <- grid[max(1L, best - 1L)]
    upper <- grid[min(length(grid), best + 1L)]
    fit <- stats::optimize(function(x) (probability_at(x, n) - target_reach)^2,
                           c(lower, upper), tol = .Machine$double.eps^0.25)
    predicted <- probability_at(fit$minimum, n)
    data.frame(n = n, log_concentration = fit$minimum,
               predicted = predicted, error = abs(predicted - target_reach))
  })
  candidates <- do.call(rbind, candidates)
  best <- candidates[order(candidates$error, candidates$n), ][1L, ]
  concentration <- exp(best$log_concentration)
  alpha <- first_reach * concentration
  beta <- (1 - first_reach) * concentration
  distribution <- extraDistr::dbbinom(0:best$n, size = best$n,
                                      alpha = alpha, beta = beta)
  cumulative <- rev(cumsum(rev(distribution)))

  structure(list(
    alpha = alpha,
    beta = beta,
    concentration = concentration,
    insertions = as.integer(best$n),
    first_reach = first_reach,
    frequency = as.integer(frequency),
    type = type,
    target_reach = target_reach,
    predicted_reach = best$predicted,
    error = best$error,
    converged = best$error <= tolerance,
    distribution = data.frame(
      contacts = 0:best$n,
      probability = distribution,
      cumulative_probability = cumulative
    ),
    candidates = candidates
  ), class = "bbd_calibration")
}

#' @export
print.bbd_calibration <- function(x, ...) {
  cat(sprintf("Beta-Binomial calibration (%s %d exposures)\n", x$type, x$frequency))
  cat(sprintf("n=%d | alpha=%.6f | beta=%.6f\n",
              x$insertions, x$alpha, x$beta))
  cat(sprintf("Target=%.6f | Predicted=%.6f | Error=%.3g | Converged=%s\n",
              x$target_reach, x$predicted_reach, x$error, x$converged))
  invisible(x)
}

