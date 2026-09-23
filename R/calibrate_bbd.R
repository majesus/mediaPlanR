#' Calibrate a Beta-Binomial effective-reach model
#'
#' Calibrates a Beta-Binomial exposure distribution so that it reproduces a
#' target effective reach. The reach after one insertion fixes the mean of
#' the Beta distribution, and only its concentration is calibrated, by a
#' one-dimensional search instead of a two-dimensional grid over both shape
#' parameters.
#'
#' @param first_reach Reach after one insertion, as a proportion strictly
#'   between zero and one.
#' @param target_reach Target probability, strictly between zero and one, of
#'   exactly `frequency` exposures (`type = "exact"`) or of at least
#'   `frequency` exposures (`type = "at_least"`).
#' @param frequency Positive integer exposure threshold.
#' @param max_insertions Maximum number of insertions considered, an integer
#'   of at least `frequency`.
#' @param type `"at_least"` (default) or `"exact"`.
#' @param tolerance Desired absolute probability error; it only determines the
#'   `converged` flag.
#'
#' @details
#' For every number of insertions `n` from `frequency` to `max_insertions`, the
#' function searches for the concentration of the Beta-Binomial, with mean
#' `first_reach`, whose probability equals `target_reach`, first on a grid of
#' the log concentration and then by [stats::optimize()] around the best grid
#' point. It returns the number of insertions with the smallest error, the
#' smaller `n` in case of ties.
#'
#' @return A `bbd_calibration` object: a list with `alpha`, `beta`,
#'   `concentration`, `insertions` (the selected `n`), `first_reach`,
#'   `frequency`, `type`, `target_reach`, `predicted_reach`, `error`,
#'   `converged`, the full `distribution` (`contacts`, `probability` and
#'   `cumulative_probability`) and the `candidates` evaluated for each `n`.
#'
#' @examples
#' # A first insertion reaches 30% and 2+ exposures should reach 20%
#' calibrate_bbd(first_reach = 0.30, target_reach = 0.20, frequency = 2,
#'               max_insertions = 6)
#'
#' @seealso [calc_beta_binomial()] for the model itself and [fit_bbd_to_reach()]
#'   to fit a Beta-Binomial to a whole schedule's reach.
#' @export
calibrate_bbd <- function(first_reach, target_reach, frequency,
                          max_insertions, type = c("at_least", "exact"),
                          tolerance = 1e-6) {
  type <- match.arg(type)
  assert_number(first_reach, "first_reach", min = 0, max = 1,
                min_open = TRUE, max_open = TRUE)
  assert_number(target_reach, "target_reach", min = 0, max = 1,
                min_open = TRUE, max_open = TRUE)
  assert_number(frequency, "frequency", min = 1, integer = TRUE)
  assert_number(max_insertions, "max_insertions", min = frequency,
                integer = TRUE)
  assert_number(tolerance, "tolerance", min = 0, min_open = TRUE)

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
