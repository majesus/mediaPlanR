#' Calibrate a Beta-Binomial effective-reach model
#'
#' Searches for a Beta-Binomial exposure distribution whose effective reach is
#' as close as possible to a target. The reach after one insertion fixes the
#' mean of the Beta distribution, and only its concentration is searched, by a
#' one-dimensional search instead of a two-dimensional grid over both shape
#' parameters. The search returns the closest candidate within the stated
#' bounds; it does not guarantee that the target is reached. Always check
#' `converged`, `predicted_reach` and `error` before using `alpha` and `beta`.
#' The concentration is searched in a finite interval, and the two limits of
#' the family, the binomial and the polarized ones, are evaluated explicitly
#' (see Details).
#'
#' The effective frequency `frequency` is a threshold chosen by the analyst for
#' the objective, the creative, the medium and the period; the function does not
#' estimate a relation between that number of exposures and recall, persuasion,
#' sales or profit. Reaching the threshold is a delivery measure, not an
#' outcome. It is advisable to examine several thresholds.
#'
#' @param R1 Reach after one insertion (the vehicle's audience), as a proportion
#'   (not a percentage) strictly between zero and one.
#' @param target_reach Target proportion of the population (not a percentage),
#'   strictly between zero and one. With `type = "at_least"` it is the effective reach: the
#'   proportion exposed `frequency` times or more. With `type = "exact"` it is
#'   the proportion exposed exactly `frequency` times (which is then not a
#'   reach in the usual sense).
#' @param frequency Positive integer: the number of exposures that defines the
#'   target (`frequency` or more exposures if `type = "at_least"`, exactly
#'   `frequency` exposures if `type = "exact"`).
#' @param max_insertions Maximum number of insertions considered, an integer
#'   of at least `frequency`.
#' @param type What `target_reach` refers to. `"at_least"` (default): the
#'   probability of `frequency` or more exposures (for `frequency = 2`, being
#'   exposed twice, three times, and so on; this is the effective reach).
#'   `"exact"`: the probability of exactly `frequency` exposures (for
#'   `frequency = 2`, exactly twice).
#' @param tolerance Desired absolute probability error; it only determines the
#'   `converged` flag.
#'
#' @details
#' For every number of insertions `n` from `frequency` to `max_insertions`, the
#' function searches for the concentration of the Beta-Binomial, with mean
#' `R1`, whose probability equals `target_reach`, first on a grid of
#' the log concentration and then by [stats::optimize()] around the best grid
#' point. It returns the number of insertions with the smallest error, the
#' smaller `n` in case of ties: the error is the criterion, and neither cost
#' nor the minimum number of insertions is prioritized except to break ties.
#' When the target cannot be reached, `converged` is `FALSE` and the closest
#' candidate is returned.
#'
#' The search interval is finite: the natural logarithm of the concentration
#' `alpha + beta` is searched between -15 and 15, that is, concentrations from
#' about 3e-7 to 3.3e6. The two limits of the family that [calc_beta_binomial()]
#' also represents, the polarized limit (concentration tending to zero: either
#' all of the insertions or none) and the binomial limit (concentration tending
#' to infinity: independent insertions), are not points of that interval, so
#' they are evaluated explicitly as one more candidate for every `n` of at
#' least two. A limit is returned only when its error is smaller, by more than
#' 1e-12, than that of the best candidate of the interval, so that the results
#' that the interval already resolves do not change. In that case `alpha`,
#' `beta` and `concentration` are `0` (polarized) or `Inf` (binomial), as in
#' [calc_beta_binomial()], where they indicate the limit and are not the
#' parameters of a proper Beta distribution, and `fit_type` says which one it
#' is. A target is outside the family only when it is farther than the limits
#' allow (for example, an effective reach above the binomial limit with that
#' number of insertions); a target merely beyond the ends of the interval
#' receives the nearest of the limit and the interval. For example, with `R1 = 0.3`, `target_reach = 0.99`,
#' `frequency = 1` and `max_insertions = 1` the only candidate has a
#' probability of 0.3, so `predicted_reach` is 0.3, `error` is 0.69 and
#' `converged` is `FALSE`.
#'
#' @return A `bbd_calibration` object: a list with `alpha`, `beta`,
#'   `concentration`, `insertions` (the selected `n`), `R1`,
#'   `frequency`, `type`, `target_reach`, `predicted_reach`, `error`,
#'   `converged`, `fit_type` (`"beta_binomial"`, `"binomial_limit"` or
#'   `"polarized_limit"`), the full `distribution` (`contacts`, `probability`
#'   and `cumulative_probability`) and the `candidates` evaluated for each `n`
#'   (the selected one for that `n`, with its `log_concentration`, which is
#'   `Inf` or `-Inf` for a limit, its `predicted` probability, its `error` and
#'   its `fit_type`).
#'
#' @examples
#' # A first insertion reaches 30% and the share exposed two or more times
#' # should be 20%
#' calibrate_bbd(R1 = 0.30, target_reach = 0.20, frequency = 2,
#'               max_insertions = 6)
#'
#' # A target that cannot be reached: the closest candidate is returned and
#' # `converged` is FALSE
#' unreachable <- calibrate_bbd(R1 = 0.30, target_reach = 0.99, frequency = 1,
#'                              max_insertions = 1)
#' unreachable$converged
#' unreachable$error
#'
#' @references
#' Metheringham, R. A. (1964). Measuring the net cumulative coverage of a print
#' campaign. Journal of Advertising Research, 4(4), 23-28.
#' \doi{10.1080/00218499.1964.12519751}
#'
#' Chandon, J.-L. (1976). A comparative study of media exposure models.
#' Unpublished doctoral dissertation, Northwestern University, Evanston, IL.
#'
#' @seealso [calc_beta_binomial()] for the model itself and [fit_bbd_to_reach()]
#'   to fit a Beta-Binomial to a whole schedule's reach.
#' @export
calibrate_bbd <- function(R1, target_reach, frequency,
                          max_insertions, type = c("at_least", "exact"),
                          tolerance = 1e-6) {
  type <- match.arg(type)
  assert_number(R1, "R1", min = 0, max = 1,
                min_open = TRUE, max_open = TRUE, proportion = TRUE)
  assert_number(target_reach, "target_reach", min = 0, max = 1,
                min_open = TRUE, max_open = TRUE, proportion = TRUE)
  assert_number(frequency, "frequency", min = 1, integer = TRUE)
  assert_number(max_insertions, "max_insertions", min = frequency,
                integer = TRUE)
  assert_number(tolerance, "tolerance", min = 0, min_open = TRUE)

  # Distribution of the number of exposures with n insertions: the
  # Beta-Binomial of mean R1 and concentration exp(log_concentration), or one
  # of the two limits of the family.
  distribution_at <- function(log_concentration, n, fit_type = "beta_binomial") {
    if (fit_type == "binomial_limit") {
      return(stats::dbinom(0:n, size = n, prob = R1))
    }
    if (fit_type == "polarized_limit") {
      out <- numeric(n + 1L)
      out[c(1L, n + 1L)] <- c(1 - R1, R1)
      return(out)
    }
    concentration <- exp(log_concentration)
    dbetabinom(0:n, size = n, alpha = R1 * concentration,
               beta = (1 - R1) * concentration)
  }
  target_probability <- function(distribution, n) {
    if (type == "exact") distribution[frequency + 1L] else
      sum(distribution[(frequency + 1L):(n + 1L)])
  }
  probability_at <- function(log_concentration, n) {
    target_probability(distribution_at(log_concentration, n), n)
  }

  limit_margin <- 1e-12
  candidates <- lapply(frequency:max_insertions, function(n) {
    grid <- seq(-15, 15, length.out = 301L)
    errors <- vapply(grid, function(x) abs(probability_at(x, n) - target_reach), numeric(1))
    best <- which.min(errors)
    lower <- grid[max(1L, best - 1L)]
    upper <- grid[min(length(grid), best + 1L)]
    fit <- stats::optimize(function(x) (probability_at(x, n) - target_reach)^2,
                           c(lower, upper), tol = .Machine$double.eps^0.25)
    predicted <- probability_at(fit$minimum, n)
    row <- data.frame(n = n, log_concentration = fit$minimum,
                      predicted = predicted,
                      error = abs(predicted - target_reach),
                      fit_type = "beta_binomial", stringsAsFactors = FALSE)
    # The limits are not in the interval of log concentrations (and with one
    # insertion they coincide with each other and with every candidate).
    if (n >= 2L) {
      for (limit in c("polarized_limit", "binomial_limit")) {
        limit_predicted <- target_probability(
          distribution_at(NA_real_, n, limit), n)
        limit_error <- abs(limit_predicted - target_reach)
        if (limit_error < row$error - limit_margin) {
          row <- data.frame(
            n = n,
            log_concentration = if (limit == "polarized_limit") -Inf else Inf,
            predicted = limit_predicted, error = limit_error,
            fit_type = limit, stringsAsFactors = FALSE)
        }
      }
    }
    row
  })
  candidates <- do.call(rbind, candidates)
  best <- candidates[order(candidates$error, candidates$n), ][1L, ]
  fit_type <- best$fit_type
  concentration <- switch(fit_type, binomial_limit = Inf, polarized_limit = 0,
                          exp(best$log_concentration))
  alpha <- switch(fit_type, binomial_limit = Inf, polarized_limit = 0,
                  R1 * concentration)
  beta <- switch(fit_type, binomial_limit = Inf, polarized_limit = 0,
                 (1 - R1) * concentration)
  distribution <- distribution_at(best$log_concentration, best$n, fit_type)
  cumulative <- rev(cumsum(rev(distribution)))

  structure(list(
    alpha = alpha,
    beta = beta,
    concentration = concentration,
    insertions = as.integer(best$n),
    R1 = R1,
    frequency = as.integer(frequency),
    type = type,
    target_reach = target_reach,
    predicted_reach = best$predicted,
    error = best$error,
    converged = best$error <= tolerance,
    fit_type = fit_type,
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
