nbd_probability <- function(x, mean_contacts, size) {
  if (is.infinite(size)) {
    stats::dpois(x, lambda = mean_contacts)
  } else {
    stats::dnbinom(x, size = size, mu = mean_contacts)
  }
}

nbd_upper_tail <- function(q, mean_contacts, size) {
  if (is.infinite(size)) {
    stats::ppois(q, lambda = mean_contacts, lower.tail = FALSE)
  } else {
    stats::pnbinom(q, size = size, mu = mean_contacts, lower.tail = FALSE)
  }
}

#' Negative-Binomial exposure-count approximation
#'
#' Builds a Poisson-Gamma (Negative-Binomial) distribution for exposure counts.
#' This is an unbounded count-process approximation intended for page views,
#' ad-server exposures, or other continuous opportunity processes. It is not a
#' finite-insertion reach model and does not represent cross-vehicle dependence.
#'
#' @param mean_contacts Positive expected exposures per person.
#' @param size Positive Gamma heterogeneity parameter. Smaller values imply
#'   stronger heterogeneity. `Inf` gives the Poisson limit.
#' @param report_max Positive integer at which the reported table becomes an
#'   open tail (`report_max` or more), not an exact final count.
#' @param opportunities Optional finite number of exposure opportunities. It is
#'   used only to diagnose the probability that the unbounded model assigns to
#'   impossible counts above that number.
#' @param tail_tolerance Non-negative diagnostic tolerance.
#'
#' @return An `nbd_exposure` object with the open-tail distribution, reach,
#'   conditional average frequency, and scope diagnostics.
#'
#' @details
#' The model assumes an individual Poisson exposure process with a Gamma-
#' distributed rate across people. Its variance is
#' \eqn{\mu + \mu^2/size}. Because its support is unbounded, the final table row
#' is explicitly labelled as an open tail. No tail mass is reassigned to an
#' impossible exact exposure count.
#'
#' Danaher (2007) provides direct media-planning support for Negative-Binomial
#' page-view models and develops the multivariate extension needed to represent
#' dependence across websites. This function is deliberately univariate and
#' therefore must not be interpreted as Danaher's multivariate model.
#'
#' @references
#' Danaher, P. J. (2007). Modeling Page Views Across Multiple Websites with an
#' Application to Internet Reach and Frequency Prediction. Marketing Science,
#' 26(3), 422-437. \doi{10.1287/mksc.1060.0226}
#'
#' Ehrenberg, A. S. C. (1959). The Pattern of Consumer Purchases. Applied
#' Statistics, 8(1), 26-41. \doi{10.2307/2985810}
#'
#' @examples
#' model <- nbd_exposure_distribution(
#'   mean_contacts = 2.5, size = 1.7, report_max = 10
#' )
#' model$reach
#'
#' @export
nbd_exposure_distribution <- function(mean_contacts, size, report_max = NULL,
                                      opportunities = NULL,
                                      tail_tolerance = 1e-6) {
  if (!is.numeric(mean_contacts) || length(mean_contacts) != 1L ||
      !is.finite(mean_contacts) || mean_contacts <= 0) {
    stop("mean_contacts must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(size) || length(size) != 1L || is.na(size) ||
      size <= 0 || (!is.finite(size) && !is.infinite(size))) {
    stop("size must be one positive number or Inf.", call. = FALSE)
  }
  if (!is.numeric(tail_tolerance) || length(tail_tolerance) != 1L ||
      !is.finite(tail_tolerance) || tail_tolerance < 0) {
    stop("tail_tolerance must be one non-negative finite number.", call. = FALSE)
  }
  if (is.null(report_max)) {
    quantile_value <- if (is.infinite(size)) {
      stats::qpois(0.999, lambda = mean_contacts)
    } else {
      stats::qnbinom(0.999, size = size, mu = mean_contacts)
    }
    report_max <- max(10L, as.integer(ceiling(quantile_value)))
  }
  if (!is.numeric(report_max) || length(report_max) != 1L ||
      !is.finite(report_max) || report_max < 1 ||
      report_max != round(report_max) || report_max > 1e6) {
    stop("report_max must be one positive integer no greater than 1,000,000.",
         call. = FALSE)
  }
  report_max <- as.integer(report_max)
  if (!is.null(opportunities)) {
    if (!is.numeric(opportunities) || length(opportunities) != 1L ||
        !is.finite(opportunities) || opportunities < 1 ||
        opportunities != round(opportunities)) {
      stop("opportunities must be NULL or one positive integer.", call. = FALSE)
    }
    opportunities <- as.integer(opportunities)
  }

  exact <- nbd_probability(0:(report_max - 1L), mean_contacts, size)
  open_tail <- nbd_upper_tail(report_max - 1L, mean_contacts, size)
  probability <- c(exact, open_tail)
  probability <- probability / sum(probability)
  contacts <- 0:report_max
  cumulative <- rev(cumsum(rev(probability)))
  reach <- 1 - probability[1L]
  impossible_mass <- if (is.null(opportunities)) NA_real_ else
    nbd_upper_tail(opportunities, mean_contacts, size)

  result <- list(
    reach = list(probability = reach, percent = 100 * reach),
    mean_contacts = mean_contacts,
    average_frequency = mean_contacts / reach,
    size = size,
    variance = mean_contacts + if (is.infinite(size)) 0 else
      mean_contacts^2 / size,
    distribution = data.frame(
      contacts = contacts,
      label = c(as.character(0:(report_max - 1L)),
                paste0(report_max, "+")),
      open_tail = contacts == report_max,
      probability = probability,
      cumulative_probability = cumulative
    ),
    diagnostics = list(
      process = "unbounded_poisson_gamma",
      scope = "continuous exposure counts; not a finite-insertion cross-media model",
      report_max = report_max,
      opportunities = opportunities,
      probability_above_opportunities = impossible_mass,
      finite_opportunity_compatible = is.null(opportunities) ||
        impossible_mass <= tail_tolerance,
      tail_tolerance = tail_tolerance
    )
  )
  class(result) <- "nbd_exposure"
  result
}

#' Fit a Negative-Binomial exposure-count model
#'
#' Estimates the mean and Gamma heterogeneity parameter from observed person-
#' level exposure counts by maximum likelihood. When the data are not
#' overdispersed relative to Poisson, the fit returns the Poisson boundary.
#'
#' @param counts Vector of observed non-negative integer exposure counts.
#' @param report_max Optional open-tail threshold passed to
#'   [nbd_exposure_distribution()].
#' @param conf_level Confidence level for the profile-scale Wald interval for
#'   `size` when an interior Negative-Binomial solution exists.
#'
#' @return An `nbd_exposure_fit` object containing estimates, likelihood
#' diagnostics, observed frequencies, and the fitted distribution.
#'
#' @examples
#' counts <- c(rep(0, 40), rep(1, 25), rep(2, 15), rep(3, 8), 5, 7)
#' fit <- fit_nbd_exposure(counts)
#' fit$estimates
#'
#' @export
fit_nbd_exposure <- function(counts, report_max = NULL, conf_level = 0.95) {
  if (!is.numeric(counts) || length(counts) < 2L || anyNA(counts) ||
      any(!is.finite(counts)) || any(counts < 0 | counts != round(counts))) {
    stop("counts must contain at least two finite non-negative integers.",
         call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be strictly between zero and one.", call. = FALSE)
  }
  mean_contacts <- mean(counts)
  if (mean_contacts <= 0) {
    stop("At least one observed exposure count must be positive.", call. = FALSE)
  }
  observed_variance <- stats::var(counts)
  poisson_log_likelihood <- sum(stats::dpois(
    counts, lambda = mean_contacts, log = TRUE
  ))
  objective <- function(log_size) {
    -sum(stats::dnbinom(counts, size = exp(log_size),
                        mu = mean_contacts, log = TRUE))
  }
  optimum <- stats::optimize(objective, interval = c(-16, 25),
                             tol = .Machine$double.eps^0.25)
  finite_log_likelihood <- -optimum$objective
  poisson_boundary <- observed_variance <= mean_contacts * (1 + 1e-8) ||
    optimum$minimum > 24.5 ||
    finite_log_likelihood <= poisson_log_likelihood + 1e-8

  if (poisson_boundary) {
    size <- Inf
    log_likelihood <- poisson_log_likelihood
    size_interval <- c(lower = NA_real_, upper = Inf)
    converged <- TRUE
  } else {
    log_size <- optimum$minimum
    size <- exp(log_size)
    log_likelihood <- -optimum$objective
    step <- 1e-4 * max(1, abs(log_size))
    curvature <- (objective(log_size + step) - 2 * objective(log_size) +
                    objective(log_size - step)) / step^2
    if (is.finite(curvature) && curvature > 0) {
      standard_error_log_size <- sqrt(1 / curvature)
      critical <- stats::qnorm(1 - (1 - conf_level) / 2)
      size_interval <- exp(log_size + c(lower = -1, upper = 1) *
                             critical * standard_error_log_size)
    } else {
      size_interval <- c(lower = NA_real_, upper = NA_real_)
    }
    converged <- log_size > -16 + 1e-4 && log_size < 25 - 1e-4
  }

  fitted <- nbd_exposure_distribution(
    mean_contacts = mean_contacts,
    size = size,
    report_max = report_max
  )
  maximum_observed <- max(counts)
  observed <- tabulate(counts + 1L, nbins = maximum_observed + 1L)
  result <- list(
    estimates = list(
      mean_contacts = mean_contacts,
      size = size,
      size_interval = size_interval,
      variance = fitted$variance
    ),
    diagnostics = list(
      n = length(counts),
      observed_variance = observed_variance,
      dispersion_index = observed_variance / mean_contacts,
      poisson_boundary = poisson_boundary,
      converged = converged,
      log_likelihood = log_likelihood,
      AIC = -2 * log_likelihood + 2 * if (poisson_boundary) 1 else 2
    ),
    observed = data.frame(
      contacts = 0:maximum_observed,
      count = observed,
      proportion = observed / length(counts)
    ),
    fitted_distribution = fitted
  )
  class(result) <- "nbd_exposure_fit"
  result
}

#' @export
print.nbd_exposure <- function(x, ...) {
  cat("Negative-Binomial exposure-count approximation\n")
  cat(sprintf("Mean=%.4f | size=%s | reach=%.2f%% | average frequency=%.3f\n",
              x$mean_contacts,
              if (is.infinite(x$size)) "Inf (Poisson)" else sprintf("%.4f", x$size),
              x$reach$percent, x$average_frequency))
  cat("Scope:", x$diagnostics$scope, "\n")
  invisible(x)
}

#' @export
print.nbd_exposure_fit <- function(x, ...) {
  cat("Fitted Negative-Binomial exposure-count model\n")
  cat(sprintf("n=%d | mean=%.4f | variance=%.4f | size=%s\n",
              x$diagnostics$n, x$estimates$mean_contacts,
              x$diagnostics$observed_variance,
              if (is.infinite(x$estimates$size)) "Inf (Poisson)" else
                sprintf("%.4f", x$estimates$size)))
  cat(sprintf("logLik=%.3f | AIC=%.3f | converged=%s\n",
              x$diagnostics$log_likelihood, x$diagnostics$AIC,
              x$diagnostics$converged))
  invisible(x)
}
