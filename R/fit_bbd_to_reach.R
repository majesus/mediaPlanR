#' Fit a Beta-Binomial distribution to an external reach estimate
#'
#' Fits one Beta-Binomial exposure distribution to a whole schedule so that it
#' preserves the insertion-weighted mean exposure probability and reproduces
#' a schedule reach supplied by the analyst. The reach can come from an ad hoc
#' formula such as [calc_hofmans_duplication()] or the Morgensztern formula, or
#' from any other independently justified estimator. This is the "estimation
#' by mean and zeros" step of the Hofmans Beta-Binomial procedure; it is not
#' the Morgensztern Sequential Aggregation Distribution ([calc_msad()]).
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.3.1.10 (the Hofmans Beta-Binomial method of Leckenby and Boyd,
#' pages 208-209) and Section 3.3.2.2 (the Morgensztern reach formula).
#'
#' @param insertions Numeric vector with the number of insertions of each
#'   vehicle (positive integers).
#' @param audiences Numeric vector with the audience of each vehicle, in people
#'   per insertion.
#' @param reach External schedule reach, in people.
#' @param universe Universe size, in people.
#' @param precision Convergence criterion, in people. The default is 100.
#' @param max_iter Maximum number of iterations of the root finder.
#'
#' @details
#' With \eqn{n_i} insertions of audience \eqn{A_i} in vehicle \eqn{i}, the
#' insertion-weighted mean exposure probability is
#' \eqn{\bar{p} = \sum_i n_i A_i / (U \sum_i n_i)}, where \eqn{U} is the
#' universe. The Beta-Binomial has \eqn{N = \sum_i n_i} trials and shape
#' parameters \eqn{\alpha = c \bar{p}} and \eqn{\beta = c (1 - \bar{p})}, so
#' its mean is preserved for every concentration \eqn{c > 0}. The function
#' solves, with [stats::uniroot()] on the log scale of \eqn{c}, for the
#' concentration whose reach, \eqn{1 - P(K = 0)}, equals the external reach.
#'
#' The reach a Beta-Binomial with this mean can take lies between the
#' polarized limit (\eqn{c \to 0}), \eqn{\bar{p} U}, and the binomial limit
#' (\eqn{c \to \infty}), \eqn{(1 - (1 - \bar{p})^N) U}. An external reach
#' outside that interval, beyond `precision`, is rejected. At either limit the
#' distribution is computed directly and `alpha` and `beta` are `0` or `Inf`.
#'
#' Aldás Manzano (1998) starts the original iterative procedure from an
#' arbitrary initial value of `alpha`; root finding needs no starting value.
#'
#' @return A list of class `"bbd_reach_fit"` with components:
#' \itemize{
#'   \item `parameters`: list with `alpha`, `beta`, `N` (total insertions), `m`
#'     (number of vehicles), `universe`, `iterations`, `converged`, `fit_type`
#'     (`"beta_binomial"`, `"polarized_limit"` or `"binomial_limit"`),
#'     `mean_probability` and `feasible_reach` (the admissible interval, in
#'     people).
#'   \item `reach`: list with the `external` reach, the `fitted` Beta-Binomial
#'     reach and their `difference`, in people.
#'   \item `distribution`: data frame with `contacts` (0 to `N`), `probability`
#'     and `cumulative_probability`.
#'   \item `iteration_history`: data frame with the evaluations of the root
#'     finder.
#' }
#'
#' @examples
#' data(bbd_reach_example)
#' fit <- do.call(fit_bbd_to_reach, bbd_reach_example)
#' fit
#'
#' @seealso
#' [calc_beta_binomial()], [calc_sainsbury()], [calc_binomial()] and
#' [calc_metheringham()], which estimate the Beta-Binomial or the exposure
#' distribution from audiences alone.
#' @importFrom extraDistr dbbinom
#' @export
fit_bbd_to_reach <- function(insertions, audiences, reach, universe,
                             precision = 100, max_iter = 100) {
  assert_numeric_vector(insertions, "insertions", min = 1, integer = TRUE)
  m <- length(insertions)
  assert_numeric_vector(audiences, "audiences", min = 0, min_open = TRUE,
                        length = m)
  assert_number(universe, "universe", min = 0, min_open = TRUE)
  if (any(audiences > universe)) {
    stop("audiences cannot exceed universe.", call. = FALSE)
  }
  assert_number(reach, "reach", min = 0, max = universe, min_open = TRUE)
  assert_number(precision, "precision", min = 0, min_open = TRUE)
  assert_number(max_iter, "max_iter", min = 1, integer = TRUE)

  audience_props <- audiences / universe
  N <- sum(insertions)
  mean_probability <- sum(insertions * audience_props) / N

  coverage_for_log_concentration <- function(log_concentration) {
    concentration <- exp(log_concentration)
    alpha <- concentration * mean_probability
    beta <- concentration * (1 - mean_probability)
    (1 - extraDistr::dbbinom(0, size = N, alpha = alpha, beta = beta)) * universe
  }

  feasible_min <- mean_probability * universe
  feasible_max <- (1 - (1 - mean_probability)^N) * universe
  if (reach < feasible_min - precision || reach > feasible_max + precision) {
    stop(sprintf("reach is outside the feasible Beta-Binomial interval [%.0f, %.0f].",
                 feasible_min, feasible_max), call. = FALSE)
  }

  history <- data.frame(
    iteration = integer(), alpha = numeric(), beta = numeric(),
    coverage_bbd = numeric(), difference = numeric()
  )
  eval_count <- 0L
  objective <- function(log_concentration) {
    eval_count <<- eval_count + 1L
    concentration <- exp(log_concentration)
    coverage <- coverage_for_log_concentration(log_concentration)
    history <<- rbind(history, data.frame(
      iteration = eval_count,
      alpha = concentration * mean_probability,
      beta = concentration * (1 - mean_probability),
      coverage_bbd = coverage, difference = coverage - reach
    ))
    coverage - reach
  }

  if (abs(feasible_min - reach) <= precision) {
    alpha <- 0
    beta <- 0
    distribution <- numeric(N + 1L)
    distribution[c(1L, N + 1L)] <- c(1 - mean_probability, mean_probability)
    fitted_reach <- feasible_min
    iterations <- 0L
    fit_type <- "polarized_limit"
  } else if (abs(feasible_max - reach) <= precision) {
    alpha <- Inf
    beta <- Inf
    distribution <- stats::dbinom(0:N, size = N, prob = mean_probability)
    fitted_reach <- feasible_max
    iterations <- 0L
    fit_type <- "binomial_limit"
  } else {
    root <- stats::uniroot(objective, c(-30, 30),
                           tol = max(.Machine$double.eps^0.5, precision / universe),
                           maxiter = max_iter)$root
    concentration <- exp(root)
    alpha <- concentration * mean_probability
    beta <- concentration * (1 - mean_probability)
    fitted_reach <- coverage_for_log_concentration(root)
    iterations <- eval_count
    distribution <- extraDistr::dbbinom(0:N, size = N, alpha = alpha, beta = beta)
    fit_type <- "beta_binomial"
  }
  difference <- fitted_reach - reach

  structure(list(
    parameters = list(
      alpha = alpha,
      beta = beta,
      N = N,
      m = m,
      universe = universe,
      iterations = iterations,
      converged = abs(difference) <= precision,
      fit_type = fit_type,
      mean_probability = mean_probability,
      feasible_reach = c(min = feasible_min, max = feasible_max)
    ),
    reach = list(external = reach, fitted = fitted_reach, difference = difference),
    distribution = data.frame(
      contacts = 0:N,
      probability = distribution,
      cumulative_probability = rev(cumsum(rev(distribution)))
    ),
    iteration_history = history
  ), class = "bbd_reach_fit")
}

#' @export
print.bbd_reach_fit <- function(x, ...) {
  format_number <- function(v) format(v, big.mark = ",", scientific = FALSE)
  p <- x$parameters
  cat("Beta-Binomial fit to an external reach\n")
  cat("======================================\n")
  cat(sprintf("Universe: %s people | Vehicles: %d | Total insertions: %d\n",
              format_number(p$universe), p$m, as.integer(p$N)))
  cat(sprintf("Fit: %s | alpha = %s | beta = %s\n", p$fit_type,
              format(p$alpha, digits = 5), format(p$beta, digits = 5)))
  cat(sprintf("External reach: %s people (%.2f%%)\n",
              format_number(x$reach$external),
              100 * x$reach$external / p$universe))
  cat(sprintf("Fitted reach:   %s people (%.2f%%) | difference: %s people\n",
              format_number(round(x$reach$fitted)),
              100 * x$reach$fitted / p$universe,
              format_number(round(abs(x$reach$difference)))))
  cat(sprintf("Iterations: %d | converged: %s\n", p$iterations,
              if (p$converged) "yes" else "no"))
  contacts <- x$distribution$contacts
  mean_contacts <- sum(contacts * x$distribution$probability)
  sd_contacts <- sqrt(sum((contacts - mean_contacts)^2 * x$distribution$probability))
  cat(sprintf("Mean exposures: %.2f | Standard deviation: %.2f | Mode: %d\n",
              mean_contacts, sd_contacts, which.max(x$distribution$probability) - 1L))
  invisible(x)
}
