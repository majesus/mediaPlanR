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
#' When it is useful: several models ([calc_agostini_duplication()],
#' [calc_hofmans_duplication()], the Morgensztern formula) and many real
#' sources (a panel, a syndicated survey, planning software) give only the
#' reach of a schedule, not how many times people are exposed. This function
#' completes the picture: it finds the Beta-Binomial exposure distribution
#' with that reach and the mean number of exposures implied by the audiences, so
#' that the frequency distribution, the average frequency and the effective
#' reach can be read from it. It is a bridge from a reach-only figure to a
#' distribution, not a model of duplication: the result is only as good as the
#' reach supplied and as the assumption that the Beta-Binomial shape suits the
#' schedule.
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
#' @param population Population size, in people. No audience can exceed it;
#'   otherwise the function stops with an error.
#' @param precision Convergence criterion, in people: the fit is `converged` when
#'   the fitted and the external reach differ by at most this many people. The
#'   default, `NULL`, is `1e-4 * population` (100 people for a universe of one
#'   million). It is expressed in people, so it must be adapted to the scale
#'   of `population`; with `population = 1`, for example, a value of 100 would
#'   accept any reach, which is why the default is relative. It measures the
#'   fit to the target, and it is also the margin by which a reach may lie
#'   outside the interval of the Beta-Binomial family with this mean (see
#'   below). It never widens the bounds of the schedule: those are checked with
#'   the rounding error of double arithmetic only, so a reach one person below
#'   the largest audience, or above the population or the total number of
#'   contacts, is rejected whatever `precision` is.
#' @param max_iter Maximum number of iterations of the root finder.
#'
#' @details
#' With \eqn{n_i} insertions of audience \eqn{A_i} in vehicle \eqn{i}, the
#' insertion-weighted mean exposure probability is
#' \eqn{\bar{p} = \sum_{i=1}^{m} n_i A_i / (P \sum_{i=1}^{m} n_i)}, where
#' \eqn{m} is the number of vehicles and \eqn{P} is the
#' population (with the audiences \eqn{A_i} in people). The Beta-Binomial has
#' \eqn{N = \sum_{i=1}^{m} n_i} trials and shape
#' parameters \eqn{\alpha = c \bar{p}} and \eqn{\beta = c (1 - \bar{p})}, so
#' its mean is preserved for every concentration \eqn{c > 0}. The function
#' solves, with [stats::uniroot()] on the log scale of \eqn{c}, for the
#' concentration whose reach, \eqn{1 - \Pr(K = 0)}, equals the external reach.
#'
#' Two different conditions are checked, in this order. First, the external
#' reach must be logically possible for the schedule: at least the largest
#' audience of a vehicle (one insertion of that vehicle alone reaches those
#' people) and at most the smaller of the population and the total number of
#' contacts, \eqn{\sum_i n_i A_i}. A reach outside these bounds cannot occur
#' whatever the distribution, and the function stops. Second, the reach that a
#' Beta-Binomial with this mean can take lies between the polarized limit
#' (\eqn{c \to 0}), \eqn{\bar{p} P}, and the binomial limit
#' (\eqn{c \to \infty}), \eqn{(1 - (1 - \bar{p})^N) P}. A reach that is
#' possible for the schedule but outside that interval, beyond `precision`, is
#' not representable by this family and is also rejected, with a different
#' message. That interval, returned as `feasible_reach`, belongs to the family
#' with this mean and ignores the audiences of the vehicles: the interval that
#' the schedule can actually use is its intersection with the bounds of the
#' first condition, which are returned as `schedule_bounds`.
#'
#' The fitted reach is also checked against the bounds of the schedule before
#' it is returned. A limit is chosen only when the target lies at or beyond the
#' reach that the family attains at that end of the concentration range, and
#' only if that limit is itself inside the bounds of the schedule; it is not
#' chosen merely because it is within `precision` of the target. When no
#' Beta-Binomial with this mean is compatible with the schedule, the function
#' stops instead of returning an impossible fit. For two insertions of
#' audiences of 50 and 10 people in a universe of 100, for example, the family
#' covers 30 to 51 people, the schedule 50 to 60, and a reach of 50 is fitted
#' with a reach of 50, not with the polarized limit of 30. At either limit the
#' distribution is computed directly (all the mass at zero and at `N` in the
#' polarized limit; a Binomial(`N`, `mean_probability`) in the binomial limit)
#' and `alpha` and `beta` are reported as `0` or `Inf`. These values only flag
#' the limit, as `fit_type` does: they are not parameters of a proper Beta
#' distribution (the quotient `alpha / (alpha + beta)` is undefined there), and
#' the mean stays fixed at `mean_probability`.
#'
#' Fitting one distribution to the aggregate reach does not recover the
#' exposure marginals of each vehicle or the duplication between vehicles, and
#' `converged = TRUE` shows only that the numerical fit is within `precision`
#' of the reach supplied, not that the reach is coherent with the individual
#' vehicles beyond the bounds above.
#'
#' Aldás Manzano (1998) starts the original iterative procedure from an
#' arbitrary initial value of `alpha`; root finding needs no starting value.
#'
#' @return A list of class `"bbd_reach_fit"` with components:
#' \itemize{
#'   \item `parameters`: list with `alpha`, `beta`, `N` (total insertions), `m`
#'     (number of vehicles), `population`, `iterations`, `converged`, `fit_type`
#'     (`"beta_binomial"`, `"polarized_limit"` or `"binomial_limit"`),
#'     `mean_probability`, `feasible_reach` (the interval of reaches that a
#'     Beta-Binomial with this mean can take, in people) and `schedule_bounds`
#'     (the logical bounds of the schedule, in people; the reach that can be
#'     used lies in the intersection of both).
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
#' # From a reach-only formula to an exposure distribution: the reach of
#' # Hofmans' duplication model, completed with a Beta-Binomial distribution
#' data(duplication_example)
#' hofmans <- do.call(calc_hofmans_duplication, duplication_example)
#' fit_bbd_to_reach(
#'   insertions = c(1, 1, 1), audiences = duplication_example$audiences,
#'   reach = hofmans$reach$people, population = duplication_example$population
#' )
#'
#' @seealso
#' [calc_hofmans_duplication()] and [calc_agostini_duplication()], whose reach
#' this function can complete with a distribution. For models that estimate the
#' exposure distribution without an external reach, see [calc_sainsbury()] and
#' [calc_binomial()] (they need only audiences, population and insertions),
#' [calc_beta_binomial()] (also needs the reach after two insertions) and
#' [calc_metheringham()] (also needs the observed duplications).
#' @export
fit_bbd_to_reach <- function(insertions, audiences, reach, population,
                             precision = NULL, max_iter = 100) {
  assert_numeric_vector(insertions, "insertions", min = 1, integer = TRUE)
  m <- length(insertions)
  assert_numeric_vector(audiences, "audiences", min = 0, min_open = TRUE,
                        length = m)
  assert_number(population, "population", min = 0, min_open = TRUE)
  if (any(audiences > population)) {
    stop("audiences cannot exceed population.", call. = FALSE)
  }
  assert_number(reach, "reach", min = 0, max = population, min_open = TRUE)
  if (is.null(precision)) precision <- 1e-4 * population
  assert_number(precision, "precision", min = 0, min_open = TRUE)
  assert_number(max_iter, "max_iter", min = 1, integer = TRUE)

  audience_props <- audiences / population
  N <- sum(insertions)
  mean_probability <- sum(insertions * audience_props) / N

  # Reach, in people, of the Beta-Binomial with mean `mean_probability` and
  # concentration exp(log_concentration). The probability of no exposure is
  # the product, over j = 0, ..., N - 1, of (beta + j) / (alpha + beta + j);
  # computed with log1p() it keeps its accuracy at both ends of the
  # concentration range, where the difference of log-beta functions does not.
  coverage_for_log_concentration <- function(log_concentration) {
    concentration <- exp(log_concentration)
    alpha <- concentration * mean_probability
    beta <- concentration * (1 - mean_probability)
    j <- seq_len(N) - 1
    log_zero <- sum(log1p(-alpha / (alpha + beta + j)))
    -expm1(log_zero) * population
  }

  show <- function(x) format(signif(x, 7), scientific = FALSE, trim = TRUE)
  # The bounds of the schedule are structural: they are checked with the
  # rounding error of double arithmetic only, never with `precision`, which
  # measures the fit to the target and must not widen what a schedule can reach.
  slack <- exact_constraint_slack(population)
  logical_min <- max(audiences)
  logical_max <- min(population, sum(insertions * audiences))
  if (reach < logical_min - slack) {
    stop("reach (", show(reach), " people) is smaller than the largest ",
         "audience of a vehicle (", show(logical_min), " people): a single ",
         "insertion of that vehicle already reaches that many people, so no ",
         "distribution can have this reach.", call. = FALSE)
  }
  if (reach > logical_max + slack) {
    stop("reach (", show(reach), " people) is larger than the population or ",
         "the total number of contacts of the schedule (", show(logical_max),
         "): no distribution can have this reach.", call. = FALSE)
  }

  feasible_min <- mean_probability * population
  feasible_max <- (1 - (1 - mean_probability)^N) * population
  if (reach < feasible_min - precision || reach > feasible_max + precision) {
    stop(sprintf(paste0("reach is outside the feasible Beta-Binomial interval ",
                        "[%s, %s]: it is possible for the schedule, but the ",
                        "Beta-Binomial family with this mean cannot ",
                        "represent it."),
                 show(feasible_min), show(feasible_max)), call. = FALSE)
  }

  # Range of log-concentrations searched; beyond exp(20) the Beta-Binomial
  # probabilities computed from log-beta functions lose their accuracy, and a
  # target that close to the binomial limit is treated as the limit.
  log_concentration_range <- c(-30, 20)
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

  # The fitted reach is returned only if it lies within the bounds of the
  # schedule. A limit is selected only when the target lies at or beyond the
  # reach that the root finder can resolve at that end of the concentration
  # range, never merely because the limit is within `precision` of the target.
  admissible <- function(value) {
    value >= logical_min - slack && value <= logical_max + slack
  }
  no_admissible_fit <- function(value) {
    stop(sprintf(paste0(
      "No Beta-Binomial fit with this mean is compatible with the schedule: ",
      "the closest reach that the family can take (%s people) lies outside ",
      "the bounds of the schedule [%s, %s]. Use a model that keeps the ",
      "audience of each vehicle."),
      show(value), show(logical_min), show(logical_max)), call. = FALSE)
  }

  # The coverage increases with the concentration: at its lowest end it is the
  # polarized limit and at its highest end the binomial limit. A limit is
  # selected when the target lies outside the family (by at most `precision`,
  # checked above) or within the numerical resolution of the root finder of
  # that end, and only when the limit is itself inside the bounds of the
  # schedule: it is never selected merely because it is within `precision`.
  snap <- min(precision, 1e-8 * population)
  polarized <- if (reach < feasible_min) TRUE else
    reach <= feasible_min + snap && admissible(feasible_min)
  binomial <- !polarized &&
    (if (reach > feasible_max) TRUE else
      reach >= feasible_max - snap && admissible(feasible_max))
  if (polarized) {
    if (!admissible(feasible_min)) no_admissible_fit(feasible_min)
    alpha <- 0
    beta <- 0
    distribution <- numeric(N + 1L)
    distribution[c(1L, N + 1L)] <- c(1 - mean_probability, mean_probability)
    fitted_reach <- feasible_min
    iterations <- 0L
    fit_type <- "polarized_limit"
  } else if (binomial) {
    if (!admissible(feasible_max)) no_admissible_fit(feasible_max)
    alpha <- Inf
    beta <- Inf
    distribution <- stats::dbinom(0:N, size = N, prob = mean_probability)
    fitted_reach <- feasible_max
    iterations <- 0L
    fit_type <- "binomial_limit"
  } else {
    root_tolerance <- max(min(.Machine$double.eps^0.5, precision / population),
                          .Machine$double.eps)
    root <- stats::uniroot(objective, c(log_concentration_range[1L],
                                        log_concentration_range[2L]),
                           tol = root_tolerance, maxiter = max_iter)$root
    iterations <- eval_count
    # The root is approximate. If the target lies on a bound of the schedule,
    # move the concentration, in the direction in which the coverage enters
    # the bounds, until the fitted reach is physically possible.
    fitted_reach <- coverage_for_log_concentration(root)
    step <- 1e-10
    for (move in seq_len(80L)) {
      if (admissible(fitted_reach)) break
      root <- if (fitted_reach < logical_min) {
        min(log_concentration_range[2L], root + step)
      } else {
        max(log_concentration_range[1L], root - step)
      }
      fitted_reach <- coverage_for_log_concentration(root)
      step <- 2 * step
    }
    if (!admissible(fitted_reach)) no_admissible_fit(fitted_reach)
    concentration <- exp(root)
    alpha <- concentration * mean_probability
    beta <- concentration * (1 - mean_probability)
    distribution <- dbetabinom(0:N, size = N, alpha = alpha, beta = beta)
    fit_type <- "beta_binomial"
  }
  difference <- fitted_reach - reach

  structure(list(
    parameters = list(
      alpha = alpha,
      beta = beta,
      N = N,
      m = m,
      population = population,
      iterations = iterations,
      converged = abs(difference) <= precision,
      fit_type = fit_type,
      mean_probability = mean_probability,
      feasible_reach = c(min = feasible_min, max = feasible_max),
      schedule_bounds = c(min = logical_min, max = logical_max)
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
  format_people <- function(v) format(signif(v, 6), big.mark = ",",
                                      scientific = FALSE, trim = TRUE)
  p <- x$parameters
  cat("Beta-Binomial fit to an external reach\n")
  cat("======================================\n")
  cat(sprintf("Universe: %s people | Vehicles: %d | Total insertions: %d\n",
              format_number(p$population), p$m, as.integer(p$N)))
  cat(sprintf("Fit: %s | alpha = %s | beta = %s\n", p$fit_type,
              format(p$alpha, digits = 5), format(p$beta, digits = 5)))
  cat(sprintf("External reach: %s people (%.2f%%)\n",
              format_people(x$reach$external),
              100 * x$reach$external / p$population))
  cat(sprintf("Fitted reach:   %s people (%.2f%%) | difference: %s people\n",
              format_people(x$reach$fitted),
              100 * x$reach$fitted / p$population,
              format_people(abs(x$reach$difference))))
  cat(sprintf("Iterations: %d | converged: %s\n", p$iterations,
              if (p$converged) "yes" else "no"))
  contacts <- x$distribution$contacts
  mean_contacts <- sum(contacts * x$distribution$probability)
  sd_contacts <- sqrt(sum((contacts - mean_contacts)^2 * x$distribution$probability))
  cat(sprintf("Mean exposures: %.2f | Standard deviation: %.2f | Mode: %d\n",
              mean_contacts, sd_contacts, which.max(x$distribution$probability) - 1L))
  invisible(x)
}
