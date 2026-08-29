poisson_binomial_distribution <- function(probabilities) {
  if (!is.numeric(probabilities) || anyNA(probabilities) ||
      any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
    stop("probabilities must be finite values between zero and one", call. = FALSE)
  }
  distribution <- 1
  for (p in probabilities) {
    distribution <- c(distribution * (1 - p), 0) + c(0, distribution * p)
  }
  distribution / sum(distribution)
}

new_reach_result <- function(probability, population, model, parameters = list()) {
  if (!is.numeric(probability) || length(probability) < 1L ||
      anyNA(probability) || any(!is.finite(probability)) || any(probability < -1e-12)) {
    stop("The model produced an invalid contact distribution", call. = FALSE)
  }
  probability <- pmax(probability, 0)
  probability <- probability / sum(probability)
  contacts <- seq_along(probability) - 1L
  reach_probability <- 1 - probability[1L]
  mean_contacts_population <- sum(contacts * probability)
  average_frequency <- if (reach_probability > 0) {
    mean_contacts_population / reach_probability
  } else 0
  cumulative <- rev(cumsum(rev(probability)))
  structure(list(
    model = model,
    population = population,
    reach = data.frame(
      probability = reach_probability,
      percent = 100 * reach_probability,
      people = population * reach_probability
    ),
    distribution = data.frame(
      contacts = contacts,
      probability = probability,
      percent = 100 * probability,
      people = population * probability
    ),
    cumulative = data.frame(
      min_contacts = contacts,
      probability = cumulative,
      percent = 100 * cumulative,
      people = population * cumulative
    ),
    average_frequency = average_frequency,
    parameters = parameters
  ), class = "media_reach")
}

#' Estimate reach and contact distribution for a media plan
#'
#' A `media_plan`-native front end for `calc_sainsbury()`/`calc_binomial()`:
#' reads `audience`, `insertions`, and `population` from `plan` and calls the
#' requested model directly, so the two share a single implementation.
#'
#' @param plan A `media_plan` object.
#' @param model `sainsbury` (default) runs [calc_sainsbury()]: heterogeneous
#'   vehicle probabilities, combined via the exact Poisson-binomial
#'   convolution. `binomial` runs [calc_binomial()]: every vehicle is treated
#'   as sharing the plan's average probability. Both assume random
#'   duplication *and* random accumulation (a repeat insertion in the same
#'   vehicle is treated as independent, exactly like an insertion in a
#'   different vehicle) -- see `vignette("mediaPlanR-intro")`.
#'
#' @return A `media_reach` object with a complete zero-to-N distribution.
#'
#' @seealso [calc_sainsbury()] and [calc_binomial()], called directly by this
#'   function. For the experimental Negative-Binomial approximation, call
#'   [nbd_exposure_distribution()] directly with your own `mean_contacts` --
#'   it is scoped to continuous exposure processes, not finite insertion
#'   schedules, so it is not offered here or in [optimize_media_plan()].
#'
#' @export
estimate_reach <- function(plan, model = c("sainsbury", "binomial")) {
  assert_media_plan(plan)
  model <- match.arg(model)
  d <- plan$data
  classical <- if (model == "sainsbury") {
    calc_sainsbury(d$audience, plan$population, d$insertions)
  } else {
    calc_binomial(d$audience, plan$population, d$insertions)
  }
  probability <- c(1 - classical$reach$percent / 100, classical$distribution$percent / 100)
  parameters <- if (model == "binomial") {
    list(n = sum(d$insertions), p = classical$mean_probability)
  } else {
    list(opportunities = sum(d$insertions))
  }
  new_reach_result(probability, plan$population, model, parameters)
}

#' @export
print.media_reach <- function(x, ...) {
  cat(sprintf("Reach model: %s\n", x$model))
  cat(sprintf("Reach: %.2f%% (%.0f people) | Average frequency: %.3f\n",
              x$reach$percent, x$reach$people, x$average_frequency))
  invisible(x)
}

#' Compare reach estimates under several models
#'
#' @param plan A `media_plan` object.
#' @param models Character vector containing `sainsbury` and/or `binomial`.
#'   See `estimate_reach()`.
#' @return A data frame with one row per model.
#' @export
compare_reach_models <- function(plan, models = c("sainsbury", "binomial")) {
  allowed <- c("sainsbury", "binomial")
  if (!length(models) || any(!models %in% allowed)) {
    stop("Unknown reach model", call. = FALSE)
  }
  results <- lapply(models, function(model) estimate_reach(plan, model))
  data.frame(
    model = models,
    reach_probability = vapply(results, function(x) x$reach$probability, numeric(1)),
    reach_percent = vapply(results, function(x) x$reach$percent, numeric(1)),
    reach_people = vapply(results, function(x) x$reach$people, numeric(1)),
    average_frequency = vapply(results, function(x) x$average_frequency, numeric(1)),
    stringsAsFactors = FALSE
  )
}
