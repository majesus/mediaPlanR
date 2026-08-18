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
#' @param plan A `media_plan` object.
#' @param model Reach model. `independent` is the exact Poisson-binomial model
#'   for independent opportunities; `binomial` uses their average probability;
#'   `nbd` is an experimental unbounded count approximation and requires `k`.
#' @param k Positive NBD heterogeneity parameter.
#'
#' @return A `media_reach` object with a complete zero-to-N distribution.
#' @export
estimate_reach <- function(plan, model = c("independent", "binomial", "nbd"),
                           k = NULL) {
  assert_media_plan(plan)
  model <- match.arg(model)
  d <- plan$data
  probabilities <- rep(d$audience / plan$population, d$insertions)
  if (!length(probabilities)) {
    return(new_reach_result(1, plan$population, model))
  }

  if (model == "independent") {
    probability <- poisson_binomial_distribution(probabilities)
    parameters <- list(opportunities = length(probabilities))
  } else if (model == "binomial") {
    n <- length(probabilities)
    p <- mean(probabilities)
    probability <- stats::dbinom(0:n, size = n, prob = p)
    parameters <- list(n = n, p = p)
  } else {
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
      stop("k must be one positive finite number for the NBD model", call. = FALSE)
    }
    mean_contacts <- sum(probabilities)
    max_contacts <- length(probabilities)
    nbd_model <- nbd_exposure_distribution(
      mean_contacts = mean_contacts,
      size = k,
      report_max = max_contacts,
      opportunities = max_contacts
    )
    probability <- nbd_model$distribution$probability
    parameters <- list(mean_contacts = mean_contacts, k = k,
                       last_bin_is_open = TRUE,
                       experimental = TRUE,
                       scope = nbd_model$diagnostics$scope,
                       probability_above_opportunities =
                         nbd_model$diagnostics$probability_above_opportunities)
  }
  result <- new_reach_result(probability, plan$population, model, parameters)
  if (model == "nbd" && result$reach$probability > 0) {
    # The final displayed bin is open-ended, so its label cannot be used to
    # recover the exact mean. The NBD mean is known analytically.
    result$average_frequency <- mean_contacts / result$reach$probability
  }
  result
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
#' @param models Character vector containing `independent`, `binomial`, or
#'   `nbd`.
#' @param k NBD heterogeneity parameter when `nbd` is requested.
#' @return A data frame with one row per model.
#' @export
compare_reach_models <- function(plan,
                                 models = c("independent", "binomial"),
                                 k = NULL) {
  allowed <- c("independent", "binomial", "nbd")
  if (!length(models) || any(!models %in% allowed)) {
    stop("Unknown reach model", call. = FALSE)
  }
  results <- lapply(models, function(model) estimate_reach(plan, model, k = k))
  data.frame(
    model = models,
    reach_probability = vapply(results, function(x) x$reach$probability, numeric(1)),
    reach_percent = vapply(results, function(x) x$reach$percent, numeric(1)),
    reach_people = vapply(results, function(x) x$reach$people, numeric(1)),
    average_frequency = vapply(results, function(x) x$average_frequency, numeric(1)),
    stringsAsFactors = FALSE
  )
}
