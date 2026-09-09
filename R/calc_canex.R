# Computes alpha and beta of the Beta-Binomial by the method of moments from
# R1 and R2. Explicitly represents the binomial limit (alpha = beta = Inf,
# when R2 matches the reach under independence) and the polarized limit
# (alpha = beta = 0, when R2 = R1). Used internally by: calc_canex(),
# calc_csd(), calc_mbd(), calc_msad().
calculate_bbd_params <- function(R1, R2) {
  if (!is.numeric(R1) || !is.numeric(R2) || length(R1) != 1L ||
      length(R2) != 1L || !is.finite(R1) || !is.finite(R2) ||
      R1 <= 0 || R1 > 1 || R2 <= 0 || R2 > 1) {
    stop("R1 and R2 must be numeric and lie in the interval (0, 1]")
  }
  if (R2 < R1) {
    stop("R2 cannot be smaller than R1 (reach must be non-decreasing)")
  }

  independence_limit <- 2 * R1 - R1^2
  if (R2 > independence_limit + 1e-10) {
    stop("R2 is incompatible with a Beta-Binomial exposure model: it exceeds the independence limit")
  }

  denom <- 2 * R1 - R2 - R1^2
  if (abs(denom) < 1e-9) {
    return(list(alpha = Inf, beta = Inf, p = R1, type = "binomial_limit"))
  }
  if (abs(R2 - R1) < 1e-10) {
    return(list(alpha = 0, beta = 0, p = R1, type = "polarized_limit"))
  }

  alpha <- R1 * (R2 - R1) / denom
  beta <- alpha * (1 - R1) / R1
  if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
    stop("R1 and R2 do not imply valid Beta-Binomial parameters")
  }
  list(alpha = alpha, beta = beta, p = R1, type = "beta_binomial")
}

# Mean and variance of a Beta-Binomial(k, alpha, beta). Used internally.
calculate_mean_variance <- function(k, alpha, beta) {
  mean_val <- k * alpha / (alpha + beta)
  variance <- k * alpha * beta * (alpha + beta + k) /
    ((alpha + beta)^2 * (alpha + beta + 1))
  list(mean = mean_val, variance = variance)
}

# P(X = x) for a Beta-Binomial(k, alpha, beta), on the log scale for
# numerical stability. Used internally.
calculate_marginal_prob <- function(x, k, alpha, beta) {
  if (x < 0 || x > k) return(0)

  log_num <- lgamma(k + 1) + lgamma(alpha + beta) +
    lgamma(alpha + x) + lgamma(beta + k - x)
  log_den <- lgamma(x + 1) + lgamma(k - x + 1) + lgamma(alpha) +
    lgamma(beta) + lgamma(alpha + beta + k)

  exp(log_num - log_den)
}

# Correlation coefficient between exposure to two vehicles, from their joint
# exposure probability (pij) and their marginal probabilities (pi, pj); 0 if
# either vehicle's variance is zero. Used internally.
calculate_duplication <- function(pij, pi, pj) {
  eps <- 1e-10
  if (pi < eps || pi > 1 - eps || pj < eps || pj > 1 - eps) return(0)

  denominator <- sqrt(pi * (1 - pi) * pj * (1 - pj))
  if (denominator < eps) return(0)

  (pij - pi * pj) / denominator
}

# Converts the raw duplication matrix (proportion of the population
# simultaneously exposed to each pair of vehicles) into the correlation-
# coefficient matrix that the CANEX model uses as its joint-probability
# adjustment term. Used internally.
transform_duplications <- function(duplications, vehicles_data) {
  m <- nrow(duplications)
  correlations <- diag(1, m)

  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i != j) {
        correlations[i, j] <- calculate_duplication(
          duplications[i, j], vehicles_data$R1[i], vehicles_data$R1[j]
        )
      }
    }
  }
  correlations
}

#' @encoding UTF-8
#' @title Validate CANEX model inputs
#' @description Checks that the vehicle data, the duplication matrix, and the
#' population meet the minimum CANEX model requirements, and estimates the
#' size of the exposure-combination grid that will need to be evaluated.
#' @param vehicles_data Data frame with columns k, R1, R2
#' @param duplications Square matrix of raw duplications
#' @param population Positive integer. Target population size
#' @return Invisibly \code{NULL}. Called for its side effects (raises an
#' error if any requirement is not met).
#' @keywords internal
#' @noRd
validate_canex_inputs <- function(vehicles_data, duplications, population) {
  required_cols <- c("k", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required_cols %in% names(vehicles_data))) {
    stop("vehicles_data must be a data.frame with columns k, R1 and R2")
  }
  m <- nrow(vehicles_data)
  if (m < 1) {
    stop("vehicles_data must contain at least one vehicle")
  }
  if (any(vehicles_data$k < 1) || any(vehicles_data$k != round(vehicles_data$k))) {
    stop("k must be a positive integer for each vehicle")
  }
  if (any(vehicles_data$R1 <= 0) || any(vehicles_data$R1 > 1) ||
      any(vehicles_data$R2 <= 0) || any(vehicles_data$R2 > 1)) {
    stop("R1 and R2 must lie in the interval (0, 1] for every vehicle")
  }
  if (any(vehicles_data$R2 < vehicles_data$R1)) {
    stop("R2 cannot be smaller than R1 for any vehicle")
  }
  if (!is.matrix(duplications) || nrow(duplications) != m || ncol(duplications) != m) {
    stop("duplications must be a square matrix with dimension equal to the number of vehicles (", m, ")")
  }
  if (any(duplications < 0 | duplications > 1)) {
    stop("Every value in the duplication matrix must be between 0 and 1")
  }
  if (!isTRUE(all.equal(duplications, t(duplications), check.attributes = FALSE))) {
    warning("duplications is not symmetric; its upper triangle will be used")
  }
  if (!is.numeric(population) || length(population) != 1 || population <= 0) {
    stop("population must be a single positive number")
  }

  if (m >= 2L) {
    for (i in seq_len(m - 1L)) {
      for (j in (i + 1L):m) {
        pij <- duplications[i, j]
        lower <- max(0, vehicles_data$R1[i] + vehicles_data$R1[j] - 1)
        upper <- min(vehicles_data$R1[i], vehicles_data$R1[j])
        if (pij < lower - 1e-10 || pij > upper + 1e-10) {
          stop(sprintf("Duplication [%d,%d] is outside its feasible Frechet bounds [%.6f, %.6f]",
                       i, j, lower, upper), call. = FALSE)
        }
      }
    }
    corr <- transform_duplications(duplications, vehicles_data)
    corr[lower.tri(corr)] <- t(corr)[lower.tri(corr)]
    min_eigenvalue <- min(eigen(corr, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eigenvalue < -1e-8) {
      stop(sprintf("Pairwise duplications imply a non-positive-semidefinite correlation matrix (minimum eigenvalue %.6g)",
                   min_eigenvalue), call. = FALSE)
    }
  }

  total_combinations <- prod(vehicles_data$k + 1)
  if (total_combinations > 2e6) {
    stop(sprintf(
      paste(
        "The number of exposure combinations (%.0f) exceeds the practical",
        "computation limit (2,000,000). Reduce the number of vehicles or",
        "the number of insertions (k) per vehicle; this is a known",
        "computational limit of the CANEX model for large schedules."
      ),
      total_combinations
    ))
  }

  invisible(NULL)
}

#' @encoding UTF-8
#' @title Calculate the CANEX model (Canonical Expansion Model)
#' @description Implements Danaher's (1991) canonical expansion model to
#' calculate the reach and frequency distribution of a multi-vehicle
#' schedule, explicitly accounting for each vehicle's exposure heterogeneity
#' (via Beta-Binomial) and the observed duplications between vehicles (via a
#' second-order correlation term).
#'
#' @param vehicles_data Data frame with each media vehicle's data:
#' \itemize{
#'   \item k: Number of insertions for the vehicle (positive integer)
#'   \item R1: Reach after the first insertion (0-1)
#'   \item R2: Reach after the second insertion (0-1)
#' }
#' @param duplications Square matrix where element \verb{[i,j]} is the
#' proportion of the population simultaneously exposed to vehicles i and j
#' @param population Integer. Target population size (defaults to 1,000,000)
#'
#' @details
#' The model follows these steps:
#' \enumerate{
#'   \item Computes each vehicle's Beta-Binomial parameters (alpha, beta)
#'   \item Transforms the raw duplication matrix into correlations
#'   \item Generates the joint exposure probability distribution, truncating
#'   to zero the negative probabilities that the truncated canonical
#'   expansion can produce, and renormalizing the result so total
#'   probability mass sums back to 1
#'   \item Aggregates the joint distribution by total number of exposures and
#'   calculates the reach and frequency metrics
#' }
#'
#' The number of exposure combinations to evaluate grows exponentially with
#' the number of vehicles and insertions (\code{prod(k_i + 1)}), so the model
#' is intended for small- or medium-sized schedules; for large schedules the
#' function stops with an informative error instead of exhausting available
#' memory.
#'
#' @return An object of class \code{"reach_canex"}: a list with components:
#' \itemize{
#'   \item total_reach: Proportion of the population reached (0-1)
#'   \item total_reach_people: Number of people reached
#'   \item distribution: Data frame with columns contacts, percent, people
#'   \item cumulative: Data frame with columns min_contacts, percent, people
#'   \item stats: List with avg_contacts (average exposures among those
#'   reached) and zero_contacts_prob (probability of zero exposures)
#'   \item diagnostics: Truncated negative mass, mass before renormalization,
#'   and the smallest eigenvalue of the correlation matrix. These values let
#'   you assess how much the second-order approximation had to be corrected.
#' }
#'
#' @examples
#' # Two vehicles (values taken from a model reference case)
#' vehicles <- data.frame(
#'   k = c(2, 2),
#'   R1 = c(0.4902, 0.033),
#'   R2 = c(0.5805, 0.0502)
#' )
#' duplications <- matrix(
#'   c(1.000, 0.0157,
#'     0.0157, 1.000),
#'   nrow = 2, byrow = TRUE
#' )
#' results <- calc_canex(vehicles, duplications)
#' print(results)
#'
#' # Three vehicles with a custom population
#' vehicles2 <- data.frame(
#'   k = c(2, 2, 2),
#'   R1 = c(0.4902, 0.033, 0.0300),
#'   R2 = c(0.5805, 0.0502, 0.0371)
#' )
#' duplications2 <- matrix(
#'   c(1.000, 0.0157, 0.0139,
#'     0.0157, 1.000, 0.0003,
#'     0.0139, 0.0003, 1.000),
#'   nrow = 3, byrow = TRUE
#' )
#' results2 <- calc_canex(vehicles2, duplications2, population = 500000)
#' total_reach <- results2$total_reach
#' avg_contacts <- results2$stats$avg_contacts
#'
#' @seealso
#' \code{\link{calc_beta_binomial}} for the univariate model (a single vehicle)
#' \code{\link{calc_cbd}}, whose between-vehicle step reuses this same
#' canonical expansion, extended to insertion-level Beta-Binomial expansion
#'
#' @references
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the "duplication of viewing
#' law". Journal of Marketing Research, 28(3), 361-367.
#'
#' @importFrom stats aggregate
#' @export
calc_canex <- function(vehicles_data, duplications, population = 1000000) {
  validate_canex_inputs(vehicles_data, duplications, population)

  m <- nrow(vehicles_data)
  correlations <- transform_duplications(duplications, vehicles_data)
  correlation_matrix <- correlations
  correlation_matrix[lower.tri(correlation_matrix)] <-
    t(correlation_matrix)[lower.tri(correlation_matrix)]
  min_eigenvalue <- min(eigen(correlation_matrix, symmetric = TRUE,
                              only.values = TRUE)$values)

  bbd_params <- lapply(seq_len(m), function(i) {
    calculate_bbd_params(vehicles_data$R1[i], vehicles_data$R2[i])
  })
  mv_params <- lapply(seq_len(m), function(i) {
    params <- bbd_params[[i]]
    if (params$type == "binomial_limit") {
      list(mean = vehicles_data$k[i] * params$p,
           variance = vehicles_data$k[i] * params$p * (1 - params$p))
    } else if (params$type == "polarized_limit") {
      list(mean = vehicles_data$k[i] * params$p,
           variance = vehicles_data$k[i]^2 * params$p * (1 - params$p))
    } else {
      calculate_mean_variance(vehicles_data$k[i], params$alpha, params$beta)
    }
  })

  precalculated_marginals <- lapply(seq_len(m), function(i) {
    params <- bbd_params[[i]]
    vapply(0:vehicles_data$k[i], function(x) {
      if (params$type == "binomial_limit") {
        stats::dbinom(x, vehicles_data$k[i], params$p)
      } else if (params$type == "polarized_limit") {
        if (x == 0) 1 - params$p else if (x == vehicles_data$k[i]) params$p else 0
      } else {
        calculate_marginal_prob(x, vehicles_data$k[i], params$alpha, params$beta)
      }
    }, numeric(1))
  })

  var_sqrt_inv <- vapply(mv_params, function(p) {
    if (p$variance > 1e-9) 1 / sqrt(p$variance) else 0
  }, numeric(1))
  means <- vapply(mv_params, function(p) p$mean, numeric(1))

  exposure_grid <- as.matrix(expand.grid(lapply(vehicles_data$k, function(k) 0:k)))

  # Marginal probabilities across the whole grid (vectorized per vehicle)
  marginals_matrix <- vapply(seq_len(m), function(i) {
    precalculated_marginals[[i]][exposure_grid[, i] + 1]
  }, numeric(nrow(exposure_grid)))

  base_prob <- rep(1, nrow(exposure_grid))
  for (i in seq_len(m)) base_prob <- base_prob * marginals_matrix[, i]

  # Duplication adjustment term (second-order canonical interactions)
  z_scores <- matrix(0, nrow = nrow(exposure_grid), ncol = m)
  for (i in seq_len(m)) {
    z_scores[, i] <- (exposure_grid[, i] - means[i]) * var_sqrt_inv[i]
  }

  dup_term <- rep(0, nrow(exposure_grid))
  if (m >= 2) {
    for (i in 1:(m - 1)) {
      for (j in (i + 1):m) {
        if (var_sqrt_inv[i] != 0 && var_sqrt_inv[j] != 0) {
          dup_term <- dup_term + correlations[i, j] * z_scores[, i] * z_scores[, j]
        }
      }
    }
  }

  base_prob[!is.finite(base_prob)] <- 0
  dup_term[!is.finite(dup_term)] <- 0

  # Adjusted joint probability, truncated at 0 (see @details). Diagnostics
  # expose how much the second-order approximation had to be corrected.
  raw_joint_prob <- base_prob * (1 + dup_term)
  negative_mass <- -sum(pmin(raw_joint_prob, 0))
  joint_prob <- pmax(0, raw_joint_prob)

  total_exposures <- rowSums(exposure_grid)
  agg <- stats::aggregate(joint_prob, by = list(exposures = total_exposures), FUN = sum)
  names(agg) <- c("exposures", "probability")

  report <- calculate_metrics(agg, population)
  report$diagnostics <- list(
    negative_mass_truncated = negative_mass,
    mass_before_renormalization = sum(joint_prob),
    correlation_min_eigenvalue = if (m >= 2L) min_eigenvalue else 1
  )
  report
}

# From a probability distribution by total number of exposures (columns
# exposures, probability; not necessarily normalized to 1), calculates
# reach, the exposure distribution (and cumulative), and the CANEX model's
# summary metrics. Returns a "reach_canex" object. Used internally.
calculate_metrics <- function(distribution, population = 1000000) {
  if (!is.data.frame(distribution) ||
      !all(c("exposures", "probability") %in% names(distribution)) ||
      anyNA(distribution[c("exposures", "probability")]) ||
      any(distribution$probability < 0) || !0 %in% distribution$exposures) {
    stop("distribution must contain non-negative probabilities and an explicit zero-exposure row",
         call. = FALSE)
  }
  if (!is.numeric(population) || length(population) != 1L ||
      !is.finite(population) || population <= 0) {
    stop("population must be one positive finite number", call. = FALSE)
  }
  distribution <- distribution[order(distribution$exposures), ]

  # The truncated canonical expansion can leave total probability mass below
  # 1 after truncating negative probabilities to 0; it is renormalized so
  # the distribution sums back to 1.
  total_prob <- sum(distribution$probability)
  if (total_prob > 0 && abs(total_prob - 1) > 1e-6) {
    distribution$probability <- distribution$probability / total_prob
  }

  distribution$percent <- distribution$probability * 100
  distribution$people <- round(distribution$probability * population)

  cumulative_people <- vapply(distribution$exposures, function(n) {
    sum(distribution$people[distribution$exposures >= n])
  }, numeric(1))

  cumulative_dist <- data.frame(
    min_contacts = distribution$exposures,
    people = cumulative_people,
    percent = (cumulative_people / population) * 100
  )

  reach_prob <- 1 - distribution$probability[1]
  reach_people <- round(reach_prob * population)

  avg_contacts <- if (reach_prob > 1e-9) {
    sum(distribution$exposures * distribution$probability) / reach_prob
  } else {
    0
  }

  report <- list(
    total_reach = reach_prob,
    total_reach_people = reach_people,
    distribution = data.frame(
      contacts = distribution$exposures,
      percent = distribution$percent,
      people = distribution$people
    ),
    cumulative = data.frame(
      min_contacts = cumulative_dist$min_contacts,
      percent = cumulative_dist$percent,
      people = cumulative_dist$people
    ),
    stats = list(
      avg_contacts = avg_contacts,
      zero_contacts_prob = distribution$probability[1]
    )
  )

  class(report) <- "reach_canex"
  report
}

#' @export
print.reach_canex <- function(x, ...) {
  print_reach_report(
    "CANEX MODEL (Canonical Expansion)",
    "model that accounts for heterogeneity and duplications between vehicles",
    x$total_reach * 100, x$total_reach_people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list("Probability of 0 exposures (%)" = x$stats$zero_contacts_prob * 100)
  )
  invisible(x)
}
