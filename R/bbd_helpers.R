# Internal helpers shared by the Beta-Binomial based models (calc_beta_binomial,
# calc_metheringham, calc_canex, calc_csd, calc_msad, calc_cbd, calc_mbd).

# Alpha and beta of a Beta-Binomial by the method of moments, from the reach
# after one insertion (R1) and after two insertions (R2), as proportions.
# Both bounds of the admissible range are represented explicitly: the
# binomial limit (alpha = beta = Inf) when R2 equals the reach under
# independence, 2 * R1 - R1^2, and the polarized limit (alpha = beta = 0)
# when R2 = R1.
calculate_bbd_params <- function(R1, R2) {
  if (!is.numeric(R1) || !is.numeric(R2) || length(R1) != 1L ||
      length(R2) != 1L || !is.finite(R1) || !is.finite(R2) ||
      R1 <= 0 || R1 > 1 || R2 <= 0 || R2 > 1) {
    stop("R1 and R2 must be numeric and lie in the interval (0, 1].",
         call. = FALSE)
  }
  if (R2 < R1) {
    stop("R2 cannot be smaller than R1 (reach must be non-decreasing).",
         call. = FALSE)
  }

  independence_limit <- 2 * R1 - R1^2
  if (R2 > independence_limit + 1e-10) {
    stop("R2 is incompatible with a Beta-Binomial exposure model: it exceeds ",
         "the independence limit, 2 * R1 - R1^2 (the implied duplication is ",
         "lower than random duplication would produce).", call. = FALSE)
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
    stop("R1 and R2 do not imply valid Beta-Binomial parameters.",
         call. = FALSE)
  }
  list(alpha = alpha, beta = beta, p = R1, type = "beta_binomial")
}

# Mean and variance of a Beta-Binomial(k, alpha, beta).
calculate_mean_variance <- function(k, alpha, beta) {
  mean_val <- k * alpha / (alpha + beta)
  variance <- k * alpha * beta * (alpha + beta + k) /
    ((alpha + beta)^2 * (alpha + beta + 1))
  list(mean = mean_val, variance = variance)
}

# P(X = x) for a Beta-Binomial(k, alpha, beta), on the log scale for
# numerical stability.
calculate_marginal_prob <- function(x, k, alpha, beta) {
  if (x < 0 || x > k) return(0)

  log_num <- lgamma(k + 1) + lgamma(alpha + beta) +
    lgamma(alpha + x) + lgamma(beta + k - x)
  log_den <- lgamma(x + 1) + lgamma(k - x + 1) + lgamma(alpha) +
    lgamma(beta) + lgamma(alpha + beta + k)

  exp(log_num - log_den)
}

# Correlation between the exposure of two vehicles, from their joint exposure
# probability (pij) and their marginal probabilities (pi, pj); 0 when either
# vehicle has (numerically) no variance. This is the canonical correlation of
# Danaher's (1991) expansion.
calculate_duplication <- function(pij, pi, pj) {
  eps <- 1e-10
  if (pi < eps || pi > 1 - eps || pj < eps || pj > 1 - eps) return(0)

  denominator <- sqrt(pi * (1 - pi) * pj * (1 - pj))
  if (denominator < eps) return(0)

  (pij - pi * pj) / denominator
}

# Correlation matrix of the canonical expansion from the raw duplication
# matrix (proportion of the population exposed to each pair of vehicles).
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
