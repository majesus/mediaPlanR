utils::globalVariables(".data")

# Internal helpers shared by the Beta-Binomial based models (calc_beta_binomial,
# calc_metheringham, calc_canex, calc_csd, calc_msad, calc_cbd, calc_mbd).

# Alpha and beta of a Beta-Binomial by the method of moments, from the reach
# after one insertion (R1) and after two insertions (R2), as proportions.
# Both bounds of the admissible range are represented explicitly: the
# binomial limit (alpha = beta = Inf) when R2 equals the reach under
# independence, 2 * R1 - R1^2, and the polarized limit (alpha = beta = 0)
# when R2 = R1. The limits are recognized on the scale of the variance of the
# one-insertion exposure, R1 * (1 - R1), through
# theta = (2 * R1 - R1^2 - R2) / (R1 * (1 - R1)) = 1 / (alpha + beta + 1),
# which lies in [0, 1]; the tolerance below is therefore relative, and does not
# depend on the magnitude of R1.
calculate_bbd_params <- function(R1, R2) {
  if (!is.numeric(R1) || !is.numeric(R2) || length(R1) != 1L ||
      length(R2) != 1L || !is.finite(R1) || !is.finite(R2) ||
      R1 <= 0 || R1 > 1 || R2 <= 0 || R2 > 1) {
    stop("R1 and R2 must be numeric and lie in the interval (0, 1]. They are ",
         "reach proportions, for example 0.30 for 30%, not percentages.",
         call. = FALSE)
  }
  if (R2 < R1) {
    stop("R2 cannot be smaller than R1 (reach must be non-decreasing). R2 is ",
         "the cumulative reach after two insertions of the same vehicle, so it ",
         "must be at least R1 (here R1 = ", format(R1, digits = 6), ").",
         call. = FALSE)
  }

  relative_tolerance <- 1e-9
  variance_scale <- R1 * (1 - R1)
  denom <- 2 * R1 - R2 - R1^2
  if (variance_scale == 0) {
    # R1 = 1: every person is reached by the first insertion.
    return(list(alpha = Inf, beta = Inf, p = R1, type = "binomial_limit"))
  }
  theta <- denom / variance_scale
  if (theta < -relative_tolerance) {
    stop("R2 is incompatible with a Beta-Binomial exposure model: it exceeds ",
         "the independence limit, 2 * R1 - R1^2 (the implied duplication is ",
         "lower than random duplication would produce). For R1 = ",
         format(R1, digits = 6), ", R2 must lie between ", format(R1, digits = 6),
         " and ", format(2 * R1 - R1^2, digits = 6), ". Check that R2 is the ",
         "cumulative reach after two insertions of the vehicle, expressed as a ",
         "proportion.", call. = FALSE)
  }
  if (theta < relative_tolerance) {
    return(list(alpha = Inf, beta = Inf, p = R1, type = "binomial_limit"))
  }
  if ((R2 - R1) / variance_scale < relative_tolerance) {
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
# vehicle has exactly no variance (a probability of zero or one). This is the
# canonical correlation of Danaher's (1991) expansion. No absolute threshold is
# used, so that a perfect coincidence between two very small audiences is not
# mistaken for independence.
calculate_duplication <- function(pij, pi, pj) {
  if (!(pi > 0 && pi < 1 && pj > 0 && pj < 1)) return(0)

  denominator <- sqrt(pi * (1 - pi)) * sqrt(pj * (1 - pj))
  if (!is.finite(denominator) || denominator <= 0) return(0)

  (pij - pi * pj) / denominator
}

# Beta-Binomial probability function, on the log scale for numerical
# stability. alpha and beta must be finite and positive: the binomial and
# polarized limits are handled by the callers.
dbetabinom <- function(x, size, alpha, beta) {
  exp(lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta))
}

# Necessary conditions for the joint existence of the exposures to one
# insertion of every vehicle. Each triple of binary exposures with marginals
# p_i and pairwise joint probabilities d_ij has eight cells that must be
# non-negative; they hold if and only if the triple probability t lies in
# [lower, upper], with
#   lower = max(0, d_ij + d_ik - p_i, d_ij + d_jk - p_j, d_ik + d_jk - p_k)
#   upper = min(d_ij, d_ik, d_jk, 1 - p_i - p_j - p_k + d_ij + d_ik + d_jk).
# The conditions for all triples are necessary, but not sufficient, for four or
# more vehicles. `tolerance` is relative to the smallest of the three audiences.
check_triple_feasibility <- function(R1, duplications, tolerance) {
  n <- length(R1)
  if (n < 3L) return(invisible(NULL))
  triples <- utils::combn(n, 3L)
  for (column in seq_len(ncol(triples))) {
    i <- triples[1L, column]; j <- triples[2L, column]; k <- triples[3L, column]
    dij <- duplications[i, j]; dik <- duplications[i, k]; djk <- duplications[j, k]
    lower <- max(0, dij + dik - R1[i], dij + djk - R1[j], dik + djk - R1[k])
    upper <- min(dij, dik, djk,
                 1 - R1[i] - R1[j] - R1[k] + dij + dik + djk)
    slack <- tolerance * min(R1[i], R1[j], R1[k])
    if (lower > upper + slack) {
      stop(sprintf(paste0(
        "The duplications of vehicles %d, %d and %d are jointly impossible: ",
        "their pairwise values satisfy the Frechet bounds, but the exposure ",
        "to the three vehicles would have to lie in [%.8g, %.8g], which is ",
        "empty."), i, j, k, lower, upper), call. = FALSE)
    }
  }
  invisible(NULL)
}

# Canonical correlation below which a pair of vehicles is reported as having a
# duplication materially under the random one. Package decision: Hong (1998,
# p. 238) notes that the models assume a duplication not below the random one,
# R1_i * R1_j, but the published reference data (Kim 2005, Appendix B; Hong
# 1998, Appendix E; 80 plans) include 12 pairs slightly below it, with
# canonical correlations down to -0.071. The threshold leaves those cases
# silent and reports the ones that make the models diverge (for R1 = 0.30, a
# duplication of 0.05 has correlation -0.19).
low_duplication_correlation <- -0.1

# Warns about pairs of vehicles whose duplication is materially below the
# random one (canonical correlation under `low_duplication_correlation`).
warn_low_duplication <- function(R1, duplications, caller) {
  n <- length(R1)
  low <- NULL
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      rho <- calculate_duplication(duplications[i, j], R1[i], R1[j])
      if (rho < low_duplication_correlation) {
        low <- c(low, sprintf("(%d, %d)", i, j))
      }
    }
  }
  if (length(low)) {
    warning(warningCondition(
      sprintf(paste0(
        "%s(): the duplication of vehicles %s is well below the random ",
        "duplication (the product of their audiences; canonical correlation ",
        "under %.2f). The models assume a duplication not below the random ",
        "one (Hong 1998, p. 238), and their results can diverge considerably ",
        "outside that domain; see the section 'Domain of validity' of ",
        "?calc_canex."),
        caller, paste(low, collapse = ", "), low_duplication_correlation),
      class = "mediaPlanR_low_duplication", call = NULL))
  }
  invisible(NULL)
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
