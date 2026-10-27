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

# Mean and variance of a Beta-Binomial(k, alpha, beta). With
# s = alpha + beta and p = alpha / s, the variance is k p (1 - p) (s + k) /
# (s + 1): the form k alpha beta (s + k) / (s^2 (s + 1)) multiplies three
# potentially huge quantities and overflows for a large, finite concentration,
# although the variance itself is a modest number.
calculate_mean_variance <- function(k, alpha, beta) {
  concentration <- alpha + beta
  probability <- alpha / concentration
  list(mean = k * probability,
       variance = k * probability * (beta / concentration) *
         (1 + (k - 1) / (concentration + 1)))
}

# P(X = x) for a Beta-Binomial(k, alpha, beta). It takes the value from the
# complete probability function (see dbetabinom()), so that every consumer
# works with one routine whose masses add up to one.
calculate_marginal_prob <- function(x, k, alpha, beta) {
  dbetabinom(x, size = k, alpha = alpha, beta = beta)
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

# Probability function of a Beta-Binomial(size, alpha, beta) at the counts `x`.
# alpha and beta must be finite and positive: the binomial and polarized limits
# are handled by the callers.
#
# The complete function, 0, ..., size, is always normalized, so that its mass
# is one and its mean is size * alpha / (alpha + beta) up to rounding. Two
# evaluations are used, each where it is the more accurate (measured against a
# 400-bit reference over audiences from 1e-6 to 0.98, one to 2,000 insertions
# and concentrations from 3e-3 to 1e8):
#  * For a concentration alpha + beta below 10, the log-beta form
#    lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta),
#    whose relative error stays below about 1e-12 there, also in the polarized
#    range (concentration near zero).
#  * Otherwise, the recurrence
#      P(x + 1) / P(x) = (size - x) / (x + 1) * (alpha + x) / (beta + size - 1 - x)
#    accumulated on the log scale. The log-beta form subtracts quantities of
#    the order of lgamma(alpha + beta) and loses digits in proportion to the
#    concentration: near the independence limit (alpha + beta of 1e8 or more)
#    its masses added up to one only to about 1e-8 and its mean was off by the
#    same amount. The recurrence keeps both within about 1e-12 at any
#    concentration, since the ratio of consecutive probabilities is a quotient
#    of numbers of the same order. When both shape parameters are at least one,
#    the ratio is evaluated as log(alpha / beta) + log1p(x / alpha) -
#    log1p((size - 1 - x) / beta), which does not subtract two large
#    logarithms.
bbd_distribution <- function(size, alpha, beta) {
  if (!is.numeric(alpha) || !is.numeric(beta) || length(alpha) != 1L ||
      length(beta) != 1L || !is.finite(alpha) || !is.finite(beta) ||
      alpha <= 0 || beta <= 0) {
    stop("Internal error: the Beta-Binomial probability function needs finite ",
         "positive shape parameters; the binomial and polarized limits must ",
         "be handled by the caller.", call. = FALSE)
  }
  if (size < 1) return(1)
  if (alpha + beta < 10) {
    counts <- 0:size
    probability <- exp(lchoose(size, counts) +
                         lbeta(counts + alpha, size - counts + beta) -
                         lbeta(alpha, beta))
  } else {
    steps <- seq_len(size) - 1
    shape_term <- if (alpha >= 1 && beta >= 1) {
      log(alpha / beta) + log1p(steps / alpha) - log1p((size - 1 - steps) / beta)
    } else {
      log(alpha + steps) - log(beta + size - 1 - steps)
    }
    log_probability <- c(0, cumsum(log((size - steps) / (steps + 1)) +
                                     shape_term))
    probability <- exp(log_probability - max(log_probability))
  }
  probability <- probability / sum(probability)
  # A rounded normalization can leave the mass just above one on some
  # platforms. Round the rescaling denominator upward in that case.
  total <- sum(probability)
  if (total > 1) {
    probability <- probability / (total * (1 + .Machine$double.eps))
  }
  probability
}

dbetabinom <- function(x, size, alpha, beta) {
  distribution <- bbd_distribution(size, alpha, beta)
  inside <- is.finite(x) & x >= 0 & x <= size & x == round(x)
  out <- numeric(length(x))
  out[inside] <- distribution[x[inside] + 1]
  out
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
# p. 238) formulates, for the models and data of his study, the hypothesis that
# the duplication is not below the random one, R1_i * R1_j (a lower duplication
# is valid data in the logical sense, bounded only by Frechet), and the
# published reference data (Kim 2005, Appendix B; Hong
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
        "under %.2f). The reference studies of these models mostly used ",
        "duplications not below the random one (Hong 1998, p. 238), and the ",
        "approximations can diverge considerably in this range, although a ",
        "lower duplication can be valid data: check how it was defined and ",
        "read the diagnostics. See the section 'Domain of validity' of ",
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
