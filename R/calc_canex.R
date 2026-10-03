# Total negative mass above which calc_canex() warns (package decision).
canex_negative_mass_threshold <- 1e-4

# Checks the CANEX inputs and the size of the exposure-combination grid.
validate_canex_inputs <- function(vehicles_data, duplications, population) {
  required_cols <- c("k", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required_cols %in% names(vehicles_data))) {
    stop("vehicles_data must be a data frame with columns k, R1 and R2.",
         call. = FALSE)
  }
  m <- nrow(vehicles_data)
  if (m < 1L) {
    stop("vehicles_data must contain at least one vehicle.", call. = FALSE)
  }
  assert_numeric_vector(vehicles_data$k, "vehicles_data$k", min = 1,
                        integer = TRUE, length = m)
  assert_numeric_vector(vehicles_data$R1, "vehicles_data$R1", min = 0,
                        max = 1, min_open = TRUE, length = m)
  assert_numeric_vector(vehicles_data$R2, "vehicles_data$R2", min = 0,
                        max = 1, min_open = TRUE, length = m)
  if (any(vehicles_data$R2 < vehicles_data$R1)) {
    stop("R2 cannot be smaller than R1 for any vehicle.", call. = FALSE)
  }
  assert_symmetric_matrix(duplications, "duplications", m)
  off_diagonal <- duplications[row(duplications) != col(duplications)]
  if (any(off_diagonal < 0 | off_diagonal > 1)) {
    stop("Every off-diagonal duplication must be a proportion between 0 and 1.",
         call. = FALSE)
  }
  assert_number(population, "population", min = 0, min_open = TRUE)

  if (m >= 2L) {
    for (i in seq_len(m - 1L)) {
      for (j in (i + 1L):m) {
        pij <- duplications[i, j]
        lower <- max(0, vehicles_data$R1[i] + vehicles_data$R1[j] - 1)
        upper <- min(vehicles_data$R1[i], vehicles_data$R1[j])
        slack <- 1e-10 * upper
        if (pij < lower - slack || pij > upper + slack) {
          stop(sprintf("Duplication [%d,%d] is outside its Frechet bounds [%.6f, %.6f].",
                       i, j, lower, upper), call. = FALSE)
        }
      }
    }
    check_triple_feasibility(vehicles_data$R1, duplications, 1e-10)
    warn_low_duplication(vehicles_data$R1, duplications, "calc_canex")
    corr <- transform_duplications(duplications, vehicles_data)
    min_eigenvalue <- min(eigen(corr, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eigenvalue < -1e-8) {
      stop(sprintf(paste0("Pairwise duplications imply a correlation matrix ",
                          "that is not positive semidefinite (minimum ",
                          "eigenvalue %.6g)."), min_eigenvalue),
           call. = FALSE)
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
    ), call. = FALSE)
  }

  invisible(NULL)
}

#' Canonical Expansion model (CANEX)
#'
#' Implements Danaher's (1991) canonical expansion model for the reach and
#' exposure distribution of a multi-vehicle schedule. Each vehicle's exposure
#' distribution is a Beta-Binomial, which accounts for the heterogeneity of
#' individuals, and the observed duplication between vehicles enters through a
#' second-order correlation term.
#'
#' @param vehicles_data Data frame with one row per vehicle and columns
#'   \itemize{
#'     \item `k`: number of insertions in the vehicle (a positive integer);
#'     \item `R1`: reach after the first insertion, as a proportion in (0, 1];
#'     \item `R2`: cumulative reach after the second insertion, as a
#'       proportion between `R1` and the independence limit
#'       `2 * R1 - R1^2`.
#'   }
#' @param duplications Symmetric matrix whose element `[i, j]` is the
#'   proportion of the population exposed to one insertion in each of vehicles
#'   `i` and `j`. The diagonal is ignored.
#' @param population Population size, in people. It only scales the reported
#'   number of people; the default is 1,000,000.
#'
#' @details
#' The model follows these steps:
#' \enumerate{
#'   \item It computes each vehicle's Beta-Binomial parameters from `R1` and
#'   `R2` by the method of moments.
#'   \item It turns the duplications into canonical correlations,
#'   \eqn{r_{ij} = (p_{ij} - p_i p_j) / \sqrt{p_i (1 - p_i) p_j (1 - p_j)}},
#'   with \eqn{p_i} the one-insertion reach of vehicle \eqn{i} and \eqn{p_{ij}}
#'   the duplication of vehicles \eqn{i} and \eqn{j}.
#'   \item It evaluates, for every combination of exposure levels, the joint
#'   probability \eqn{\prod_i f_i(x_i) \{1 + \sum_{i<j} r_{ij} (x_i - \mu_i)
#'   (x_j - \mu_j) / (\sigma_i \sigma_j)\}}, where \eqn{f_i}, \eqn{\mu_i} and
#'   \eqn{\sigma_i^2} are the Beta-Binomial probability function, mean and
#'   variance of vehicle \eqn{i}. Only second-order terms are kept.
#'   \item It sets to zero the negative probabilities that the truncated
#'   expansion can produce, renormalizes the joint distribution to sum to one,
#'   and sums it over the total number of exposures.
#' }
#' The number of combinations, \eqn{\prod_i (k_i + 1)}, grows exponentially
#' with the number of vehicles and insertions. The model is therefore intended
#' for small and medium schedules, and the function stops with an informative
#' error above 2,000,000 combinations instead of exhausting memory.
#'
#' # Truncation of negative probabilities
#'
#' Danaher (1991, pp. 363-364) shows that the probabilities of the expansion
#' sum to one and reports, in his application, negative probabilities in a
#' small fraction of the cells, with a mean magnitude of the order of 1e-7,
#' which he sets to zero. That practice rests on the negative
#' mass being negligible. This function applies it at any size and reports the
#' truncated mass in `diagnostics$negative_mass_truncated`, together with the
#' mean number of exposures that the expansion preserves
#' (`mean_exposures_expected`, the sum of the vehicles' own means) and the one
#' of the returned distribution (`mean_exposures_result`). When the truncated
#' mass is not negligible, truncating and renormalizing can change the mean. The
#' function then issues a warning of class `mediaPlanR_canex_truncation`; the
#' threshold, a total negative mass of 1e-4, is a decision of this package and
#' not of Danaher (1991). For example, two vehicles with two insertions each,
#' `R1 = c(0.3, 0.2)`, `R2 = c(0.4, 0.3)` and zero duplication have an
#' expected mean of 1 exposure and a negative mass of 0.0076, and the
#' truncated distribution has a mean of 1.0227.
#'
#' @section Domain of validity:
#' The multivariate models ([calc_canex()], [calc_csd()], [calc_msad()],
#' [calc_cbd()] and [calc_mbd()]) take the observed one-insertion duplications
#' as given and check them against the Frechet bounds implied by the audiences
#' and, for every triple of vehicles, against joint feasibility. The published
#' methods have three limits that follow from this.
#'
#' First, Hong (1998, p. 238) notes that the models he evaluates assume a
#' duplication between vehicles not below the random one, the product of the
#' audiences, and treats null observed duplications as measurement error. With
#' a lower duplication the models can diverge considerably. The functions warn
#' (class `mediaPlanR_low_duplication`) when the canonical correlation of a
#' pair is below -0.1. That threshold is a decision of this package: the
#' published reference plans (Kim 2005, Appendix B, and Hong 1998, Appendix E;
#' 80 plans) include pairs slightly below the random duplication, with
#' canonical correlations down to -0.071, and they raise no warning. For two vehicles with two insertions each, `R1 = 0.30` and
#' `R2 = 0.45`, a duplication of zero gives a reach of 0.90 under MSAD, 0.81
#' under MBD, 0.78 under CSD and CBD, and 0.79 under CANEX. Danaher's canonical
#' expansion itself admits negative correlations. [calc_agostini_duplication()]
#' and [calc_hofmans_duplication()] can be used to impute null or missing
#' duplications.
#'
#' Second, pairwise duplications that satisfy the Frechet bounds can still be
#' impossible jointly for three or more vehicles. For every triple of vehicles
#' the functions check that the exposure to all three can have a non-negative
#' probability, given the audiences and the three pairwise duplications, and
#' stop with an error when it cannot. That condition is necessary but not
#' sufficient for four or more vehicles, for which the published methods check
#' feasibility at most to first order (Cheong 2007); [calc_mbd()] and
#' [calc_cbd()] zero any negative cell that remains, and [calc_canex()]
#' truncates it.
#'
#' Third, a low duplication between vehicles with several insertions can make
#' the target of a sequential model invalid. For two vehicles with two
#' insertions each, `R1 = 0.5` and `R2 = 0.75`, [calc_csd()] and [calc_cbd()]
#' stop with a duplication of zero, and [calc_msad()] with a duplication of zero
#' or 0.5, instead of altering an invalid target.
#'
#' @return A list of class `"reach_canex"` with components:
#' \itemize{
#'   \item `population`: the population size used.
#'   \item `reach`: list with `probability`, `percent` and `people`.
#'   \item `average_frequency`: average number of exposures among the people
#'     reached.
#'   \item `distribution`: data frame with `contacts` (total exposures, from
#'     zero), `probability`, `percent`, `people` and `cumulative_probability`
#'     (the probability of at least that many exposures).
#'   \item `diagnostics`: list with `negative_mass_truncated` (probability mass
#'     that was negative before truncation), `mass_before_renormalization` and
#'     `correlation_min_eigenvalue` (the smallest eigenvalue of the
#'     correlation matrix), `mean_exposures_expected` (the sum of the
#'     vehicles' means, preserved by the expansion) and `mean_exposures_result`
#'     (the mean of the returned distribution). They show how much the
#'     second-order approximation had to be corrected.
#' }
#'
#' @examples
#' # Two vehicles, two insertions each
#' data(canex_example)
#' result <- do.call(calc_canex, canex_example)
#' result
#' result$diagnostics
#'
#' # Inputs of the first two vehicles of Kim's (2005) worked example
#' # (Tables 4.2.2.1 and 4.2.2.2), with a population of 500,000 people
#' vehicles <- data.frame(
#'   k = c(2, 2),
#'   R1 = c(0.4902, 0.0333),
#'   R2 = c(0.5805, 0.0502)
#' )
#' duplications <- matrix(c(NA, 0.0157, 0.0157, NA), nrow = 2)
#' calc_canex(vehicles, duplications, population = 500000)$reach
#'
#' @seealso
#' [calc_beta_binomial()] for the univariate model (one vehicle), and
#' [calc_csd()] and [calc_cbd()], whose between-vehicle step reuses this
#' canonical expansion.
#'
#' @references
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the "duplication of viewing
#' law". Journal of Marketing Research, 28(3), 361-367.
#' \doi{10.1177/002224379102800311}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 56-58.
#'
#' @export
calc_canex <- function(vehicles_data, duplications, population = 1000000) {
  validate_canex_inputs(vehicles_data, duplications, population)

  m <- nrow(vehicles_data)
  correlations <- transform_duplications(duplications, vehicles_data)
  min_eigenvalue <- min(eigen(correlations, symmetric = TRUE,
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
    if (p$variance > 0) 1 / sqrt(p$variance) else 0
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

  # Adjusted joint probability, truncated at zero (see Details). The
  # diagnostics expose how much the second-order approximation had to be
  # corrected.
  raw_joint_prob <- base_prob * (1 + dup_term)
  negative_mass <- -sum(pmin(raw_joint_prob, 0))
  joint_prob <- pmax(0, raw_joint_prob)

  total_exposures <- rowSums(exposure_grid)
  agg <- stats::aggregate(joint_prob, by = list(exposures = total_exposures), FUN = sum)
  names(agg) <- c("exposures", "probability")

  report <- build_canex_result(agg, population)
  expected_mean <- sum(means)
  achieved_mean <- sum(report$distribution$contacts *
                         report$distribution$probability)
  report$diagnostics <- list(
    negative_mass_truncated = negative_mass,
    mass_before_renormalization = sum(joint_prob),
    correlation_min_eigenvalue = if (m >= 2L) min_eigenvalue else 1,
    mean_exposures_expected = expected_mean,
    mean_exposures_result = achieved_mean
  )
  # The second-order expansion preserves the mean number of exposures of every
  # vehicle; truncating negative cells and renormalizing may change it. The warning
  # threshold is a decision of this package, not of Danaher (1991).
  if (negative_mass > canex_negative_mass_threshold) {
    warning(warningCondition(
      sprintf(paste0(
        "calc_canex(): the second-order expansion produced negative ",
        "probabilities with total mass %.4g, which were truncated at zero; ",
        "the expected mean number of exposures is %.6g and the returned ",
        "mean is %.6g. The ",
        "duplications are likely outside the domain where the expansion ",
        "is accurate; see ?calc_canex."),
        negative_mass, expected_mean, achieved_mean),
      class = "mediaPlanR_canex_truncation", call = NULL))
  }
  report
}

# From a distribution by total number of exposures (columns exposures and
# probability, with an explicit zero row, not necessarily normalized), builds
# the "reach_canex" object: reach, average frequency among the people reached
# and the exposure distribution with its cumulative probabilities.
build_canex_result <- function(distribution, population) {
  distribution <- distribution[order(distribution$exposures), ]

  # The truncated canonical expansion can leave total probability mass
  # different from one after negative probabilities are set to zero; the
  # distribution is renormalized.
  total_prob <- sum(distribution$probability)
  if (!is.finite(total_prob) || total_prob <= 0) {
    stop("The CANEX distribution must have positive finite total probability.",
         call. = FALSE)
  }
  distribution$probability <- distribution$probability / total_prob
  probability <- distribution$probability
  cumulative <- rev(cumsum(rev(probability)))

  # Sum the positive-contact cells rather than subtracting from one: tiny
  # nonzero audiences otherwise lose significant digits or round to zero.
  reach_probability <- sum(probability[distribution$exposures > 0])
  average_frequency <- if (reach_probability > 0) {
    sum(distribution$exposures * probability) / reach_probability
  } else {
    0
  }

  structure(list(
    population = population,
    reach = list(
      probability = reach_probability,
      percent = 100 * reach_probability,
      people = population * reach_probability
    ),
    average_frequency = average_frequency,
    distribution = data.frame(
      contacts = distribution$exposures,
      probability = probability,
      percent = 100 * probability,
      people = population * probability,
      cumulative_probability = cumulative
    )
  ), class = "reach_canex")
}

#' @export
print.reach_canex <- function(x, ...) {
  cumulative <- list(
    min_contacts = x$distribution$contacts,
    percent = 100 * x$distribution$cumulative_probability,
    people = x$population * x$distribution$cumulative_probability
  )
  print_reach_report(
    "CANEX MODEL (Canonical Expansion)",
    "heterogeneous vehicles with observed duplication between vehicles",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = cumulative,
    parameters = list("Probability of 0 exposures (%)" =
                        x$distribution$percent[1L])
  )
  invisible(x)
}
