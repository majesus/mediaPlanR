# Canonical (Danaher 1991) reach of the union of the vehicles `indices`, the
# last of which is the one being added to the aggregate of the others
# (`previous_reach`, the reach of that aggregate). With z_i the probability of
# zero exposures of vehicle i, mu_i and sigma_i its mean and standard
# deviation, rho_ij the canonical correlation and
#   a_m = sum over the pairs of the m vehicles of rho_ij (mu_i / sigma_i) (mu_j / sigma_j),
# the zero probability is z_1 ... z_m (1 + a_m). The reach of the union, 1 -
# z_1 ... z_m (1 + a_m), is computed from the reach of the aggregate (R_L) and
# that of the added vehicle (R_R) as R_L + R_R - D, where
#   D = R_L R_R + z_1 ... z_m d_m
# is the probability of being reached by both and d_m the terms of a_m that
# involve the added vehicle (the pairs of the aggregate are in R_L). The
# subtraction 1 - z_1 ... z_m (1 + a_m) loses the whole reach, or most of it,
# when the audiences are small and the product rounds to one. The standardized
# means are combined pairwise, so that the product of two small variances is
# never formed, and a pair of zero correlation contributes nothing.
csd_canonical_reach <- function(indices, marginals, single_reach,
                                duplications, tolerance, previous_reach) {
  means <- vapply(indices, function(i) {
    contacts <- seq_along(marginals[[i]]) - 1L
    sum(contacts * marginals[[i]])
  }, numeric(1))
  variances <- vapply(seq_along(indices), function(position) {
    i <- indices[position]
    contacts <- seq_along(marginals[[i]]) - 1L
    sum((contacts - means[position])^2 * marginals[[i]])
  }, numeric(1))
  if (any(!(variances > 0))) {
    stop("CSD requires non-degenerate vehicle exposure marginals.",
         call. = FALSE)
  }
  standardized_means <- means / sqrt(variances)

  zero_base <- prod(vapply(indices, function(i) marginals[[i]][1L],
                           numeric(1)))
  expansion_adjustment <- 0
  added_adjustment <- 0
  added_position <- length(indices)
  if (added_position >= 2L) {
    pairs <- utils::combn(seq_along(indices), 2L)
    for (column in seq_len(ncol(pairs))) {
      left_position <- pairs[1L, column]
      right_position <- pairs[2L, column]
      i <- indices[left_position]
      j <- indices[right_position]
      rho <- calculate_duplication(
        duplications[i, j], single_reach[i], single_reach[j]
      )
      if (rho == 0) next
      term <- rho * standardized_means[left_position] *
        standardized_means[right_position]
      expansion_adjustment <- expansion_adjustment + term
      if (right_position == added_position) {
        added_adjustment <- added_adjustment + term
      }
    }
  }

  zero_probability <- zero_base * (1 + expansion_adjustment)
  if (!is.finite(zero_probability) ||
      zero_probability < -tolerance || zero_probability > 1 + tolerance) {
    stop(sprintf(
      paste0("The truncated canonical expansion produced an invalid ",
             "zero-exposure probability (%.8f)."),
      zero_probability
    ), call. = FALSE)
  }
  zero_probability <- min(1, max(0, zero_probability))

  added_reach <- distribution_reach(marginals[[indices[added_position]]])
  duplicated_reach <- previous_reach * added_reach + zero_base * added_adjustment
  reach <- min(1, max(0, previous_reach + added_reach - duplicated_reach))

  list(
    reach = reach,
    zero_probability = zero_probability,
    random_zero_probability = zero_base,
    expansion_adjustment = expansion_adjustment,
    duplicated_reach = duplicated_reach
  )
}

#' Canonical Sequential Aggregation Distribution model
#'
#' Implements Kim's (2005) Canonical Sequential Aggregation Distribution
#' (CSD): the Beta-Binomial marginal of each vehicle is combined sequentially
#' with the vehicles already aggregated, while the reach at every step comes
#' from Danaher's canonical expansion, truncated to its pairwise terms (see the
#' section "Order of the expansion" of [calc_canex()]). The non-random convolution
#' preserves both marginal distributions and sets the zero-exposure cell to one
#' minus the canonical reach.
#'
#' @param vehicles_data Data frame with one row per vehicle (at least two rows)
#'   and columns `insertions` (planned insertions, a positive integer), `R1` and
#'   `R2`. `R1` is the reach after one insertion, a proportion between 0 and 1
#'   (not a percentage) and strictly between zero and one, and `R2` the
#'   cumulative reach after two insertions (the proportion exposed at least
#'   once in two insertions), a proportion with `R1 <= R2 <= 2 * R1 - R1^2`
#'   (the upper limit is the reach under independent insertions, the bound of
#'   the Beta-Binomial family; see [calc_beta_binomial()]). If `R2` is
#'   outside that range the function stops with an explanatory error. `R2` may
#'   be `NA` only when `insertions` is one.
#' @param duplications Symmetric matrix whose element `[i, j]` is the
#'   proportion of the population that is exposed to both vehicle `i` and
#'   vehicle `j` (one insertion in each), a number between 0 and 1 (not a
#'   percentage). It is not the proportion exposed to each vehicle separately.
#'   The diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (vehicles in decreasing
#'   order of one-insertion reach `R1`), `"given"` (the row order), or a
#'   permutation of the row indices. The vehicles are combined one at a time in
#'   this order. The order does not change the reach of the whole schedule
#'   but can change the exposure distribution. Use an explicit permutation to
#'   reproduce a published aggregation order, such as the one of Kim's worked
#'   example (labeled "TD forward" in the dissertation: aggregation guided by
#'   the total duplication of each pair, between- plus within-vehicle, in
#'   forward order).
#' @param population Number of people in the population (a count, not a
#'   proportion). It only converts probabilities into people: the `people`
#'   columns of the result are the probability times `population`, and with the
#'   default, 1, they equal the probability.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'   The Fréchet and triple-feasibility checks apply it relative to the smaller
#'   audience involved.
#'
#' @return A `reach_csd` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `vehicle_marginals`, the one-insertion
#'   `vehicle_reach`, the canonical `correlation_matrix`, the
#'   `aggregation_order` and `aggregation_rule`, the aggregation `steps` and
#'   numerical `diagnostics`. The diagnostics include
#'   `gross_mean_contacts`, the mean number of exposures per person that the
#'   inputs imply (the sum over vehicles of insertions times `R1`, averaged over
#'   the whole population), `distribution_mean_contacts`, the same mean computed
#'   from the returned distribution, and `mean_error`, their difference.
#'
#' @details
#' For a subset of \eqn{m} vehicles, CSD estimates the all-zero probability as
#' \deqn{P(0,\ldots,0)=\prod_{i=1}^{m} f_i(0)\left[1+\sum_{i=1}^{m-1}
#' \sum_{j=i+1}^{m}\rho_{ij}
#' \frac{(0-\mu_i)(0-\mu_j)}{\sigma_i\sigma_j}\right],}
#' where \eqn{f_i}, \eqn{\mu_i} and \eqn{\sigma_i} are the Beta-Binomial
#' probability function, mean and standard deviation of vehicle \eqn{i}, and
#' \eqn{\rho_{ij}} is the canonical correlation obtained from the observed
#' one-insertion duplication. Reach is one minus this probability. It is
#' evaluated as the reach of the vehicles already aggregated plus that of the
#' vehicle added minus the reach of both, which is the same quantity without
#' the cancellation by which one minus a product of probabilities close to one
#' loses the reach of a small audience (the reach of the schedule is the sum of
#' the positive cells of its distribution).
#'
#' The canonical expansion is a truncated approximation (pairwise terms only).
#' Unlike [calc_canex()], which truncates negative probabilities and renormalizes the
#' full joint grid, `calc_csd()` does not alter an invalid canonical target: it
#' stops if the resulting reach is not a probability or is incompatible with
#' the two margins being combined. This makes approximation failure visible to
#' the analyst.
#'
#' Kim's worked example rounds intermediate values to four decimals. Exact
#' calculations from the same inputs therefore differ by a few hundredths of a
#' percentage point from some displayed cells; the unrounded calculation is
#' returned. The inputs of that example are not distributed with the package.
#'
#' @references
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the duplication of viewing law.
#' Journal of Marketing Research, 28(3), 361-367.
#' \doi{10.1177/002224379102800311}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 78-97.
#' Handle 2152/1590 (University of Texas at Austin repository).
#'
#' @examples
#' data(csd_example)
#' result <- do.call(calc_csd, csd_example)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_canex()] for the full-grid canonical expansion,
#'   [calc_msad()] for the Morgensztern sequential alternative and
#'   [calc_cbd()] for a multivariate model of conditional expansion (not a
#'   sequential-aggregation model) that uses the same canonical expansion at
#'   the (0,1) level.
#' @inheritSection calc_canex Domain of validity
#' @section Computational limits:
#' The cells of the largest joint table (the exposure levels of the vehicles
#' already combined times those of the vehicle added) are counted from the
#' insertions before anything is allocated, and a schedule above the budget
#' is refused with an explanation. The budget is the option
#' `mediaPlanR.max_table_cells`, `1e7` cells by default (80 MB for each
#' matrix of that size); see the section "Computational limits" of [mediaPlanR].
#' @export
calc_csd <- function(vehicles_data, duplications,
                     aggregation_order = c("audience_desc", "given"),
                     population = 1, tolerance = 1e-10) {
  input <- validate_sequential_inputs(vehicles_data, duplications,
                                      aggregation_order, population, tolerance)
  n <- input$n
  insertions <- input$insertions
  R1 <- input$R1
  R2 <- input$R2
  order_index <- input$order_index
  order_rule <- input$order_rule
  check_sequential_table_size(insertions, order_index, "calc_csd")

  correlation_matrix <- diag(1, n)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      correlation_matrix[i, j] <- correlation_matrix[j, i] <-
        calculate_duplication(duplications[i, j], R1[i], R1[j])
    }
  }
  minimum_eigenvalue <- min(eigen(
    correlation_matrix, symmetric = TRUE, only.values = TRUE
  )$values)
  if (minimum_eigenvalue < -sqrt(tolerance)) {
    stop(sprintf(
      paste0("Pairwise duplications imply a non-positive-semidefinite ",
             "correlation matrix (minimum eigenvalue %.8g)."),
      minimum_eigenvalue
    ), call. = FALSE)
  }

  marginals <- lapply(seq_len(n), function(i) {
    vehicle_exposure_distribution(insertions[i], R1[i], R2[i])
  })
  vehicle_reach <- vapply(marginals, distribution_reach, numeric(1))

  current <- marginals[[order_index[1L]]]
  included <- order_index[1L]
  steps <- vector("list", n - 1L)
  for (step in 2:n) {
    added <- order_index[step]
    subset <- c(included, added)
    canonical <- csd_canonical_reach(
      subset, marginals, R1, duplications, tolerance,
      previous_reach = distribution_reach(current)
    )
    conformed <- sequential_conform_pair(
      current, marginals[[added]], canonical$reach, tolerance, "canonical",
      duplicated_reach = canonical$duplicated_reach
    )
    current <- conformed$distribution
    included <- subset
    steps[[step - 1L]] <- data.frame(
      step = step - 1L,
      added_vehicle = added,
      target_reach = canonical$reach,
      zero_probability = canonical$zero_probability,
      random_zero_probability = canonical$random_zero_probability,
      expansion_adjustment = canonical$expansion_adjustment,
      duplicated_reach = unname(conformed$diagnostics["duplicated_reach"]),
      row_margin_error = unname(conformed$diagnostics["row_margin_error"]),
      column_margin_error = unname(conformed$diagnostics["column_margin_error"])
    )
  }
  steps <- do.call(rbind, steps)
  contacts <- seq_along(current) - 1L
  cumulative <- rev(cumsum(rev(current)))
  # The reach is the sum of the positive cells, not 1 - P(0), which loses it
  # for a small audience; the average frequency, 1 plus the excess of the
  # levels above one over that reach, cannot fall below one.
  reach <- distribution_reach(current)
  average_frequency <- 1 + sum((contacts[-1L] - 1L) * current[-1L]) / reach

  result <- list(
    reach = list(
      probability = reach,
      percent = 100 * reach,
      people = population * reach
    ),
    average_frequency = average_frequency,
    distribution = data.frame(
      contacts = contacts,
      probability = current,
      percent = 100 * current,
      people = population * current,
      cumulative_probability = cumulative
    ),
    vehicle_marginals = lapply(seq_len(n), function(i) {
      data.frame(contacts = 0:insertions[i], probability = marginals[[i]])
    }),
    vehicle_reach = vehicle_reach,
    correlation_matrix = correlation_matrix,
    aggregation_order = order_index,
    aggregation_rule = order_rule,
    steps = steps,
    diagnostics = list(
      probability_sum = sum(current),
      minimum_probability = min(current),
      correlation_min_eigenvalue = minimum_eigenvalue,
      gross_mean_contacts = sum(insertions * R1),
      distribution_mean_contacts = sum(contacts * current),
      mean_error = sum(contacts * current) - sum(insertions * R1)
    )
  )
  class(result) <- "reach_csd"
  result
}

#' Print the result of CSD, MSAD, CBD or MBD
#'
#' By default, the full report used by the classical models: headline metrics,
#' model parameters, the vehicles and aggregation steps, the exposure
#' distribution, the cumulative distribution (the share of the population
#' exposed N times or more, for each N), and numerical diagnostics.
#' With `full = FALSE`, a compact summary: reach, average frequency,
#' aggregation order and numerical diagnostics.
#'
#' Every result class has its own print method, because the objects are long
#' lists and each model reports different intermediate results: CSD and MSAD
#' list the vehicles and the aggregation steps (CSD also the canonical
#' correlations between vehicles), CBD lists how each vehicle is modeled and
#' the canonical zero probability, and MBD lists the peeling steps. The four
#' methods are documented together on this page.
#'
#' @param x A result of [calc_csd()], [calc_msad()], [calc_cbd()] or
#'   [calc_mbd()].
#' @param full Logical. `TRUE` (default) prints the full report; `FALSE`
#'   prints the compact summary.
#' @param max_rows Maximum number of exposure levels listed in the full report;
#'   longer distributions are cut with a note. Use `Inf` to list all of them.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' data(csd_example)
#' csd <- do.call(calc_csd, csd_example)
#' csd
#' print(csd, full = FALSE)
#' @name print_sequential
#' @export
print.reach_csd <- function(x, full = TRUE, max_rows = 30L, ...) {
  if (isTRUE(full)) {
    print_sequential_report(
      x, "CANONICAL SEQUENTIAL AGGREGATION DISTRIBUTION (CSD)",
      paste("Beta-Binomial vehicles aggregated sequentially; the reach of",
            "every step comes from the pairwise canonical expansion"),
      parameters = list("Aggregation order" = x$aggregation_order,
                        "Aggregation rule" = x$aggregation_rule),
      tables = list(
        "Vehicles" = sequential_vehicle_table(x),
        "Aggregation steps" = sequential_step_table(x$steps),
        "Canonical correlations between vehicles" = {
          correlations <- round(x$correlation_matrix, 4L)
          dimnames(correlations) <- rep(
            list(paste0("V", seq_len(nrow(correlations)))), 2L)
          correlations
        }
      ),
      diagnostics = c(
        sequential_diagnostic_lines(x),
        sprintf("Smallest eigenvalue of the correlation matrix: %.4g",
                x$diagnostics$correlation_min_eigenvalue)
      ),
      max_rows = max_rows
    )
    return(invisible(x))
  }
  cat("Canonical Sequential Aggregation Distribution (CSD)\n")
  cat(sprintf("Reach: %.2f%% | Average frequency: %.3f\n",
              x$reach$percent, x$average_frequency))
  cat("Aggregation order:", paste(x$aggregation_order, collapse = " -> "),
      sprintf("(%s)\n", x$aggregation_rule))
  cat(sprintf("Probability sum: %.12f | Mean error: %.3g\n",
              x$diagnostics$probability_sum,
              x$diagnostics$mean_error))
  invisible(x)
}
