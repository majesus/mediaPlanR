# Morgensztern reach of a subset of vehicles (Aldas Manzano, 1998, equation
# 3.106; Kim, 2005, p. 67): `single_reach` are one-insertion audiences,
# `vehicle_reach` the reach of each vehicle for its own insertions.
msad_morgensztern_reach <- function(indices, single_reach,
                                     vehicle_reach, duplications) {
  if (length(indices) == 1L) return(vehicle_reach[indices])
  total_vehicle_reach <- sum(vehicle_reach[indices])
  denominator <- total_vehicle_reach
  pairs <- utils::combn(indices, 2L)
  for (column in seq_len(ncol(pairs))) {
    i <- pairs[1L, column]
    j <- pairs[2L, column]
    duplication <- duplications[i, j]
    k_ij <- (single_reach[i] + single_reach[j]) /
      (single_reach[i] + single_reach[j] - duplication)
    denominator <- denominator +
      k_ij * duplication * vehicle_reach[i] * vehicle_reach[j] /
      (single_reach[i] * single_reach[j])
  }
  total_vehicle_reach^2 / denominator
}

#' Morgensztern Sequential Aggregation Distribution model
#'
#' Implements the MSAD structure described by Leckenby and Rice (1986) and
#' Kim (2005): each vehicle is expanded with a Beta-Binomial marginal, the
#' reach of every sub-schedule is estimated with the Morgensztern formula, and
#' the marginals are combined sequentially with a non-random,
#' margin-preserving convolution whose zero cell equals one minus that reach.
#' Kim specifies the MSAD algorithm but refers readers to Lee (1988) for its
#' complete worked numerical example; Kim's own three-vehicle example is CSD,
#' not MSAD.
#'
#' @param vehicles_data Data frame with one row per vehicle (at least two rows)
#'   and columns `insertions` (planned insertions, a positive integer), `R1` and
#'   `R2`. `R1` is the reach after one insertion, a proportion between 0 and 1
#'   (not a percentage) and strictly between zero and one, and `R2` the
#'   cumulative reach after two insertions (the proportion exposed at least
#'   once in two insertions), a proportion with `R1 <= R2 <= 2 * R1 - R1^2`. If
#'   `R2` is outside that range the function stops with an explanatory error.
#'   `R2` may be `NA` only when `insertions` is one.
#' @param duplications Symmetric matrix whose element `[i, j]` is the
#'   proportion of the population that is exposed to both vehicle `i` and
#'   vehicle `j` (one insertion in each), a number between 0 and 1 (not a
#'   percentage). It is not the proportion exposed to each vehicle separately.
#'   The diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (Kim's rule of combining
#'   the vehicle with the largest audience first: vehicles in decreasing order
#'   of one-insertion reach `R1`),
#'   `"given"` (the row order), or a permutation of the row indices. The
#'   vehicles are combined one at a time in this order. The order does not
#'   change the reach of the whole schedule but can change the exposure
#'   distribution.
#' @param population Number of people in the population (a count, not a
#'   proportion). It only converts probabilities into people: the `people`
#'   columns of the result are the probability times `population`, and with the
#'   default, 1, they equal the probability.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'   The Fréchet and triple-feasibility checks apply it relative to the smaller
#'   audience involved.
#'
#' @return A `reach_msad` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `vehicle_marginals`, the one-insertion
#'   `vehicle_reach`, the `aggregation_order` and `aggregation_rule`, the
#'   aggregation `steps` and numerical `diagnostics` (see [calc_csd()] for
#'   `gross_mean_contacts`, `distribution_mean_contacts` and `mean_error`).
#'
#' @details
#' For a subset of \eqn{m} vehicles, the Morgensztern reach is
#' \deqn{R_m = \frac{(\sum_{i=1}^{m} R_{n_i})^2}{\sum_{i=1}^{m} R_{n_i} +
#' \sum_{i=1}^{m-1} \sum_{j=i+1}^{m} K_{ij} A_{ij} R_{n_i} R_{n_j} /
#' (A_i A_j)},}
#' with \eqn{K_{ij}=(A_i+A_j)/(A_i+A_j-A_{ij})}, where \eqn{R_{n_i}} is the
#' reach of vehicle \eqn{i} for its own \eqn{n_i} insertions, \eqn{A_i} its
#' one-insertion audience and \eqn{A_{ij}} the one-insertion duplication of
#' vehicles \eqn{i} and \eqn{j}, all as proportions of the population. The
#' double sum runs over every pair of vehicles. At every aggregation step the joint table is
#' conformed to the two input marginal distributions and to the Morgensztern
#' union reach. Consequently all probabilities remain non-negative, the
#' margins are preserved, and the zero-exposure probability is exactly
#' \eqn{1 - R_m}.
#'
#' The function reproduces the three-vehicle numerical example of Lee (1988,
#' pp. 81-93; two insertions per vehicle, aggregation in the order Lee obtains
#' with his total-duplication criterion, `aggregation_order = "given"` with the
#' vehicles listed in that order) to
#' within 0.0011 in every cell of the exposure distribution, which Lee prints
#' to three decimals.
#'
#' MSAD is intended to reduce, but does not guarantee the elimination of, the
#' declining reach phenomenon (Leckenby & Rice, 1986): the reach that some
#' exposure-distribution models estimate for a schedule can fall when
#' insertions or vehicles are added, which cannot happen in reality. The reach formula can also be incompatible with the
#' supplied marginal distributions. In that case the function stops and reports
#' the feasible interval instead of silently altering the target. The
#' aggregation order can change the exposure distribution even when the final
#' schedule reach is unchanged.
#'
#' @references
#' Leckenby, J. D., & Rice, M. D. (1986). The declining reach phenomenon in
#' exposure distribution models. Journal of Advertising, 15(3), 13-20.
#' \doi{10.1080/00913367.1986.10673014}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 65-71. Handle
#' 2152/1590 (University of Texas at Austin repository). Kim
#' identifies the detailed MSAD numerical example as Lee, H.-K. (1988),
#' Sequential aggregation advertising media models, unpublished doctoral
#' dissertation, The University of Texas at Austin, pp. 81-101; the numerical
#' example itself occupies pp. 81-93.
#'
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.3.2.2, equation 3.106, for the Morgensztern reach formula.
#'
#' @examples
#' data(msad_example)
#' result <- do.call(calc_msad, msad_example)
#' result$reach
#'
#' @seealso [calc_csd()] for the canonical alternative and [calc_mbd()] and
#'   [calc_cbd()] for models that peel vehicles into insertion-level
#'   distributions.
#' @inheritSection calc_canex Domain of validity
#' @export
calc_msad <- function(vehicles_data, duplications,
                      aggregation_order = c("audience_desc", "given"),
                      population = 1, tolerance = 1e-10) {
  input <- validate_sequential_inputs(vehicles_data, duplications,
                                      aggregation_order, population, tolerance,
                                      caller = "calc_msad")
  n <- input$n
  insertions <- input$insertions
  R1 <- input$R1
  R2 <- input$R2
  order_index <- input$order_index
  order_rule <- input$order_rule

  marginals <- lapply(seq_len(n), function(i) {
    vehicle_exposure_distribution(insertions[i], R1[i], R2[i])
  })
  vehicle_reach <- vapply(marginals, function(x) 1 - x[1L], numeric(1))

  current <- marginals[[order_index[1L]]]
  included <- order_index[1L]
  steps <- vector("list", n - 1L)
  for (step in 2:n) {
    added <- order_index[step]
    subset <- c(included, added)
    target <- msad_morgensztern_reach(
      subset, R1, vehicle_reach, duplications
    )
    conformed <- sequential_conform_pair(
      current, marginals[[added]], target, tolerance, "Morgensztern"
    )
    current <- conformed$distribution
    included <- subset
    steps[[step - 1L]] <- data.frame(
      step = step - 1L,
      added_vehicle = added,
      target_reach = target,
      duplicated_reach = unname(conformed$diagnostics["duplicated_reach"]),
      row_margin_error = unname(conformed$diagnostics["row_margin_error"]),
      column_margin_error = unname(conformed$diagnostics["column_margin_error"])
    )
  }
  steps <- do.call(rbind, steps)
  contacts <- seq_along(current) - 1L
  cumulative <- rev(cumsum(rev(current)))
  reach <- 1 - current[1L]
  average_frequency <- sum(contacts * current) / reach

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
    aggregation_order = order_index,
    aggregation_rule = order_rule,
    steps = steps,
    diagnostics = list(
      probability_sum = sum(current),
      minimum_probability = min(current),
      gross_mean_contacts = sum(insertions * R1),
      distribution_mean_contacts = sum(contacts * current),
      mean_error = sum(contacts * current) - sum(insertions * R1)
    )
  )
  class(result) <- "reach_msad"
  result
}

#' @rdname print_sequential
#' @export
print.reach_msad <- function(x, full = TRUE, max_rows = 30L, ...) {
  if (isTRUE(full)) {
    print_sequential_report(
      x, "MORGENSZTERN SEQUENTIAL AGGREGATION DISTRIBUTION (MSAD)",
      paste("Beta-Binomial vehicles aggregated sequentially; the reach of",
            "every step comes from the Morgensztern formula"),
      parameters = list("Aggregation order" = x$aggregation_order,
                        "Aggregation rule" = x$aggregation_rule),
      tables = list(
        "Vehicles" = sequential_vehicle_table(x),
        "Aggregation steps" = sequential_step_table(x$steps)
      ),
      diagnostics = sequential_diagnostic_lines(x),
      max_rows = max_rows
    )
    return(invisible(x))
  }
  cat("Morgensztern Sequential Aggregation Distribution (MSAD)\n")
  cat(sprintf("Reach: %.2f%% | Average frequency: %.3f\n",
              x$reach$percent, x$average_frequency))
  cat("Aggregation order:", paste(x$aggregation_order, collapse = " -> "),
      sprintf("(%s)\n", x$aggregation_rule))
  cat(sprintf("Probability sum: %.12f | Mean error: %.3g\n",
              x$diagnostics$probability_sum,
              x$diagnostics$mean_error))
  invisible(x)
}
