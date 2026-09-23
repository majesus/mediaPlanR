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
#' @param vehicles_data Data frame with columns `insertions`, `R1` and `R2`.
#'   `R1` is the reach after one insertion and `R2` the cumulative reach after
#'   two insertions, as proportions. `R2` may be `NA` only when `insertions`
#'   is one.
#' @param duplications Symmetric matrix of pairwise one-insertion audience
#'   duplications, as proportions of the population. The diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (Kim's larger-audience-
#'   first rule), `"given"` (the row order), or a permutation of the row
#'   indices.
#' @param population Positive population used only to express probabilities
#'   as people. The default, 1, leaves `people` equal to `probability`.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'
#' @return A `reach_msad` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `vehicle_marginals`, the one-insertion
#'   `vehicle_reach`, the `aggregation_order` and `aggregation_rule`, the
#'   aggregation `steps` and numerical `diagnostics`.
#'
#' @details
#' For a subset of vehicles, the Morgensztern reach is
#' \deqn{R_m = \frac{(\sum_i R_{n_i})^2}{\sum_i R_{n_i} +
#' \sum_{i<j} K_{ij} A_{ij} R_{n_i} R_{n_j} / (A_i A_j)},}
#' with \eqn{K_{ij}=(A_i+A_j)/(A_i+A_j-A_{ij})}, where \eqn{R_{n_i}} is the
#' reach of vehicle \eqn{i} for its own \eqn{n_i} insertions, \eqn{A_i} its
#' one-insertion audience and \eqn{A_{ij}} the one-insertion duplication of
#' vehicles \eqn{i} and \eqn{j}. At every aggregation step the joint table is
#' conformed to the two input marginal distributions and to the Morgensztern
#' union reach. Consequently all probabilities remain non-negative, the
#' margins are preserved, and the zero-exposure probability is exactly
#' \eqn{1 - R_m}.
#'
#' MSAD is intended to reduce, but does not guarantee the elimination of,
#' declining reach. The reach formula can also be incompatible with the
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
#' Doctoral dissertation, The University of Texas at Austin, pp. 65-71. Kim
#' identifies the detailed MSAD numerical example as Lee, H.-K. (1988),
#' Sequential aggregation advertising media models, unpublished doctoral
#' dissertation, The University of Texas at Austin, pp. 81-101.
#'
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.3.2.2, equation 3.106, for the Morgensztern reach formula.
#'
#' @examples
#' # MSAD calculation derived from the inputs of Kim's CSD example. The
#' # resulting MSAD values are not presented by Kim as a published benchmark.
#' data(msad_kim2005)
#' result <- do.call(calc_msad, msad_kim2005)
#' result$reach
#'
#' @seealso [calc_csd()] for the canonical alternative and [calc_mbd()] and
#'   [calc_cbd()] for models that peel vehicles into insertion-level
#'   distributions.
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

#' @export
print.reach_msad <- function(x, ...) {
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
