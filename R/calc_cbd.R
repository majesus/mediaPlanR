#__________________________________________________________#

# The full (0,1)^n joint exposure grid via Danaher's (1991a) second-order
# canonical expansion: every vehicle is a Bernoulli(R1_i) marginal, and every
# cell is adjusted by the sum of pairwise correlation terms only (no
# three-way or higher terms) -- the same canonical formula already used by
# calc_canex()/calc_csd() for the zero cell, evaluated here for all 2^n
# cells (Kim 2005, pp.56-57 and 60). Returns an environment keyed by
# mbd_key(), exactly the shape mbd_peel_vehicle() expects, so the two models
# share the same peeling mechanism (Danaher 1992a) once this initial grid is
# built.
cbd_binary_grid <- function(R1, correlation_matrix) {
  n <- length(R1)
  mu <- R1
  sigma <- sqrt(R1 * (1 - R1))
  grid <- new.env(parent = emptyenv())
  for (t in mbd_all_subsets(seq_len(n))) {
    x <- integer(n)
    if (length(t)) x[t] <- 1L
    z <- prod(ifelse(x == 1L, R1, 1 - R1))
    adjustment <- 0
    if (n >= 2L) {
      pairs <- utils::combn(n, 2L)
      for (column in seq_len(ncol(pairs))) {
        i <- pairs[1L, column]; j <- pairs[2L, column]
        adjustment <- adjustment +
          correlation_matrix[i, j] * (x[i] - mu[i]) * (x[j] - mu[j]) /
          (sigma[i] * sigma[j])
      }
    }
    grid[[mbd_key(t)]] <- z * (1 + adjustment)
  }
  grid
}

#' Conditional Beta Distribution model
#'
#' Implements the Conditional Beta Distribution (CBD) of Leckenby and Kim,
#' reported in Kim (1994) and reviewed in Kim (2005), for several vehicles with
#' several insertions each. Between-vehicle duplication is modeled first, at
#' the one-insertion (0,1) level, by Danaher's (1991) second-order canonical
#' expansion -- the mechanism [calc_canex()] and [calc_csd()] use. Vehicles are
#' then peeled off one at a time, in reverse aggregation order, and each is
#' expanded from its (0,1) exposure state into its own insertion-level
#' Beta-Binomial distribution by Danaher's (1992a) conditional convolution --
#' the mechanism [calc_mbd()] uses for its own peeling step. CBD and MBD
#' therefore share their within-vehicle expansion exactly and differ only in how
#' the initial (0,1) joint grid is built: canonical expansion here, Waring's
#' inclusion-exclusion theorem for MBD.
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1` and `R2`,
#'   with the same convention as [calc_csd()] and [calc_mbd()] (`R2` may be
#'   `NA` only when `insertions` is one). At most 12 vehicles are supported.
#' @param duplications Symmetric matrix of pairwise one-insertion audience
#'   duplications, as proportions of the population. The diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (vehicles in decreasing
#'   order of one-insertion reach), `"given"` (the row order), or a
#'   permutation of the row indices. Vehicles are peeled off starting from the
#'   *last* position of this order, as in [calc_mbd()].
#' @param population Positive population used only to express probabilities
#'   as people. The default, 1, leaves `people` equal to `probability`.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'
#' @return A `reach_cbd` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `aggregation_order` and `aggregation_rule`,
#'   the peeling `steps` and `diagnostics`, which include whether the
#'   negative-probability safety net of [calc_mbd()] had to be engaged
#'   (`negative_mass_adjusted`, `cells_adjusted`).
#'
#' @details
#' Unlike [calc_mbd()], the between-vehicle step of CBD needs no imputation
#' for three or more vehicles: the canonical expansion gives every cell of the
#' (0,1) grid directly from the pairwise correlations. For binary exposure the
#' expansion uses the mean \eqn{R_{1i}} and variance \eqn{R_{1i}(1 - R_{1i})}
#' of each vehicle's one-insertion exposure, which is the single-insertion
#' case of the Beta-Binomial mean and variance in Kim (2005, pp. 60-61); this
#' guarantees that summing out all other vehicles returns each vehicle's own
#' Bernoulli marginal exactly.
#'
#' Kim (2005, pp. 59-64) reviews the specification of CBD as an existing model
#' but does not present a numerical example of it, as it does for CSD, so there
#' is no published worked example to validate this implementation against
#' directly. The implementation is validated by construction: the (0,1) grid
#' recovers each vehicle's own `R1` as its exact marginal, its peeling step is
#' the separately validated mechanism of [calc_mbd()], and with zero
#' correlation the model reduces to the exact convolution of the vehicles' own
#' Beta-Binomial marginals.
#'
#' Because the canonical expansion can assign small negative probabilities to
#' some (0,1) cells -- the limitation documented for [calc_canex()] -- any
#' cell that is still negative after peeling is set to zero and that mass is
#' redistributed proportionally, as in the final safety net of [calc_mbd()].
#' This is reported in `diagnostics`.
#'
#' The peeling step has the exponential cost of [calc_mbd()], so the number of
#' vehicles is limited to 12.
#'
#' @references
#' Kim, H. (1994). A conditional beta distribution model for advertising
#' reach/frequency estimation. Unpublished doctoral dissertation, The
#' University of Texas at Austin.
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 59-64.
#'
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the "duplication of viewing
#' law". Journal of Marketing Research, 28(3), 361-367.
#' \doi{10.1177/002224379102800311}
#'
#' Danaher, P. J. (1992a). A Markov-chain model for multivariate
#' magazine-exposure distributions. Journal of Business & Economic Statistics,
#' 10(4), 401-407. \doi{10.1080/07350015.1992.10509915}
#'
#' @examples
#' # Same three-vehicle inputs as the CSD example of Kim (2005), so that CBD
#' # can be compared with CSD and CANEX on identical data
#' data(csd_kim2005)
#' result <- do.call(calc_cbd, csd_kim2005)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_mbd()] for the same within-vehicle mechanism with an imputed
#'   between-vehicle step, and [calc_canex()] and [calc_csd()] for the canonical
#'   expansion that this model's first step reuses.
#' @export
calc_cbd <- function(vehicles_data, duplications,
                     aggregation_order = c("audience_desc", "given"),
                     population = 1, tolerance = 1e-8) {
  input <- validate_sequential_inputs(vehicles_data, duplications,
                                      aggregation_order, population, tolerance,
                                      max_vehicles = 12L, caller = "calc_cbd")
  n <- input$n
  insertions <- input$insertions
  R1 <- input$R1
  R2 <- input$R2
  order_index <- input$order_index
  order_rule <- input$order_rule
  vehicle_bbd <- lapply(seq_len(n), function(i) {
    if (insertions[i] >= 2L) calculate_bbd_params(R1[i], R2[i])
    else list(alpha = NA_real_, beta = NA_real_, p = R1[i], type = "single_insertion")
  })

  correlation_matrix <- diag(1, n)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      correlation_matrix[i, j] <- correlation_matrix[j, i] <-
        calculate_duplication(duplications[i, j], R1[i], R1[j])
    }
  }

  grid <- cbd_binary_grid(R1, correlation_matrix)
  remaining <- seq_len(n)
  table <- list()
  for (t in mbd_all_subsets(remaining)) table[[mbd_key(t)]] <- grid[[mbd_key(t)]]

  peel_order <- rev(order_index)
  steps <- vector("list", n)
  for (step in seq_along(peel_order)) {
    v <- peel_order[step]
    other <- setdiff(remaining, v)
    other_subsets <- mbd_all_subsets(other)
    vp <- vehicle_bbd[[v]]
    table <- mbd_peel_vehicle(table, other_subsets, v, vp$alpha, vp$beta, vp$p,
                              insertions[v], tolerance)
    remaining <- other
    steps[[step]] <- data.frame(
      step = step, peeled_vehicle = v,
      mechanism = if (is.na(vp$alpha)) "single_insertion" else "beta_binomial",
      remaining_vehicles = length(remaining)
    )
  }
  distribution_raw <- table[[mbd_key(integer(0))]]

  safety <- mbd_safety_net(distribution_raw, tolerance)
  distribution <- safety$distribution
  contacts <- seq_along(distribution) - 1L
  cumulative <- rev(cumsum(rev(distribution)))
  reach <- 1 - distribution[1L]
  average_frequency <- sum(contacts * distribution) / reach
  steps <- do.call(rbind, steps)

  result <- list(
    reach = list(probability = reach, percent = 100 * reach, people = population * reach),
    average_frequency = average_frequency,
    distribution = data.frame(
      contacts = contacts, probability = distribution, percent = 100 * distribution,
      people = population * distribution, cumulative_probability = cumulative
    ),
    aggregation_order = order_index,
    aggregation_rule = order_rule,
    steps = steps,
    diagnostics = list(
      probability_sum = sum(distribution),
      minimum_probability = min(distribution),
      negative_mass_adjusted = safety$negative_mass_adjusted,
      cells_adjusted = safety$cells_adjusted,
      gross_mean_contacts = sum(insertions * R1),
      distribution_mean_contacts = sum(contacts * distribution),
      mean_error = sum(contacts * distribution) - sum(insertions * R1)
    )
  )
  class(result) <- "reach_cbd"
  result
}

#' @export
print.reach_cbd <- function(x, ...) {
  cat("Conditional Beta Distribution (CBD)\n")
  cat(sprintf("Reach: %.2f%% | Average frequency: %.3f\n",
              x$reach$percent, x$average_frequency))
  cat("Aggregation order:", paste(x$aggregation_order, collapse = " -> "),
      sprintf("(%s)\n", x$aggregation_rule))
  cat(sprintf("Probability sum: %.12f | Mean error: %.3g\n",
              x$diagnostics$probability_sum, x$diagnostics$mean_error))
  if (x$diagnostics$negative_mass_adjusted > 0) {
    cat(sprintf("Safety net engaged: %d cell(s), %.6g probability mass adjusted.\n",
                x$diagnostics$cells_adjusted, x$diagnostics$negative_mass_adjusted))
  }
  invisible(x)
}
