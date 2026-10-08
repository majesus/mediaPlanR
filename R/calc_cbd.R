#__________________________________________________________#

# The full (0,1)^n joint exposure grid via Danaher's (1991a) second-order
# canonical expansion: every vehicle is a Bernoulli(R1_i) marginal, and every
# cell is adjusted by the sum of pairwise correlation terms only (no
# three-way or higher terms) -- the same canonical formula already used by
# calc_canex()/calc_csd() for the zero cell, evaluated here for all 2^n
# cells (Kim 1994, pp. 115-124; Kim 2005, pp. 56-57 and 60). Returns an
# environment keyed by mbd_key() (the subset of vehicles exposed to their first
# insertion).
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
#' Implements the Conditional Beta Distribution (CBD) of Kim (1994), reviewed
#' in Kim (2005), for several vehicles with several insertions each. The
#' "(0,1) grid" is the set of the \eqn{2^m} combinations of being exposed (1) or
#' not exposed (0) to the first insertion of each of the \eqn{m} vehicles; the
#' notation "(0,1)" refers to those two states, not to an interval.
#' Between-vehicle duplication is modeled first, at the one-insertion (0,1)
#' level, by Danaher's (1991) second-order canonical expansion -- the mechanism
#' [calc_canex()] and [calc_csd()] use. Conditionally on the (0,1) exposure
#' state of every vehicle, the remaining insertions of each vehicle follow a
#' Beta-Binomial distribution whose Beta parameters are updated by that state,
#' and the vehicles are independent given the grid (Danaher 1992a). The zero
#' cell of the grid is then conformed to the canonical expansion of all the
#' insertions.
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1` and `R2`,
#'   with the same convention as [calc_csd()] (`R1` and `R2` are proportions
#'   between 0 and 1, not percentages, `R1` is strictly between zero and one,
#'   `R1 <= R2 <= 2 * R1 - R1^2`, and `R2` may be `NA` only when `insertions`
#'   is one; the function stops with an explanatory error if `R2` is outside
#'   that range). At most 12 vehicles are supported.
#' @param duplications Symmetric matrix whose element `[i, j]` is the
#'   proportion of the population that is exposed to both vehicle `i` and
#'   vehicle `j` (one insertion in each), a number between 0 and 1 (not a
#'   percentage). It is not the proportion exposed to each vehicle separately.
#'   The diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (vehicles in decreasing
#'   order of one-insertion reach `R1`), `"given"` (the row order), or a
#'   permutation of the row indices. CBD does not use it: in Kim's
#'   specification the vehicles are conditionally independent given the (0,1)
#'   grid, so neither the reach nor the distribution depends on the order. The
#'   argument is accepted, validated and reported only so that the same call,
#'   and the same example data (for instance [csd_example]), work for
#'   [calc_csd()], [calc_msad()], [calc_cbd()] and [calc_mbd()].
#' @param population Number of people in the population (a count, not a
#'   proportion). It only converts probabilities into people: the `people`
#'   columns of the result are the probability times `population`, and with the
#'   default, 1, they equal the probability.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'   The Fréchet and triple-feasibility checks apply it relative to the smaller
#'   audience involved.
#'
#' @return A `reach_cbd` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `aggregation_order` and `aggregation_rule`,
#'   the within-vehicle `vehicle_expansion` (the distribution used for each
#'   vehicle) and `diagnostics`, which include the canonical zero-exposure
#'   probability, the zero-cell rescaling factor and whether the
#'   negative-probability safety net of [calc_mbd()] had to be engaged
#'   (`negative_mass_adjusted`, `cells_adjusted`).
#'
#' @details
#' The model follows Kim (1994, pp. 115-124):
#'
#' 1. The joint distribution of exposure to one insertion of each vehicle (the
#'    \eqn{2^m} cells of the (0,1) grid) is obtained from the second-order
#'    canonical expansion with Bernoulli marginals, from the pairwise
#'    correlations implied by the observed one-insertion duplications.
#' 2. Given that vehicle \eqn{i} was (\eqn{x_i = 1}) or was not (\eqn{x_i = 0})
#'    exposed to its first insertion, its remaining \eqn{n_i - 1} insertions
#'    follow a Beta-Binomial distribution with parameters
#'    \eqn{(\alpha + x_i, \beta + 1 - x_i)}, where \eqn{\alpha} and \eqn{\beta}
#'    are fitted from `R1` and `R2`. The vehicle's total exposure is that
#'    distribution shifted by \eqn{x_i}. The distributions of the vehicles are
#'    convolved within each cell of the grid and weighted by the cell
#'    probability. A vehicle with one insertion contributes only its state
#'    \eqn{x_i}.
#' 3. In the all-zero cell of the grid, the probability of zero exposures is
#'    set to the canonical expansion of the zero cell computed with all the
#'    insertions (Danaher 1991, as in [calc_canex()]), and the remaining levels
#'    of that cell are rescaled so that the cell keeps its total probability
#'    (Kim 1994, p. 124).
#'
#' With a binomial-limit vehicle (`R2` equal to the reach under independence)
#' the conditional distribution is the binomial one, and with a polarized one
#' (`R2` equal to `R1`) it is the all-or-nothing exposure; both are the limits
#' of the Beta-Binomial updating.
#'
#' Unlike [calc_mbd()], the between-vehicle step of CBD needs no imputation
#' for three or more vehicles. The canonical expansion uses the mean
#' \eqn{R_{1i}} and variance \eqn{R_{1i}(1 - R_{1i})} of each vehicle's
#' one-insertion exposure; this guarantees that, in the grid built in step 1
#' and before any correction, summing out all other vehicles returns each
#' vehicle's own Bernoulli marginal exactly, and with zero correlation the model
#' reduces to the exact convolution of the vehicles' Beta-Binomial marginals.
#' That property belongs to the uncorrected grid, not to the returned
#' distribution: the zero-cell adjustment of step 3 and, if it is engaged, the
#' truncation of negative cells and renormalization change the marginals of each
#' vehicle and the mean number of exposures. The departure of the mean is
#' reported in `diagnostics$mean_error` (between 0.0003 and 0.013 exposures in
#' the package's reference schedules), so the marginals of the result should be
#' judged through those diagnostics, separating the binary marginal of each
#' vehicle, its distribution within the vehicle and the final distribution of
#' the plan. As with [calc_canex()], the `average_frequency` is the mean of the
#' returned distribution over its reach: it uses the corrected mean, not the
#' gross contacts per person of the input, so the two bases should not be mixed.
#'
#' # Validation against published results
#'
#' The implementation reproduces the CBD columns of the two-vehicle
#' comparisons of Kim (2005, Appendix B) and of Hong (1998, Appendix E) within
#' the rounding of those sources (0.01 percentage points), taking the
#' duplication that each source implies. The sources do not print those
#' duplications: they were reconstructed from another model's published column
#' (CANEX for Kim, the MSAD reach for Hong), so this agreement checks the
#' consistency between columns of published tables, and algorithmic fidelity,
#' not predictive accuracy against independent observations (the maintainer's
#' local tests, whose input tables are not distributed, accept up to 0.02
#' percentage points, although the largest deviation measured is below 0.01).
#' The function also reproduces the worked example of Kim (1994,
#' pp. 125-139; three vehicles with 2, 1 and 3 insertions, SMRB 1979 data) except
#' for its zero cell. In that example the zero of the canonical expansion is
#' printed as 0.5066,
#' whereas the formula applied to the printed inputs gives about 0.510 (and no
#' choice of the inputs within their rounding goes below 0.507), so the printed
#' value cannot be reproduced from the printed inputs. Page 139 of that source
#' contains verifiable errata (a probability of 0.0055 printed as 0.06% instead
#' of 0.55%, and percentages that add to 99.54% although labeled 100), but they
#' do not by themselves establish the cause of the discrepancy in the zero
#' cell, which is treated here as an unexplained difference with the printed
#' value. This function evaluates the formula, so its zero and one-contact
#' probabilities differ from the printed ones by 0.36 and 0.29 percentage points
#' in that example. See the package tests for the exact cases.
#'
#' Because the canonical expansion can assign small negative probabilities to
#' some (0,1) cells -- the limitation documented for [calc_canex()] -- any
#' cell that is still negative at the end is set to zero and that mass is
#' redistributed proportionally, as in the final safety net of [calc_mbd()].
#' This step is an extension of the package, not part of Kim's specification,
#' and is reported in `diagnostics`. If the canonical zero-exposure probability
#' is not a probability, the function stops, as [calc_csd()] does.
#'
#' The number of cells of the (0,1) grid grows as \eqn{2^m}, so the number of
#' vehicles is limited to 12.
#'
#' @references
#' Kim, H. (1994). A conditional beta distribution model for advertising media
#' reach/frequency estimation. Unpublished doctoral dissertation, The
#' University of Texas at Austin, pp. 115-139.
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 59-64.
#' Handle 2152/1590 (University of Texas at Austin repository).
#'
#' Hong, J. (1998). Advertising media models for Internet reach/frequency
#' estimation. Unpublished doctoral dissertation, The University of Texas at
#' Austin, Appendix E.
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
#' # Same three-vehicle inputs as the CSD example, so that CBD can be compared
#' # with CSD and CANEX on identical data
#' data(csd_example)
#' result <- do.call(calc_cbd, csd_example)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_mbd()] for a model with an imputed between-vehicle step, and
#'   [calc_canex()] and [calc_csd()] for the canonical expansion that this
#'   model's first and last steps reuse.
#' @inheritSection calc_canex Domain of validity
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

  correlation_matrix <- diag(1, n)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      correlation_matrix[i, j] <- correlation_matrix[j, i] <-
        calculate_duplication(duplications[i, j], R1[i], R1[j])
    }
  }

  # Step 1: (0,1) grid by the second-order canonical expansion.
  grid <- cbd_binary_grid(R1, correlation_matrix)

  # Step 2: conditional exposure distribution of each vehicle, given its
  # state (0 or 1) at the first insertion.
  conditional <- lapply(seq_len(n), function(i) {
    cbd_vehicle_conditionals(insertions[i], R1[i], R2[i])
  })

  # Step 3 needs the canonical zero probability with all the insertions.
  marginals <- lapply(seq_len(n), function(i) {
    vehicle_exposure_distribution(insertions[i], R1[i], R2[i])
  })
  zero_probability <- cbd_canonical_zero(marginals, correlation_matrix,
                                         tolerance)

  total_length <- sum(insertions) + 1L
  distribution_raw <- numeric(total_length)
  zero_cell_factor <- NA_real_
  for (t in mbd_all_subsets(seq_len(n))) {
    cell <- 1
    for (i in seq_len(n)) {
      cell <- cbd_convolve(cell, conditional[[i]][[if (i %in% t) 2L else 1L]])
    }
    cell_probability <- grid[[mbd_key(t)]]
    cell <- cell_probability * cell
    if (length(t) == 0L) {
      level_zero <- cell[1L]
      denominator <- cell_probability - level_zero
      if (is.finite(denominator) && abs(denominator) > 1e-12 * abs(cell_probability)) {
        zero_cell_factor <- (cell_probability - zero_probability) / denominator
        cell[-1L] <- cell[-1L] * zero_cell_factor
        cell[1L] <- zero_probability
      }
    }
    distribution_raw[seq_along(cell)] <- distribution_raw[seq_along(cell)] + cell
  }

  safety <- mbd_safety_net(distribution_raw, tolerance)
  distribution <- safety$distribution
  contacts <- seq_along(distribution) - 1L
  cumulative <- rev(cumsum(rev(distribution)))
  reach <- 1 - distribution[1L]
  average_frequency <- sum(contacts * distribution) / reach

  vehicle_expansion <- data.frame(
    vehicle = seq_len(n),
    insertions = insertions,
    mechanism = vapply(seq_len(n), function(i) {
      if (insertions[i] == 1L) "single_insertion"
      else calculate_bbd_params(R1[i], R2[i])$type
    }, character(1)),
    stringsAsFactors = FALSE
  )

  result <- list(
    reach = list(probability = reach, percent = 100 * reach, people = population * reach),
    average_frequency = average_frequency,
    distribution = data.frame(
      contacts = contacts, probability = distribution, percent = 100 * distribution,
      people = population * distribution, cumulative_probability = cumulative
    ),
    aggregation_order = order_index,
    aggregation_rule = order_rule,
    vehicle_expansion = vehicle_expansion,
    diagnostics = list(
      probability_sum = sum(distribution),
      minimum_probability = min(distribution),
      negative_mass_adjusted = safety$negative_mass_adjusted,
      cells_adjusted = safety$cells_adjusted,
      canonical_zero_probability = zero_probability,
      grid_zero_probability = grid[[mbd_key(integer(0))]],
      zero_cell_factor = zero_cell_factor,
      gross_mean_contacts = sum(insertions * R1),
      distribution_mean_contacts = sum(contacts * distribution),
      mean_error = sum(contacts * distribution) - sum(insertions * R1)
    )
  )
  class(result) <- "reach_cbd"
  result
}

# Full convolution of two probability vectors indexed from zero contacts.
cbd_convolve <- function(a, b) {
  out <- numeric(length(a) + length(b) - 1L)
  for (i in seq_along(a)) {
    idx <- (i - 1L) + seq_along(b)
    out[idx] <- out[idx] + a[i] * b
  }
  out
}

# Distribution of a vehicle's total exposures (0, ..., insertions) conditional
# on its exposure state at the first insertion (Kim 1994, pp. 117-121). Returns
# a list of two vectors, for state 0 and state 1. The remaining insertions - 1
# insertions are Beta-Binomial(alpha + x, beta + 1 - x), shifted by x; the
# binomial and polarized limits are handled explicitly.
cbd_vehicle_conditionals <- function(insertions, R1, R2) {
  if (insertions == 1L) {
    return(list(c(1, 0), c(0, 1)))
  }
  remaining <- insertions - 1L
  params <- calculate_bbd_params(R1, R2)
  conditional_for_state <- function(x) {
    if (params$type == "binomial_limit") {
      d <- stats::dbinom(0:remaining, size = remaining, prob = R1)
    } else if (params$type == "polarized_limit") {
      d <- numeric(remaining + 1L)
      d[if (x == 0L) 1L else remaining + 1L] <- 1
    } else {
      d <- dbetabinom(0:remaining, size = remaining,
                               alpha = params$alpha + x,
                               beta = params$beta + 1 - x)
    }
    c(rep(0, x), d, rep(0, 1L - x))
  }
  list(conditional_for_state(0L), conditional_for_state(1L))
}

# Second-order canonical expansion of the all-zero cell with all the
# insertions (Danaher 1991, equation 6), from the vehicles' own exposure
# marginals and the canonical correlations. Vehicles with exactly no
# variance contribute no interaction term, as in calc_canex().
cbd_canonical_zero <- function(marginals, correlation_matrix, tolerance) {
  n <- length(marginals)
  means <- vapply(marginals, function(m) sum((seq_along(m) - 1L) * m), numeric(1))
  variances <- vapply(seq_len(n), function(i) {
    sum((seq_along(marginals[[i]]) - 1L - means[i])^2 * marginals[[i]])
  }, numeric(1))
  zero_base <- prod(vapply(marginals, function(m) m[1L], numeric(1)))
  adjustment <- 0
  if (n >= 2L) {
    pairs <- utils::combn(n, 2L)
    for (column in seq_len(ncol(pairs))) {
      i <- pairs[1L, column]; j <- pairs[2L, column]
      if (variances[i] > 0 && variances[j] > 0) {
        adjustment <- adjustment + correlation_matrix[i, j] * means[i] *
          means[j] / sqrt(variances[i] * variances[j])
      }
    }
  }
  zero <- zero_base * (1 + adjustment)
  if (!is.finite(zero) || zero < -tolerance || zero > 1 + tolerance) {
    stop(sprintf(
      paste0("The second-order canonical expansion produced an invalid ",
             "zero-exposure probability (%.8f)."), zero), call. = FALSE)
  }
  min(1, max(0, zero))
}

#' @rdname print_sequential
#' @export
print.reach_cbd <- function(x, full = TRUE, max_rows = 30L, ...) {
  if (isTRUE(full)) {
    print_sequential_report(
      x, "CONDITIONAL BETA DISTRIBUTION (CBD)",
      paste("between-vehicle duplication on the (0,1) grid of the first",
            "insertion, then each vehicle's Beta-Binomial conditioned on",
            "that state"),
      parameters = list("Aggregation order (reported only; it does not affect CBD)" =
                          x$aggregation_order,
                        "Aggregation rule" = x$aggregation_rule),
      tables = list("Vehicles" = x$vehicle_expansion),
      diagnostics = c(
        sequential_diagnostic_lines(x),
        sprintf("Canonical zero probability: %.6f | Zero probability of the (0,1) grid: %.6f",
                x$diagnostics$canonical_zero_probability,
                x$diagnostics$grid_zero_probability)
      ),
      max_rows = max_rows
    )
    return(invisible(x))
  }
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
