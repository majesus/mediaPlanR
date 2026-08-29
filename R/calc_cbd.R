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
#' Implements the Conditional Beta Distribution (CBD), developed by Leckenby
#' and Kim and reported in Kim (1994), for several vehicles with several
#' insertions each. Between-vehicle duplication is modelled first, at the
#' single-insertion (0,1) level, via Danaher's (1991a) second-order canonical
#' expansion -- the same mechanism \code{\link{calc_canex}}/\code{\link{calc_csd}}
#' use. Vehicles are then peeled off one at a time, in reverse aggregation
#' order, each expanded from its (0,1) exposure state into its own
#' insertion-level Beta-Binomial distribution via Danaher's (1992a)
#' conditional convolution -- the identical mechanism \code{\link{calc_mbd}}
#' uses for its own peeling step. CBD and MBD therefore share their
#' within-vehicle expansion exactly; they differ only in how the initial
#' (0,1) joint grid is built (canonical expansion here, Waring's
#' inclusion-exclusion for MBD).
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1`, and `R2`,
#'   using the same convention as [calc_csd()]/[calc_mbd()] (`R2` may be
#'   `NA` only when `insertions` is one).
#' @param duplications Symmetric matrix of pairwise one-insertion audience
#'   duplication proportions. Diagonal values are ignored.
#' @param aggregation_order Either `"audience_desc"`, `"given"`, or a
#'   permutation of row indices. Vehicles are peeled off starting from the
#'   *last* position in this order, matching [calc_mbd()]'s convention.
#' @param population Positive population used only to express probabilities
#'   as people.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'
#' @return A `reach_cbd` object containing reach, the complete contact
#'   distribution, the aggregation order used, and diagnostics, including
#'   whether the same negative-probability safety net [calc_mbd()] uses had
#'   to be engaged.
#'
#' @details
#' Unlike [calc_mbd()], CBD's between-vehicle step needs no imputation for
#' three or more vehicles: the canonical expansion gives every cell of the
#' (0,1) grid directly from pairwise correlations alone, with no
#' higher-order terms and no Beta-Binomial-based guess for triples or
#' larger. Kim (2005) reviews CBD's specification (pp.59-64) as an existing
#' model -- rather than walking through a numerical example of it the way
#' she does for her own CSD -- so, unlike [calc_csd()], there is no
#' published worked example to validate this implementation against
#' directly. It is validated instead by construction: the (0,1) grid it
#' builds satisfies the same margin-recovery property already checked for
#' [calc_canex()]/[calc_csd()] (summing out every other vehicle reproduces
#' each vehicle's own Bernoulli(R1) marginal exactly), and its peeling step
#' is the identical, separately-validated mechanism used by [calc_mbd()].
#'
#' Because the canonical expansion can assign small negative probabilities
#' to some (0,1) cells -- the same known limitation documented for
#' [calc_canex()]/[calc_csd()] -- `calc_cbd()` applies the same final
#' safety net [calc_mbd()] does: any cell still negative after peeling is
#' zeroed and that mass is redistributed proportionally. This is reported in
#' `diagnostics` (`negative_mass_adjusted`, `cells_adjusted`).
#'
#' The peeling step has the same exponential cost as [calc_mbd()]'s, so
#' `calc_cbd()` applies the same practical cap (`max_vehicles`, default 12).
#'
#' @references
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 59-64
#' (reviewing Leckenby, J. D., & Kim, H. G. (1994), unpublished, as the
#' primary source of CBD's specification).
#'
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the "duplication of viewing
#' law". Journal of Marketing Research, 28(3), 361-367.
#' \doi{10.1177/002224379102800311}
#'
#' @examples
#' # Same three-vehicle inputs as calc_csd()'s Kim (2005) example, to compare
#' # CBD directly against CSD and CANEX on identical data.
#' vehicles <- data.frame(
#'   insertions = c(2, 2, 2),
#'   R1 = c(0.4902, 0.0333, 0.0300),
#'   R2 = c(0.5805, 0.0502, 0.0371)
#' )
#' duplication <- matrix(
#'   c(NA, 0.0157, 0.0139,
#'     0.0157, NA, 0.0003,
#'     0.0139, 0.0003, NA),
#'   nrow = 3, byrow = TRUE
#' )
#' result <- calc_cbd(vehicles, duplication, aggregation_order = 1:3)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_mbd()] for the same within-vehicle mechanism with a
#'   different (imputed) between-vehicle step; [calc_canex()] and
#'   [calc_csd()] for the canonical expansion this model's first step reuses.
#' @export
calc_cbd <- function(vehicles_data, duplications,
                     aggregation_order = c("audience_desc", "given"),
                     population = 1, tolerance = 1e-8) {
  required <- c("insertions", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required %in% names(vehicles_data)) ||
      nrow(vehicles_data) < 2L) {
    stop("vehicles_data must contain at least two rows and columns insertions, R1, and R2.",
         call. = FALSE)
  }
  n <- nrow(vehicles_data)
  max_vehicles <- 12L
  if (n > max_vehicles) {
    stop(sprintf(paste0(
      "calc_cbd() supports at most %d vehicles. Its peeling step has the ",
      "same exponential cost as calc_mbd()'s, for which Cheong (2007) only ",
      "tested computationally up to 12-13 vehicles."), max_vehicles),
      call. = FALSE)
  }
  insertions <- vehicles_data$insertions
  R1 <- vehicles_data$R1
  R2 <- vehicles_data$R2
  if (!is.numeric(insertions) || anyNA(insertions) || any(!is.finite(insertions)) ||
      any(insertions < 1 | insertions != round(insertions))) {
    stop("insertions must contain positive finite integers.", call. = FALSE)
  }
  if (!is.numeric(R1) || anyNA(R1) || any(!is.finite(R1)) || any(R1 <= 0 | R1 >= 1)) {
    stop("R1 must contain finite proportions strictly between zero and one.", call. = FALSE)
  }
  needs_R2 <- insertions >= 2L
  if (!is.numeric(R2) || anyNA(R2[needs_R2]) || any(!is.finite(R2[needs_R2]))) {
    stop("R2 must be finite for every vehicle with at least two insertions.", call. = FALSE)
  }
  vehicle_bbd <- lapply(seq_len(n), function(i) {
    if (needs_R2[i]) calculate_bbd_params(R1[i], R2[i])
    else list(alpha = NA_real_, beta = NA_real_, p = R1[i], type = "single_insertion")
  })

  if (!is.matrix(duplications) || !is.numeric(duplications) ||
      !identical(dim(duplications), c(n, n))) {
    stop("duplications must be a numeric square matrix matching vehicles_data.", call. = FALSE)
  }
  off_diagonal <- row(duplications) != col(duplications)
  if (anyNA(duplications[off_diagonal]) || any(!is.finite(duplications[off_diagonal])) ||
      !isTRUE(all.equal(duplications[upper.tri(duplications)],
                        t(duplications)[upper.tri(duplications)],
                        tolerance = tolerance, check.attributes = FALSE))) {
    stop("duplications must be finite and symmetric outside its diagonal.", call. = FALSE)
  }
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      lower <- max(0, R1[i] + R1[j] - 1)
      upper <- min(R1[i], R1[j])
      if (duplications[i, j] < lower - tolerance || duplications[i, j] > upper + tolerance) {
        stop(sprintf("duplication [%d,%d] is outside its Frechet bounds [%.8f, %.8f].",
                     i, j, lower, upper), call. = FALSE)
      }
    }
  }
  if (!is.numeric(population) || length(population) != 1L || !is.finite(population) ||
      population <= 0) {
    stop("population must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) ||
      tolerance <= 0) {
    stop("tolerance must be one positive finite number.", call. = FALSE)
  }

  if (is.numeric(aggregation_order)) {
    if (length(aggregation_order) != n || anyNA(aggregation_order) ||
        any(!is.finite(aggregation_order)) || any(aggregation_order != round(aggregation_order))) {
      stop("A numeric aggregation_order must contain integer row indices.", call. = FALSE)
    }
    order_index <- as.integer(aggregation_order)
    if (!identical(sort(order_index), seq_len(n))) {
      stop("A numeric aggregation_order must be a permutation of row indices.", call. = FALSE)
    }
    order_rule <- "custom"
  } else {
    aggregation_order <- match.arg(aggregation_order)
    order_index <- if (aggregation_order == "audience_desc") {
      order(-R1, seq_len(n))
    } else seq_len(n)
    order_rule <- aggregation_order
  }

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
    table <- mbd_peel_vehicle(table, other_subsets, v, vp$alpha, vp$beta,
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
