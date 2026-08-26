csd_canonical_reach <- function(indices, marginals, single_reach,
                                duplications, tolerance) {
  means <- vapply(indices, function(i) {
    contacts <- seq_along(marginals[[i]]) - 1L
    sum(contacts * marginals[[i]])
  }, numeric(1))
  variances <- vapply(seq_along(indices), function(position) {
    i <- indices[position]
    contacts <- seq_along(marginals[[i]]) - 1L
    sum((contacts - means[position])^2 * marginals[[i]])
  }, numeric(1))
  if (any(variances <= tolerance)) {
    stop("CSD requires non-degenerate vehicle exposure marginals.",
         call. = FALSE)
  }

  zero_base <- prod(vapply(indices, function(i) marginals[[i]][1L],
                           numeric(1)))
  expansion_adjustment <- 0
  if (length(indices) >= 2L) {
    pairs <- utils::combn(seq_along(indices), 2L)
    for (column in seq_len(ncol(pairs))) {
      left_position <- pairs[1L, column]
      right_position <- pairs[2L, column]
      i <- indices[left_position]
      j <- indices[right_position]
      rho <- calculate_duplication(
        duplications[i, j], single_reach[i], single_reach[j]
      )
      expansion_adjustment <- expansion_adjustment +
        rho * means[left_position] * means[right_position] /
        sqrt(variances[left_position] * variances[right_position])
    }
  }

  zero_probability <- zero_base * (1 + expansion_adjustment)
  if (!is.finite(zero_probability) ||
      zero_probability < -tolerance || zero_probability > 1 + tolerance) {
    stop(sprintf(
      paste0("The second-order canonical expansion produced an invalid ",
             "zero-contact probability (%.8f)."),
      zero_probability
    ), call. = FALSE)
  }
  zero_probability <- min(1, max(0, zero_probability))

  list(
    reach = 1 - zero_probability,
    zero_probability = zero_probability,
    random_zero_probability = zero_base,
    expansion_adjustment = expansion_adjustment
  )
}

#' Canonical Sequential Aggregation Distribution model
#'
#' Implements Kim's (2005) Canonical Sequential Aggregation Distribution
#' (CSD): vehicle-level Beta-Binomial marginals are combined sequentially,
#' while the target reach at every step is obtained from Danaher's second-order
#' canonical expansion. The non-random convolution preserves both input
#' margins and sets the zero-contact cell to one minus canonical reach.
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1`, and `R2`.
#'   `R1` is one-insertion reach and `R2` is two-insertion cumulative reach,
#'   expressed as proportions. `R2` may be `NA` only when `insertions` is one.
#' @param duplications Symmetric matrix of pairwise one-insertion audience
#'   duplication proportions. Diagonal values are ignored.
#' @param aggregation_order Either `"audience_desc"`, `"given"`, or a
#'   permutation of row indices. Use an explicit permutation when reproducing
#'   a published aggregation criterion such as Kim's TD forward example.
#' @param population Positive population used only to express probabilities as
#'   people.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'
#' @return A `reach_csd` object containing reach, the complete contact
#'   distribution, cumulative probabilities, vehicle marginals, aggregation
#'   steps, and numerical diagnostics.
#'
#' @details
#' For a subset of vehicles, CSD estimates the all-zero probability as
#' \deqn{P(0,\ldots,0)=\prod_i f_i(0)\left[1+\sum_{i<j}\rho_{ij}
#' \frac{(0-\mu_i)(0-\mu_j)}{\sigma_i\sigma_j}\right],}
#' where `f_i` is the Beta-Binomial marginal and `rho_ij` is obtained from the
#' observed one-insertion duplication. Reach is one minus this probability.
#'
#' The canonical expansion is a second-order approximation. Unlike the legacy
#' full-grid CANEX implementation, `calc_csd()` does not truncate or
#' renormalize an invalid canonical target. It stops if the resulting reach is
#' not a probability or is incompatible with the two margins being combined.
#' This makes approximation failure visible to the analyst.
#'
#' Kim's worked example rounds intermediate values to four decimals. Exact
#' calculations from the published inputs therefore differ by a few hundredths
#' of a percentage point from some displayed cells; the unrounded calculation
#' is returned.
#'
#' @references
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the duplication of viewing law.
#' Journal of Marketing Research, 28(3), 361-367.
#' \doi{10.1177/002224379102800311}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 78-97.
#'
#' @examples
#' # Kim (2005), Tables 4.2.2.1-4.2.2.10: TD forward order.
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
#' result <- calc_csd(vehicles, duplication, aggregation_order = 1:3)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_canex()] for a full-grid canonical expansion and
#'   [calc_msad()] for the Morgensztern sequential alternative.
#' @export
calc_csd <- function(vehicles_data, duplications,
                     aggregation_order = c("audience_desc", "given"),
                     population = 1, tolerance = 1e-10) {
  required <- c("insertions", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required %in% names(vehicles_data)) ||
      nrow(vehicles_data) < 2L) {
    stop("vehicles_data must contain at least two rows and columns insertions, R1, and R2.",
         call. = FALSE)
  }
  n <- nrow(vehicles_data)
  insertions <- vehicles_data$insertions
  R1 <- vehicles_data$R1
  R2 <- vehicles_data$R2
  if (!is.numeric(insertions) || anyNA(insertions) ||
      any(!is.finite(insertions)) ||
      any(insertions < 1 | insertions != round(insertions))) {
    stop("insertions must contain positive finite integers.", call. = FALSE)
  }
  if (!is.numeric(R1) || anyNA(R1) || any(!is.finite(R1)) ||
      any(R1 <= 0 | R1 >= 1)) {
    stop("R1 must contain finite proportions strictly between zero and one.",
         call. = FALSE)
  }
  needs_R2 <- insertions >= 2L
  if (!is.numeric(R2) || anyNA(R2[needs_R2]) ||
      any(!is.finite(R2[needs_R2]))) {
    stop("R2 must be finite for every vehicle with at least two insertions.",
         call. = FALSE)
  }
  invisible(lapply(which(needs_R2), function(i) {
    calculate_bbd_params(R1[i], R2[i])
  }))

  if (!is.matrix(duplications) || !is.numeric(duplications) ||
      !identical(dim(duplications), c(n, n))) {
    stop("duplications must be a numeric square matrix matching vehicles_data.",
         call. = FALSE)
  }
  off_diagonal <- row(duplications) != col(duplications)
  if (anyNA(duplications[off_diagonal]) ||
      any(!is.finite(duplications[off_diagonal])) ||
      !isTRUE(all.equal(duplications[upper.tri(duplications)],
                        t(duplications)[upper.tri(duplications)],
                        tolerance = tolerance, check.attributes = FALSE))) {
    stop("duplications must be finite and symmetric outside its diagonal.",
         call. = FALSE)
  }
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      lower <- max(0, R1[i] + R1[j] - 1)
      upper <- min(R1[i], R1[j])
      if (duplications[i, j] < lower - tolerance ||
          duplications[i, j] > upper + tolerance) {
        stop(sprintf(
          "duplication [%d,%d] is outside its Frechet bounds [%.8f, %.8f].",
          i, j, lower, upper
        ), call. = FALSE)
      }
    }
  }
  if (!is.numeric(population) || length(population) != 1L ||
      !is.finite(population) || population <= 0) {
    stop("population must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance <= 0) {
    stop("tolerance must be one positive finite number.", call. = FALSE)
  }

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

  if (is.numeric(aggregation_order)) {
    if (length(aggregation_order) != n || anyNA(aggregation_order) ||
        any(!is.finite(aggregation_order)) ||
        any(aggregation_order != round(aggregation_order))) {
      stop("A numeric aggregation_order must contain integer row indices.",
           call. = FALSE)
    }
    order_index <- as.integer(aggregation_order)
    if (!identical(sort(order_index), seq_len(n))) {
      stop("A numeric aggregation_order must be a permutation of row indices.",
           call. = FALSE)
    }
    order_rule <- "custom"
  } else {
    aggregation_order <- match.arg(aggregation_order)
    order_index <- if (aggregation_order == "audience_desc") {
      order(-R1, seq_len(n))
    } else seq_len(n)
    order_rule <- aggregation_order
  }

  marginals <- lapply(seq_len(n), function(i) {
    msad_vehicle_distribution(insertions[i], R1[i], R2[i])
  })
  vehicle_reach <- vapply(marginals, function(x) 1 - x[1L], numeric(1))

  current <- marginals[[order_index[1L]]]
  included <- order_index[1L]
  steps <- vector("list", n - 1L)
  for (step in 2:n) {
    added <- order_index[step]
    subset <- c(included, added)
    canonical <- csd_canonical_reach(
      subset, marginals, R1, duplications, tolerance
    )
    conformed <- sequential_conform_pair(
      current, marginals[[added]], canonical$reach, tolerance, "canonical"
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

#' @export
print.reach_csd <- function(x, ...) {
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
