msad_vehicle_distribution <- function(insertions, R1, R2) {
  if (insertions == 1L) {
    return(c(1 - R1, R1))
  }
  params <- calculate_bbd_params(R1, R2)
  if (params$type == "binomial_limit") {
    return(stats::dbinom(0:insertions, size = insertions, prob = R1))
  }
  if (params$type == "polarized_limit") {
    out <- numeric(insertions + 1L)
    out[c(1L, insertions + 1L)] <- c(1 - R1, R1)
    return(out)
  }
  extraDistr::dbbinom(0:insertions, size = insertions,
                      alpha = params$alpha, beta = params$beta)
}

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

sequential_conform_pair <- function(left, right, target_reach, tolerance,
                                    model_label = "Sequential") {
  left_reach <- 1 - left[1L]
  right_reach <- 1 - right[1L]
  lower <- max(left_reach, right_reach)
  upper <- min(1, left_reach + right_reach)
  if (target_reach < lower - tolerance || target_reach > upper + tolerance) {
    stop(sprintf(
      paste0("The %s reach %.8f is incompatible with the two ",
             "marginal reaches; the feasible interval is [%.8f, %.8f]."),
      model_label, target_reach, lower, upper
    ), call. = FALSE)
  }
  target_reach <- min(upper, max(lower, target_reach))

  none <- 1 - target_reach
  both <- left_reach + right_reach - target_reach
  left_only <- target_reach - right_reach
  right_only <- target_reach - left_reach

  joint <- matrix(0, nrow = length(left), ncol = length(right))
  joint[1L, 1L] <- none
  left_positive <- left[-1L] / left_reach
  right_positive <- right[-1L] / right_reach
  joint[-1L, 1L] <- left_only * left_positive
  joint[1L, -1L] <- right_only * right_positive
  joint[-1L, -1L] <- both * tcrossprod(left_positive, right_positive)

  row_error <- max(abs(rowSums(joint) - left))
  column_error <- max(abs(colSums(joint) - right))
  probability_error <- abs(sum(joint) - 1)
  if (max(row_error, column_error, probability_error) > tolerance * 10) {
    stop(sprintf("%s conformance failed to preserve its probability margins.",
                 model_label),
         call. = FALSE)
  }

  total_contacts <- outer(seq_along(left) - 1L, seq_along(right) - 1L, `+`)
  collapsed <- vapply(0:max(total_contacts), function(value) {
    sum(joint[total_contacts == value])
  }, numeric(1))
  collapsed <- collapsed / sum(collapsed)

  list(
    distribution = collapsed,
    joint = joint,
    diagnostics = c(
      left_reach = left_reach,
      right_reach = right_reach,
      target_reach = target_reach,
      duplicated_reach = both,
      row_margin_error = row_error,
      column_margin_error = column_error,
      probability_error = probability_error
    )
  )
}

#' Morgensztern Sequential Aggregation Distribution model
#'
#' Implements the MSAD structure described by Leckenby and Rice (1986) and
#' Kim (2005): each vehicle is expanded with a Beta-Binomial marginal,
#' schedule reach is estimated with the Morgensztern formula, and the
#' marginals are sequentially combined using a non-random, margin-preserving
#' convolution whose zero cell equals one minus the estimated schedule reach.
#' Kim specifies the MSAD algorithm but refers readers to Lee (1988) for its
#' complete worked numerical example; Kim's own three-vehicle example is CSD,
#' not MSAD.
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1`, and `R2`.
#'   `R1` is single-insertion reach and `R2` is two-insertion cumulative reach,
#'   both as proportions. `R2` may be `NA` only when `insertions` is one.
#' @param duplications Symmetric matrix of pairwise single-insertion audience
#'   duplication proportions. Its diagonal is ignored.
#' @param aggregation_order Either `"audience_desc"` (Kim's larger-audience-
#'   first rule), `"given"`, or a permutation of row indices.
#' @param population Positive population used to express probabilities as
#'   people. It does not affect the model estimates.
#' @param tolerance Numerical tolerance used for probability constraints.
#'
#' @return A `reach_msad` object containing the complete contact distribution,
#'   cumulative reach, vehicle marginals, aggregation steps, and diagnostics.
#'
#' @details
#' For a subset of vehicles, Morgensztern reach is calculated as
#' \deqn{R_m = (\sum_i R_{n_i})^2 / [\sum_i R_{n_i} +
#' \sum_{i<j} K_{ij} A_{ij}R_{n_i}R_{n_j}/(A_iA_j)]}
#' with \eqn{K_{ij}=(A_i+A_j)/(A_i+A_j-A_{ij})}. At every aggregation step,
#' the joint table is conformed to the two input marginal distributions and
#' to the Morgensztern union reach. Consequently, all probabilities remain
#' non-negative, the margins are preserved, and the zero-contact probability
#' is exactly `1 - R_m`.
#'
#' MSAD is intended to reduce, not guarantee the elimination of, declining
#' reach. The reach formula can also be incompatible with the supplied
#' marginal distributions. In that case the function stops and reports the
#' feasible interval instead of silently altering the target. Aggregation
#' order can change the contact distribution even when final schedule reach is
#' unchanged.
#'
#' @references
#' Leckenby, J. D., & Rice, M. D. (1986). The Declining Reach Phenomenon in
#' Exposure Distribution Models. Journal of Advertising, 15(3), 13-20.
#' \doi{10.1080/00913367.1986.10673014}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 65-71.
#' Kim identifies the detailed MSAD numerical example as Lee (1988), pp. 81-101.
#'
#' @examples
#' # MSAD calculation derived from the inputs of Kim's CSD example. The
#' # resulting MSAD values are not presented by Kim as a published benchmark.
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
#' result <- calc_msad(vehicles, duplication, aggregation_order = 1:3)
#' result$reach
#'
#' @export
calc_msad <- function(vehicles_data, duplications,
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
      any(insertions < 1 | insertions != round(insertions))) {
    stop("insertions must contain positive integers.", call. = FALSE)
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
  invisible(lapply(which(needs_R2), function(i) calculate_bbd_params(R1[i], R2[i])))

  if (!is.matrix(duplications) || !is.numeric(duplications) ||
      !identical(dim(duplications), c(n, n))) {
    stop("duplications must be a numeric square matrix matching vehicles_data.",
         call. = FALSE)
  }
  off_diagonal <- row(duplications) != col(duplications)
  if (anyNA(duplications[off_diagonal]) ||
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
      if (!is.finite(duplications[i, j]) ||
          duplications[i, j] < lower - tolerance ||
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
