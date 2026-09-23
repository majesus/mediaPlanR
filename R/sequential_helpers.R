# Input validation shared by the schedule-level models with the same interface
# (calc_csd, calc_msad, calc_cbd, calc_mbd): a data frame with one row per
# vehicle (`insertions`, `R1`, `R2`), a matrix of pairwise one-insertion
# duplications, an aggregation order, a population and a tolerance.
#
# Returns the validated pieces (number of vehicles, insertions, R1, R2 and the
# resolved aggregation order and rule).
validate_sequential_inputs <- function(vehicles_data, duplications,
                                       aggregation_order, population,
                                       tolerance, max_vehicles = Inf,
                                       caller = "calc_csd") {
  required <- c("insertions", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required %in% names(vehicles_data)) ||
      nrow(vehicles_data) < 2L) {
    stop("vehicles_data must contain at least two rows and columns ",
         "insertions, R1, and R2.", call. = FALSE)
  }
  n <- nrow(vehicles_data)
  if (n > max_vehicles) {
    stop(sprintf(paste0(
      "%s() supports at most %d vehicles because its exposure grid grows ",
      "exponentially with the number of vehicles (Cheong, 2007, tested the ",
      "MBD model computationally up to 12-13 vehicles)."),
      caller, as.integer(max_vehicles)), call. = FALSE)
  }
  assert_number(population, "population", min = 0, min_open = TRUE)
  assert_number(tolerance, "tolerance", min = 0, min_open = TRUE)

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
  if (!is.numeric(R2) || anyNA(R2[needs_R2]) || any(!is.finite(R2[needs_R2]))) {
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
    rule <- tryCatch(
      match.arg(aggregation_order, c("audience_desc", "given")),
      error = function(e) stop("aggregation_order must be \"audience_desc\", ",
                               "\"given\", or a permutation of the row indices.",
                               call. = FALSE)
    )
    order_index <- if (rule == "audience_desc") order(-R1, seq_len(n)) else seq_len(n)
    order_rule <- rule
  }

  list(n = n, insertions = insertions, R1 = R1, R2 = R2,
       order_index = order_index, order_rule = order_rule)
}

# Exposure distribution of one vehicle for its own insertions, from R1 and
# R2 (Beta-Binomial, with the binomial and polarized limits handled
# explicitly); a single insertion is Bernoulli(R1).
vehicle_exposure_distribution <- function(insertions, R1, R2) {
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

# Conforms the joint table of two distributions (`left`, `right`) to a target
# reach of their union: it keeps both margins, sets the zero cell to one minus
# the target reach and splits the rest non-randomly, then collapses the table
# by total number of exposures. Shared by CSD and MSAD.
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
