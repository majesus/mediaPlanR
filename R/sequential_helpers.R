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
      "MBD model on schedules of 12 vehicles, and reports prohibitive computing ",
      "time above 13)."),
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
         proportion_hint(R1), call. = FALSE)
  }
  needs_R2 <- insertions >= 2L
  # An all-NA logical column (NA written without a numeric type) is a missing
  # R2, which is only an error when some vehicle needs it.
  if (is.logical(R2) && all(is.na(R2))) R2 <- as.numeric(R2)
  if (!is.numeric(R2)) {
    stop("R2 must be numeric (NA is allowed only for vehicles with a single ",
         "insertion).", call. = FALSE)
  }
  if (anyNA(R2[needs_R2]) || any(!is.finite(R2[needs_R2]))) {
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
      !matrix_is_symmetric_relative(duplications, tolerance)) {
    stop("duplications must be finite and symmetric outside its diagonal.",
         call. = FALSE)
  }
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      lower <- max(0, R1[i] + R1[j] - 1)
      upper <- min(R1[i], R1[j])
      slack <- tolerance * upper
      if (duplications[i, j] < lower - slack ||
          duplications[i, j] > upper + slack) {
        stop(sprintf(
          "duplication [%d,%d] is outside its Frechet bounds [%.8f, %.8f].",
          i, j, lower, upper
        ), call. = FALSE)
      }
    }
  }

  check_triple_feasibility(R1, duplications, tolerance)
  warn_low_duplication(R1, duplications, caller)

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

# Computational budgets of the schedule-level models (audit MP-10). The number
# of vehicles is limited (12 for CBD and MBD), but the number of insertions of
# each vehicle is not, and the size of the objects that the models build grows
# with it. The size and the work are estimated from the inputs before anything
# is allocated, and a schedule beyond the budget is refused with an
# explanation. Both limits can be raised with an option:
#  * mediaPlanR.max_table_cells (default 1e7): cells of the largest joint table
#    that CSD and MSAD build when they combine two distributions (the
#    exposure levels of the aggregate times those of the vehicle added). At 1e7
#    cells each matrix takes 80 MB.
#  * mediaPlanR.max_work (default 1e9): elementary operations of CBD and MBD,
#    estimated as described in cbd_work_estimate() and mbd_work_estimate();
#    1e9 is about one to two minutes on an ordinary computer.
computational_budget <- function(option, default) {
  value <- getOption(option, default)
  if (is.numeric(value) && length(value) == 1L && !is.na(value) && value > 0) {
    value
  } else {
    default
  }
}

# Cells of the joint table of every step of a sequential aggregation: the
# aggregate of the vehicles already combined has 1 + (their total insertions)
# levels and the vehicle added has 1 + its insertions.
check_sequential_table_size <- function(insertions, order_index, caller) {
  sizes <- as.numeric(insertions[order_index])
  aggregate_levels <- 1 + cumsum(sizes)[-length(sizes)]
  added_levels <- 1 + sizes[-1L]
  cells <- aggregate_levels * added_levels
  limit <- computational_budget("mediaPlanR.max_table_cells", 1e7)
  if (max(cells) > limit) {
    step <- which.max(cells)
    stop(sprintf(paste0(
      "%s() would build a joint table of %s cells (%s exposure levels of the ",
      "vehicles already combined times %s of the vehicle added, at step %d of ",
      "the aggregation), beyond its limit of %s. Reduce the number of ",
      "insertions of the vehicles, or raise the limit with ",
      "options(mediaPlanR.max_table_cells = ...) if the memory allows it ",
      "(each matrix of that size takes about %s MB)."),
      caller, format(cells[step], digits = 3),
      format(aggregate_levels[step], digits = 10),
      format(added_levels[step], digits = 10), step,
      format(limit, digits = 3), format(8 * cells[step] / 1e6, digits = 2)),
      call. = FALSE)
  }
  invisible(max(cells))
}

# CBD convolves, in each of the 2^m cells of its (0,1) grid, the conditional
# distributions of the m vehicles one after another; the convolution of a
# vector of length a with one of length b makes a vector operations of length
# b. The estimate is the number of elementary operations,
#   2^m * sum_i (1 + n_1 + ... + n_(i-1)) * (n_i + 1).
cbd_work_estimate <- function(insertions) {
  n <- length(insertions)
  sizes <- as.numeric(insertions)
  prefix <- c(0, cumsum(sizes))[seq_len(n)]
  2^n * sum((1 + prefix) * (sizes + 1))
}

# MBD peels the vehicles one at a time; when r vehicles remain, each of the
# 2^(r-1) patterns of the others has a vector with as many levels as the
# insertions already peeled plus one, and each level is expanded into a
# distribution of (insertions + 1) cells:
#   sum over the peeled vehicles v of 2^(r - 1) * (1 + peeled so far) * (n_v + 1).
# The inclusion-exclusion grid (3^m terms, m of at most 12) does not depend on
# the insertions and is not part of the estimate.
mbd_work_estimate <- function(insertions, order_index) {
  sizes <- as.numeric(insertions[rev(order_index)])
  n <- length(sizes)
  levels <- 1 + c(0, cumsum(sizes))[seq_len(n)]
  remaining <- n - seq_len(n) + 1
  sum(2^(remaining - 1) * levels * (sizes + 1))
}

check_work_budget <- function(work, caller, what) {
  limit <- computational_budget("mediaPlanR.max_work", 1e9)
  if (work > limit) {
    stop(sprintf(paste0(
      "%s() would need about %s elementary operations for this schedule (%s), ",
      "beyond its limit of %s, which is roughly one to two minutes of ",
      "computation. Reduce the number of vehicles or of insertions per ",
      "vehicle, or raise the limit with options(mediaPlanR.max_work = ...) if ",
      "the computing time is acceptable."),
      caller, format(work, digits = 3), what, format(limit, digits = 3)),
      call. = FALSE)
  }
  invisible(work)
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
  dbetabinom(0:insertions, size = insertions,
                      alpha = params$alpha, beta = params$beta)
}

# Reach (probability of at least one exposure) of a distribution of counts, as
# the sum of its positive cells. It is not 1 - P(0): for a small audience P(0)
# rounds to one (below about 1e-16 the whole reach is lost, and above it the
# reach keeps only a few digits), whereas the positive cells carry their own
# digits.
distribution_reach <- function(distribution) sum(distribution[-1L])

# Conforms the joint table of two distributions (`left`, `right`) to a target
# reach of their union: it keeps both margins, sets the zero cell to one minus
# the target reach and splits the rest non-randomly, then collapses the table
# by total number of exposures. Shared by CSD and MSAD.
#
# The reach of each margin and the table are built from the positive cells,
# never from 1 - P(0), and the table is built from the probability of
# being reached by both distributions, `both`, which fixes the union
# (left_reach + right_reach - both): the cells "left only" and "right only"
# are then differences of numbers of the same order as themselves, whereas
# target_reach - right_reach loses every digit of a small audience combined
# with a large one. `duplicated_reach` supplies that probability directly when
# the caller can compute it without the cancellation (CSD); otherwise it is
# left_reach + right_reach - target_reach (MSAD). It is bounded to the
# interval that the margins allow, which makes the margins hold by
# construction.
sequential_conform_pair <- function(left, right, target_reach, tolerance,
                                    model_label = "Sequential",
                                    duplicated_reach = NULL) {
  if (!all(is.finite(left)) || !all(is.finite(right)) ||
      !is.finite(target_reach) ||
      (!is.null(duplicated_reach) && !is.finite(duplicated_reach))) {
    stop(sprintf(paste0(
      "The %s conformance received a non-finite probability (a marginal ",
      "distribution or the target reach): the inputs are outside the range ",
      "in which the model can be evaluated in double precision."),
      model_label), call. = FALSE)
  }
  left_reach <- distribution_reach(left)
  right_reach <- distribution_reach(right)
  lower <- max(left_reach, right_reach)
  upper <- min(1, left_reach + right_reach)
  if (target_reach < lower - tolerance || target_reach > upper + tolerance) {
    stop(sprintf(
      paste0("The %s reach %.8g is incompatible with the two ",
             "marginal reaches; the feasible interval is [%.8g, %.8g]."),
      model_label, target_reach, lower, upper
    ), call. = FALSE)
  }
  target_reach <- min(upper, max(lower, target_reach))

  both <- if (is.null(duplicated_reach)) {
    left_reach + right_reach - target_reach
  } else {
    duplicated_reach
  }
  both <- min(min(left_reach, right_reach),
              max(max(0, left_reach + right_reach - 1), both))
  left_only <- left_reach - both
  right_only <- right_reach - both
  none <- max(0, 1 - (left_reach + right_reach - both))

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
  if (!is.finite(max(row_error, column_error, probability_error)) ||
      max(row_error, column_error, probability_error) > tolerance * 10) {
    stop(sprintf("%s conformance failed to preserve its probability margins.",
                 model_label),
         call. = FALSE)
  }

  # Total number of exposures: the cells of each column of the table go to the
  # levels (row - 1) + (column - 1). Column by column the work is linear in
  # the number of cells.
  collapsed <- numeric(length(left) + length(right) - 1L)
  for (column in seq_along(right)) {
    index <- seq_along(left) + (column - 1L)
    collapsed[index] <- collapsed[index] + joint[, column]
  }
  collapsed <- collapsed / sum(collapsed)

  list(
    distribution = collapsed,
    joint = joint,
    diagnostics = c(
      left_reach = left_reach,
      right_reach = right_reach,
      target_reach = left_reach + right_reach - both,
      duplicated_reach = both,
      row_margin_error = row_error,
      column_margin_error = column_error,
      probability_error = probability_error
    )
  )
}
