# Internal argument validators shared by the exported functions. Every
# validator stops with a message that names the offending argument, so that
# NA, NaN, Inf, wrong types and wrong lengths never surface as cryptic
# base-R errors ("missing value where TRUE/FALSE needed").

# Floating-point slack for constraints that must hold exactly: a set cannot be
# smaller than one of its subsets, nor larger than the universe that contains
# it. It covers the rounding error of double arithmetic only (a few units in
# the last place of the magnitude involved, `scale`), so it is far below one
# person for any realistic universe. It is not a measurement tolerance: a
# violation by one person must always be reported. The slack is vectorized over
# `scale`.
exact_constraint_slack <- function(scale) {
  64 * .Machine$double.eps * abs(scale)
}

# Wording of an admissible interval as list(adjective, trailing phrase), so
# that messages read "one finite non-negative number" or "one finite number
# in [0, 1]".
interval_wording <- function(min, max, min_open, max_open, integer = FALSE) {
  if (is.finite(min) && is.finite(max)) {
    list(adjective = "", trailing = sprintf(
      " in %s%s, %s%s", if (min_open) "(" else "[", format(min),
      format(max), if (max_open) ")" else "]"))
  } else if (is.finite(min)) {
    if (!min_open && min == 0) list(adjective = "non-negative ", trailing = "")
    else if (min_open && min == 0) list(adjective = "positive ", trailing = "")
    else if (integer && !min_open && min == 1) list(adjective = "positive ", trailing = "")
    else list(adjective = "", trailing = sprintf(
      " %s %s", if (min_open) "greater than" else "of at least", format(min)))
  } else if (is.finite(max)) {
    list(adjective = "", trailing = sprintf(
      " %s %s", if (max_open) "less than" else "of at most", format(max)))
  } else {
    list(adjective = "", trailing = "")
  }
}

within_interval <- function(x, min, max, min_open, max_open) {
  all(if (min_open) x > min else x >= min) &&
    all(if (max_open) x < max else x <= max)
}

# Extra sentence for arguments that must be proportions: values above 1 and up
# to 100 are most likely percentages entered by mistake.
proportion_hint <- function(x) {
  if (is.numeric(x) && length(x) >= 1L && !anyNA(x) && all(is.finite(x)) &&
      any(x > 1) && all(x <= 100)) {
    " The values look like percentages: use proportions between 0 and 1, for example 0.30 instead of 30."
  } else {
    ""
  }
}

# One finite number, optionally restricted to an interval and/or to integers.
# `proportion = TRUE` marks arguments that must be proportions (see
# proportion_hint()).
assert_number <- function(x, name, min = -Inf, max = Inf,
                          min_open = FALSE, max_open = FALSE,
                          integer = FALSE, allow_inf = FALSE,
                          proportion = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    (allow_inf || is.finite(x))
  if (ok) {
    ok <- within_interval(x, min, max, min_open, max_open) &&
      (!integer || is.infinite(x) || x == round(x))
  }
  if (!ok) {
    w <- interval_wording(min, max, min_open, max_open, integer)
    stop(name, " must be one ", if (!allow_inf) "finite " else "", w$adjective,
         if (integer) "integer" else "number", w$trailing, ".",
         if (proportion) proportion_hint(x), call. = FALSE)
  }
  invisible(x)
}

# A numeric vector of finite values, optionally restricted to an interval
# and/or to integers, with a minimum (or exact) length.
assert_numeric_vector <- function(x, name, min = -Inf, max = Inf,
                                  min_open = FALSE, max_open = FALSE,
                                  integer = FALSE, min_length = 1L,
                                  length = NULL, proportion = FALSE) {
  ok <- is.numeric(x) && base::length(x) >= min_length && !anyNA(x) &&
    all(is.finite(x))
  if (ok && !is.null(length)) ok <- base::length(x) == length
  if (ok) {
    ok <- within_interval(x, min, max, min_open, max_open) &&
      (!integer || all(x == round(x)))
  }
  if (!ok) {
    w <- interval_wording(min, max, min_open, max_open, integer)
    size <- if (!is.null(length)) paste0("of length ", length, " with ")
    else if (min_length > 1L) paste0("of length at least ", min_length, " with ")
    else "of "
    stop(name, " must be a numeric vector ", size, "finite ", w$adjective,
         if (integer) "integers" else "numbers", w$trailing, ".",
         if (proportion) proportion_hint(x), call. = FALSE)
  }
  invisible(x)
}

# One non-missing logical value.
assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

# A symmetric numeric matrix of side `n`. The diagonal is checked only when
# `diagonal_used` is TRUE.
matrix_is_symmetric_relative <- function(m, tolerance) {
  left <- m[upper.tri(m)]
  right <- t(m)[upper.tri(m)]
  # Compare each pair on its own scale. all.equal() can switch to absolute
  # differences near zero and let a tiny audience change from disjoint to
  # perfectly coincident when its vehicle order is reversed.
  all(abs(left - right) <= tolerance * pmax(abs(left), abs(right)))
}

assert_symmetric_matrix <- function(m, name, n, tolerance = 1e-8,
                                    diagonal_used = FALSE) {
  if (!is.matrix(m) || !is.numeric(m) || !identical(dim(m), c(n, n))) {
    stop(name, " must be a numeric ", n, " x ", n, " matrix.", call. = FALSE)
  }
  values <- if (diagonal_used) m else m[row(m) != col(m)]
  if (anyNA(values) || any(!is.finite(values))) {
    stop(name, " must be finite",
         if (!diagonal_used) " outside its diagonal", ".", call. = FALSE)
  }
  if (!matrix_is_symmetric_relative(m, tolerance)) {
    stop(name, " must be symmetric", if (!diagonal_used) " outside its diagonal",
         ".", call. = FALSE)
  }
  invisible(m)
}

# Inputs of the ad hoc duplication models (Agostini, Hofmans): the audience of
# each vehicle for one insertion and the pairwise duplicated audiences, all in
# people. Each duplication must respect the Frechet bounds implied by the two
# audiences and the population, so that the matrix describes a feasible
# overlap between vehicles.
validate_duplication_inputs <- function(audiences, population,
                                        duplication_matrix) {
  assert_numeric_vector(audiences, "audiences", min = 0, min_open = TRUE,
                        min_length = 2L)
  n <- length(audiences)
  assert_number(population, "population", min = 0, min_open = TRUE)
  if (any(audiences > population)) {
    stop("audiences cannot exceed population.", call. = FALSE)
  }
  assert_symmetric_matrix(duplication_matrix, "duplication_matrix", n,
                          tolerance = 1e-9)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      lower <- max(0, audiences[i] - (population - audiences[j]))
      upper <- min(audiences[i], audiences[j])
      # Rounding error of each side: the upper bound is one of the audiences,
      # the lower bound is computed from the population.
      lower_slack <- exact_constraint_slack(population)
      upper_slack <- exact_constraint_slack(upper)
      value <- duplication_matrix[i, j]
      if (value < lower - lower_slack || value > upper + upper_slack) {
        stop(sprintf(paste0(
          "duplication_matrix[%d, %d] must lie between %.6g and %.6g, the ",
          "bounds implied by audiences %d and %d and the population."),
          i, j, lower, upper, i, j), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

# An ad hoc reach formula can return a value that no plan could produce. The
# reach of a schedule cannot be smaller than its largest audience, nor larger
# than the population or the gross audience. The value is returned as
# computed, but the analyst is told.
check_reach_bounds <- function(reach, audiences, population, model) {
  if (!is.finite(reach)) {
    stop(model, " reach must be finite; the supplied values exceed the representable range.",
         call. = FALSE)
  }
  tolerance <- 1e-9 * population
  lower <- max(audiences)
  upper <- min(population, sum(audiences))
  if (reach < lower - tolerance || reach > upper + tolerance) {
    warning(sprintf(paste0(
      "The %s formula returned a reach of %.6g people, outside the logical ",
      "range [%.6g, %.6g] for these audiences and this population. The ",
      "duplication inputs (or the coefficient) are not consistent with the ",
      "model."), model, reach, lower, upper), call. = FALSE)
  }
  invisible(reach)
}
