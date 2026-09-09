mbd_local_bbd <- function(R1_subset, dup_values) {
  mean_r1 <- mean(R1_subset)
  mean_dup <- mean(dup_values)
  mean_r2 <- 2 * mean_r1 - mean_dup
  tryCatch(
    calculate_bbd_params(mean_r1, mean_r2),
    error = function(e) {
      stop(sprintf(paste0(
        "calc_mbd(): could not impute the co-exposure probability for a ",
        "subset of %d vehicles (mean audience %.6f, mean pairwise ",
        "duplication %.6f): %s. This means the pairwise duplications are ",
        "individually feasible (they satisfy their own Frechet bounds) but ",
        "are jointly inconsistent with the Beta-Binomial imputation this ",
        "subset of three or more vehicles relies on (Cheong 2007, Ch. 4.2)."),
        length(R1_subset), mean_r1, mean_dup, conditionMessage(e)), call. = FALSE)
    }
  )
}

mbd_top_cell <- function(params, size) {
  if (params$type == "binomial_limit") {
    stats::dbinom(size, size, params$p)
  } else if (params$type == "polarized_limit") {
    params$p
  } else {
    extraDistr::dbbinom(size, size, alpha = params$alpha, beta = params$beta)
  }
}

mbd_shrink_to_subtuples <- function(candidate, subtuple_values, decay = 0.99) {
  for (bound in subtuple_values) {
    if (candidate > bound) candidate <- decay * bound
  }
  max(candidate, 0)
}

mbd_key <- function(subset) {
  if (length(subset) == 0L) "0" else paste(sort(subset), collapse = "_")
}

# All subsets of `set`, including the empty set and `set` itself. Written to
# avoid utils::combn()'s scalar-interpretation trap: combn(x, k) treats a
# length-one numeric x as the count seq_len(x) rather than the one-element
# set {x}, which silently corrupts any subset enumeration once a branch
# shrinks to a single remaining element.
mbd_all_subsets <- function(set) {
  n <- length(set)
  if (n == 0L) return(list(integer(0)))
  if (n == 1L) return(list(integer(0), set))
  out <- list(integer(0))
  for (k in seq_len(n)) out <- c(out, utils::combn(set, k, simplify = FALSE))
  out
}

# S[[key]] = P(all vehicles named in key are exposed), key = mbd_key(subset).
# Pairs are taken directly from observed duplications; triples and larger are
# imputed from a Beta-Binomial fitted to the subset's own mean audience and
# mean pairwise duplication (Cheong 2007, Ch. 4.2 and Appendix D, Steps 3-4),
# then shrunk (Cheong's first-order consistency check) so that no subset's
# co-exposure probability exceeds that of any of its immediate subsets.
mbd_coexposure_sums <- function(R1, duplications, tolerance) {
  n <- length(R1)
  S <- list("0" = 1)
  for (i in seq_len(n)) S[[as.character(i)]] <- R1[i]
  if (n >= 2) {
    for (p in utils::combn(n, 2, simplify = FALSE)) {
      S[[mbd_key(p)]] <- duplications[p[1], p[2]]
    }
  }
  if (n >= 3) {
    for (size in 3:n) {
      for (combo in utils::combn(n, size, simplify = FALSE)) {
        pair_keys <- utils::combn(combo, 2, simplify = FALSE)
        dup_vals <- vapply(pair_keys, function(pp) duplications[pp[1], pp[2]], numeric(1))
        params <- mbd_local_bbd(R1[combo], dup_vals)
        top <- mbd_top_cell(params, size)
        sub_combos <- utils::combn(combo, size - 1, simplify = FALSE)
        sub_values <- vapply(sub_combos, function(sc) S[[mbd_key(sc)]], numeric(1))
        S[[mbd_key(combo)]] <- mbd_shrink_to_subtuples(top, sub_values)
      }
    }
  }
  S
}

# The exclusive (0,1) grid over all n vehicles: exclusive[[key(T)]] = the
# probability that exactly the vehicles in T are exposed and no others, by
# direct inclusion-exclusion on the co-exposure sums S(.) (Waring 1792;
# Cheong 2007, pp. 58-59 and 63-64). Cost is O(3^n); calc_mbd() caps n.
mbd_exclusive_grid <- function(S, n, tolerance) {
  all_idx <- seq_len(n)
  all_subsets <- mbd_all_subsets(all_idx)
  exclusive <- new.env(parent = emptyenv())
  for (t in all_subsets) {
    complement <- setdiff(all_idx, t)
    v_subsets <- mbd_all_subsets(complement)
    total <- 0
    for (v in v_subsets) {
      sign <- if (length(v) %% 2 == 0) 1 else -1
      total <- total + sign * S[[mbd_key(union(t, v))]]
    }
    if (total < 0 && total > -tolerance) total <- 0
    exclusive[[mbd_key(t)]] <- total
  }
  exclusive
}

# Solves Cheong's (2007, Appendix B) simultaneous equations for the
# proportions (a, b) of the "0" and "1" insertion-level Beta-Binomial rows
# that a specific row of the joint table is split into, subject to the same
# data-consistency bounds Cheong derives from the marginal BBD of the vehicle
# being expanded (P0 and P1 of that BBD, Appendix B).
mbd_conditional_allocate <- function(exposed_mass, total_mass, alpha_c, beta_c) {
  if (total_mass <= 0) return(list(a = 1, b = 0))
  cc <- exposed_mass / total_mass
  if (is.infinite(alpha_c)) {
    # Binomial limit (alpha_c = beta_c = Inf): the "0" and "1" rows this
    # splits between are identical Binomial distributions (see
    # mbd_peel_vehicle()), so any split with a + b = 1 reproduces the same
    # row; ab1/lower/upper below would otherwise divide Inf by Inf.
    return(list(a = 1 - cc, b = cc))
  }
  ab1 <- alpha_c + beta_c + 1
  lower <- alpha_c / ab1
  upper <- (alpha_c + 1) / ab1
  if (cc < lower) cc <- lower
  if (cc > upper) cc <- upper
  b <- cc * ab1 - alpha_c
  b <- min(max(b, 0), 1)
  list(a = 1 - b, b = b)
}

# Peels vehicle `v` (one of the still-unexpanded vehicles) out of `table`, a
# named list keyed by mbd_key() of the *other* remaining vehicles' exposure
# subset, each holding a numeric vector over the pseudo-vehicle's current
# exposure levels (0, 1, 2, ...). Every level of every remaining pattern gets
# its own Beta-Binomial conditional split (Cheong 2007, pp. 65-72): this is
# what lets, e.g., the "B exposed" and "B not exposed" rows of a pseudo-
# vehicle expand vehicle A differently. Single-insertion vehicles need no
# split: exposure is already a deterministic 0/1 exposure contribution.
mbd_peel_vehicle <- function(table, other_keys_subsets, v, alpha_c, beta_c, p_c,
                             vehicle_size, tolerance) {
  new_table <- vector("list", length(other_keys_subsets))
  names(new_table) <- vapply(other_keys_subsets, mbd_key, character(1))
  if (is.na(alpha_c)) {
    for (i in seq_along(other_keys_subsets)) {
      os <- other_keys_subsets[[i]]
      col0 <- table[[mbd_key(os)]]
      col1 <- table[[mbd_key(union(os, v))]]
      new_len <- length(col0) + 1L
      new_rows <- numeric(new_len)
      new_rows[seq_along(col0)] <- new_rows[seq_along(col0)] + col0
      new_rows[seq_along(col1) + 1L] <- new_rows[seq_along(col1) + 1L] + col1
      new_table[[i]] <- new_rows
    }
    return(new_table)
  }
  if (is.infinite(alpha_c)) {
    # Binomial limit: as alpha_c, beta_c -> Inf with alpha_c/(alpha_c+beta_c)
    # fixed at p_c, both Beta(alpha_c, beta_c+1) and Beta(alpha_c+1, beta_c)
    # converge to the same point mass at p_c, so conditioning on the "0" or
    # "1" row leaves the vehicle's own distribution unchanged (no person-level
    # heterogeneity to condition on). Computing this directly also avoids
    # extraDistr::dbbinom(alpha = Inf, beta = Inf), which returns NaN.
    dist0 <- dist1 <- stats::dbinom(0:vehicle_size, size = vehicle_size, prob = p_c)
  } else if (alpha_c == 0 && beta_c == 0) {
    # Polarized limit: Beta(0, beta_c+1) is a point mass at p=0 and
    # Beta(alpha_c+1, 0) a point mass at p=1, so the "0" row is surely
    # zero exposures and the "1" row is surely vehicle_size exposures
    # (Cheong's all-or-nothing exposure).
    dist0 <- c(1, numeric(vehicle_size))
    dist1 <- c(numeric(vehicle_size), 1)
  } else {
    alpha0 <- alpha_c; beta0 <- beta_c + 1
    alpha1 <- alpha_c + 1; beta1 <- beta_c
    dist0 <- extraDistr::dbbinom(0:vehicle_size, size = vehicle_size, alpha = alpha0, beta = beta0)
    dist1 <- extraDistr::dbbinom(0:vehicle_size, size = vehicle_size, alpha = alpha1, beta = beta1)
  }
  for (i in seq_along(other_keys_subsets)) {
    os <- other_keys_subsets[[i]]
    col0 <- table[[mbd_key(os)]]
    col1 <- table[[mbd_key(union(os, v))]]
    new_len <- length(col0) + vehicle_size
    new_rows <- numeric(new_len)
    for (r in seq_along(col0)) {
      m0 <- col0[r]; m1 <- col1[r]
      total_mass <- m0 + m1
      alloc <- mbd_conditional_allocate(m1, total_mass, alpha_c, beta_c)
      row_dist <- alloc$a * dist0 + alloc$b * dist1
      idx <- (r - 1L) + seq_along(row_dist)
      new_rows[idx] <- new_rows[idx] + total_mass * row_dist
    }
    new_table[[i]] <- new_rows
  }
  new_table
}

# Cheong's (2007) "MBD-ADJ": zero any remaining negative cell of the final
# collapsed distribution and redistribute that mass proportionally across the
# non-negative cells (Ch. 4.4, steps 1-4).
mbd_safety_net <- function(distribution, tolerance) {
  negative_mass <- -sum(pmin(distribution, 0))
  adjusted <- pmax(distribution, 0)
  positive_sum <- sum(adjusted)
  if (negative_mass > tolerance && positive_sum > tolerance) {
    adjusted <- adjusted + negative_mass * adjusted / positive_sum
  }
  adjusted <- adjusted / sum(adjusted)
  list(distribution = adjusted, negative_mass_adjusted = negative_mass,
       cells_adjusted = sum(distribution < -tolerance))
}

#' Multivariate Beta Binomial Distribution model
#'
#' Implements Cheong's (2007) Multivariate Beta Binomial Distribution (MBD).
#' Vehicle co-exposure probabilities for pairs (observed) and for three or
#' more vehicles (imputed from a Beta-Binomial fitted to the subset's own
#' mean audience and mean pairwise duplication) are combined into the full
#' \eqn{2^m} joint zero/one exposure grid via Waring's (1792) inclusion-
#' exclusion theorem. Vehicles are then peeled off one at a time, in reverse
#' aggregation order: each remaining exposure pattern's own row is expanded
#' from a zero/one state into the peeled vehicle's own insertion-level
#' Beta-Binomial distribution using a row-specific conditional allocation,
#' and the two component distributions are convolved into a growing
#' pseudo-vehicle.
#'
#' @param vehicles_data Data frame with columns `insertions`, `R1`, and `R2`,
#'   using the same convention as [calc_csd()] (`R2` may be `NA` only when
#'   `insertions` is one).
#' @param duplications Symmetric matrix of pairwise one-insertion audience
#'   duplication proportions. Diagonal values are ignored.
#' @param aggregation_order Either `"audience_desc"` (Cheong's rule: vehicles
#'   are aggregated in decreasing order of audience and duplication
#'   magnitude), `"given"`, or a permutation of row indices. Vehicles are
#'   peeled off starting from the *last* position in this order, matching
#'   Cheong's own worked examples.
#' @param population Positive population used only to express probabilities
#'   as people.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'
#' @return A `reach_mbd` object containing reach, the complete exposure
#'   distribution, the aggregation order used, and diagnostics, including
#'   whether the final negative-probability safety net (Cheong's "MBD-ADJ")
#'   had to be engaged and how much probability mass it redistributed.
#'
#' @details
#' `calc_mbd()` (Cheong 2007) is unrelated to `fit_bbd_to_reach()` (which
#' fits one Beta-Binomial to an externally given reach target). The two are
#' easy to confuse by name alone: MBD is Cheong's (2007) *Multivariate* Beta
#' Binomial Distribution described here, while `fit_bbd_to_reach()` fits a
#' *single-vehicle* Beta-Binomial.
#'
#' # What is, and is not, guaranteed
#'
#' Cheong's own dissertation reports that the inclusion-exclusion grid can
#' produce small negative cell probabilities once four or more vehicles are
#' combined, because the co-exposure probability of three or more vehicles is
#' *imputed* (there is no closed-form multivariate distribution behind it),
#' not observed. Cheong's worked four-vehicle example is fully corrected by a
#' first-order consistency check (every combined-exposure probability is
#' shrunk to remain below each of its immediate subsets); the five-vehicle
#' example is not, and the dissertation explicitly states that resolving this
#' in general would require increasingly complex higher-order checks that
#' were never implemented, proposing iterative proportional fitting as
#' unstarted future work. `calc_mbd()` therefore applies exactly the checks
#' Cheong specifies (first-order only) and, exactly as Cheong's own MBD-ADJ
#' variant does, zeroes any cell that is still negative afterwards and
#' redistributes that mass proportionally across the non-negative cells of
#' the final collapsed distribution. This is reported in `diagnostics`
#' (`negative_mass_adjusted`, `cells_adjusted`); a non-zero value means the
#' result for that specific schedule relies on this fallback rather than on a
#' value Cheong verified as internally consistent without it.
#'
#' Cheong also reports that the aggregation order can change the collapsed
#' distribution, that this was not investigated systematically, and that the
#' model was only tested computationally up to 12-13 vehicles because of the
#' exponential cost of the exposure grid. `calc_mbd()` stops with an
#' informative error above `max_vehicles` for this reason, and warns for four
#' or more vehicles, the range where Cheong's own checks are known to become
#' insufficient on their own.
#'
#' In exchange, Cheong reports MBD as the most accurate of the eleven models
#' tested for reach alone (comScore 2003 data, N=440 schedules), but only
#' middling for the complete exposure-frequency distribution, behind the
#' already-implemented [calc_canex()] and the (not yet implemented)
#' Conditional Beta Distribution model.
#'
#' @references
#' Cheong, Y. (2007). Multivariate Beta Binomial Distribution Model as a Web
#' Media Exposure Model. Doctoral dissertation, The University of Texas at
#' Austin.
#'
#' Waring, E. (1792). Meditationes Algebraicae. Cambridge.
#'
#' @examples
#' # Cheong (2007), Chapter 4.2: three-vehicle conceptual example.
#' vehicles <- data.frame(
#'   insertions = c(2, 1, 3),
#'   R1 = c(0.146, 0.110, 0.252),
#'   R2 = c(0.191, NA, 0.318)
#' )
#' duplication <- matrix(
#'   c(NA, 0.032, 0.063,
#'     0.032, NA, 0.041,
#'     0.063, 0.041, NA),
#'   nrow = 3, byrow = TRUE
#' )
#' result <- calc_mbd(vehicles, duplication, aggregation_order = 1:3)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_csd()] and [calc_msad()] for sequential aggregation models
#'   with a completely verified reference example; [calc_cbd()] for the
#'   model sharing this function's exact within-vehicle peeling step, with a
#'   different (canonical-expansion, not imputed) between-vehicle step.
#' @export
calc_mbd <- function(vehicles_data, duplications,
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
      "calc_mbd() supports at most %d vehicles. Cheong (2007) only tested ",
      "the model computationally up to 12-13 vehicles because the exposure ",
      "grid grows exponentially; beyond that the result would be both slow ",
      "and unverified against any published benchmark."), max_vehicles),
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

  if (n >= 4L) {
    warning(paste0(
      "calc_mbd(): with 4 or more vehicles, Cheong (2007) applies only a ",
      "first-order consistency check to the imputed co-exposure grid. This ",
      "resolved the negative-probability problem in Cheong's own 4-vehicle ",
      "example, but explicitly did not in the 5-vehicle example (higher-",
      "order checks were identified as necessary but never implemented). ",
      "Check diagnostics$negative_mass_adjusted and diagnostics$cells_adjusted."),
      call. = FALSE)
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

  S <- mbd_coexposure_sums(R1, duplications, tolerance)
  exclusive_env <- mbd_exclusive_grid(S, n, tolerance)

  remaining <- seq_len(n)
  table <- list()
  for (t in mbd_all_subsets(remaining)) table[[mbd_key(t)]] <- exclusive_env[[mbd_key(t)]]

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
  class(result) <- "reach_mbd"
  result
}

#' @export
print.reach_mbd <- function(x, ...) {
  cat("Multivariate Beta Binomial Distribution (MBD)\n")
  cat(sprintf("Reach: %.2f%% | Average frequency: %.3f\n",
              x$reach$percent, x$average_frequency))
  cat("Aggregation order:", paste(x$aggregation_order, collapse = " -> "),
      sprintf("(%s)\n", x$aggregation_rule))
  cat(sprintf("Probability sum: %.12f | Mean error: %.3g\n",
              x$diagnostics$probability_sum, x$diagnostics$mean_error))
  if (x$diagnostics$negative_mass_adjusted > 0) {
    cat(sprintf("Safety net engaged: %d cell(s), %.6g probability mass adjusted (Cheong's MBD-ADJ).\n",
                x$diagnostics$cells_adjusted, x$diagnostics$negative_mass_adjusted))
  }
  invisible(x)
}
