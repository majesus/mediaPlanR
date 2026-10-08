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
    dbetabinom(size, size, alpha = params$alpha, beta = params$beta)
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
    # dbetabinom(alpha = Inf, beta = Inf), which returns NaN.
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
    dist0 <- dbetabinom(0:vehicle_size, size = vehicle_size, alpha = alpha0, beta = beta0)
    dist1 <- dbetabinom(0:vehicle_size, size = vehicle_size, alpha = alpha1, beta = beta1)
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

# Package safety net, applied to the FINAL collapsed distribution: zero any
# remaining negative cell and redistribute that mass proportionally across the
# non-negative cells. It uses the correction of Cheong's (2007, Ch. 4.4, steps
# 1-4) "MBD-ADJ", but it is not that procedure: MBD-ADJ applies it to the table
# that Cheong calls the UD (univariate distribution; one cell per exposure
# pattern, 2^m cells), once it is formed and before it is expanded and
# collapsed, so the results can differ even when the final distribution has no
# negative cell (see Cheong's Tables 4.4.3 and 4.4.4). The function is also used
# on the UD of schedule #151 in the tests, to check the direction of the
# correction.
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
#' Vehicle co-exposure probabilities for pairs (observed) and for three or more
#' vehicles (imputed from a Beta-Binomial fitted to the subset's own mean
#' audience and mean pairwise duplication) are combined into the full
#' \eqn{2^m} joint zero/one exposure grid by Waring's (1792) inclusion-exclusion
#' theorem. Vehicles are then peeled off one at a time, in reverse aggregation
#' order: each remaining exposure pattern's row is expanded from a zero/one
#' state into the peeled vehicle's own insertion-level Beta-Binomial
#' distribution by a row-specific conditional allocation, and the two component
#' distributions are convolved into a growing pseudo-vehicle.
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
#'   order of one-insertion reach `R1`, ties in the row order; duplications are
#'   not used to sort), `"given"` (the row order), or a permutation of the row
#'   indices. Vehicles are peeled off starting from the *last* position of this
#'   order, as in Cheong's worked examples. In MBD the order can change both the
#'   reach and the exposure distribution (see Details).
#' @param population Number of people in the population (a count, not a
#'   proportion). It only converts probabilities into people: the `people`
#'   columns of the result are the probability times `population`, and with the
#'   default, 1, they equal the probability.
#' @param tolerance Positive numerical tolerance for probability constraints.
#'   The Fréchet and triple-feasibility checks apply it relative to the smaller
#'   audience involved.
#'
#' @return A `reach_mbd` object: a list with `reach` (`probability`, `percent`
#'   and `people`), `average_frequency`, the complete exposure `distribution`
#'   (`contacts`, `probability`, `percent`, `people` and
#'   `cumulative_probability`), the `aggregation_order` and `aggregation_rule`,
#'   the peeling `steps` and `diagnostics`, which report whether the final
#'   negative-probability safety net (this package's correction of the final
#'   distribution, not Cheong's "MBD-ADJ", which corrects the UD before it is
#'   expanded; see Details) had to be engaged and
#'   how much probability mass it redistributed (`negative_mass_adjusted`,
#'   `cells_adjusted`).
#'
#' @details
#' `calc_mbd()` (Cheong, 2007) is a different procedure from
#' [fit_bbd_to_reach()], which fits
#' one Beta-Binomial to an externally given reach. MBD is the *Multivariate*
#' Beta Binomial Distribution of several vehicles described here, whereas
#' [fit_bbd_to_reach()] fits a single Beta-Binomial to a whole schedule.
#'
#' # What is, and is not, guaranteed
#'
#' Cheong's dissertation reports that the inclusion-exclusion grid can produce
#' small negative cell probabilities once four or more vehicles are combined,
#' because the co-exposure probability of three or more vehicles is *imputed*
#' (no closed-form multivariate distribution stands behind it) rather than
#' observed. Cheong's four-vehicle example is fully corrected by a first-order
#' consistency check (every combined-exposure probability is shrunk to remain
#' below each of its immediate subsets); the five-vehicle example is not, and
#' the dissertation states that resolving this in general would need
#' increasingly complex higher-order checks that were never implemented,
#' proposing iterative proportional fitting as future work. `calc_mbd()`
#' therefore applies exactly the checks Cheong specifies (first order only)
#' and then, as a package safety net, sets any cell that is still negative in
#' the final collapsed distribution to zero and redistributes that mass
#' proportionally across the non-negative cells. This is reported in
#' `diagnostics`; a non-zero value means the result for that schedule relies on
#' this fallback rather than on a value Cheong verified as internally
#' consistent without it. The function warns for four or more vehicles, the
#' range where Cheong's own checks are known to be insufficient by themselves.
#'
#' This safety net is not Cheong's MBD-ADJ. Cheong (2007, Ch. 4.4) applies the
#' same zero-and-redistribute correction to the table that the dissertation
#' calls the UD (univariate distribution; one cell for each pattern of exposure
#' to the `m` vehicles, `2^m` cells), once it is formed and *before* it is
#' expanded and collapsed, so that the later steps work with non-negative
#' values; here the
#' correction acts only on the final distribution, and only if it contains a
#' negative cell. The two procedures can give different results even when the
#' final distribution has no negative cell, so no result of this function
#' should be described as "MBD-ADJ", and the empirical evidence for MBD-ADJ
#' does not transfer to it. The package does not implement the variants MBD,
#' MBD-ADJ and MBD-ADJ2 of the study as such (see the comparison of variants
#' below).
#'
#' Cheong also reports that the aggregation order can change the collapsed
#' distribution, that this was not investigated systematically, and that the
#' model was evaluated on schedules of 12 vehicles because the computing time
#' of the exposure grid becomes prohibitive above 13 vehicles. `calc_mbd()`
#' stops above 12 vehicles for this reason.
#'
#' # Validation against the published example
#'
#' For the three-vehicle example of Cheong (2007, p. 75), which Cheong,
#' Leckenby and Eakin (2011, Table 4) also publish, the largest absolute
#' difference between this function and the printed seven-cell distribution is
#' 0.0020348568 in probability (0.20348568 percentage points), in the cell of
#' three exposures. The regression test accepts differences below 0.0025: that
#' is a threshold above the real maximum, not an estimate of the error of the
#' function. The rounding of one printed cell to three decimals (at most
#' 0.0005) is smaller than that maximum. The difference of every cell is
#' recorded in the installed file `DATA-PROVENANCE.md`. This is an approximate
#' replication of one printed example, not an independent validation of the
#' model's predictions. The printed intermediate tables contain arithmetic
#' inconsistencies (a product printed as .024(.057) = .003, which is .0014; a
#' sum of Beta-Binomial parameters of 7.294, where the printed formula gives
#' 7.543; an exclusive-grid sum of 1.001). They are a plausible explanation for
#' some of the differences, but their full causal attribution has not been
#' established: the public tests do not include an independent implementation
#' of the printed procedure.
#' In the binomial limit of a vehicle's own distribution (`R2` equal to the
#' reach under independence), the conditional distributions of the peeling step
#' coincide, so the result of this model does not depend on the duplication
#' between vehicles in that limit; this follows from Cheong's construction.
#'
#' # Published evidence belongs to the study's variants
#'
#' Of the eleven models Cheong evaluated (comScore 2003 data, 440 schedules;
#' Cheong, 2007, Table 6.2.1), the three versions of MBD were the most accurate
#' for reach alone, but not for the complete exposure distribution. The figures
#' below are the study's, for its own implementations of each version, not
#' measurements of this package:
#' \tabular{lrr}{
#'   Model of the study \tab AER (percent) \tab APE (percent)\cr
#'   MBD \tab 1.34 \tab 10.19\cr
#'   MBD-ADJ \tab 1.40 \tab 12.10\cr
#'   MBD-ADJ2 \tab 1.18 \tab 11.83\cr
#'   CANEX \tab 1.69 \tab 6.91\cr
#'   CBD \tab 1.60 \tab 8.80
#' }
#' The best result for reach (1.18%) belongs to MBD-ADJ2, not to a plain MBD,
#' and not to the safety net of this function, so it must not be read as the
#' expected error of [calc_mbd()]. For the complete distribution, [calc_canex()]
#' and the Conditional Beta Distribution model ([calc_cbd()]) were more accurate
#' than all the MBD versions.
#'
#' @references
#' Cheong, Y. (2007). Multivariate Beta Binomial Distribution Model as a Web
#' Media Exposure Model. Doctoral dissertation, The University of Texas at
#' Austin.
#' Handle 2152/3215 (University of Texas at Austin repository).
#'
#' Cheong, Y., Leckenby, J. D., & Eakin, T. (2011). Evaluating the
#' multivariate beta binomial distribution for estimating magazine and
#' Internet exposure frequency distributions. Journal of Advertising, 40(1),
#' 7-23. doi:10.2753/JOA0091-3367400101
#'
#' Waring, E. (1792). On the principles of translating algebraic quantities into
#' probable relations and annuities. Cambridge. (The work Cheong, 2007, and
#' Aldás Manzano, 1998, cite for the theorem; not to be confused with Waring's
#' *Meditationes Algebraicae*, 1770.)
#'
#' @examples
#' data(mbd_example)
#' result <- do.call(calc_mbd, mbd_example)
#' result$reach
#' result$distribution
#'
#' @seealso [calc_csd()] and [calc_msad()] for sequential aggregation models,
#'   and [calc_cbd()] for the model that uses a canonical-expansion
#'   between-vehicle step instead of an imputed one.
#' @inheritSection calc_canex Domain of validity
#' @export
calc_mbd <- function(vehicles_data, duplications,
                     aggregation_order = c("audience_desc", "given"),
                     population = 1, tolerance = 1e-8) {
  input <- validate_sequential_inputs(vehicles_data, duplications,
                                      aggregation_order, population, tolerance,
                                      max_vehicles = 12L, caller = "calc_mbd")
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

#' @rdname print_sequential
#' @export
print.reach_mbd <- function(x, full = TRUE, max_rows = 30L, ...) {
  if (isTRUE(full)) {
    print_sequential_report(
      x, "MULTIVARIATE BETA BINOMIAL DISTRIBUTION (MBD)",
      paste("co-exposure grid from observed duplications, with vehicles",
            "peeled one at a time into insertion-level Beta-Binomial",
            "distributions"),
      parameters = list("Aggregation order" = x$aggregation_order,
                        "Aggregation rule" = x$aggregation_rule),
      tables = list("Peeling steps (reverse aggregation order)" = x$steps),
      diagnostics = sequential_diagnostic_lines(x),
      max_rows = max_rows
    )
    return(invisible(x))
  }
  cat("Multivariate Beta Binomial Distribution (MBD)\n")
  cat(sprintf("Reach: %.2f%% | Average frequency: %.3f\n",
              x$reach$percent, x$average_frequency))
  cat("Aggregation order:", paste(x$aggregation_order, collapse = " -> "),
      sprintf("(%s)\n", x$aggregation_rule))
  cat(sprintf("Probability sum: %.12f | Mean error: %.3g\n",
              x$diagnostics$probability_sum, x$diagnostics$mean_error))
  if (x$diagnostics$negative_mass_adjusted > 0) {
    cat(sprintf("Safety net engaged on the final distribution: %d cell(s), %.6g probability mass adjusted (not Cheong's UD-stage MBD-ADJ).\n",
                x$diagnostics$cells_adjusted, x$diagnostics$negative_mass_adjusted))
  }
  invisible(x)
}
