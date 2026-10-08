#' Cumulative reach under Hofmans' accumulation model
#'
#' Implements Hofmans' (1966) accumulation model for the cumulative reach (the
#' proportion of the population exposed at least once) of several insertions in
#' one vehicle -- the "accumulation" domain in Aldás
#' Manzano's (1998) three-way split. It adapts Agostini's duplication formula
#' by replacing the number of vehicles with the number of insertions, and it
#' needs only the audience after one and two insertions. It is unrelated to
#' [calc_hofmans_duplication()], the same author's separate model for several
#' vehicles with one insertion each.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.1.1.4, equations 3.7-3.12.
#'
#' Hofmans, P. (1966). Measuring the cumulative net coverage of any
#' combination of media. Journal of Marketing Research, 3(3), 269-278.
#' \doi{10.1177/002224376600300307}
#'
#' @param R1 Reach after the first insertion, as a proportion between 0 and 1
#'   (not a percentage), in (0, 1].
#' @param R2 Cumulative reach after the second insertion, as a proportion
#'   (not a percentage) with `R1 < R2 < 2 * R1` and `R2 <= 1`.
#' @param insertions Number of insertions in the vehicle up to which the
#'   cumulative reach is computed (\eqn{N} in the formulas), an integer of at
#'   least 2 (at least 3 when `R3` is supplied).
#' @param R3 Optional cumulative reach after the third insertion, as a
#'   proportion (not a percentage) with `R2 < R3 < 3 * R1` and `R3 <= 1`. It is
#'   observed data, like `R1` and `R2`, not something the function estimates:
#'   when supplied, the exponent `alpha` of Hofmans' variable coefficient is
#'   estimated from it, so that the curve reproduces `R3` (see Details).
#' @param show_steps Logical. If `TRUE`, prints the parameters `k`, `d` and
#'   `alpha` and the table of cumulative reach.
#'
#' @details
#' Let \eqn{d = 2 R_1 - R_2} be the duplicated reach of two insertions (the
#' proportion of the population exposed to both), assumed constant for every
#' pair of insertions, and
#' \eqn{k = 2 R_1 / R_2} the coefficient that makes the formula reproduce
#' \eqn{R_2} exactly. Cumulative reach after \eqn{N} insertions is
#' \deqn{R_N = \frac{(N R_1)^2}{N R_1 + k_N \, d \binom{N}{2}}.}
#' With a constant coefficient, \eqn{k_N = k} (equation 3.11 of Aldás Manzano,
#' 1998). Hofmans showed that the coefficient is not constant and proposed
#' \eqn{k_N = k (N - 1)^{\alpha - 1}} (equation 3.12), where the exponent is
#' estimated from a third observation,
#' \deqn{\hat{\alpha} = \log\left[\frac{(3 R_1 - R_3) R_2}
#' {(2 R_1 - R_2) R_3}\right] / \log 2.}
#' `R3` therefore selects the model: without it, `alpha = 1` and the constant
#' coefficient formula applies; with it, `alpha` is estimated and the curve
#' reproduces `R3` exactly.
#'
#' The model assumes a constant reach per insertion (`R1`) and a constant
#' duplication between every pair of insertions.
#'
#' Valid `R1` and `R2` do not guarantee a valid curve: the extrapolation can
#' decrease, exceed 1 or exceed \eqn{N R_1}. For example, `R1 = 0.60`, `R2 = 0.84`
#' and `N = 4` give a cumulative reach of 1.05 at `N = 4`. The curve is then
#' returned unchanged, with a warning.
#'
#' @return A list of class `"reach_hofmans_accumulation"` with components:
#' \itemize{
#'   \item `results`: data frame with `insertions` (number of insertions) and `RN`
#'     (cumulative reach, as a proportion).
#'   \item `parameters`: list with `k`, `d`, `alpha` and `R3` (`NA` when not
#'     supplied).
#'   \item `plot`: a ggplot2 object showing the evolution of cumulative reach,
#'     or `NULL` when the suggested package ggplot2 is not installed.
#' }
#'
#' @examples
#' # Constant-coefficient model (equation 3.11)
#' result <- calc_hofmans_accumulation(R1 = 0.06, R2 = 0.103, insertions = 5)
#' result$results
#' result$parameters
#'
#' # With an observed third insertion, Hofmans' exponent is estimated
#' calc_hofmans_accumulation(R1 = 0.06, R2 = 0.103, insertions = 5,
#'                          R3 = 0.135)$parameters
#'
#' @seealso
#' [calc_beta_binomial()] for a stochastic accumulation model that also
#' returns the exposure distribution, and [calc_hofmans_duplication()] for the
#' same author's model for several vehicles.
#' @export
calc_hofmans_accumulation <- function(R1, R2, insertions, R3 = NULL,
                                      show_steps = FALSE) {
  assert_number(R1, "R1", min = 0, max = 1, min_open = TRUE, proportion = TRUE)
  assert_number(R2, "R2", min = 0, max = 1, min_open = TRUE, proportion = TRUE)
  assert_number(insertions, "insertions", min = 2, integer = TRUE)
  N <- insertions
  assert_flag(show_steps, "show_steps")
  if (R2 <= R1) {
    stop("R2 must be greater than R1: cumulative reach must increase.",
         call. = FALSE)
  }
  d <- 2 * R1 - R2
  if (d <= 0) {
    stop("R2 must be smaller than 2 * R1: the audience duplicated between two ",
         "insertions (2 * R1 - R2) must be positive, otherwise the model is ",
         "undefined.", call. = FALSE)
  }
  if (!is.null(R3)) {
    assert_number(R3, "R3", min = 0, max = 1, min_open = TRUE,
                  proportion = TRUE)
    if (R3 <= R2 || R3 >= 3 * R1) {
      stop("R3 must lie between R2 and 3 * R1.", call. = FALSE)
    }
    if (N < 3) {
      stop("insertions must be at least 3 when R3 is supplied.", call. = FALSE)
    }
  }

  k <- 2 * R1 / R2
  alpha <- if (is.null(R3)) 1 else
    (log((3 * R1 - R3) / d) + log(R2 / R3)) / log(2)

  n <- seq_len(N)
  RN <- numeric(N)
  RN[1L] <- R1
  RN[2L] <- R2
  if (N >= 3) {
    m <- 3:N
    RN[m] <- m * R1 / (1 + k * (m - 1)^alpha * d / (2 * R1))
  }
  results <- data.frame(insertions = n, RN = RN)

  if (any(diff(RN) < -1e-12) || any(RN > 1 + 1e-12) ||
      any(RN > n * R1 + 1e-12)) {
    warning("The cumulative reach curve leaves its logical range (it must ",
            "not decrease, exceed 1, or exceed N * R1). The supplied reach ",
            "values are not consistent with Hofmans' model.", call. = FALSE)
  }

  plot_hofmans <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_hofmans <- ggplot2::ggplot(results, ggplot2::aes(x = .data$insertions, y = .data$RN * 100)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(size = 2, color = "steelblue") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(.data$RN * 100, 1), "%")),
        vjust = -0.8, size = 3
      ) +
      ggplot2::scale_y_continuous(limits = c(0, max(results$RN * 100) * 1.15)) +
      ggplot2::labs(
        x = "Number of insertions",
        y = "Reach (%)",
        title = "Evolution of cumulative audience",
        subtitle = "Hofmans accumulation model"
      ) +
      ggplot2::theme_minimal()
  }

  result <- structure(list(
    results = results,
    parameters = list(k = k, d = d, alpha = alpha,
                      R3 = if (is.null(R3)) NA_real_ else R3),
    plot = plot_hofmans
  ), class = "reach_hofmans_accumulation")
  if (show_steps) print(result)
  invisible(result)
}

#' @export
print.reach_hofmans_accumulation <- function(x, ...) {
  cat("HOFMANS MODEL (accumulation)\n")
  cat("============================\n")
  cat(sprintf("k = 2 R1 / R2 = %.4f | d = 2 R1 - R2 = %.4f | alpha = %.4f%s\n",
              x$parameters$k, x$parameters$d, x$parameters$alpha,
              if (is.na(x$parameters$R3)) " (constant coefficient)" else
                " (estimated from R3)"))
  cat("\nCUMULATIVE REACH:\n")
  print(data.frame(
    Insertions = x$results$insertions,
    Reach = sprintf("%.2f%%", 100 * x$results$RN)
  ), row.names = FALSE)
  invisible(x)
}

#__________________________________________________________#

#' Reach under Hofmans' duplication model
#'
#' Implements Hofmans' (1966) *duplication* model: an ad hoc correction of
#' Agostini's (1961) formula for several vehicles with one insertion each --
#' the same "duplication" domain as [calc_agostini_duplication()]. Where
#' Agostini uses one empirical coefficient for every vehicle pair, Hofmans
#' computes a pairwise coefficient directly from each pair's own audiences and
#' duplication, so no coefficient has to be fitted to external data. Like
#' Agostini's formula, it estimates total reach only, not the exposure
#' distribution. It is unrelated to [calc_hofmans_accumulation()], the same
#' author's model for one vehicle with several insertions.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.2.1.2, equations 3.59-3.62.
#'
#' Hofmans, P. (1966). Measuring the cumulative net coverage of any
#' combination of media. Journal of Marketing Research, 3(3), 269-278.
#' \doi{10.1177/002224376600300307}
#'
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 44-45.
#'
#' @param audiences Numeric vector with the audience of each vehicle for one
#'   insertion, in people.
#' @param population Population size, in people. No audience can exceed it;
#'   otherwise the function stops with an error.
#' @param duplication_matrix Symmetric numeric matrix whose element `[i, j]` is
#'   the number of people who are in the audience of both vehicle `i` and
#'   vehicle `j` (one insertion in each), in people (not a proportion). Diagonal
#'   values are ignored.
#'
#' @details
#' For two vehicles, reach is exactly \eqn{A_1 + A_2 - A_{12}}. Equating that
#' value to Agostini's formula gives the coefficient
#' \eqn{k_{ij} = (A_i + A_j) / (A_i + A_j - A_{ij})}, which Hofmans applies to
#' every pair of a plan with \eqn{m} vehicles:
#' \deqn{R_m = \frac{(\sum_{i=1}^{m} A_i)^2}{\sum_{i=1}^{m} A_i +
#' \sum_{i=1}^{m-1} \sum_{j=i+1}^{m} k_{ij} A_{ij}}.}
#' The double sum runs over every pair of vehicles, each pair counted once.
#'
#' The formula is empirical and can return a reach outside its logical range
#' (below the largest audience, or above the population or the gross
#' audience). This can happen because the duplications are not mutually
#' consistent, but also because the formula is an approximation: five
#' independent vehicles that each reach 50% of the population, with a
#' duplication of 25% in every pair (data that come from a valid joint
#' distribution, with a true reach of 96.875%), give a reach of 107.14%. The
#' value is then returned unchanged, with a warning, so that an out-of-range
#' result is not attributed automatically to the input data.
#'
#' @return A list of class `"reach_hofmans_duplication"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `gross_audience`: sum of the vehicle audiences (duplicated people
#'     counted once per vehicle), in people.
#'   \item `weighted_duplication`: the sum \eqn{\sum_{i=1}^{m-1} \sum_{j=i+1}^{m} k_{ij} A_{ij}}, in
#'     people.
#'   \item `n_vehicles`: number of vehicles.
#' }
#'
#' @examples
#' data(duplication_example)
#' do.call(calc_hofmans_duplication, duplication_example)
#'
#' @seealso [calc_agostini_duplication()], the single-coefficient formula
#' this model refines pair by pair.
#' @export
calc_hofmans_duplication <- function(audiences, population, duplication_matrix) {
  validate_duplication_inputs(audiences, population, duplication_matrix)

  pairs <- utils::combn(length(audiences), 2L)
  Ai <- audiences[pairs[1L, ]]
  Aj <- audiences[pairs[2L, ]]
  Aij <- duplication_matrix[cbind(pairs[1L, ], pairs[2L, ])]
  Kij <- 1 / (1 - (Aij / population) / (Ai / population + Aj / population))
  weighted_duplication <- sum(Kij * Aij)

  gross_audience <- sum(audiences)
  if (!is.finite(gross_audience) || !is.finite(weighted_duplication)) {
    stop("Total audience and weighted duplication must be finite; counts exceed the representable range.",
         call. = FALSE)
  }
  reach <- gross_audience / (1 + weighted_duplication / gross_audience)
  check_reach_bounds(reach, audiences, population, "Hofmans")

  structure(list(
    reach = list(percent = 100 * (reach / population), people = reach),
    gross_audience = gross_audience,
    weighted_duplication = weighted_duplication,
    n_vehicles = length(audiences)
  ), class = "reach_hofmans_duplication")
}

#' @export
print.reach_hofmans_duplication <- function(x, ...) {
  print_reach_report(
    "HOFMANS MODEL (duplication)",
    "ad hoc reach formula with a pairwise duplication coefficient",
    x$reach$percent, x$reach$people,
    parameters = list(
      "Gross audience (people)" = x$gross_audience,
      "Weighted pairwise duplication (people)" = x$weighted_duplication
    )
  )
  invisible(x)
}
