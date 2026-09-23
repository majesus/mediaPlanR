#' Cumulative audience under Hofmans' accumulation model
#'
#' Implements Hofmans' (1966) accumulation model for the cumulative audience
#' of several insertions in one vehicle -- the "accumulation" domain in Aldás
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
#' @param R1 Reach after the first insertion, as a proportion in (0, 1].
#' @param R2 Cumulative reach after the second insertion, as a proportion
#'   with `R1 < R2 < 2 * R1`.
#' @param N Number of insertions up to which the cumulative reach is computed,
#'   an integer of at least 2.
#' @param R3 Optional cumulative reach after the third insertion, as a
#'   proportion with `R2 < R3 < 3 * R1`. When supplied, it is used to estimate
#'   the exponent `alpha` of Hofmans' variable coefficient (see Details).
#' @param show_steps Logical. If `TRUE`, prints the intermediate calculation
#'   steps and the resulting table.
#'
#' @details
#' Let \eqn{d = 2 R_1 - R_2} be the audience duplicated between two
#' insertions, assumed constant for every pair of insertions, and
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
#' The model assumes a constant audience and a constant duplication between
#' every pair of insertions.
#'
#' @return A list of class `"reach_hofmans_accumulation"` with components:
#' \itemize{
#'   \item `results`: data frame with `N` (number of insertions) and `RN`
#'     (cumulative reach, as a proportion).
#'   \item `parameters`: list with `k`, `d`, `alpha` and `R3` (`NA` when not
#'     supplied).
#'   \item `plot`: a ggplot2 object showing the evolution of cumulative reach.
#' }
#'
#' @examples
#' # Constant-coefficient model (equation 3.11)
#' result <- calc_hofmans_accumulation(R1 = 0.06, R2 = 0.103, N = 5)
#' result$results
#' result$parameters
#'
#' # With an observed third insertion, Hofmans' exponent is estimated
#' calc_hofmans_accumulation(R1 = 0.06, R2 = 0.103, N = 5, R3 = 0.135)$parameters
#'
#' @seealso
#' [calc_beta_binomial()] for a stochastic accumulation model that also
#' returns the exposure distribution, and [calc_hofmans_duplication()] for the
#' same author's model for several vehicles.
#' @importFrom ggplot2 .data
#' @export
calc_hofmans_accumulation <- function(R1, R2, N, R3 = NULL, show_steps = FALSE) {
  assert_number(R1, "R1", min = 0, max = 1, min_open = TRUE)
  assert_number(R2, "R2", min = 0, max = 1, min_open = TRUE)
  assert_number(N, "N", min = 2, integer = TRUE)
  assert_flag(show_steps, "show_steps")
  if (R2 <= R1) {
    stop("R2 must be greater than R1: cumulative reach must increase.",
         call. = FALSE)
  }
  d <- 2 * R1 - R2
  if (d <= 1e-9) {
    stop("R2 must be smaller than 2 * R1: the audience duplicated between two ",
         "insertions (2 * R1 - R2) must be positive, otherwise the model is ",
         "undefined.", call. = FALSE)
  }
  if (!is.null(R3)) {
    assert_number(R3, "R3", min = 0, max = 1, min_open = TRUE)
    if (R3 <= R2 || R3 >= 3 * R1) {
      stop("R3 must lie between R2 and 3 * R1.", call. = FALSE)
    }
    if (N < 3) {
      stop("N must be at least 3 when R3 is supplied.", call. = FALSE)
    }
  }

  k <- 2 * R1 / R2
  alpha <- if (is.null(R3)) 1 else
    log((3 * R1 - R3) * R2 / (d * R3)) / log(2)

  n <- seq_len(N)
  RN <- numeric(N)
  RN[1L] <- R1
  RN[2L] <- R2
  if (N >= 3) {
    m <- 3:N
    RN[m] <- (m * R1)^2 / (m * R1 + k * (m - 1)^alpha * (m / 2) * d)
  }
  results <- data.frame(N = n, RN = RN)

  if (any(diff(RN) < -1e-12) || any(RN > 1 + 1e-12) ||
      any(RN > n * R1 + 1e-12)) {
    warning("The cumulative reach curve leaves its logical range (it must ",
            "not decrease, exceed 1, or exceed N * R1). The supplied reach ",
            "values are not consistent with Hofmans' model.", call. = FALSE)
  }

  plot_hofmans <- ggplot2::ggplot(results, ggplot2::aes(x = .data$N, y = .data$RN * 100)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(size = 2, color = "steelblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(.data$RN * 100, 1), "%")),
      vjust = -0.8, size = 3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max(results$RN * 100) * 1.15)) +
    ggplot2::labs(
      x = "Number of insertions (N)",
      y = "Reach (%)",
      title = "Evolution of cumulative audience",
      subtitle = "Hofmans accumulation model"
    ) +
    ggplot2::theme_minimal()

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
    N = x$results$N,
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
#' @param population Population size, in people.
#' @param duplication_matrix Symmetric numeric matrix with the audience
#'   duplicated between every pair of vehicles, in people. Diagonal values are
#'   ignored.
#'
#' @details
#' For two vehicles, reach is exactly \eqn{A_1 + A_2 - A_{12}}. Equating that
#' value to Agostini's formula gives the coefficient
#' \eqn{k_{ij} = (A_i + A_j) / (A_i + A_j - A_{ij})}, which Hofmans applies to
#' every pair of a plan with \eqn{m} vehicles:
#' \deqn{R_m = \frac{(\sum_i A_i)^2}{\sum_i A_i + \sum_{i<j} k_{ij} A_{ij}}.}
#'
#' The formula is empirical and can return a reach outside its logical range
#' (below the largest audience, or above the population or the gross
#' audience) when the duplications are not mutually consistent. The value is
#' then returned unchanged, with a warning.
#'
#' @return A list of class `"reach_hofmans_duplication"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `gross_audience`: sum of the vehicle audiences, in people.
#'   \item `weighted_duplication`: the sum \eqn{\sum_{i<j} k_{ij} A_{ij}}, in
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
  Kij <- (Ai + Aj) / (Ai + Aj - Aij)
  weighted_duplication <- sum(Kij * Aij)

  gross_audience <- sum(audiences)
  reach <- gross_audience^2 / (gross_audience + weighted_duplication)
  check_reach_bounds(reach, audiences, population, "Hofmans")

  structure(list(
    reach = list(percent = 100 * reach / population, people = reach),
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
