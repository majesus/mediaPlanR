
#' @encoding UTF-8
#' @title Cumulative audience under the Hofmans accumulation model
#' @description Implements the Hofmans (1966) model to calculate the cumulative
#' audience of a media plan with multiple insertions in one vehicle -- the
#' "accumulation" domain in Aldas Manzano's (1998) three-way split. The model
#' accounts for duplication between insertions and uses an adjustment parameter
#' (alpha) to improve the estimate of cumulative audiences. It is unrelated to
#' \code{\link{calc_hofmans_duplication}} below, the same author's separate
#' model for several vehicles with a single insertion each.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#' (Sec. 3.1.1.4, formulas \[3.7\]-\[3.12\].)
#'
#' @param R1 Numeric. Reach after the first insertion (as a proportion between 0 and 1)
#' @param R2 Numeric. Reach after the second insertion (as a proportion between 0 and 1)
#' @param N Integer. Number of insertions for which to calculate cumulative audience
#' @param show_steps Logical. If TRUE, prints the intermediate calculation steps
#'
#' @details
#' The Hofmans model calculates cumulative reach in two stages:
#' \enumerate{
#'   \item Uses a first formulation to calculate R3:
#'     \itemize{
#'       \item R3 = (3R1)^2 / (3R1 + k(2R1-R2)(3 choose 2))
#'       \item where k = 2R1/R2
#'     }
#'   \item For N>3, applies an improved formulation that incorporates an alpha parameter:
#'     \itemize{
#'       \item RN = (NR1)^2 / (NR1 + k*(N-1)^a*(N/2)*d)
#'       \item where alpha is computed from R3
#'       \item and d = 2R1-R2 is the duplication between insertions
#'     }
#' }
#'
#' The model assumes:
#' \itemize{
#'   \item Constant audience across all insertions
#'   \item Constant duplication between pairs of insertions
#'   \item Non-linear accumulation behaviour for N > 3
#' }
#'
#' @return A list of class "reach_hofmans_accumulation" containing:
#' \itemize{
#'   \item results: Data frame with:
#'     \itemize{
#'       \item N: Insertion number
#'       \item RN: Cumulative reach (proportion)
#'     }
#'   \item parameters: List with the calculated parameters:
#'     \itemize{
#'       \item k: Calculated k factor
#'       \item d: Duplication between insertions
#'       \item alpha: Adjustment parameter for N>3
#'     }
#'   \item plot: Plot of the evolution of reach
#' }
#'
#' @examples
#' # Basic example with 5 insertions
#' R1 <- 0.06    # 6% reach after the first insertion
#' R2 <- 0.103   # 10.3% reach after the second insertion
#' result <- calc_hofmans_accumulation(R1, R2, N = 5)
#'
#' # Inspect the results
#' print(result$results)
#' print(result$parameters)
#'
#' # Example with input validation
#' \dontrun{
#' invalid_R1 <- 1.2  # >100% reach
#' result <- calc_hofmans_accumulation(invalid_R1, R2, N = 5)
#' # Raises an error due to the invalid reach
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial distribution
#' \code{\link{calc_sainsbury}} for the Sainsbury model
#' \code{\link{calc_binomial}} for the Binomial model
#' \code{\link{calc_metheringham}} for the Metheringham model
#' @importFrom ggplot2 .data
calc_hofmans_accumulation <- function(R1, R2, N, show_steps = TRUE) {
  # Input validation
  if (any(c(R1, R2) > 1) || R1 <= 0 || R2 <= 0) {
    stop("R1 and R2 must be greater than 0 and at most 1")
  }
  if (N < 3 || N != round(N)) {
    stop("N must be an integer greater than or equal to 3")
  }
  if (R2 <= R1) {
    stop("Reach must be increasing: R1 < R2")
  }
  if (abs(2 * R1 - R2) < 1e-9) {
    stop("2*R1 - R2 is practically 0: the Hofmans model is undefined for these R1, R2 values (division by zero)")
  }

  # Initial calculations
  k <- 2 * R1 / R2
  d <- 2 * R1 - R2

  if (show_steps) {
    cat("\nSTEP 1: initial calculations")
    cat("\n- k = 2R1/R2 =", round(k, 4))
    cat("\n- d = 2R1-R2 =", round(d, 4))
  }

  # Calculate R3 using formula [3.11, Aldas-Manzano, 1998]
  n3 <- 3
  numerator3 <- (n3 * R1)^2
  denominator3 <- n3 * R1 + k * (2 * R1 - R2) * choose(n3, 2)
  R3 <- numerator3 / denominator3

  if (show_steps) {
    cat("\n\nSTEP 2: calculate R3 using formula [3.11]")
    cat("\n- R3 =", round(R3, 4))
  }

  # Calculate alpha using R3
  alpha <- log((3 * R1 - R3) * R2 / ((2 * R1 - R2) * R3)) / log(2)

  if (show_steps) {
    cat("\n\nSTEP 3: calculate alpha")
    cat("\n- alpha =", round(alpha, 4))
  }

  # Calculate reach for each insertion
  results <- data.frame(
    N = 1:N,
    RN = numeric(N)
  )

  for (n in 1:N) {
    if (n == 1) {
      results$RN[n] <- R1
    } else if (n == 2) {
      results$RN[n] <- R2
    } else if (n == 3) {
      results$RN[n] <- R3
    } else {
      # Calculate RN using the final Hofmans formula
      numerator <- (n * R1)^2
      denominator <- n * R1 + k * (n - 1)^alpha * (n / 2) * d
      results$RN[n] <- numerator / denominator
    }
  }

  # Build the plot (a reusable ggplot2 object, not a base-graphics side effect)
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
      subtitle = "Hofmans model"
    ) +
    ggplot2::theme_minimal()

  if (show_steps) {
    cat("\n\nRESULTS:\n")
    print(data.frame(
      N = results$N,
      Reach = paste0(round(results$RN * 100, 2), "%")
    ))

    cat("\nCHECKS:")
    cat("\n- Reach always increasing:", all(diff(results$RN) >= 0))
    cat("\n- Reach values between 0 and 1:", all(results$RN >= 0 & results$RN <= 1))
    cat("\n- R1, R2 match the inputs:",
        all.equal(c(results$RN[1:2]), c(R1, R2)))
  }

  # Return results and plot
  invisible(structure(list(
    results = results,
    parameters = list(k = k, d = d, alpha = alpha),
    plot = plot_hofmans
  ), class = "reach_hofmans_accumulation"))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Reach under the Hofmans duplication model
#' @description Implements Hofmans' (1966) *duplication* model: an ad hoc
#' correction of Agostini's (1961) formula for several vehicles with a
#' single insertion each -- the same "duplication" domain as
#' \code{\link{calc_agostini_duplication}}. Where Agostini corrects the
#' random-duplication assumption with one empirical coefficient shared by
#' every vehicle pair, Hofmans replaces it with a pairwise coefficient
#' computed directly from each pair's own observed audiences and duplication
#' -- no coefficient needs to be fitted from an external calibration data
#' set. Like Agostini, it estimates total reach only, not the contact
#' distribution: \code{\link{calc_hofmans_accumulation}} above (an unrelated,
#' same-author model for one vehicle with several insertions) is the one
#' that produces a distribution.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la
#' distribucion de contactos en la planificacion de medios publicitarios
#' impresos. Tesis doctoral, Universidad de Valencia, Espana. (Sec. 3.2.1.2.)
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 44-45,
#' independently reviews the identical formula.
#'
#' @param audiences Numeric vector with the individual audience of each vehicle
#' @param population Population size
#' @param duplication_matrix Symmetric matrix with the pairwise duplicated
#'   audience between vehicles, in the same units as \code{audiences}
#'   (people, not a proportion). Diagonal values are ignored.
#'
#' @details
#' \deqn{R_m = \frac{(\sum_i A_i)^2}{\sum_i A_i + \sum_{i<j} K_{ij} A_{ij}}}
#' with \eqn{K_{ij} = (A_i + A_j)/(A_i + A_j - A_{ij})}. Both Aldas Manzano
#' (1998) and Kim (2005) report this exact formula, independently
#' attributing it to Hofmans (1966).
#'
#' @return A list of class "reach_hofmans_duplication" containing:
#' \itemize{
#'   \item reach: List with percent and people
#' }
#'
#' @examples
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' duplication_matrix <- matrix(c(
#'      0, 60000, 40000,
#'  60000,     0, 50000,
#'  40000, 50000,     0
#' ), nrow = 3, byrow = TRUE)
#' calc_hofmans_duplication(audiences, population, duplication_matrix)
#'
#' @export
#' @seealso \code{\link{calc_agostini_duplication}}, the single-coefficient
#'   counterpart this model refines pair by pair.
calc_hofmans_duplication <- function(audiences, population, duplication_matrix) {
  if (!is.numeric(audiences) || !is.numeric(population)) {
    stop("audiences and population must be numeric")
  }
  n <- length(audiences)
  if (n < 2L || anyNA(audiences) || any(!is.finite(audiences))) {
    stop("audiences must contain at least two finite values")
  }
  if (length(population) != 1L || !is.finite(population) || population <= 0 ||
      any(audiences <= 0) || any(audiences > population)) {
    stop("audiences must be positive and smaller than the population")
  }
  if (!is.matrix(duplication_matrix) || !is.numeric(duplication_matrix) ||
      !identical(dim(duplication_matrix), c(n, n))) {
    stop("duplication_matrix must be a numeric square matrix matching audiences")
  }
  if (!isTRUE(all.equal(duplication_matrix[upper.tri(duplication_matrix)],
                        t(duplication_matrix)[upper.tri(duplication_matrix)],
                        check.attributes = FALSE))) {
    stop("duplication_matrix must be symmetric outside its diagonal")
  }

  pairs <- utils::combn(n, 2L)
  kD <- 0
  for (col in seq_len(ncol(pairs))) {
    i <- pairs[1L, col]; j <- pairs[2L, col]
    Aij <- duplication_matrix[i, j]
    upper_bound <- min(audiences[i], audiences[j])
    if (!is.finite(Aij) || Aij < 0 || Aij > upper_bound ||
        audiences[i] + audiences[j] - Aij <= 0) {
      stop(sprintf(paste0(
        "duplication_matrix[%d,%d] must be between 0 and min(audience %d, ",
        "audience %d)"), i, j, i, j), call. = FALSE)
    }
    Kij <- (audiences[i] + audiences[j]) / (audiences[i] + audiences[j] - Aij)
    kD <- kD + Kij * Aij
  }
  A <- sum(audiences)
  reach <- A^2 / (A + kD)

  structure(list(
    reach = list(percent = 100 * reach / population, people = reach)
  ), class = "reach_hofmans_duplication")
}

#' @export
print.reach_hofmans_duplication <- function(x, ...) {
  print_reach_report(
    "HOFMANS MODEL (duplication)",
    "ad hoc correction of Agostini's formula using a pairwise duplication coefficient",
    x$reach$percent, x$reach$people
  )
  invisible(x)
}
