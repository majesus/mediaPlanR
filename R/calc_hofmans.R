
#' @encoding UTF-8
#' @title Cumulative audience under the Hofmans cumulative-audience model
#' @description Implements the Hofmans (1966) model to calculate the cumulative
#' audience of a media plan with multiple insertions in one vehicle. The model
#' accounts for duplication between insertions and uses an adjustment parameter
#' (alpha) to improve the estimate of cumulative audiences.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
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
#' @return A list of class "reach_hofmans" containing:
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
#' result <- calc_hofmans(R1, R2, N = 5)
#'
#' # Inspect the results
#' print(result$results)
#' print(result$parameters)
#'
#' # Example with input validation
#' \dontrun{
#' invalid_R1 <- 1.2  # >100% reach
#' result <- calc_hofmans(invalid_R1, R2, N = 5)
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
calc_hofmans <- function(R1, R2, N, show_steps = TRUE) {
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
  ), class = "reach_hofmans"))
}
