
#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculate the R1 and R2 coefficients (Beta-Binomial model)
#' @name calc_R1_R2
#'
#' @description Calculates the R1 and R2 values from the alpha and beta shape
#' parameters of the Beta-Binomial cumulative-net-audience model. These values
#' are key to evaluating net audience and the contact distribution (and
#' cumulative distribution). If the success probability is Beta distributed
#' with shape parameters alpha and beta, the contact distribution is a compound
#' distribution: the Beta-Binomial distribution.
#'
#' @param A Alpha shape parameter; must be numeric and positive
#' @param B Beta shape parameter; must be numeric and positive
#'
#' @details
#' The R1 and R2 coefficients are duplication measures for audiences:
#' \itemize{
#'   \item R1 measures the proportion of people reached after the first
#'   insertion in the chosen vehicle
#'   \item R2 measures the proportion of people reached after the second
#'   insertion in the chosen vehicle
#' }
#'
#' The calculation process:
#' \enumerate{
#'   \item Computes R1 directly as A/(A+B)
#'   \item Optimizes R2 through an iterative process
#'   \item Checks that R1 and R2 fall in the (0,1) range
#' }
#'
#' @return A list with two components:
#' \itemize{
#'   \item R1: Cumulative-audience coefficient (proportion) after the first insertion
#'   \item R2: Cumulative-audience coefficient (proportion) after the second insertion
#' }
#'
#' @examples
#' # Calculate R1 and R2 for alpha=0.5 and beta=0.3
#' result <- calc_R1_R2(0.5, 0.3)
#'
#' # Inspect the results
#' print(paste("R1:", round(result$R1, 4)))
#' print(paste("R2:", round(result$R2, 4)))
#'
#' # Check that the values fall in the expected range
#' stopifnot(result$R1 >= 0, result$R1 <= 1)
#' stopifnot(result$R2 >= 0, result$R2 <= 1)
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial model
#' \code{\link{calc_sainsbury}} for estimates under the Sainsbury model
calc_R1_R2 <- function(A, B) {
  if (!is.numeric(A) || !is.numeric(B) || A <= 0 || B <= 0) {
    stop("A and B must be numeric and positive.")
  }

  R1 <- A / (A + B)

  R2_objective <- function(R2) {
    (A - (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2))^2
  }

  fit <- stats::optimize(R2_objective, c(0, 1))
  R2 <- fit$minimum

  return(list(R1 = R1, R2 = R2))
}
