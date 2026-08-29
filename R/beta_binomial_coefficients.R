
#__________________________________________________________#

# Internal helper: derives the R1/R2 cumulative-audience coefficients from
# the alpha/beta shape parameters of a Beta-Binomial distribution. R2 has no
# closed form, so it is solved numerically; calculate_bbd_params() (in
# calc_canex.R) performs the inverse recovery of alpha/beta from R1/R2.
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
