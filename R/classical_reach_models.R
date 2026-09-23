#__________________________________________________________#

# Shared input validation of the plan-level classical models (Sainsbury and
# Binomial): one audience per vehicle, one insertion count per vehicle.
validate_vehicle_plan <- function(audiences, population, insertions) {
  assert_numeric_vector(audiences, "audiences", min = 0)
  assert_number(population, "population", min = 0, min_open = TRUE)
  if (any(audiences > population)) {
    stop("audiences cannot exceed population.", call. = FALSE)
  }
  assert_numeric_vector(insertions, "insertions", min = 0, integer = TRUE,
                        length = length(audiences))
  invisible(TRUE)
}

#' Reach and exposure distribution under the Sainsbury model
#'
#' Implements the Sainsbury model, developed by E. J. Sainsbury at the London
#' Press Exchange, to calculate reach and the exposure distribution (and its
#' cumulative counterpart) of a set of advertising vehicles. The model assumes
#' random duplication *and* random accumulation, homogeneous individual
#' exposure probabilities and heterogeneous vehicle exposure probabilities.
#' The probability that an individual is exposed to one insertion in vehicle
#' `i` is therefore the ratio between the vehicle's audience and the
#' population. Because duplication and accumulation are both random, every
#' insertion -- whether in a different vehicle or a repeat insertion in the
#' same one -- is an independent Bernoulli trial with that vehicle's own
#' exposure probability, and the number of exposures follows a
#' Poisson-binomial distribution.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.2.2.2 for one insertion per vehicle; Section 3.3.1.2, equations
#' 3.86-3.87, for several insertions (the multivariable binomial model with
#' independent vehicles that Aldás Manzano attributes to Chandon).
#'
#' Chandon, J.-L. (1976). A comparative study of media exposure models.
#' Unpublished doctoral dissertation, Northwestern University, Evanston, IL.
#' (Aldás Manzano, 1998, cites Chandon's work as Chandon, 1985.)
#'
#' @param audiences Numeric vector with the audience of each vehicle, in people
#'   per insertion.
#' @param population Population size, in people.
#' @param insertions Non-negative integer vector with the number of insertions
#'   planned in each vehicle. It defaults to one insertion per vehicle, the
#'   model's original scope; values above one apply the same independence
#'   hypothesis to repeat insertions in the same vehicle.
#'
#' @details
#' The exposure distribution is computed exactly, by dynamic convolution of the
#' per-insertion Bernoulli distributions, in \eqn{O(N^2)} operations for \eqn{N}
#' insertions. Reach equals \eqn{1 - \prod_i (1 - A_i / P)^{n_i}}, where
#' \eqn{A_i} is the audience of vehicle \eqn{i}, \eqn{n_i} its number of
#' insertions and \eqn{P} the population.
#'
#' @return A list of class `"reach_sainsbury"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `distribution`: list with `percent` and `people`, one value for each
#'     number of exposures from one to the total number of insertions.
#'   \item `cumulative`: list with `percent` and `people`, for individuals
#'     exposed at least once, at least twice, and so on.
#' }
#'
#' @examples
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_sainsbury(audiences, population)
#' result$reach$percent
#' result$distribution$people
#'
#' # The same three vehicles with several insertions each
#' calc_sainsbury(audiences, population, insertions = c(4, 6, 10))
#'
#' @seealso
#' [calc_binomial()] for the homogeneous-vehicle counterpart,
#' [calc_beta_binomial()] and [calc_metheringham()] for models with
#' heterogeneous individuals, and [estimate_reach()] to run this model from a
#' [media_plan()] object.
#' @export
calc_sainsbury <- function(audiences, population,
                           insertions = rep(1L, length(audiences))) {
  validate_vehicle_plan(audiences, population, insertions)

  # One probability per insertion planned
  probs <- rep(audiences / population, insertions)

  if (!length(probs)) {
    return(structure(list(
      reach = list(percent = 0, people = 0),
      distribution = list(percent = numeric(0), people = numeric(0)),
      cumulative = list(percent = numeric(0), people = numeric(0))
    ), class = "reach_sainsbury"))
  }

  full_distribution <- poisson_binomial_distribution(probs)
  P <- full_distribution[-1L]
  R <- rev(cumsum(rev(P)))
  reach <- 1 - prod(1 - probs)

  structure(list(
    reach = list(percent = reach * 100, people = reach * population),
    distribution = list(percent = P * 100, people = P * population),
    cumulative = list(percent = R * 100, people = R * population)
  ), class = "reach_sainsbury")
}

#__________________________________________________________#

#' Reach and exposure distribution under the Binomial model
#'
#' Implements the Binomial model of Lee and Burkhart (1960), reviewed by
#' Chandon, to calculate reach and the exposure distribution (and its
#' cumulative counterpart) of a media plan with several vehicles. The model
#' assumes random duplication *and* random accumulation (repeat insertions in
#' the same vehicle are also independent), homogeneous individuals and
#' homogeneous vehicles. Under these assumptions every insertion is an
#' independent Bernoulli trial with the same exposure probability, which is
#' the mean audience of the plan's vehicles divided by the population.
#' Exposure probabilities are assumed stationary over time.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.2.2.1 for one insertion per vehicle; Section 3.3.1.1 for several
#' insertions.
#'
#' Lee, A. M., & Burkhart, A. J. (1960). Some optimization problems in
#' advertising media planning. Operational Research Quarterly, 11(3), 113-122.
#'
#' Chandon, J.-L. (1976). A comparative study of media exposure models.
#' Unpublished doctoral dissertation, Northwestern University, Evanston, IL.
#' (Aldás Manzano, 1998, cites Chandon's work as Chandon, 1985.)
#'
#' @param audiences Numeric vector with the audience of each vehicle, in people
#'   per insertion.
#' @param population Population size, in people.
#' @param insertions Non-negative integer vector with the number of insertions
#'   planned in each vehicle. It defaults to one insertion per vehicle, the
#'   model's original scope.
#'
#' @details
#' Aldás Manzano (1998, Section 3.3.1.1) defines the exposure probability as
#' \eqn{p = \bar{A} / P}, where \eqn{\bar{A}} is the simple mean of the
#' vehicle audiences and every vehicle receives the same number of insertions
#' \eqn{n}, so the plan has \eqn{N = n m} insertions. With unequal insertion
#' counts, this function uses the insertion-weighted mean audience,
#' \eqn{\sum_i n_i A_i / \sum_i n_i}. The two definitions coincide when all
#' vehicles receive the same number of insertions, and the weighted mean
#' preserves the plan's expected number of exposures in general.
#'
#' The number of exposures then follows a Binomial distribution with
#' \eqn{N} trials and success probability \eqn{p}, and reach equals
#' \eqn{1 - (1 - p)^N}.
#'
#' @return A list of class `"reach_binomial"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `distribution`: list with `percent` and `people`, one value for each
#'     number of exposures from one to the total number of insertions.
#'   \item `cumulative`: list with `percent` and `people`, for individuals
#'     exposed at least once, at least twice, and so on.
#'   \item `mean_probability`: exposure probability used for every insertion.
#' }
#'
#' @examples
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_binomial(audiences, population)
#' result$reach$percent
#' result$mean_probability
#'
#' # The same three vehicles with several insertions each
#' calc_binomial(audiences, population, insertions = c(4, 6, 10))
#'
#' @seealso
#' [calc_sainsbury()] for heterogeneous vehicles,
#' [calc_beta_binomial()] and [calc_metheringham()] for models with
#' heterogeneous individuals, and [estimate_reach()] to run this model from a
#' [media_plan()] object.
#' @export
calc_binomial <- function(audiences, population,
                          insertions = rep(1L, length(audiences))) {
  validate_vehicle_plan(audiences, population, insertions)

  probs <- rep(audiences / population, insertions)

  if (!length(probs)) {
    return(structure(list(
      reach = list(percent = 0, people = 0),
      distribution = list(percent = numeric(0), people = numeric(0)),
      cumulative = list(percent = numeric(0), people = numeric(0)),
      mean_probability = NA_real_
    ), class = "reach_binomial"))
  }
  p <- mean(probs)
  n <- length(probs)

  full_distribution <- stats::dbinom(0:n, size = n, prob = p)
  P <- full_distribution[-1L]
  R <- rev(cumsum(rev(P)))
  reach <- 1 - full_distribution[1L]

  structure(list(
    reach = list(percent = reach * 100, people = reach * population),
    distribution = list(percent = P * 100, people = P * population),
    cumulative = list(percent = R * 100, people = R * population),
    mean_probability = p
  ), class = "reach_binomial")
}

#__________________________________________________________#

#' Reach and exposure distribution under the Beta-Binomial model
#'
#' Implements the Beta-Binomial accumulation model for one vehicle with
#' several insertions. Individuals differ in their exposure probability, which
#' follows a Beta distribution with shape parameters alpha and beta; given
#' that probability, the number of exposures in `n` insertions is Binomial.
#' Mixing the two yields the Beta-Binomial exposure distribution. The model
#' needs only two data points, the audience after one insertion (`A1`) and
#' after two insertions (`A2`), to estimate both shape parameters.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.1.2.5, equations 3.22-3.28, with the parameter estimators on
#' page 134.
#'
#' @param A1 Vehicle audience after the first insertion, in people.
#' @param A2 Cumulative vehicle audience after the second insertion, in
#'   people.
#' @param P Population size, in people.
#' @param n Total number of planned insertions (a positive integer).
#'
#' @details
#' With \eqn{R_1 = A_1 / P} and \eqn{R_2 = A_2 / P}, the method-of-moments
#' estimators are
#' \deqn{\hat{\alpha} = \frac{R_1 (R_2 - R_1)}{2 R_1 - R_1^2 - R_2}, \qquad
#' \hat{\beta} = \frac{\hat{\alpha} (1 - R_1)}{R_1}.}
#' The estimators require \eqn{R_1 \le R_2 \le 2 R_1 - R_1^2}. The two bounds
#' are valid degenerate cases: at \eqn{R_2 = R_1} all individuals are either
#' always or never exposed (the *polarized* limit, `alpha = beta = 0`), and at
#' \eqn{R_2 = 2 R_1 - R_1^2} exposures are independent (the *binomial* limit,
#' `alpha = beta = Inf`). Both limits are handled explicitly.
#'
#' @return A list of class `"reach_beta_binomial"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `distribution`: list with `percent` and `people`, for one to `n`
#'     exposures.
#'   \item `cumulative`: list with `percent` and `people`, for individuals
#'     exposed at least once, at least twice, and so on.
#'   \item `parameters`: list with `alpha`, `beta`, `mean_probability` (the
#'     mean of the Beta distribution, `A1 / P`), `zero_contact_probability`
#'     (the percentage of the population with no exposure) and `type`
#'     (`"beta_binomial"`, `"binomial_limit"` or `"polarized_limit"`).
#' }
#'
#' @examples
#' result <- calc_beta_binomial(A1 = 500000, A2 = 550000, P = 1000000, n = 5)
#' result$reach$percent
#' result$parameters$alpha
#' result$parameters$beta
#'
#' @seealso
#' [calc_sainsbury()], [calc_binomial()] and [calc_metheringham()] for plans
#' with several vehicles, [calc_hofmans_accumulation()] for an ad hoc
#' accumulation model, and [nbd_exposure_distribution()] for the experimental
#' Negative-Binomial count approximation.
#' @export
calc_beta_binomial <- function(A1, A2, P, n) {
  assert_number(P, "P", min = 0, min_open = TRUE)
  assert_number(A1, "A1", min = 0, min_open = TRUE)
  assert_number(A2, "A2", min = 0, min_open = TRUE)
  if (A1 > P || A2 > P) {
    stop("A1 and A2 cannot exceed the total population P.", call. = FALSE)
  }
  assert_number(n, "n", min = 1, integer = TRUE)
  n <- as.integer(n)

  R1 <- A1 / P
  R2 <- A2 / P
  # Shared method-of-moments helper: it covers the binomial limit (R2 at the
  # independence bound, alpha = beta = Inf) and the polarized limit
  # (R2 = R1, alpha = beta = 0) explicitly instead of passing a non-finite
  # alpha/beta on to extraDistr::dbbinom(), which returns NaN there.
  params <- calculate_bbd_params(R1, R2)

  P_dist <- if (params$type == "binomial_limit") {
    stats::dbinom(0:n, size = n, prob = params$p)
  } else if (params$type == "polarized_limit") {
    out <- numeric(n + 1L)
    out[c(1L, n + 1L)] <- c(1 - params$p, params$p)
    out
  } else {
    extraDistr::dbbinom(0:n, size = n, alpha = params$alpha, beta = params$beta)
  }

  R_dist <- rev(cumsum(rev(P_dist)))
  reach <- 1 - P_dist[1L]

  structure(list(
    reach = list(percent = reach * 100, people = reach * P),
    distribution = list(percent = P_dist[-1L] * 100, people = P_dist[-1L] * P),
    cumulative = list(percent = R_dist[-1L] * 100, people = R_dist[-1L] * P),
    parameters = list(
      alpha = params$alpha,
      beta = params$beta,
      mean_probability = R1,
      zero_contact_probability = P_dist[1L] * 100,
      type = params$type
    )
  ), class = "reach_beta_binomial")
}

#__________________________________________________________#

#' @export
print.reach_sainsbury <- function(x, ...) {
  print_reach_report(
    "SAINSBURY MODEL",
    "random duplication and accumulation, with heterogeneous vehicles",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative
  )
  invisible(x)
}

#' @export
print.reach_binomial <- function(x, ...) {
  print_reach_report(
    "BINOMIAL MODEL",
    "random duplication and accumulation, with homogeneous vehicles",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list("Mean exposure probability" = x$mean_probability)
  )
  invisible(x)
}

#' @export
print.reach_beta_binomial <- function(x, ...) {
  print_reach_report(
    "BETA-BINOMIAL MODEL",
    "heterogeneous individuals, exposure probability Beta-distributed",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list(
      "Alpha (shape of the Beta distribution)" = x$parameters$alpha,
      "Beta (shape of the Beta distribution)" = x$parameters$beta,
      "Probability of 0 exposures (%)" = x$parameters$zero_contact_probability
    ),
    notes = sprintf("Mean of the Beta distribution: %.3f",
                    x$parameters$mean_probability)
  )
  invisible(x)
}

#__________________________________________________________#

# Linearizes the upper triangle (diagonal included) of a symmetric matrix
# row by row: (1,1), (1,2), ..., (1,n), (2,2), (2,3), ... Used internally by
# calc_metheringham().
matrix_to_vector <- function(m) {
  index <- which(upper.tri(m, diag = TRUE), arr.ind = TRUE)
  index <- index[order(index[, 1L], index[, 2L]), , drop = FALSE]
  m[index]
}

# Number of pairs of insertions between two vehicles (off-diagonal, n_i * n_j)
# and within one vehicle (diagonal, choose(n_i, 2)). Together they add up to
# choose(N, 2) pairs for N = sum(n_i) insertions. Used internally by
# calc_metheringham().
create_opportunity_matrix <- function(insertions) {
  m <- outer(insertions, insertions)
  diag(m) <- choose(insertions, 2)
  m
}

#' Reach and exposure distribution under the Metheringham model
#'
#' Implements Metheringham's (1964) model for a plan with several vehicles
#' and several insertions per vehicle, in the version of Aldás Manzano (1998).
#' Individuals are heterogeneous, with Beta-distributed exposure
#' probabilities; vehicles are treated as homogeneous, so that duplication
#' between different vehicles is equivalent to accumulation within one
#' hypothetical "average" vehicle. The audience and the duplication of that
#' average vehicle are the averages, over all pairs of insertions, of the
#' observed audiences and duplications. They fix the two parameters of a
#' Beta-Binomial distribution for the plan's `N = sum(insertions)`
#' insertions.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.3.1.5 (several insertions per vehicle, pages 196-198) and
#' Section 3.2.2.9 (one insertion per vehicle).
#'
#' Metheringham, R. A. (1964). Measuring the net cumulative coverage of a
#' print campaign. Journal of Advertising Research, 4(4), 23-28.
#' \doi{10.1080/00218499.1964.12519751}
#'
#' @param audiences Numeric vector with the audience of each vehicle, in people
#'   per insertion.
#' @param insertions Non-negative integer vector with the number of insertions
#'   planned in each vehicle. At least two insertions are required in total.
#' @param duplication_matrix Symmetric numeric matrix, in people. Element
#'   `[i, j]` with `i != j` is the audience duplicated between one insertion in
#'   vehicle `i` and one insertion in vehicle `j`. The diagonal element
#'   `[i, i]` is the audience duplicated between two insertions in the same
#'   vehicle `i`, that is, `2 * audiences[i]` minus the audience accumulated
#'   by two insertions in vehicle `i`. Diagonal elements are needed only for
#'   vehicles with at least two insertions, and elements involving a vehicle
#'   with no insertions are ignored.
#' @param population Population size, in people.
#'
#' @details
#' There are `N = sum(insertions)` insertions and `choose(N, 2)` pairs of
#' insertions: `insertions[i] * insertions[j]` pairs between vehicles `i` and
#' `j`, and `choose(insertions[i], 2)` pairs within vehicle `i`. The model
#' computes
#' \enumerate{
#'   \item the insertion-weighted mean audience
#'     \eqn{\bar{A}_1 = \sum_i n_i A_i / N};
#'   \item the pair-weighted mean duplication \eqn{\bar{D}}, over all pairs of
#'     insertions;
#'   \item the mean audience accumulated by two insertions of the average
#'     vehicle, \eqn{\bar{A}_2 = 2 \bar{A}_1 - \bar{D}}, which equals the
#'     average, over all pairs of insertions, of the audience reached by the
#'     pair;
#'   \item alpha and beta by the Beta-Binomial estimators of
#'     [calc_beta_binomial()], and the exposure distribution for `N`
#'     insertions.
#' }
#' The exposure distribution therefore ranges from zero to `N` exposures, and
#' its mean equals the plan's gross number of exposures per person,
#' \eqn{\sum_i n_i A_i / P}.
#'
#' @return A list of class `"reach_metheringham"` with components:
#' \itemize{
#'   \item `reach`, `distribution`, `cumulative` and `parameters`, as in
#'     [calc_beta_binomial()] evaluated for `N` insertions;
#'   \item `mean_audience`: insertion-weighted mean audience (`A1`), in
#'     people;
#'   \item `mean_duplication`: pair-weighted mean duplication (`D`), in
#'     people;
#'   \item `second_audience`: mean audience accumulated by two insertions of
#'     the average vehicle (`A2`), in people;
#'   \item `opportunity_matrix`: number of pairs of insertions between and
#'     within vehicles;
#'   \item `opportunity_vector` and `duplication_vector`: the upper triangles
#'     of the opportunity and duplication matrices, linearized row by row;
#'   \item `total_insertions`: the number of insertions `N`.
#' }
#'
#' @examples
#' data(metheringham_example)
#' result <- do.call(calc_metheringham, metheringham_example)
#' result$reach$percent
#' result$parameters$alpha
#'
#' # One insertion per vehicle reduces to Section 3.2.2.9 of Aldas Manzano
#' # (1998); the diagonal is not needed then
#' calc_metheringham(
#'   audiences = c(300000, 400000, 200000), insertions = c(1, 1, 1),
#'   duplication_matrix = matrix(c(NA, 150000, 90000,
#'                                 150000, NA, 110000,
#'                                 90000, 110000, NA), nrow = 3),
#'   population = 1000000
#' )$reach$percent
#'
#' @seealso
#' [calc_beta_binomial()], called internally, for the univariate model;
#' [calc_sainsbury()] and [calc_binomial()] for models that assume random
#' duplication.
#' @export
calc_metheringham <- function(audiences, insertions, duplication_matrix,
                              population) {
  assert_numeric_vector(audiences, "audiences", min = 0)
  n_vehicles <- length(audiences)
  assert_numeric_vector(insertions, "insertions", min = 0, integer = TRUE,
                        length = n_vehicles)
  assert_number(population, "population", min = 0, min_open = TRUE)
  if (any(audiences > population)) {
    stop("audiences cannot exceed population.", call. = FALSE)
  }
  total_insertions <- sum(insertions)
  if (total_insertions < 2) {
    stop("At least two insertions in total are required to observe duplication.",
         call. = FALSE)
  }
  if (!is.matrix(duplication_matrix) || !is.numeric(duplication_matrix) ||
      !identical(dim(duplication_matrix), c(n_vehicles, n_vehicles))) {
    stop("duplication_matrix must be a numeric ", n_vehicles, " x ",
         n_vehicles, " matrix.", call. = FALSE)
  }

  opportunity_matrix <- create_opportunity_matrix(insertions)
  used <- opportunity_matrix > 0

  # Only the entries that carry pairs of insertions are needed and checked.
  needed <- duplication_matrix[used]
  if (anyNA(needed) || any(!is.finite(needed))) {
    stop("duplication_matrix must be finite wherever a pair of insertions ",
         "exists (off-diagonal entries between vehicles with insertions, ",
         "and diagonal entries of vehicles with at least two insertions).",
         call. = FALSE)
  }
  if (!isTRUE(all.equal(duplication_matrix[used & upper.tri(used)],
                        t(duplication_matrix)[used & upper.tri(used)],
                        check.attributes = FALSE))) {
    stop("duplication_matrix must be symmetric.", call. = FALSE)
  }
  tolerance <- 1e-9 * population
  for (i in seq_len(n_vehicles)) {
    for (j in i:n_vehicles) {
      if (!used[i, j]) next
      lower <- max(0, audiences[i] + audiences[j] - population)
      upper <- min(audiences[i], audiences[j])
      value <- duplication_matrix[i, j]
      if (value < lower - tolerance || value > upper + tolerance) {
        stop(sprintf(paste0(
          "duplication_matrix[%d, %d] must lie between %.6g and %.6g, the ",
          "bounds implied by the audiences and the population."),
          i, j, lower, upper), call. = FALSE)
      }
    }
  }

  duplication <- duplication_matrix
  duplication[!used] <- 0
  duplication_vector <- matrix_to_vector(duplication)
  opportunity_vector <- matrix_to_vector(opportunity_matrix)

  A1 <- sum(audiences * insertions) / total_insertions
  D <- sum(duplication_vector * opportunity_vector) / sum(opportunity_vector)
  A2 <- 2 * A1 - D
  if (A1 <= 0) {
    stop("At least one vehicle with insertions must have a positive audience.",
         call. = FALSE)
  }

  fitted <- tryCatch(
    calc_beta_binomial(A1 = A1, A2 = A2, P = population, n = total_insertions),
    error = function(e) {
      stop("The mean audience (", format(A1, digits = 6), ") and mean ",
           "duplication (", format(D, digits = 6), ") are incompatible with a ",
           "Beta-Binomial exposure model: ", conditionMessage(e),
           call. = FALSE)
    }
  )

  structure(list(
    reach = fitted$reach,
    distribution = fitted$distribution,
    cumulative = fitted$cumulative,
    parameters = fitted$parameters,
    mean_audience = A1,
    mean_duplication = D,
    second_audience = A2,
    opportunity_matrix = opportunity_matrix,
    opportunity_vector = opportunity_vector,
    duplication_vector = duplication_vector,
    total_insertions = total_insertions
  ), class = "reach_metheringham")
}

#' @export
print.reach_metheringham <- function(x, ...) {
  print_reach_report(
    "METHERINGHAM MODEL",
    paste("duplication between homogeneous vehicles reduced to accumulation",
          "within one average vehicle"),
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list(
      "Alpha (shape of the Beta distribution)" = x$parameters$alpha,
      "Beta (shape of the Beta distribution)" = x$parameters$beta,
      "Probability of 0 exposures (%)" = x$parameters$zero_contact_probability,
      "Mean audience (A1, people)" = x$mean_audience,
      "Mean duplication (D, people)" = x$mean_duplication,
      "Audience after 2 insertions (A2, people)" = x$second_audience,
      "Total insertions (N)" = x$total_insertions
    )
  )
  cat("\nPAIRS OF INSERTIONS:\n")
  cat("--------------------\n")
  print(x$opportunity_matrix)
  cat("Off-diagonal: pairs between vehicles. Diagonal: pairs within a vehicle.\n")
  invisible(x)
}
