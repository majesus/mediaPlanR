#__________________________________________________________#

#' @encoding UTF-8
#' @title Reach and contact distribution (and cumulative) under the Sainsbury model
#' @description Implements the Sainsbury model, developed by E. J. Sainsbury at the
#' London Press Exchange, to calculate reach and the contact distribution for a set
#' of advertising vehicles. The model assumes random duplication *and* random
#' accumulation, homogeneous individual exposure probabilities, and heterogeneous
#' vehicle exposure probabilities for a more precise estimate of reach and the
#' contact distribution (and cumulative distribution). From the last two
#' assumptions it follows that the probability of an individual being exposed to
#' vehicle i is the ratio between vehicle i's audience (favourable cases) and the
#' population (total cases). From the random-duplication and random-accumulation
#' assumptions it follows that every insertion -- whether in a different vehicle
#' or a repeat insertion in the same one -- is an independent Bernoulli trial with
#' that vehicle's own exposure probability.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#' (Sec. 3.2.2.2 for a single insertion per vehicle; Sec. 3.3.1.2, reviewing
#' Chandon, J.-L. (1985), *A comparative study of media exposure models*,
#' doctoral dissertation, University of Pennsylvania, for several insertions.)
#'
#' @param audiences Numeric vector with the individual audience of each vehicle
#' @param population Population size
#' @param insertions Positive integer vector, one value per vehicle, with the
#'   number of insertions planned in it. Defaults to one insertion per vehicle,
#'   the model's original scope; values above one apply the same independence
#'   hypothesis to repeat insertions in that vehicle.
#'
#' @details
#' The simplified Sainsbury model calculates:
#' \enumerate{
#'   \item Reach, treating duplication between vehicles (and, when `insertions`
#'   is above one, accumulation within a vehicle) as the product of the
#'   individual probabilities
#'   \item The contact distribution for each exposure level i
#'   \item The cumulative contact distribution (exposed at least i times)
#' }
#'
#' The process includes:
#' \itemize{
#'   \item Converting audiences to probabilities, repeating each vehicle's
#'   probability once per insertion planned in it
#'   \item Computing every possible combination of insertions
#'   \item Estimating joint probabilities
#'   \item Aggregating results: contact distribution (and cumulative)
#' }
#'
#' @return A list of class "reach_sainsbury" containing:
#' \itemize{
#'   \item reach: List with reach:
#'     \itemize{
#'       \item percent: Reach as a percentage
#'       \item people: Reach in number of people
#'     }
#'   \item distribution: List with the contact distribution:
#'     \itemize{
#'       \item percent: Vector with the probability of each number of exposures
#'       \item people: Vector with the number of people for each number of exposures
#'     }
#'   \item cumulative: List with the cumulative distribution:
#'     \itemize{
#'       \item percent: Vector with cumulative probabilities
#'       \item people: Vector with the number of people exposed at least i times
#'     }
#' }
#'
#' @examples
#' # Basic example: three vehicles, one insertion each
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_sainsbury(audiences, population)
#'
#' # Inspect the results
#' print(result$reach$percent)  # Reach as a percentage
#' print(result$distribution$people)  # People by number of contacts
#'
#' # Same three vehicles, several insertions each
#' calc_sainsbury(audiences, population, insertions = c(4, 6, 10))
#'
#' # Example with input validation
#' \dontrun{
#' invalid_audiences <- c(300000, -400000, 200000)
#' result <- calc_sainsbury(invalid_audiences, population)
#' # Raises an error due to the negative audience
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_binomial}} for estimates under the Binomial distribution
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial distribution
#' \code{\link{calc_metheringham}} for estimates under the Metheringham distribution
#' \code{\link{calc_hofmans_accumulation}} for estimates under the Hofmans distribution
#' \code{\link{estimate_reach}} to run this model directly from a \code{media_plan}
#' @importFrom utils combn
calc_sainsbury <- function(audiences, population,
                          insertions = rep(1L, length(audiences))) {
  # Input validation
  if (!is.numeric(audiences) || !is.numeric(population)) {
    stop("audiences and population must be numeric")
  }
  if (length(audiences) < 1L || anyNA(audiences) || any(!is.finite(audiences))) {
    stop("audiences must contain at least one finite value")
  }
  if (length(population) != 1L || !is.finite(population) ||
      any(audiences < 0) || any(audiences > population)) {
    stop("audiences must be positive and smaller than the population")
  }
  if (population <= 0) {
    stop("population must be positive")
  }
  if (!is.numeric(insertions) || length(insertions) != length(audiences) ||
      anyNA(insertions) || any(insertions < 0) || any(insertions != round(insertions))) {
    stop("insertions must provide one non-negative integer per vehicle")
  }

  # Convert audiences to probabilities, one per insertion planned
  probs <- rep(audiences / population, insertions)

  if (!length(probs)) {
    return(structure(list(
      reach = list(percent = 0, people = 0),
      distribution = list(percent = numeric(0), people = numeric(0)),
      cumulative = list(percent = numeric(0), people = numeric(0))
    ), class = "reach_sainsbury"))
  }

  # Exact Poisson-binomial distribution using dynamic convolution. This is
  # O(N^2), whereas enumerating every combination is O(2^N).
  full_distribution <- poisson_binomial_distribution(probs)
  P <- full_distribution[-1L]
  R <- rev(cumsum(rev(P)))

  # Total reach
  reach <- 1 - prod(1 - probs)

  return(structure(list(
    reach = list(
      percent = reach * 100,
      people = reach * population
    ),
    distribution = list(
      percent = P * 100,
      people = P * population
    ),
    cumulative = list(
      percent = R * 100,
      people = R * population
    )
  ), class = "reach_sainsbury"))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Reach and contact distribution (and cumulative) under the Binomial model
#' @description Implements the Binomial model, developed by Lee and Burkart
#' (1960) and reviewed by Chandon (1985), to calculate the reach and contact
#' distribution (and cumulative distribution) of a media plan with several
#' vehicles. The Binomial model assumes random duplication *and* random
#' accumulation (repeat insertions in the same vehicle are also treated as
#' independent), and homogeneity of both the vehicle exposure probabilities
#' and the individual exposure probabilities. Combining these last two
#' assumptions, the exposure probability of any individual to a given vehicle
#' is computed as the mean of every vehicle's audience. Exposure
#' probabilities are assumed stationary over time.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#' (Sec. 3.2.2.1 for a single insertion per vehicle, reviewing Chandon (1985);
#' Sec. 3.3.1.1 for several insertions, reviewing Lee, T. C., & Burkart, A.
#' (1960).)
#'
#' @param audiences Numeric vector with the individual audience of each vehicle
#' @param population Population size
#' @param insertions Positive integer vector, one value per vehicle, with the
#'   number of insertions planned in it. Defaults to one insertion per vehicle,
#'   the model's original scope; values above one apply the same independence
#'   hypothesis to repeat insertions in that vehicle.
#'
#' @details
#' The Binomial model calculates:
#' \enumerate{
#'   \item Reach, treating the plan as one hypothetical "average" vehicle whose
#'   audience is the simple mean of every vehicle's audience
#'   \item The contact distribution for each exposure level
#'   \item The cumulative contact distribution (exposed at least i times)
#' }
#'
#' The methodology includes:
#' \itemize{
#'   \item Converting audiences to individual probabilities, one per insertion
#'   planned in each vehicle
#'   \item Computing the mean exposure probability across every insertion
#'   \item Applying the Binomial model for the plan's total number of insertions
#'   \item Computing the contact distributions (and cumulative)
#' }
#'
#' @return A list of class "reach_binomial" containing:
#' \itemize{
#'   \item reach: List with reach:
#'     \itemize{
#'       \item percent: Reach as a percentage
#'       \item people: Reach in number of people
#'     }
#'   \item distribution: List with the contact distribution:
#'     \itemize{
#'       \item percent: Vector with the probability of each number of exposures
#'       \item people: Vector with the number of people for each number of exposures
#'     }
#'   \item cumulative: List with the cumulative distribution:
#'     \itemize{
#'       \item percent: Vector with cumulative probabilities
#'       \item people: Vector with the number of people exposed at least i times
#'     }
#'   \item mean_probability: Mean exposure probability used for every insertion
#' }
#'
#' @examples
#' # Basic example: three vehicles, one insertion each
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_binomial(audiences, population)
#'
#' # Inspect the results
#' print(paste("Total reach:", result$reach$percent, "%"))
#' print(paste("Mean probability:", result$mean_probability))
#'
#' # Same three vehicles, several insertions each
#' calc_binomial(audiences, population, insertions = c(4, 6, 10))
#'
#' # Check that the distributions sum to 1 (100%)
#' \dontrun{
#' sum_dist <- sum(result$distribution$percent) / 100
#' print(paste("Distribution sum:", round(sum_dist, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} for estimates under the Sainsbury distribution
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial distribution
#' \code{\link{calc_metheringham}} for estimates under the Metheringham distribution
#' \code{\link{calc_hofmans_accumulation}} for estimates under the Hofmans distribution
#' \code{\link{estimate_reach}} to run this model directly from a \code{media_plan}
calc_binomial <- function(audiences, population,
                         insertions = rep(1L, length(audiences))) {
  # Input validation
  if (!is.numeric(audiences) || !is.numeric(population)) {
    stop("audiences and population must be numeric")
  }
  if (length(audiences) < 1L || anyNA(audiences) || any(!is.finite(audiences))) {
    stop("audiences must contain at least one finite value")
  }
  if (length(population) != 1L || !is.finite(population) ||
      any(audiences < 0) || any(audiences > population)) {
    stop("audiences must be positive and smaller than the total population")
  }
  if (population <= 0) {
    stop("population must be positive")
  }
  if (!is.numeric(insertions) || length(insertions) != length(audiences) ||
      anyNA(insertions) || any(insertions < 0) || any(insertions != round(insertions))) {
    stop("insertions must provide one non-negative integer per vehicle")
  }

  # Convert audiences to a mean probability, one per insertion planned
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

  # Exact Binomial distribution via stats::dbinom(), the same primitive
  # estimate_reach(model = "binomial") uses -- it calls this function
  # directly, so the two share one implementation, not just one formula.
  full_distribution <- stats::dbinom(0:n, size = n, prob = p)
  P <- full_distribution[-1L]
  R <- rev(cumsum(rev(P)))

  # Total reach
  reach <- 1 - full_distribution[1L]

  return(structure(list(
    reach = list(
      percent = reach * 100,
      people = reach * population
    ),
    distribution = list(
      percent = P * 100,
      people = P * population
    ),
    cumulative = list(
      percent = R * 100,
      people = R * population
    ),
    mean_probability = p
  ), class = "reach_binomial"))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Reach and contact distribution (and cumulative) under the Beta-Binomial model
#' @description Implements the Beta-Binomial model to calculate net cumulative
#' audience and the contact distribution (and cumulative distribution). The
#' Beta-Binomial model accounts for heterogeneity in individuals' exposure
#' probability. It combines two steps: it models the success probability with a
#' Beta distribution of shape parameters alpha and beta -- which reduces the data
#' required for estimation to just two numbers -- and uses that probability in the
#' Binomial distribution (mixed with the Beta distribution) to obtain the contact
#' distribution (and cumulative distribution). It is useful when the success
#' probability is not known a priori and can vary across individuals. The alpha
#' and beta parameters let the shape of the distribution reflect the uncertainty
#' about the success probability.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#' (Sec. 3.1.2.5, formulas \[3.22\]-\[3.27\] for the model, and the A/B
#' estimators on p. 134.)
#'
#' @param A1 Vehicle audience after the first insertion
#' @param A2 Vehicle audience after the second insertion
#' @param P Total population size
#' @param n Total number of planned insertions (must be a positive integer)
#'
#' @details
#' The Beta-Binomial model:
#' \enumerate{
#'   \item Computes the alpha and beta parameters from A1 and A2
#'   \item Models exposure heterogeneity with the Beta distribution
#'   \item Combines the Beta distribution with the Binomial for the contact distribution
#'   \item Computes exact probabilities for each exposure level
#' }
#'
#' The process includes:
#' \itemize{
#'   \item Estimating the duplication coefficients R1 and R2
#'   \item Computing the model's alpha and beta parameters
#'   \item Generating the contact distribution
#'   \item Computing the contact distribution (and cumulative)
#' }
#'
#' @return A list of class "reach_beta_binomial" containing:
#' \itemize{
#'   \item reach: List with reach:
#'     \itemize{
#'       \item percent: Reach as a percentage
#'       \item people: Reach in number of people
#'     }
#'   \item distribution: List with the contact distribution:
#'     \itemize{
#'       \item percent: Vector with the probability of each number of exposures
#'       \item people: Vector with the number of people for each number of exposures
#'     }
#'   \item cumulative: List with the cumulative distribution:
#'     \itemize{
#'       \item percent: Vector with cumulative probabilities
#'       \item people: Vector with the number of people exposed at least i times
#'     }
#'   \item parameters: List with the model parameters:
#'     \itemize{
#'       \item alpha: Estimated alpha parameter
#'       \item beta: Estimated beta parameter
#'       \item zero_contact_probability: Probability of no exposure
#'     }
#' }
#'
#' @note
#' The Beta-Binomial model is especially well suited when:
#' \itemize{
#'   \item There is significant heterogeneity in the population
#'   \item Cumulative audience data are available (A1 and A2)
#' }
#'
#' @examples
#' # Basic example
#' result <- calc_beta_binomial(
#'   A1 = 500000,    # First audience
#'   A2 = 550000,    # Second audience
#'   P = 1000000,    # Total population
#'   n = 5           # Number of insertions
#' )
#'
#' # Inspect the results
#' print(paste("Reach:", round(result$reach$percent, 2), "%"))
#' print(paste("Alpha:", round(result$parameters$alpha, 4)))
#' print(paste("Beta:", round(result$parameters$beta, 4)))
#'
#' # Check consistency of the distributions
#' \dontrun{
#' sum_dist <- sum(result$distribution$percent) / 100
#' print(paste("Distribution sum:", round(sum_dist +
#'             result$parameters$zero_contact_probability / 100, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} for estimates under the Sainsbury distribution
#' \code{\link{calc_binomial}} for estimates under the Binomial distribution
#' \code{\link{calc_metheringham}} for estimates under the Metheringham distribution
#' \code{\link{calc_hofmans_accumulation}} for estimates under the Hofmans distribution
#' \code{\link{nbd_exposure_distribution}} for the experimental Negative-Binomial
#' (NBD) approximation to exposure counts
calc_beta_binomial <- function(A1, A2, P, n) {
  # Input validation
  if (!all(is.numeric(c(A1, A2, P, n)))) {
    stop("All arguments must be numeric")
  }
  if (A1 <= 0 || A2 <= 0 || P <= 0) {
    stop("audiences and population must be positive")
  }
  if (A1 > P || A2 > P) {
    stop("audiences cannot exceed the total population")
  }
  if (n <= 0 || n != round(n)) {
    stop("n must be a positive integer")
  }

  # Ensure n is an integer
  n <- as.integer(n)

  # Compute R1 and R2
  R1 <- A1 / P
  R2 <- A2 / P

  # Method-of-moments alpha and beta, via the same shared helper
  # calc_canex()/calc_csd()/calc_msad()/calc_mbd() use. This also covers the
  # binomial limit (R2 at the independence bound, alpha = beta = Inf) and the
  # polarized limit (R2 = R1, alpha = beta = 0) explicitly instead of passing
  # a non-finite alpha/beta on to extraDistr::dbbinom(), which silently
  # returns NaN for the whole distribution at those exact boundaries.
  params <- calculate_bbd_params(R1, R2)

  # Compute the contact distribution (P)
  P_dist <- if (params$type == "binomial_limit") {
    stats::dbinom(0:n, size = n, prob = params$p)
  } else if (params$type == "polarized_limit") {
    out <- numeric(n + 1L)
    out[c(1L, n + 1L)] <- c(1 - params$p, params$p)
    out
  } else {
    extraDistr::dbbinom(0:n, size = n, alpha = params$alpha, beta = params$beta)
  }

  # Compute the cumulative distribution (R)
  R_dist <- sapply(0:n, function(k) sum(P_dist[(k + 1):length(P_dist)]))

  # Total reach is 1 minus the probability of 0 contacts
  reach <- 1 - P_dist[1]

  # Drop the 0-contact cell from the final distributions
  P_no_zero <- P_dist[-1]
  R_no_zero <- R_dist[-1]

  return(structure(list(
    reach = list(
      percent = reach * 100,
      people = reach * P
    ),
    distribution = list(
      percent = P_no_zero * 100,
      people = P_no_zero * P
    ),
    cumulative = list(
      percent = R_no_zero * 100,
      people = R_no_zero * P
    ),
    parameters = list(
      alpha = params$alpha,
      beta = params$beta,
      zero_contact_probability = P_dist[1] * 100
    )
  ), class = "reach_beta_binomial"))
}

#__________________________________________________________#

#' @export
print.reach_sainsbury <- function(x, ...) {
  print_reach_report(
    "SAINSBURY MODEL",
    "model that assumes independence between vehicles and vehicle heterogeneity",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative
  )
  invisible(x)
}

#' @export
print.reach_binomial <- function(x, ...) {
  print_reach_report(
    "BINOMIAL MODEL",
    "model that assumes independence between vehicles and homogeneity",
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
    "model that accounts for heterogeneity in the population",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list(
      "Alpha (shape of the Beta distribution)" = x$parameters$alpha,
      "Beta (shape of the Beta distribution)" = x$parameters$beta,
      "Probability of 0 contacts (%)" = x$parameters$zero_contact_probability
    ),
    notes = sprintf("Theoretical mean of the Beta distribution: %.3f",
                    x$parameters$alpha / (x$parameters$alpha + x$parameters$beta))
  )
  invisible(x)
}

#__________________________________________________________#

# Linearizes a symmetric matrix (e.g. of duplications or contact
# opportunities) by walking its upper triangle, including the diagonal, in
# the order (1,1), (1,2), ..., (1,n), (2,2), (2,3), .... Used internally by
# calc_metheringham().
matrix_to_vector <- function(m) {
  n <- nrow(m)
  v <- numeric()
  for (i in 1:n) {
    for (j in i:n) {
      v <- c(v, m[i, j])
    }
  }
  return(v)
}

# From the number of insertions per vehicle, computes the number of contact
# opportunities between each pair of vehicles (off-diagonal) and within a
# single vehicle (on the diagonal, as choose(insertions, 2)). Used internally
# by calc_metheringham().
create_opportunity_matrix <- function(insertions) {
  n <- length(insertions)
  m <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        m[i, j] <- choose(insertions[i], 2)
      } else {
        m[i, j] <- insertions[i] * insertions[j]
        m[j, i] <- m[i, j]  # symmetric
      }
    }
  }
  return(m)
}

#' @encoding UTF-8
#' @title Reach and contact distribution (and cumulative) under the Metheringham model
#' @description Implements Metheringham's (1964) model to calculate reach and
#' the contact distribution (and cumulative distribution) for several
#' vehicles. Individuals have heterogeneous, Beta-distributed exposure
#' probabilities; vehicles are treated as homogeneous, which makes the
#' duplication problem between vehicles equivalent to an accumulation problem
#' within one hypothetical "average" vehicle. Audience and duplication are
#' therefore averaged across vehicles first (A1, D), from which the
#' cumulative audience after two insertions of that average vehicle follows
#' (A2 = 2 x A1 - D). A1 and A2 are then the same inputs
#' \code{\link{calc_beta_binomial}} takes for one vehicle with several
#' insertions, so this function estimates alpha and beta from them and
#' evaluates the Beta-Binomial contact distribution for the plan's actual
#' number of vehicles.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#' (Sec. 3.2.2.9.)
#'
#' @param audiences Numeric vector with the audience of each vehicle
#' @param insertions Numeric vector with the number of insertions per vehicle
#' @param duplication_matrix Symmetric matrix with the duplication values between vehicles
#' @param population Population size
#'
#' @details
#' The function performs the following calculations:
#' \enumerate{
#'   \item Mean audience (A1): the insertion-weighted mean of the audiences,
#'   \eqn{A1 = \sum(Audience_i \times Insertions_i) / \sum(Insertions_i)}
#'   \item Mean duplication (D): the opportunity-weighted mean of the
#'   duplications, considering every combination between vehicles ii, ij
#'   \item Cumulative audience after two insertions of the hypothetical
#'   average vehicle: \eqn{A2 = 2 \times A1 - D}
#'   \item Alpha and beta of the Beta-Binomial distribution implied by A1 and
#'   A2, and the resulting reach and contact distribution for the plan's
#'   actual number of vehicles (via \code{\link{calc_beta_binomial}})
#' }
#'
#' @return A list of class "reach_metheringham" containing:
#' \itemize{
#'   \item reach: List with reach:
#'     \itemize{
#'       \item percent: Reach as a percentage
#'       \item people: Reach in number of people
#'     }
#'   \item distribution: List with the contact distribution:
#'     \itemize{
#'       \item percent: Vector with the probability of each number of exposures
#'       \item people: Vector with the number of people for each number of exposures
#'     }
#'   \item cumulative: List with the cumulative distribution:
#'     \itemize{
#'       \item percent: Vector with cumulative probabilities
#'       \item people: Vector with the number of people exposed at least i times
#'     }
#'   \item parameters: List with alpha, beta, and the probability of 0 contacts
#'   \item mean_audience: Insertion-weighted mean audience (A1)
#'   \item mean_duplication: Opportunity-weighted mean duplication (D)
#'   \item second_audience: Cumulative audience after two insertions of the
#'         hypothetical average vehicle (A2)
#'   \item opportunity_matrix: Matrix with the number of contact opportunities
#'         between pairs of insertions
#'   \item opportunity_vector: Linearized version of the opportunity matrix
#'   \item duplication_vector: Linearized version of the duplication matrix
#' }
#'
#' @note
#' The duplication matrix must be symmetric, where:
#' \itemize{
#'   \item The diagonal holds each vehicle's duplication with itself
#'   \item Element `[i,j]` holds the duplication between vehicles i and j
#'   \item `matrix[i,j] = matrix[j,i]` must hold
#'   \item For n vehicles, the matrix must be n x n
#' }
#'
#' @examples
#' # Basic example with three vehicles
#' duplication_matrix <- matrix(c(
#'   150000, 200000, 180000,
#'   200000, 120000, 140000,
#'   180000, 140000, 170000
#' ), nrow = 3, byrow = TRUE)
#'
#' result <- calc_metheringham(
#'   audiences = c(1500000, 800000, 1200000),
#'   insertions = c(4, 3, 5),
#'   duplication_matrix = duplication_matrix,
#'   population = 10000000
#' )
#' result$reach$percent
#' result$parameters$alpha
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} for estimates under the Sainsbury distribution
#' \code{\link{calc_binomial}} for estimates under the Binomial distribution
#' \code{\link{calc_beta_binomial}}, called internally, for estimates under
#' the Beta-Binomial distribution from a single vehicle's own A1/A2
#' \code{\link{calc_hofmans_accumulation}} for estimates under the Hofmans distribution
# Main Metheringham function
calc_metheringham <- function(audiences, insertions, duplication_matrix, population) {
  if (length(audiences) != length(insertions)) {
    stop("audiences and insertions must have the same length")
  }
  if (any(insertions < 0) || any(audiences < 0)) {
    stop("audiences and insertions must be non-negative")
  }
  if (sum(insertions) <= 0) {
    stop("Total insertions must be greater than 0")
  }
  if (!is.numeric(population) || length(population) != 1L ||
      !is.finite(population) || population <= 0) {
    stop("population must be one positive finite number")
  }

  n_vehicles <- length(audiences)

  if (!is.matrix(duplication_matrix)) {
    stop("duplication_matrix must be a matrix")
  }

  if (nrow(duplication_matrix) != n_vehicles || ncol(duplication_matrix) != n_vehicles) {
    stop("duplication_matrix dimensions do not match the number of vehicles")
  }

  if (!all(duplication_matrix == t(duplication_matrix))) {
    warning("duplication_matrix is not symmetric. Its upper triangle will be used.")
    duplication_matrix[lower.tri(duplication_matrix)] <- t(duplication_matrix)[lower.tri(duplication_matrix)]
  }

  opportunity_matrix <- create_opportunity_matrix(insertions)

  duplication_vector <- matrix_to_vector(duplication_matrix)
  opportunity_vector <- matrix_to_vector(opportunity_matrix)

  if (sum(opportunity_vector) <= 0) {
    stop("There are no contact opportunities between vehicles (check insertions)")
  }

  A1 <- sum(audiences * insertions) / sum(insertions)
  D <- sum(duplication_vector * opportunity_vector) / sum(opportunity_vector)
  A2 <- 2 * A1 - D

  beta_binomial <- calc_beta_binomial(A1 = A1, A2 = A2, P = population, n = n_vehicles)

  result <- list(
    reach = beta_binomial$reach,
    distribution = beta_binomial$distribution,
    cumulative = beta_binomial$cumulative,
    parameters = beta_binomial$parameters,
    mean_audience = A1,
    mean_duplication = D,
    second_audience = A2,
    opportunity_matrix = opportunity_matrix,
    opportunity_vector = opportunity_vector,
    duplication_vector = duplication_vector,
    total_insertions = sum(insertions)
  )

  class(result) <- "reach_metheringham"
  return(result)
}


#' @export
print.reach_metheringham <- function(x, ...) {
  print_reach_report(
    "METHERINGHAM MODEL",
    "model that reduces duplication between homogeneous vehicles to accumulation within one hypothetical average vehicle",
    x$reach$percent, x$reach$people,
    distribution = x$distribution, cumulative = x$cumulative,
    parameters = list(
      "Alpha (shape of the Beta distribution)" = x$parameters$alpha,
      "Beta (shape of the Beta distribution)" = x$parameters$beta,
      "Probability of 0 contacts (%)" = x$parameters$zero_contact_probability,
      "Mean audience (A1, people)" = x$mean_audience,
      "Mean duplication (D, people)" = x$mean_duplication,
      "Cumulative audience after 2 insertions (A2, people)" = x$second_audience
    )
  )

  cat("\nCONTACT OPPORTUNITY MATRIX:\n")
  cat("---------------------------\n")
  print(x$opportunity_matrix)
  cat("Interpretation: number of possible insertion pairs between vehicles.\n")
  cat("Diagonal: opportunities within the same vehicle. Off-diagonal: opportunities between different vehicles.\n")
  invisible(x)
}
