#__________________________________________________________#

#' @encoding UTF-8
#' @title Reach and contact distribution (and cumulative) under the Sainsbury model
#' @description Implements the Sainsbury model, developed by E. J. Sainsbury at the
#' London Press Exchange, to calculate reach and the contact distribution for a set
#' of advertising vehicles with a single insertion per vehicle. The model assumes
#' random duplication, homogeneous individual exposure probabilities, and
#' heterogeneous vehicle exposure probabilities for a more precise estimate of
#' reach and the contact distribution (and cumulative distribution). From the last
#' two assumptions it follows that the probability of an individual being exposed
#' to vehicle i is the ratio between vehicle i's audience (favourable cases) and
#' the population (total cases). From the random-duplication assumption it follows
#' that exposure remains a Bernoulli variable with a different exposure probability
#' for each vehicle.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiences Numeric vector with the individual audience of each vehicle
#' @param population Population size
#'
#' @details
#' The simplified Sainsbury model calculates:
#' \enumerate{
#'   \item Reach, treating duplication between vehicles as the product of the
#'   individual probabilities
#'   \item The contact distribution for each exposure level i
#'   \item The cumulative contact distribution (exposed at least i times)
#' }
#'
#' The process includes:
#' \itemize{
#'   \item Converting audiences to probabilities
#'   \item Computing every possible combination of vehicles
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
#' # Basic example with three vehicles
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_sainsbury(audiences, population)
#'
#' # Inspect the results
#' print(result$reach$percent)  # Reach as a percentage
#' print(result$distribution$people)  # People by number of contacts
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
#' \code{\link{calc_hofmans}} for estimates under the Hofmans distribution
#' @importFrom utils combn
calc_sainsbury <- function(audiences, population) {
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

  # Convert audiences to probabilities
  probs <- audiences / population
  n <- length(probs)

  # Exact Poisson-binomial distribution using dynamic convolution. This is
  # O(n^2), whereas enumerating every combination is O(2^n).
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
#' @description Implements the Binomial model, developed by Chandon (1985), to
#' calculate the reach and contact distribution (and cumulative distribution) of
#' a media plan with n vehicles and a single insertion per vehicle. The Binomial
#' model assumes random duplication (i.e. exposure to one vehicle does not change
#' the probability of being exposed to another), and homogeneity of both the
#' vehicle exposure probabilities and the individual exposure probabilities.
#' Combining these last two assumptions, the exposure probability of any
#' individual to a given vehicle is computed as the mean of every vehicle's
#' audience. Exposure probabilities are assumed stationary over time.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiences Numeric vector with the individual audience of each vehicle
#' @param population Population size
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
#'   \item Converting audiences to individual probabilities
#'   \item Computing the mean exposure probability
#'   \item Applying the Binomial model for n insertions
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
#' }
#'
#' @examples
#' # Basic example with three vehicles
#' audiences <- c(300000, 400000, 200000)
#' population <- 1000000
#' result <- calc_binomial(audiences, population)
#'
#' # Inspect the results
#' print(paste("Total reach:", result$reach$percent, "%"))
#' print(paste("Mean probability:", result$mean_probability))
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
#' \code{\link{calc_hofmans}} for estimates under the Hofmans distribution
calc_binomial <- function(audiences, population) {
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

  # Convert audiences to a mean probability
  probs <- audiences / population
  p <- mean(probs)
  n <- length(audiences)

  P <- numeric(n) # Contact distribution
  R <- numeric(n) # Cumulative distribution

  # Compute the contact distribution (P)
  for (i in 1:n) {
    P[i] <- choose(n, i) * p^i * (1 - p)^(n - i)
  }

  # Compute the cumulative distribution (R)
  for (i in 1:n) {
    R[i] <- sum(P[i:n])
  }

  # Total reach
  reach <- 1 - (1 - p)^n

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
#' \code{\link{calc_hofmans}} for estimates under the Hofmans distribution
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

  # Compute alpha and beta
  alpha <- (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2)
  beta <- (alpha * (1 - R1)) / R1

  # Validate alpha and beta
  if (is.na(alpha) || is.na(beta) || alpha <= 0 || beta <= 0) {
    stop("Could not compute valid parameters from the data provided")
  }

  # Compute the contact distribution (P)
  P_dist <- extraDistr::dbbinom(0:n, size = n, alpha = alpha, beta = beta)

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
      alpha = alpha,
      beta = beta,
      zero_contact_probability = P_dist[1] * 100
    )
  ), class = "reach_beta_binomial"))
}

#__________________________________________________________#

#' @export
print.reach_sainsbury <- function(x, ...) {
  cat("SAINSBURY MODEL\n")
  cat("===============\n")
  cat("Description: model that assumes independence between vehicles and vehicle heterogeneity\n\n")

  # Headline metrics
  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%% (%.0f people)\n",
              x$reach$percent, x$reach$people))

  # Contact distribution
  cat("\nCONTACT DISTRIBUTION:\n")
  cat("----------------------\n")
  cat("(Percentage of the population receiving exactly N contacts)\n")
  for (i in seq_along(x$distribution$percent)) {
    cat(sprintf("%d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribution$percent[i],
                x$distribution$people[i]))
  }

  # Cumulative distribution
  cat("\nCUMULATIVE DISTRIBUTION:\n")
  cat("-------------------------\n")
  cat("(Percentage of the population receiving N or more contacts)\n")
  for (i in seq_along(x$cumulative$percent)) {
    cat(sprintf(">= %d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$cumulative$percent[i],
                x$cumulative$people[i]))
  }

  # Summary statistics
  total_contacts <- sum(seq_along(x$distribution$percent) *
                           x$distribution$people)
  average_contacts <- total_contacts / sum(x$distribution$people)
  cat("\nSUMMARY STATISTICS:\n")
  cat("--------------------\n")
  cat(sprintf("Average contacts per person reached: %.2f\n",
              average_contacts))
}

#' @export
print.reach_binomial <- function(x, ...) {
  cat("BINOMIAL MODEL\n")
  cat("==============\n")
  cat("Description: model that assumes independence between vehicles and homogeneity\n\n")

  # Headline metrics
  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%% (%.0f people)\n",
              x$reach$percent, x$reach$people))
  cat(sprintf("Mean exposure probability: %.3f\n", x$mean_probability))

  # Contact distribution
  cat("\nCONTACT DISTRIBUTION:\n")
  cat("----------------------\n")
  cat("(Percentage of the population receiving exactly N contacts)\n")
  for (i in seq_along(x$distribution$percent)) {
    cat(sprintf("%d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribution$percent[i],
                x$distribution$people[i]))
  }

  # Cumulative distribution
  cat("\nCUMULATIVE DISTRIBUTION:\n")
  cat("-------------------------\n")
  cat("(Percentage of the population receiving N or more contacts)\n")
  for (i in seq_along(x$cumulative$percent)) {
    cat(sprintf(">= %d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$cumulative$percent[i],
                x$cumulative$people[i]))
  }

  # Summary statistics
  total_contacts <- sum(seq_along(x$distribution$percent) *
                           x$distribution$people)
  average_contacts <- total_contacts / sum(x$distribution$people)
  cat("\nSUMMARY STATISTICS:\n")
  cat("--------------------\n")
  cat(sprintf("Average contacts per person reached: %.2f\n",
              average_contacts))
}

#' @export
print.reach_beta_binomial <- function(x, ...) {
  cat("BETA-BINOMIAL MODEL\n")
  cat("===================\n")
  cat("Description: model that accounts for heterogeneity in the population\n\n")

  # Headline metrics
  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%% (%.0f people)\n",
              x$reach$percent, x$reach$people))

  # Model parameters
  cat("\nMODEL PARAMETERS:\n")
  cat("-------------------\n")
  cat(sprintf("Alpha: %.3f (shape of the Beta distribution)\n", x$parameters$alpha))
  cat(sprintf("Beta: %.3f (shape of the Beta distribution)\n", x$parameters$beta))
  cat(sprintf("Probability of 0 contacts: %.2f%%\n",
              x$parameters$zero_contact_probability))

  # Contact distribution
  cat("\nCONTACT DISTRIBUTION:\n")
  cat("----------------------\n")
  cat("(Percentage of the population receiving exactly N contacts)\n")
  for (i in seq_along(x$distribution$percent)) {
    cat(sprintf("%d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribution$percent[i],
                x$distribution$people[i]))
  }

  # Cumulative distribution
  cat("\nCUMULATIVE DISTRIBUTION:\n")
  cat("-------------------------\n")
  cat("(Percentage of the population receiving N or more contacts)\n")
  for (i in seq_along(x$cumulative$percent)) {
    cat(sprintf(">= %d contact%s: %.2f%% (%.0f people)\n",
                i, ifelse(i == 1, "", "s"),
                x$cumulative$percent[i],
                x$cumulative$people[i]))
  }

  # Summary statistics
  total_contacts <- sum(seq_along(x$distribution$percent) *
                           x$distribution$people)
  average_contacts <- total_contacts / sum(x$distribution$people)
  cat("\nSUMMARY STATISTICS:\n")
  cat("--------------------\n")
  cat(sprintf("Average contacts per person reached: %.2f\n",
              average_contacts))
  cat(sprintf("Theoretical mean of the Beta distribution: %.3f\n",
              x$parameters$alpha / (x$parameters$alpha + x$parameters$beta)))
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
#' @title Metrics under the Metheringham model
#' @description Calculates the core metrics for the Metheringham model, namely
#' mean audience (A1), mean duplication (D), and the audience after the second
#' exposure for the hypothetical average vehicle (A2). Metheringham's (1964)
#' model assumes individuals have heterogeneous probabilities that are Beta
#' distributed across the population. Vehicles are homogeneous (i.e. every
#' vehicle shares the same Beta distribution of exposure probabilities).
#' Cumulative audience and duplication are averaged across vehicles to design
#' one hypothetical average vehicle.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiences Numeric vector with the audience of each vehicle
#' @param insertions Numeric vector with the number of insertions per vehicle
#' @param duplication_matrix Symmetric matrix with the duplication values between vehicles
#'
#' @details
#' The function performs the following core calculations:
#' \enumerate{
#'   \item Mean audience after the first insertion (A1):
#'     \itemize{
#'       \item Computes the insertion-weighted mean of the audiences
#'       \item Formula: A1 = SUM(Audience_i x Insertions_i) / SUM(Insertions_i)
#'     }
#'   \item Mean duplication (D):
#'     \itemize{
#'       \item Computes the opportunity-weighted mean of the duplications
#'       \item Considers every possible combination between vehicles ii, ij
#'     }
#'   \item Audience after the second insertion (A2):
#'     \itemize{
#'       \item Computes the audience exposed at least once after the second insertion
#'       \item Formula: A2 = 2 x A1 - D
#'     }
#' }
#'
#' @return An object of class 'reach_metheringham' containing:
#' \itemize{
#'   \item mean_audience: Insertion-weighted mean audience (A1)
#'   \item mean_duplication: Opportunity-weighted mean duplication (D)
#'   \item second_audience: Audience after the second insertion (A2)
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
#' metrics <- calc_metheringham(
#'   audiences = c(1500000, 800000, 1200000),
#'   insertions = c(4, 3, 5),
#'   duplication_matrix = duplication_matrix
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} for estimates under the Sainsbury distribution
#' \code{\link{calc_binomial}} for estimates under the Binomial distribution
#' \code{\link{calc_beta_binomial}} for estimates under the Beta-Binomial distribution
#' \code{\link{calc_hofmans}} for estimates under the Hofmans distribution
# Main Metheringham function
calc_metheringham <- function(audiences, insertions, duplication_matrix) {
  if (length(audiences) != length(insertions)) {
    stop("audiences and insertions must have the same length")
  }
  if (any(insertions < 0) || any(audiences < 0)) {
    stop("audiences and insertions must be non-negative")
  }
  if (sum(insertions) <= 0) {
    stop("Total insertions must be greater than 0")
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

  result <- list(
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
  cat("Metheringham model\n")
  cat("-------------------\n")

  cat("\nMEAN AUDIENCE (A1):\n")
  cat(sprintf("%.0f people\n", x$mean_audience))
  cat("Interpretation: vehicle audience\n")

  cat("\nMEAN DUPLICATION (D):\n")
  cat(sprintf("%.0f people\n", x$mean_duplication))
  cat("Interpretation: average number of people who see any two insertions\n")

  cat("\nSECOND-INSERTION AUDIENCE (A2):\n")
  cat(sprintf("%.0f people\n", x$second_audience))
  cat("Interpretation: cumulative audience after two insertions (people exposed at least once)\n")

  cat("\nCONTACT OPPORTUNITY MATRIX:\n")
  print(x$opportunity_matrix)
  cat("Interpretation: number of possible insertion pairs between vehicles\n")
  cat("- Diagonal: contact opportunities within the same vehicle\n")
  cat("- Off-diagonal: contact opportunities between different vehicles\n")

  cat("\nOPPORTUNITY VECTOR:\n")
  print(x$opportunity_vector)
  cat("Interpretation: linearized version of the opportunity matrix\n")
  cat("Order: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3), ...\n")

  cat("\nKEY FINDINGS:\n")
  cat(sprintf("- Total insertions: %d\n", x$total_insertions))
  cat(sprintf("- Average audience per insertion: %.0f people\n", x$mean_audience))
  cat(sprintf("- Average duplication: %.1f%%\n",
              (x$mean_duplication / x$mean_audience) * 100))
  cat(sprintf("- Increase on the second insertion: %.1f%%\n",
              ((x$second_audience - x$mean_audience) / x$mean_audience) * 100))
}
