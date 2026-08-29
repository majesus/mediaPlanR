#' @encoding UTF-8
#' @title Cumulative reach under the Agostini duplication model
#' @description Implements the Agostini (1961) model to estimate the cumulative
#' (net) reach of a media plan with several vehicles, each with a single
#' insertion -- the "duplication" domain in Aldas Manzano's (1998) three-way
#' split, shared with \code{\link{calc_hofmans_duplication}}. It corrects the
#' random-duplication (independence) assumption with an empirical coefficient k
#' that adjusts the predicted duplication between each newly added vehicle and
#' the reach accumulated so far.
#'
#' @references
#' Agostini, J. M. (1961). How to estimate unduplicated audiences.
#' Journal of Advertising Research, 1(3), 11-14.
#'
#' @param audiences Numeric vector with the individual audience of each vehicle,
#' in the order in which they are added to the plan
#' @param population Population size
#' @param k Numeric. Agostini's empirical duplication coefficient (default 0.9).
#' Values below 1 imply less duplication than expected under independence (more
#' complementary audiences); values above 1 imply more duplication (more
#' overlapping audiences). Ideally k should be calibrated from observed
#' duplication data for the media type being analysed; absent that, the
#' literature recommends indicative values between 0.85 and 1.15.
#'
#' @details
#' Starting from the cumulative reach after adding the first i-1 vehicles,
#' R(i-1), the model adds vehicle i via:
#' \deqn{R(i) = R(i-1) + Audience_i - k \times \frac{R(i-1) \times Audience_i}{Population}}
#' with R(1) = Audience_1. When k = 1 the model coincides exactly with the
#' random-duplication (independence) assumption applied iteratively, i.e. the
#' same starting hypothesis as the Sainsbury and Binomial models, but without
#' needing to know every audience simultaneously in advance. Unlike
#' \code{\link{calc_metheringham}} or \code{\link{calc_canex}}, the Agostini
#' model does not require an observed duplication matrix between every pair of
#' vehicles, only a single aggregate empirical coefficient, which makes it
#' especially practical when only a mean estimated duplication is available for
#' the media type.
#'
#' @return A list of class "reach_agostini_duplication" containing:
#' \itemize{
#'   \item reach: List with the plan's final cumulative reach:
#'     \itemize{
#'       \item percent: Final reach as a percentage
#'       \item people: Final reach in number of people
#'     }
#'   \item cumulative: List with the evolution of cumulative reach after each
#'   vehicle is added (percent and people)
#'   \item k: Duplication coefficient used
#'   \item n_vehicles: Number of vehicles added
#' }
#'
#' @examples
#' audiences <- c(300000, 400000, 200000)
#' result <- calc_agostini_duplication(audiences, population = 1000000, k = 0.9)
#' print(result)
#'
#' # k = 1 is equivalent to the random-duplication (independence) assumption
#' result_independence <- calc_agostini_duplication(audiences, population = 1000000, k = 1)
#' result_independence$reach$percent
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} for the random-duplication assumption with vehicle heterogeneity
#' \code{\link{calc_binomial}} for the random-duplication assumption with vehicle homogeneity
#' \code{\link{calc_metheringham}} for the adjustment via observed mean duplication
#' \code{\link{calc_hofmans_duplication}} for the same role with a pairwise, unfitted coefficient
calc_agostini_duplication <- function(audiences, population, k = 0.9) {
  if (!is.numeric(audiences) || !is.numeric(population) || !is.numeric(k)) {
    stop("audiences, population and k must be numeric")
  }
  if (length(audiences) < 1) {
    stop("audiences must contain at least one vehicle")
  }
  if (any(audiences <= 0) || any(audiences > population)) {
    stop("audiences must be positive and no greater than the population")
  }
  if (length(population) != 1 || population <= 0) {
    stop("population must be a single positive number")
  }
  if (length(k) != 1 || k < 0) {
    stop("k must be a single non-negative number")
  }

  n <- length(audiences)
  cumulative_people <- numeric(n)
  cumulative_people[1] <- audiences[1]

  if (n > 1) {
    for (i in 2:n) {
      cumulative_people[i] <- cumulative_people[i - 1] + audiences[i] -
        k * (cumulative_people[i - 1] * audiences[i] / population)
    }
  }
  # Cumulative reach can never exceed the total population, even with a
  # poorly calibrated k coefficient.
  cumulative_people <- pmin(pmax(cumulative_people, 0), population)

  final_reach <- cumulative_people[n]

  structure(list(
    reach = list(
      percent = final_reach / population * 100,
      people = final_reach
    ),
    cumulative = list(
      percent = cumulative_people / population * 100,
      people = cumulative_people
    ),
    k = k,
    n_vehicles = n
  ), class = "reach_agostini_duplication")
}

#' @export
print.reach_agostini_duplication <- function(x, ...) {
  cat("AGOSTINI MODEL\n")
  cat("==============\n")
  cat(sprintf("Description: random duplication corrected via the empirical coefficient k = %.3f\n\n", x$k))

  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%% (%.0f people)\n", x$reach$percent, x$reach$people))

  cat("\nCUMULATIVE REACH AFTER EACH VEHICLE IS ADDED:\n")
  cat("----------------------------------------------\n")
  for (i in seq_len(x$n_vehicles)) {
    cat(sprintf("After vehicle %d: %.2f%% (%.0f people)\n",
                i, x$cumulative$percent[i], x$cumulative$people[i]))
  }

  invisible(x)
}
