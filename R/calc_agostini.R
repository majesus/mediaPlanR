#' Reach under Agostini's duplication model
#'
#' Implements Agostini's (1961) ad hoc formula for the reach of a plan with one
#' insertion in each of several vehicles when the audience of each vehicle and
#' the audience duplicated between every pair of vehicles are known. It
#' belongs to the "duplication" domain of Aldás Manzano's (1998) three-way
#' split, together with [calc_hofmans_duplication()], and estimates reach
#' only, not the exposure distribution.
#'
#' @references
#' Agostini, J. M. (1961). How to estimate unduplicated audiences. Journal of
#' Advertising Research, 1(3), 11-14. \doi{10.1080/00218499.1961.12519620}
#'
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Section 3.2.1.1, equations 3.54-3.57.
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
#' @param k Agostini's empirical duplication coefficient. The default, 1.125,
#'   is the value Agostini fitted to a 1957 French print readership survey and
#'   found to work for French and US magazines. Other authors fitted other
#'   values to other data (Aldás Manzano, 1998, p. 156), so `k` should be
#'   calibrated to the media type analyzed whenever data allow it.
#'
#' @details
#' Let \eqn{A = \sum_{i=1}^{m} A_i} be the gross audience (with \eqn{m} the
#' number of vehicles), also called duplicated
#' audience: the simple sum of the vehicle audiences, in which a person who
#' belongs to the audiences of several vehicles is counted once for each of
#' them (Aldás Manzano, 1998, "audiencia bruta"; Kim, 2005, "gross audience").
#' It is not the reach, which counts each person once. Let
#' \eqn{D = \sum_{i=1}^{m-1} \sum_{j=i+1}^{m} A_{ij}} be the sum of the
#' duplicated audiences of every pair of vehicles, each pair counted once. Agostini observed that
#' the ratio of reach to gross audience is a function of the ratio of
#' duplication to gross audience, \eqn{R_m / A = 1 / (1 + k D / A)}, which
#' gives
#' \deqn{R_m = \frac{A^2}{A + k D}.}
#' The coefficient \eqn{k} is constant across vehicle pairs.
#' [calc_hofmans_duplication()] replaces it by a pairwise coefficient computed
#' from each pair's own audiences.
#'
#' The formula is empirical and can return a reach outside its logical range
#' (below the largest audience, or above the population or the gross
#' audience). This can happen because the duplications or `k` are not
#' consistent with the formula, but also because the formula is an
#' approximation: five independent vehicles that each reach 50% of the
#' population, with a duplication of 25% in every pair (a valid joint
#' distribution, with a true reach of 96.875%), give 117.6% with the default
#' `k`. The value is then returned unchanged, with a warning.
#'
#' @return A list of class `"reach_agostini_duplication"` with components:
#' \itemize{
#'   \item `reach`: list with `percent` and `people`.
#'   \item `k`: the duplication coefficient used.
#'   \item `gross_audience`: sum of the vehicle audiences (duplicated people
#'     counted once per vehicle), in people.
#'   \item `total_duplication`: sum of the pairwise duplicated audiences, in
#'     people.
#'   \item `n_vehicles`: number of vehicles.
#' }
#'
#' @examples
#' data(duplication_example)
#' result <- do.call(calc_agostini_duplication, duplication_example)
#' result
#'
#' # A different duplication coefficient
#' calc_agostini_duplication(
#'   duplication_example$audiences, duplication_example$population,
#'   duplication_example$duplication_matrix, k = 1
#' )$reach$percent
#'
#' @seealso
#' [calc_hofmans_duplication()] for the pairwise refinement of this formula,
#' and [calc_sainsbury()] and [calc_binomial()] for models that assume random
#' duplication instead of observing it.
#' @export
calc_agostini_duplication <- function(audiences, population, duplication_matrix,
                                      k = 1.125) {
  validate_duplication_inputs(audiences, population, duplication_matrix)
  assert_number(k, "k", min = 0)

  gross_audience <- sum(audiences)
  total_duplication <- sum(duplication_matrix[upper.tri(duplication_matrix)])
  if (!is.finite(gross_audience) || !is.finite(total_duplication)) {
    stop("Total audience and duplication must be finite; counts exceed the representable range.",
         call. = FALSE)
  }
  correction <- k * (total_duplication / gross_audience)
  if (!is.finite(correction)) {
    stop("The duplication correction must be finite; k exceeds the representable range.",
         call. = FALSE)
  }
  reach <- gross_audience / (1 + correction)
  check_reach_bounds(reach, audiences, population, "Agostini")

  structure(list(
    reach = list(percent = 100 * (reach / population), people = reach),
    k = k,
    gross_audience = gross_audience,
    total_duplication = total_duplication,
    n_vehicles = length(audiences)
  ), class = "reach_agostini_duplication")
}

#' @export
print.reach_agostini_duplication <- function(x, ...) {
  print_reach_report(
    "AGOSTINI MODEL (duplication)",
    "ad hoc reach formula with one duplication coefficient for all vehicle pairs",
    x$reach$percent, x$reach$people,
    parameters = list(
      "Duplication coefficient (k)" = x$k,
      "Gross audience (people)" = x$gross_audience,
      "Total pairwise duplication (people)" = x$total_duplication
    )
  )
  invisible(x)
}
