#' Target-audience quality metrics
#'
#' Separates target composition, target rating and affinity. This prevents the
#' common error of multiplying a gross audience by an affinity index and
#' obtaining a target audience larger than the gross audience.
#'
#' @param gross_audience Gross audience of each channel, in people.
#' @param target_audience Audience belonging to the target, in people.
#' @param gross_universe Total planning universe, in people.
#' @param target_universe Target population within the universe, in people.
#'
#' @details
#' All inputs must be finite and logically compatible: the target audience
#' cannot exceed the gross audience or the target universe, the gross audience
#' cannot exceed the gross universe, and the target universe cannot exceed the
#' gross universe. Scalars are recycled to a common length.
#'
#' The affinity index compares the target's share of a channel's audience with
#' the target's share of the universe,
#' \deqn{\mathrm{affinity} = 100 \times \frac{T / G}{T_u / G_u},}
#' where \eqn{T} and \eqn{G} are the target and gross audiences and \eqn{T_u}
#' and \eqn{G_u} the target and gross universes. Algebraically this equals the
#' target rating divided by the gross rating, \eqn{100 \times (T / T_u) /
#' (G / G_u)}, so the two readings of affinity always coincide and only one
#' column is returned. An index above 100 means the channel over-delivers the
#' target relative to the universe.
#'
#' @return A data frame with one row per channel and columns
#' \itemize{
#'   \item `gross_audience` and `target_audience`, as supplied;
#'   \item `target_composition`: share of the gross audience that belongs to
#'     the target, `target_audience / gross_audience` (`NA` when the gross
#'     audience is zero);
#'   \item `target_rating`: share of the target universe reached,
#'     `target_audience / target_universe`;
#'   \item `gross_rating`: share of the gross universe reached,
#'     `gross_audience / gross_universe`;
#'   \item `affinity_index`: affinity, as defined above (`NA` when the gross
#'     audience is zero).
#' }
#'
#' @examples
#' audience_metrics(
#'   gross_audience = c(300000, 180000),
#'   target_audience = c(180000, 90000),
#'   gross_universe = 1000000,
#'   target_universe = 400000
#' )
#' @export
audience_metrics <- function(gross_audience, target_audience,
                             gross_universe, target_universe) {
  args <- list(gross_audience = gross_audience,
               target_audience = target_audience,
               gross_universe = gross_universe,
               target_universe = target_universe)
  for (name in names(args)) {
    assert_numeric_vector(args[[name]], name, min = 0)
  }
  n <- max(lengths(args))
  if (!all(lengths(args) %in% c(1L, n))) {
    stop("Inputs must have length one or a common length.", call. = FALSE)
  }
  gross_universe <- rep(gross_universe, length.out = n)
  target_universe <- rep(target_universe, length.out = n)
  gross_audience <- rep(gross_audience, length.out = n)
  target_audience <- rep(target_audience, length.out = n)
  if (any(gross_universe <= 0) || any(target_universe <= 0) ||
      any(target_universe > gross_universe) ||
      any(gross_audience > gross_universe) ||
      any(target_audience > gross_audience) ||
      any(target_audience > target_universe)) {
    stop("Audience and universe values are not logically compatible.",
         call. = FALSE)
  }
  composition <- ifelse(gross_audience > 0, target_audience / gross_audience,
                        NA_real_)
  data.frame(
    gross_audience = gross_audience,
    target_audience = target_audience,
    target_composition = composition,
    target_rating = target_audience / target_universe,
    gross_rating = gross_audience / gross_universe,
    affinity_index = composition / (target_universe / gross_universe) * 100
  )
}
