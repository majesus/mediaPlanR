#' Calculate target-audience quality metrics
#'
#' Separates target composition from affinity. This prevents the common error
#' of multiplying gross audience by an affinity index and obtaining a target
#' audience larger than the gross audience.
#'
#' @param gross_audience Gross audience of each channel in people.
#' @param target_audience Audience belonging to the target in people.
#' @param gross_universe Total planning universe in people.
#' @param target_universe Target population in the universe, in people.
#'
#' @return A data frame containing target composition, target rating,
#' selectivity and affinity index.
#' @export
audience_metrics <- function(gross_audience, target_audience,
                             gross_universe, target_universe) {
  args <- list(gross_audience, target_audience, gross_universe, target_universe)
  if (!all(vapply(args, is.numeric, logical(1))) ||
      anyNA(unlist(args)) || any(!is.finite(unlist(args)))) {
    stop("All audience inputs must be finite numeric values", call. = FALSE)
  }
  n <- max(length(gross_audience), length(target_audience))
  if (!length(gross_audience) %in% c(1L, n) ||
      !length(target_audience) %in% c(1L, n) ||
      !length(gross_universe) %in% c(1L, n) ||
      !length(target_universe) %in% c(1L, n)) {
    stop("Inputs must have length one or a common length", call. = FALSE)
  }
  gross_universe <- rep(gross_universe, length.out = n)
  target_universe <- rep(target_universe, length.out = n)
  gross_audience <- rep(gross_audience, length.out = n)
  target_audience <- rep(target_audience, length.out = n)
  if (any(gross_universe <= 0) || any(target_universe <= 0) ||
      any(target_universe > gross_universe) || any(gross_audience < 0) ||
      any(gross_audience > gross_universe) || any(target_audience < 0) ||
      any(target_audience > gross_audience) || any(target_audience > target_universe)) {
    stop("Audience and universe values are not logically compatible", call. = FALSE)
  }
  composition <- ifelse(gross_audience > 0, target_audience / gross_audience, NA_real_)
  population_composition <- target_universe / gross_universe
  target_rating <- target_audience / target_universe
  gross_rating <- gross_audience / gross_universe
  data.frame(
    gross_audience = gross_audience,
    target_audience = target_audience,
    target_composition = composition,
    target_rating = target_rating,
    gross_rating = gross_rating,
    affinity_index = composition / population_composition * 100,
    selectivity_index = target_rating / gross_rating * 100
  )
}

