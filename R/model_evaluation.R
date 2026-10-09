evaluation_prediction_data <- function(predicted, predicted_scale) {
  if (inherits(predicted, "nbd_exposure_fit")) {
    predicted <- predicted$fitted_distribution
  }
  if (inherits(predicted, "nbd_exposure")) {
    if (any(predicted$distribution$open_tail)) {
      stop(paste0(
        "NBD predictions contain an open tail. Conventional Kim APE requires ",
        "an exact finite support; collapse the observed and predicted tails ",
        "identically and provide them as explicit data frames."
      ), call. = FALSE)
    }
  }

  if (is.data.frame(predicted)) {
    if (!all(c("contacts", "predicted") %in% names(predicted))) {
      stop("predicted data must contain columns contacts and predicted.",
           call. = FALSE)
    }
    if (identical(predicted_scale, "auto")) {
      stop("predicted_scale must be declared for a predicted data frame.",
           call. = FALSE)
    }
    return(list(
      data = predicted,
      value_column = "predicted",
      scale = predicted_scale,
      source = "data_frame"
    ))
  }

  distribution <- predicted$distribution
  if (!is.data.frame(distribution) || !"contacts" %in% names(distribution)) {
    stop(paste0(
      "predicted must be a supported model object or a data frame with ",
      "columns contacts and predicted."
    ), call. = FALSE)
  }
  if ("open_tail" %in% names(distribution) && any(distribution$open_tail)) {
    stop(paste0(
      "The predicted distribution contains an open tail. Conventional Kim ",
      "APE requires identical exact supports."
    ), call. = FALSE)
  }
  if ("probability" %in% names(distribution)) {
    value_column <- "probability"
    detected_scale <- "probability"
  } else if ("percent" %in% names(distribution)) {
    value_column <- "percent"
    detected_scale <- "percent"
  } else {
    stop("The model object's distribution has no probability or percent column.",
         call. = FALSE)
  }
  if (!identical(predicted_scale, "auto") &&
      !identical(predicted_scale, detected_scale)) {
    stop(sprintf(
      "predicted_scale='%s' conflicts with the model object's '%s' scale.",
      predicted_scale, detected_scale
    ), call. = FALSE)
  }
  list(
    data = data.frame(
      contacts = distribution$contacts,
      predicted = distribution[[value_column]]
    ),
    value_column = "predicted",
    scale = detected_scale,
    source = class(predicted)[1L]
  )
}

evaluation_validate_distribution <- function(data, value_column, scale,
                                             schedule_column, label,
                                             sum_tolerance) {
  required <- c("contacts", value_column)
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop(sprintf("%s must contain columns contacts and %s.",
                 label, value_column), call. = FALSE)
  }
  if (!is.null(schedule_column) && !schedule_column %in% names(data)) {
    stop(sprintf("%s has no schedule column named '%s'.",
                 label, schedule_column), call. = FALSE)
  }
  contacts <- data$contacts
  values <- data[[value_column]]
  if (!is.numeric(contacts) || anyNA(contacts) || any(!is.finite(contacts)) ||
      any(contacts < 0 | contacts != round(contacts))) {
    stop(sprintf("%s contacts must be finite non-negative integers.", label),
         call. = FALSE)
  }
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
      any(values < 0)) {
    stop(sprintf("%s values must be finite and non-negative.", label),
         call. = FALSE)
  }
  cell_limit <- switch(scale, probability = 1, percent = 100, Inf)
  if (any(values > cell_limit * (1 + 1e-12))) {
    stop(sprintf(paste0(
      "%s values on the '%s' scale cannot exceed %s in any cell. The sum ",
      "tolerance applies to the total of a distribution, never to an ",
      "individual cell."), label, scale, format(cell_limit)), call. = FALSE)
  }
  schedule <- if (is.null(schedule_column)) {
    rep("1", nrow(data))
  } else {
    as.character(data[[schedule_column]])
  }
  if (anyNA(schedule) || any(!nzchar(schedule))) {
    stop(sprintf("%s schedule identifiers cannot be missing or empty.", label),
         call. = FALSE)
  }
  keys <- paste(schedule, contacts, sep = "\r")
  if (anyDuplicated(keys)) {
    stop(sprintf("%s contains duplicate schedule/exposure rows.", label),
         call. = FALSE)
  }

  split_rows <- split(seq_len(nrow(data)), schedule)
  for (name in names(split_rows)) {
    rows <- split_rows[[name]]
    schedule_contacts <- as.integer(sort(contacts[rows]))
    if (!identical(schedule_contacts,
                   seq.int(0L, max(schedule_contacts)))) {
      stop(sprintf(
        "%s schedule '%s' must explicitly contain every exposure level from 0 to its maximum.",
        label, name
      ), call. = FALSE)
    }
  }

  probability <- switch(
    scale,
    count = values,
    probability = values,
    percent = values / 100
  )
  original_mass <- vapply(split_rows, function(rows) sum(probability[rows]),
                          numeric(1))
  if (scale == "count") {
    if (any(!is.finite(original_mass))) {
      bad <- names(original_mass)[!is.finite(original_mass)][1L]
      stop(sprintf(
        paste0("%s count weights for schedule '%s' are too large to be summed ",
               "(the total is not finite); rescale them, for example by ",
               "dividing by their maximum."), label, bad), call. = FALSE)
    }
    if (any(original_mass <= 0)) {
      stop(sprintf("Every %s count distribution must have positive total weight.",
                   label), call. = FALSE)
    }
    for (rows in split_rows) {
      probability[rows] <- probability[rows] / sum(probability[rows])
    }
  } else if (any(abs(original_mass - 1) > sum_tolerance)) {
    bad <- names(original_mass)[abs(original_mass - 1) > sum_tolerance][1L]
    stop(sprintf(
      paste0("%s probabilities for schedule '%s' sum to %.8f; declare the ",
             "correct scale or provide a complete distribution."),
      label, bad, original_mass[bad]
    ), call. = FALSE)
  }
  data.frame(
    schedule = schedule,
    contacts = as.integer(contacts),
    probability = probability,
    original_mass = unname(original_mass[schedule]),
    stringsAsFactors = FALSE
  )
}

#' Evaluate an observed exposure distribution against a model prediction
#'
#' Computes Kim's relative reach error and exposure-distribution error for one
#' or more schedules, together with model-agnostic diagnostics. Observed data
#' are always supplied by the analyst; the function never treats model inputs
#' or fitted values as observations.
#'
#' @param observed Data frame with columns `contacts` and `observed`. It must
#'   include an explicit zero-exposure row and every integer exposure level up to
#'   its maximum. For several schedules, also include the column named by
#'   `schedule_col`.
#' @param predicted Either a supported model result whose `distribution` is a
#'   data frame with `contacts` and `probability` or `percent` columns (for
#'   example the results of [calc_csd()], [calc_msad()], [calc_cbd()],
#'   [calc_mbd()], [calc_canex()] and also of
#'   [estimate_reach()]), or a data frame with columns `contacts` and
#'   `predicted`. Data-frame predictions must use the
#'   same schedule column when `schedule_col` is supplied.
#' @param observed_scale Required declaration of the `observed` column:
#'   `"count"`, `"probability"`, or `"percent"`. Counts may be weighted and
#'   are normalized within schedule.
#' @param predicted_scale Scale of a predicted data frame. The default `"auto"`
#'   is allowed only for supported model objects, whose scale is known.
#' @param schedule_col Optional name of a schedule identifier column present in
#'   both data frames. Model objects represent one schedule and therefore
#'   require `schedule_col = NULL`.
#' @param sum_tolerance Maximum absolute departure from probability mass one
#'   accepted for the total of each probability/percent distribution (in
#'   probability units, so 0.005 is half a percentage point). The default
#'   tolerates small deviations compatible with publication rounding and
#'   rejects total-mass discrepancies larger than the stated threshold. It
#'   cannot distinguish rounding from a small omitted tail: the analyst must
#'   establish that both tables cover the same complete support, or identically
#'   collapsed tails, before interpreting the comparison. The tolerance applies
#'   to the total only: no individual probability may exceed one (or percentage
#'   exceed 100), whatever the tolerance. Masses within the tolerance are used
#'   as supplied and are not renormalized (see Details); counts, in contrast,
#'   are always normalized.
#' @param censored_last_level `FALSE` (default) when the last exposure level of
#'   both distributions is an exact count. Set it to `TRUE` when it is a
#'   collapsed tail ("`k` or more exposures", as when a Negative-Binomial table
#'   has been truncated at `k` identically in observed and predicted data). The
#'   exact mean number of exposures is then not available: the observed and
#'   predicted means and `mean_contact_bias` are `NA`, and the means of the
#'   censored codes (the last level counted as exactly `k`) are reported in
#'   separate columns instead.
#'
#' @return An `exposure_model_evaluation` object, a list with:
#' \itemize{
#'   \item `summary`: `schedules`, `kim_aer`, `kim_ape` (proportions),
#'     `mean_total_variation`, `mean_cell_mae`, `mean_reach_absolute_error`
#'     (a difference between proportions: multiply by 100 for percentage
#'     points) and `mean_contact_bias` (exposures per person). With
#'     `censored_last_level = TRUE`, `mean_contact_bias` is `NA` and
#'     `mean_censored_contact_bias` is added.
#'   \item `by_schedule`: one row per schedule. The reaches are
#'     `observed_reach` and `predicted_reach`. The errors in reach are
#'     `reach_signed_error` (predicted minus observed), `reach_absolute_error`
#'     and `kim_relative_reach_error`. The errors in the distribution are
#'     `kim_distribution_error`, `total_variation` and `cell_mae`. The mean
#'     numbers of exposures are `observed_mean_contacts` and
#'     `predicted_mean_contacts`. When the last level is censored, the columns
#'     `observed_censored_mean` and `predicted_censored_mean` are added.
#'     All are proportions or exposures per person.
#'   \item `aligned_distribution`: the observed and predicted probabilities by
#'     schedule and exposure level, after normalizing counts and converting
#'     percentages to probabilities.
#'   \item `input_mass`: the total mass of each supplied distribution before any
#'     normalization, by schedule (`observed_input_mass`,
#'     `predicted_input_mass`).
#'   \item `inputs`: the scales, the source of the prediction,
#'     `sum_tolerance` and `censored_last_level`.
#' }
#'
#' @details
#' For schedule `i`, the function calculates
#' \deqn{e_{R,i}=|R_i^{obs}-R_i^{pred}|/R_i^{obs}}
#' and
#' \deqn{e_{P,i}=\sum_{j\geq 1}|p_{ij}^{obs}-p_{ij}^{pred}|/R_i^{obs}.}
#' where \eqn{R_i} is the reach (one minus the share with zero exposures) and
#' \eqn{p_{ij}} the share exposed exactly \eqn{j} times. `kim_aer` (Kim's
#' average percentage error in reach, AER) and `kim_ape` (Kim's average
#' percentage error in the exposure distribution, APE) are the means of
#' \eqn{e_{R,i}} and \eqn{e_{P,i}} across schedules, reported here as
#' proportions (multiply by 100 for percentages). They are descriptive
#' predictive-error measures, not inferential tests.
#'
#' The other elements of `summary` are, averaged over schedules:
#' \itemize{
#'   \item `mean_total_variation`: half the sum over all exposure levels
#'     (including zero) of the absolute difference between observed and
#'     predicted probabilities;
#'   \item `mean_cell_mae`: the mean absolute difference per exposure level;
#'   \item `mean_reach_absolute_error`: \eqn{|R_i^{obs}-R_i^{pred}|};
#'   \item `mean_contact_bias`: predicted minus observed mean number of
#'     exposures per person (positive when the model overestimates exposures).
#' }
#'
#' Exact support equality is required. Open-tail NBD output is rejected because
#' a cell such as `10+` is not equivalent to an exact ten-exposure cell. To
#' evaluate a censored distribution, the analyst must first collapse observed
#' and predicted tails identically, provide explicit data frames and set
#' `censored_last_level = TRUE`. The mean number of exposures of such a table
#' treats the last cell as exactly `k` exposures, so it understates the mean of
#' the process (a Negative-Binomial with mean 2, size 1 and a last level of
#' `3+` has a table mean of about 1.41); that is why the exact mean and the bias
#' are then reported as `NA`. The AER and APE of the collapsed cells remain
#' valid comparisons of the collapsed distributions, not of the exact ones.
#'
#' The observed reach must be positive: the relative errors divide by it, so a
#' schedule whose observed mass is entirely at zero exposures stops with an
#' error, even though the absolute distances could be computed. Because the
#' reach is one minus the zero cell, it is resolved only down to the
#' double-precision epsilon (about 2.2e-16): any observed reach above that is
#' accepted, including one person reached in a universe of a billion.
#'
#' Masses are used as supplied. A probability or percent distribution whose
#' total is within `sum_tolerance` of one is not renormalized, so its reach
#' is one minus its zero cell: a predicted table with cells 0.5 and 0.496
#' (total 0.996) has a predicted reach of 0.5, and the other metrics are those
#' of the rounded tables, not exactly of two normalized distributions;
#' `input_mass` reports the supplied totals. This keeps the replication of
#' published tables, which carry rounding, unaltered. Counts are always
#' normalized. A total within `sum_tolerance` of one does not show that a table
#' is complete: a mass of 0.996 can be rounding or a small omitted tail, and
#' having every level up to the declared maximum does not show that no
#' posterior tail was omitted. The total only rejects larger deficits.
#' Whether both tables cover the same complete support, or identically
#' collapsed tails, is a property of how the data were prepared and is the
#' responsibility of the analyst.
#'
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 117-118.
#' Handle 2152/1590 (University of Texas at Austin repository).
#'
#' @examples
#' # Observations must be supplied explicitly. These counts are invented for
#' # the example (they are not real data); there is one row per exposure level,
#' # from zero to the total number of insertions, and zero exposures included.
#' data(csd_example)
#' csd <- do.call(calc_csd, csd_example)
#' observed <- data.frame(
#'   contacts = 0:9,
#'   observed = c(520, 150, 130, 80, 50, 40, 20, 5, 3, 2)
#' )
#' predicted <- data.frame(
#'   contacts = 0:9,
#'   predicted = 100 * csd$distribution$probability
#' )
#' evaluation <- evaluate_exposure_model(
#'   observed, predicted,
#'   observed_scale = "count", predicted_scale = "percent"
#' )
#' evaluation$summary[c("kim_aer", "kim_ape")]
#'
#' # A fitted model object can be passed directly as the prediction.
#' evaluate_exposure_model(observed, csd, observed_scale = "count")
#'
#' @export
evaluate_exposure_model <- function(
    observed, predicted, observed_scale,
    predicted_scale = c("auto", "count", "probability", "percent"),
    schedule_col = NULL, sum_tolerance = 0.005,
    censored_last_level = FALSE) {
  observed_scale <- match.arg(observed_scale,
                              c("count", "probability", "percent"))
  predicted_scale <- match.arg(predicted_scale)
  assert_flag(censored_last_level, "censored_last_level")
  if (!is.null(schedule_col) &&
      (!is.character(schedule_col) || length(schedule_col) != 1L ||
       is.na(schedule_col) || !nzchar(schedule_col))) {
    stop("schedule_col must be NULL or one non-empty column name.",
         call. = FALSE)
  }
  if (!is.numeric(sum_tolerance) || length(sum_tolerance) != 1L ||
      !is.finite(sum_tolerance) || sum_tolerance < 0 || sum_tolerance >= 1) {
    stop("sum_tolerance must be one finite number in [0, 1).",
         call. = FALSE)
  }
  if (!is.data.frame(observed) ||
      !all(c("contacts", "observed") %in% names(observed))) {
    stop("observed must be a data frame with columns contacts and observed.",
         call. = FALSE)
  }

  prediction <- evaluation_prediction_data(predicted, predicted_scale)
  if (!is.null(schedule_col) && prediction$source != "data_frame") {
    stop("schedule_col can be used only when predicted is a data frame.",
         call. = FALSE)
  }
  observed_probability <- evaluation_validate_distribution(
    observed, "observed", observed_scale, schedule_col, "observed",
    sum_tolerance
  )
  predicted_probability <- evaluation_validate_distribution(
    prediction$data, prediction$value_column, prediction$scale,
    schedule_col, "predicted", sum_tolerance
  )

  observed_keys <- paste(observed_probability$schedule,
                         observed_probability$contacts, sep = "\r")
  predicted_keys <- paste(predicted_probability$schedule,
                          predicted_probability$contacts, sep = "\r")
  if (!setequal(observed_keys, predicted_keys)) {
    missing_prediction <- setdiff(observed_keys, predicted_keys)
    extra_prediction <- setdiff(predicted_keys, observed_keys)
    stop(sprintf(
      paste0("Observed and predicted supports differ (%d missing predicted ",
             "cells; %d extra predicted cells)."),
      length(missing_prediction), length(extra_prediction)
    ), call. = FALSE)
  }

  observed_probability$key <- observed_keys
  predicted_probability$key <- predicted_keys
  order_index <- match(observed_probability$key, predicted_probability$key)
  observed_mass <- unique(observed_probability[c("schedule", "original_mass")])
  predicted_mass <- unique(predicted_probability[c("schedule", "original_mass")])
  names(observed_mass)[2L] <- "observed_input_mass"
  names(predicted_mass)[2L] <- "predicted_input_mass"
  input_mass <- merge(observed_mass, predicted_mass, by = "schedule",
                      sort = TRUE)
  aligned <- data.frame(
    schedule = observed_probability$schedule,
    contacts = observed_probability$contacts,
    observed_probability = observed_probability$probability,
    predicted_probability = predicted_probability$probability[order_index]
  )
  aligned <- aligned[order(aligned$schedule, aligned$contacts), ]
  rownames(aligned) <- NULL

  rows_by_schedule <- split(seq_len(nrow(aligned)), aligned$schedule)
  by_schedule <- do.call(rbind, lapply(names(rows_by_schedule), function(id) {
    rows <- rows_by_schedule[[id]]
    distribution <- aligned[rows, ]
    zero_row <- distribution$contacts == 0L
    observed_reach <- 1 - distribution$observed_probability[zero_row]
    predicted_reach <- 1 - distribution$predicted_probability[zero_row]
    # The reach is one minus the zero cell (see Details), so it cannot be
    # resolved below the double-precision epsilon.
    if (observed_reach <= .Machine$double.eps) {
      stop(sprintf(
        paste0("Observed reach is zero for schedule '%s' (or below the ",
               "double-precision resolution of 1 minus the zero cell, ",
               "2.2e-16); relative AER/APE are undefined."),
        id
      ), call. = FALSE)
    }
    absolute_cell_error <- abs(
      distribution$observed_probability - distribution$predicted_probability
    )
    contacts <- distribution$contacts
    observed_mean <- sum(contacts * distribution$observed_probability)
    predicted_mean <- sum(contacts * distribution$predicted_probability)
    row <- data.frame(
      schedule = id,
      observed_reach = observed_reach,
      predicted_reach = predicted_reach,
      reach_signed_error = predicted_reach - observed_reach,
      reach_absolute_error = abs(predicted_reach - observed_reach),
      kim_relative_reach_error =
        abs(predicted_reach - observed_reach) / observed_reach,
      kim_distribution_error = sum(absolute_cell_error[!zero_row]) /
        observed_reach,
      total_variation = 0.5 * sum(absolute_cell_error),
      cell_mae = mean(absolute_cell_error),
      observed_mean_contacts = if (censored_last_level) NA_real_ else
        observed_mean,
      predicted_mean_contacts = if (censored_last_level) NA_real_ else
        predicted_mean
    )
    if (censored_last_level) {
      row$observed_censored_mean <- observed_mean
      row$predicted_censored_mean <- predicted_mean
    }
    row
  }))
  rownames(by_schedule) <- NULL

  summary_list <- list(
    schedules = nrow(by_schedule),
    kim_aer = mean(by_schedule$kim_relative_reach_error),
    kim_ape = mean(by_schedule$kim_distribution_error),
    mean_total_variation = mean(by_schedule$total_variation),
    mean_cell_mae = mean(by_schedule$cell_mae),
    mean_reach_absolute_error = mean(by_schedule$reach_absolute_error),
    mean_contact_bias = mean(
      by_schedule$predicted_mean_contacts -
        by_schedule$observed_mean_contacts
    )
  )
  if (censored_last_level) {
    summary_list$mean_censored_contact_bias <- mean(
      by_schedule$predicted_censored_mean - by_schedule$observed_censored_mean
    )
  }
  result <- list(
    summary = summary_list,
    by_schedule = by_schedule,
    aligned_distribution = aligned,
    input_mass = input_mass,
    inputs = list(
      observed_scale = observed_scale,
      predicted_scale = prediction$scale,
      prediction_source = prediction$source,
      sum_tolerance = sum_tolerance,
      censored_last_level = censored_last_level
    )
  )
  class(result) <- "exposure_model_evaluation"
  result
}

#' @export
print.exposure_model_evaluation <- function(x, ...) {
  cat("Exposure-model evaluation\n")
  cat(sprintf("Schedules: %d | Kim AER: %.3f%% | Kim APE: %.3f%%\n",
              x$summary$schedules, 100 * x$summary$kim_aer,
              100 * x$summary$kim_ape))
  if (isTRUE(x$inputs$censored_last_level)) {
    cat(sprintf(paste0("Mean total variation: %.5f | Mean exposure bias: NA ",
                       "(last level censored; bias of the censored means: %.5f)\n"),
                x$summary$mean_total_variation,
                x$summary$mean_censored_contact_bias))
  } else {
    cat(sprintf("Mean total variation: %.5f | Mean exposure bias: %.5f\n",
                x$summary$mean_total_variation,
                x$summary$mean_contact_bias))
  }
  invisible(x)
}
