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
  } else if ("percentage" %in% names(distribution)) {
    value_column <- "percentage"
    detected_scale <- "percent"
  } else if ("percent" %in% names(distribution)) {
    value_column <- "percent"
    detected_scale <- "percent"
  } else {
    stop("The model object's distribution has no probability or percentage column.",
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
    stop(sprintf("%s contains duplicate schedule/contact rows.", label),
         call. = FALSE)
  }

  split_rows <- split(seq_len(nrow(data)), schedule)
  for (name in names(split_rows)) {
    rows <- split_rows[[name]]
    schedule_contacts <- as.integer(sort(contacts[rows]))
    if (!identical(schedule_contacts,
                   seq.int(0L, max(schedule_contacts)))) {
      stop(sprintf(
        "%s schedule '%s' must explicitly contain every contact level from 0 to its maximum.",
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
#'   include an explicit zero-contact row and every integer contact level up to
#'   its maximum. For several schedules, also include the column named by
#'   `schedule_col`.
#' @param predicted Either a supported model result whose `distribution`
#'   contains `contacts` and `probability`/`percent` (the legacy `percentage`
#'   column name is also accepted), or a data frame with columns `contacts`
#'   and `predicted`. Data-frame predictions must use the
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
#'   accepted for probability/percent inputs. The default accepts ordinary
#'   publication rounding but rejects incomplete distributions.
#'
#' @return An `exposure_model_evaluation` object with `summary`,
#'   `by_schedule`, the aligned probability table, and input diagnostics.
#'
#' @details
#' For schedule `i`, the function calculates
#' \deqn{e_{R,i}=|R_i^{obs}-R_i^{pred}|/R_i^{obs}}
#' and
#' \deqn{e_{P,i}=\sum_{j\geq 1}|p_{ij}^{obs}-p_{ij}^{pred}|/R_i^{obs}.}
#' `kim_aer` and `kim_ape` are the means of these quantities across schedules.
#' They are descriptive predictive-error measures, not inferential tests.
#'
#' Exact support equality is required. Open-tail NBD output is rejected because
#' a cell such as `10+` is not equivalent to an exact ten-contact cell. To
#' evaluate a censored distribution, the analyst must first collapse observed
#' and predicted tails identically and provide explicit data frames.
#'
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 97-98.
#'
#' @examples
#' # Observations must be supplied explicitly. These are percentages from
#' # Kim's Table 4.2.2.10.
#' observed <- data.frame(
#'   contacts = 0:6,
#'   observed = c(38.41, 17.89, 39.66, 2.67, 1.36, 0, 0)
#' )
#' predicted <- data.frame(
#'   contacts = 0:6,
#'   predicted = c(38.20, 18.57, 39.18, 2.49, 1.51, 0.05, 0.01)
#' )
#' evaluation <- evaluate_exposure_model(
#'   observed, predicted,
#'   observed_scale = "percent", predicted_scale = "percent"
#' )
#' evaluation$summary[c("kim_aer", "kim_ape")]
#'
#' # A fitted model object can be passed directly as the prediction.
#' data(csd_kim2005)
#' csd <- do.call(calc_csd, csd_kim2005)
#' evaluate_exposure_model(observed, csd, observed_scale = "percent")
#'
#' @export
evaluate_exposure_model <- function(
    observed, predicted, observed_scale,
    predicted_scale = c("auto", "count", "probability", "percent"),
    schedule_col = NULL, sum_tolerance = 0.005) {
  observed_scale <- match.arg(observed_scale,
                              c("count", "probability", "percent"))
  predicted_scale <- match.arg(predicted_scale)
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
    if (observed_reach <= .Machine$double.eps^0.5) {
      stop(sprintf(
        "Observed reach is zero for schedule '%s'; relative AER/APE are undefined.",
        id
      ), call. = FALSE)
    }
    absolute_cell_error <- abs(
      distribution$observed_probability - distribution$predicted_probability
    )
    contacts <- distribution$contacts
    data.frame(
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
      observed_mean_contacts = sum(
        contacts * distribution$observed_probability
      ),
      predicted_mean_contacts = sum(
        contacts * distribution$predicted_probability
      )
    )
  }))
  rownames(by_schedule) <- NULL

  result <- list(
    summary = list(
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
    ),
    by_schedule = by_schedule,
    aligned_distribution = aligned,
    input_mass = input_mass,
    inputs = list(
      observed_scale = observed_scale,
      predicted_scale = prediction$scale,
      prediction_source = prediction$source,
      sum_tolerance = sum_tolerance
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
  cat(sprintf("Mean total variation: %.5f | Mean contact bias: %.5f\n",
              x$summary$mean_total_variation,
              x$summary$mean_contact_bias))
  invisible(x)
}
