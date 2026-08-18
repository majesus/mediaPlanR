#' Create a validated cross-media plan
#'
#' `media_plan()` is the main data contract in mediaPlanR 2.0. It stores all
#' quantities in explicit units and keeps channel-level inputs separate from
#' derived metrics.
#'
#' @param data A data frame with one row per channel or vehicle.
#' @param population Positive size of the planning universe.
#' @param channel,audience,insertions,cost_per_insertion Column names in
#'   `data`. Audience is people per insertion and cost is currency per
#'   insertion.
#' @param target_audience Optional column containing people in the target
#'   audience per insertion. It must not exceed `audience`.
#' @param currency ISO-style currency label used only for reporting.
#'
#' @return An object of class `media_plan`.
#' @export
media_plan <- function(data, population,
                       channel = "channel",
                       audience = "audience",
                       insertions = "insertions",
                       cost_per_insertion = "cost_per_insertion",
                       target_audience = NULL,
                       currency = "EUR") {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("data must be a non-empty data frame", call. = FALSE)
  }
  if (!is.numeric(population) || length(population) != 1L ||
      !is.finite(population) || population <= 0) {
    stop("population must be one positive finite number", call. = FALSE)
  }
  cols <- c(channel, audience, insertions, cost_per_insertion)
  if (!all(cols %in% names(data))) {
    stop("Missing required columns: ",
         paste(setdiff(cols, names(data)), collapse = ", "), call. = FALSE)
  }
  if (!is.null(target_audience) && !target_audience %in% names(data)) {
    stop("target_audience column was not found", call. = FALSE)
  }

  out <- data.frame(
    channel = as.character(data[[channel]]),
    audience = as.numeric(data[[audience]]),
    insertions = as.numeric(data[[insertions]]),
    cost_per_insertion = as.numeric(data[[cost_per_insertion]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out) || any(!is.finite(as.matrix(out[-1L])))) {
    stop("Plan inputs cannot contain missing or non-finite values", call. = FALSE)
  }
  if (any(!nzchar(out$channel)) || anyDuplicated(out$channel)) {
    stop("channel names must be non-empty and unique", call. = FALSE)
  }
  if (any(out$audience < 0 | out$audience > population)) {
    stop("audience must be between zero and population", call. = FALSE)
  }
  if (any(out$insertions < 0 | out$insertions != round(out$insertions))) {
    stop("insertions must be non-negative integers", call. = FALSE)
  }
  if (any(out$cost_per_insertion < 0)) {
    stop("cost_per_insertion cannot be negative", call. = FALSE)
  }

  if (!is.null(target_audience)) {
    target <- as.numeric(data[[target_audience]])
    if (anyNA(target) || any(!is.finite(target)) || any(target < 0) ||
        any(target > out$audience)) {
      stop("target audience must be between zero and gross audience", call. = FALSE)
    }
    out$target_audience <- target
  }

  structure(
    list(data = out, population = population, currency = as.character(currency)[1L]),
    class = "media_plan"
  )
}

#' @export
print.media_plan <- function(x, ...) {
  cat("Cross-media plan\n")
  cat(sprintf("Universe: %.0f people | Channels: %d | Insertions: %.0f\n",
              x$population, nrow(x$data), sum(x$data$insertions)))
  print(x$data, row.names = FALSE)
  invisible(x)
}

#' Calculate unambiguous media-plan metrics
#'
#' @param plan A `media_plan` object.
#' @param reach Optional unique reach in people. When supplied, cost per
#'   thousand reached and average frequency are included.
#'
#' @return A `media_plan_metrics` object containing channel and plan metrics.
#' @export
plan_metrics <- function(plan, reach = NULL) {
  assert_media_plan(plan)
  d <- plan$data
  d$impressions <- d$audience * d$insertions
  d$spend <- d$cost_per_insertion * d$insertions
  d$rating_points <- d$impressions / plan$population * 100
  d$grp_share <- if (sum(d$rating_points) > 0) {
    d$rating_points / sum(d$rating_points)
  } else rep(NA_real_, nrow(d))
  d$cpm_impressions <- ifelse(d$impressions > 0, d$spend / d$impressions * 1000, NA_real_)
  d$cost_per_rating_point <- ifelse(d$rating_points > 0,
                                    d$spend / d$rating_points, NA_real_)
  if ("target_audience" %in% names(d)) {
    d$target_impressions <- d$target_audience * d$insertions
    d$target_composition <- ifelse(d$audience > 0,
                                   d$target_audience / d$audience, NA_real_)
  }

  total_impressions <- sum(d$impressions)
  total_spend <- sum(d$spend)
  totals <- list(
    impressions = total_impressions,
    spend = total_spend,
    grps = total_impressions / plan$population * 100,
    cpm_impressions = if (total_impressions > 0) {
      total_spend / total_impressions * 1000
    } else NA_real_
  )
  if (!is.null(reach)) {
    if (!is.numeric(reach) || length(reach) != 1L || !is.finite(reach) ||
        reach < 0 || reach > plan$population) {
      stop("reach must be between zero and population", call. = FALSE)
    }
    totals$reach <- reach
    totals$reach_percent <- reach / plan$population * 100
    totals$average_frequency <- if (reach > 0) total_impressions / reach else NA_real_
    totals$cost_per_thousand_reached <- if (reach > 0) total_spend / reach * 1000 else NA_real_
  }
  structure(list(by_channel = d, totals = totals, currency = plan$currency),
            class = "media_plan_metrics")
}

#' @export
print.media_plan_metrics <- function(x, ...) {
  cat("Media-plan metrics\n")
  print(x$by_channel, row.names = FALSE)
  cat("\nPlan totals\n")
  print(unlist(x$totals))
  invisible(x)
}

assert_media_plan <- function(x) {
  if (!inherits(x, "media_plan")) {
    stop("plan must be a media_plan object", call. = FALSE)
  }
  invisible(TRUE)
}

