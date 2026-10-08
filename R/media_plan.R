#' Create a validated cross-media plan
#'
#' `media_plan()` is the main data contract of mediaPlanR. It stores every
#' quantity in explicit units and keeps channel-level inputs separate from
#' derived metrics.
#'
#' @param data A data frame with one row per channel or vehicle.
#' @param population Positive size of the planning universe, in people.
#' @param channel,audience,insertions,cost_per_insertion Names of the columns
#'   of `data` that hold the channel label, the audience in people per
#'   insertion, the number of planned insertions (a non-negative integer) and
#'   the cost in currency units per insertion. Channel labels must be
#'   non-empty and unique.
#' @param target_audience Optional name of a column of `data` with the people
#'   in the target audience per insertion. It cannot exceed `audience`. It is
#'   descriptive: [plan_metrics()] uses it to report target impressions and
#'   composition, but [estimate_reach()] and [optimize_media_plan()] work with
#'   `audience` and `population` only, so they estimate the reach of the whole
#'   population, not of the target. To estimate the reach of the target, build a
#'   separate plan whose `population` is the size of the target universe and
#'   whose `audience` column holds the people of the target reached by each
#'   insertion.
#' @param currency Currency label used in printed output. It does not affect
#'   any calculation.
#'
#' @return An object of class `media_plan`: a list with the validated channel
#'   table `data` (columns `channel`, `audience`, `insertions`,
#'   `cost_per_insertion` and, if supplied, `target_audience`), the
#'   `population` and the `currency`.
#'
#' @examples
#' plan <- media_plan(
#'   data.frame(
#'     channel = c("TV", "Radio", "Digital"),
#'     audience = c(300000, 180000, 120000),
#'     insertions = c(4, 6, 10),
#'     cost_per_insertion = c(18000, 3500, 1200),
#'     target_audience = c(180000, 90000, 84000)
#'   ),
#'   population = 1000000,
#'   target_audience = "target_audience"
#' )
#' plan
#'
#' @seealso [plan_metrics()], [estimate_reach()] and [optimize_media_plan()]
#'   take a `media_plan` object.
#' @export
media_plan <- function(data, population,
                       channel = "channel",
                       audience = "audience",
                       insertions = "insertions",
                       cost_per_insertion = "cost_per_insertion",
                       target_audience = NULL,
                       currency = "EUR") {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("data must be a non-empty data frame.", call. = FALSE)
  }
  assert_number(population, "population", min = 0, min_open = TRUE)
  column_names <- list(channel = channel, audience = audience,
                       insertions = insertions,
                       cost_per_insertion = cost_per_insertion)
  if (!is.null(target_audience)) column_names$target_audience <- target_audience
  for (name in names(column_names)) {
    value <- column_names[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop(name, " must be the name of one column of data.", call. = FALSE)
    }
  }
  missing_columns <- setdiff(unlist(column_names), names(data))
  if (length(missing_columns)) {
    stop("Missing required columns: ",
         paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.character(currency) || length(currency) < 1L || is.na(currency[1L])) {
    stop("currency must be a character label.", call. = FALSE)
  }

  numeric_columns <- c(audience = audience, insertions = insertions,
                       cost_per_insertion = cost_per_insertion)
  if (!is.null(target_audience)) {
    numeric_columns <- c(numeric_columns, target_audience = target_audience)
  }
  for (name in names(numeric_columns)) {
    if (!is.numeric(data[[numeric_columns[[name]]]])) {
      stop(name, " must be a numeric column (logical, factor, character and ",
           "list columns are not accepted).", call. = FALSE)
    }
  }
  if (!is.character(data[[channel]]) && !is.factor(data[[channel]])) {
    stop("channel must be a character or factor column.", call. = FALSE)
  }

  out <- data.frame(
    channel = as.character(data[[channel]]),
    audience = as.numeric(data[[audience]]),
    insertions = as.numeric(data[[insertions]]),
    cost_per_insertion = as.numeric(data[[cost_per_insertion]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out) || any(!is.finite(as.matrix(out[-1L])))) {
    stop("Plan inputs cannot contain missing or non-finite values.",
         call. = FALSE)
  }
  if (any(!nzchar(out$channel)) || anyDuplicated(out$channel)) {
    stop("Channel names must be non-empty and unique.", call. = FALSE)
  }
  if (any(out$audience < 0 | out$audience > population)) {
    stop("audience must be between zero and population.", call. = FALSE)
  }
  if (any(out$insertions < 0 | out$insertions != round(out$insertions))) {
    stop("insertions must be non-negative integers.", call. = FALSE)
  }
  if (any(out$cost_per_insertion < 0)) {
    stop("cost_per_insertion cannot be negative.", call. = FALSE)
  }

  if (!is.null(target_audience)) {
    target <- as.numeric(data[[target_audience]])
    if (anyNA(target) || any(!is.finite(target)) || any(target < 0) ||
        any(target > out$audience)) {
      stop("The target audience must be between zero and the gross audience.",
           call. = FALSE)
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
  cat(sprintf("Universe: %.0f people | Channels: %d | Insertions: %.0f | Currency: %s\n",
              x$population, nrow(x$data), sum(x$data$insertions), x$currency))
  print(x$data, row.names = FALSE)
  invisible(x)
}

#' Media-plan metrics
#'
#' Computes impressions, spend, rating points and cost ratios for every
#' channel and for the whole plan, with an explicit denominator for each
#' quantity.
#'
#' @param plan A `media_plan` object.
#' @param reach Optional unique reach of the plan, in people (for example
#'   the `people` element of the `reach` returned by [estimate_reach()]).
#'   When supplied, average frequency
#'   and cost per thousand people reached are added to the plan totals. It must
#'   be compatible with the plan: at least the largest audience among the
#'   channels with insertions and at most the smaller of the population and the
#'   plan's impressions; otherwise the function stops with an error. These
#'   bounds are exact: the only slack is the rounding error of double
#'   arithmetic, so a value that is one person outside them is rejected,
#'   whatever the size of the universe. It must refer to the same universe,
#'   period and allocation as the plan. The reach of the `"binomial"` model of
#'   [estimate_reach()] belongs to a homogenized plan and can fall below the
#'   largest audience; it is then rejected here (see [calc_binomial()]).
#'
#' @details
#' For each channel, `impressions` is `audience * insertions`, `spend` is
#' `cost_per_insertion * insertions`, `rating_points` (gross rating points) is
#' `impressions / population * 100`, `grp_share` is the channel's share of the
#' plan's rating points, `cpm_impressions` is the cost per thousand
#' impressions and `cost_per_rating_point` is `spend / rating_points`. When the
#' plan has a target audience, `target_impressions` and `target_composition`
#' (`target_audience / audience`) are added.
#'
#' With a `reach`, the average frequency is `impressions / reach`, the average
#' number of contacts among the people reached (it lies between one and the
#' total number of insertions). It is `NA`, not zero, when the reach is zero,
#' because a mean over nobody is undefined; [estimate_reach()] follows the same
#' convention. The `target_audience` column only gives the
#' target's impressions and composition: the reach, rating points and costs
#' refer to the whole audience, not to the target (see [audience_metrics()]).
#'
#' @return A `media_plan_metrics` object: a list with `by_channel` (a data frame
#'   of channel-level metrics), `totals` (a list with `impressions`, `spend`,
#'   `grps`, `cpm_impressions` and, when `reach` is supplied, `reach`,
#'   `reach_percent`, `average_frequency` and `cost_per_thousand_reached`) and
#'   the `currency`.
#'
#' @examples
#' plan <- media_plan(
#'   data.frame(channel = c("TV", "Radio"), audience = c(300000, 180000),
#'              insertions = c(4, 6), cost_per_insertion = c(18000, 3500)),
#'   population = 1000000
#' )
#' plan_metrics(plan)
#'
#' # Add reach-based metrics
#' plan_metrics(plan, reach = estimate_reach(plan)$reach$people)$totals
#'
#' @seealso [media_plan()], [estimate_reach()]
#' @export
plan_metrics <- function(plan, reach = NULL) {
  assert_media_plan(plan)
  d <- plan$data
  d$impressions <- d$audience * d$insertions
  d$spend <- d$cost_per_insertion * d$insertions
  if (any(!is.finite(d$impressions)) || any(!is.finite(d$spend)) ||
      !is.finite(sum(d$impressions)) || !is.finite(sum(d$spend))) {
    stop("Total impressions and spend must be finite; the plan exceeds the representable range.",
         call. = FALSE)
  }
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
    assert_number(reach, "reach", min = 0, max = plan$population)
    assert_reach_compatible_with_plan(reach, d, plan$population,
                                      total_impressions)
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
  cat(sprintf("Media-plan metrics (currency: %s)\n", x$currency))
  print(x$by_channel, row.names = FALSE)
  cat("\nPlan totals\n")
  print(unlist(x$totals))
  invisible(x)
}

# Logical bounds of the unique reach of a plan under the finite-opportunity
# contract (a person receives at most one contact per insertion): the plan
# reaches at least as many people as its largest single audience among the
# channels with insertions, and at most as many as it has contacts (or the
# population). With no contacts at all, the reach can only be zero.
plan_reach_bounds <- function(data, population, total_impressions) {
  active <- data$audience[data$insertions > 0]
  c(lower = if (length(active)) max(active) else 0,
    upper = min(population, total_impressions))
}

# The bounds are structural: the only slack is the rounding error of double
# arithmetic (see exact_constraint_slack()), never a fraction of the universe,
# so that one person more or fewer than the bound is always reported.
assert_reach_compatible_with_plan <- function(reach, data, population,
                                              total_impressions) {
  tolerance <- exact_constraint_slack(population)
  bounds <- plan_reach_bounds(data, population, total_impressions)
  lower <- bounds[["lower"]]
  upper <- bounds[["upper"]]
  show <- function(x) format(signif(x, 7), scientific = FALSE, trim = TRUE)
  if (reach < lower - tolerance) {
    stop("reach (", show(reach), " people) is smaller than the largest ",
         "audience among the channels with insertions (", show(lower),
         " people): a single insertion of that channel already reaches that ",
         "many people.", call. = FALSE)
  }
  if (reach > upper + tolerance) {
    stop("reach (", show(reach), " people) is larger than the ",
         if (upper < population) "number of impressions of the plan" else
           "population", " (", show(upper), "): the plan cannot reach more ",
         "people than it has contacts, and a person is counted once.",
         call. = FALSE)
  }
  invisible(TRUE)
}

assert_media_plan <- function(x) {
  if (!inherits(x, "media_plan")) {
    stop("plan must be a media_plan object.", call. = FALSE)
  }
  invisible(TRUE)
}
