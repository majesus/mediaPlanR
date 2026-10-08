# Regressions from the follow-up audit of the corrected 2.0.0 manual
# (2026-10-08, second review). Each case was reproduced against the version
# that the review examined. The expected values are logical bounds or
# identities, not snapshots of the previous implementation.

single_channel_plan <- function(insertions, population) {
  media_plan(
    data.frame(channel = "A", audience = 1, insertions = insertions,
               cost_per_insertion = 1),
    population = population
  )
}

test_that("H01: the bounds of the reach are exact in small and large universes", {
  for (population in c(100, 1e6, 1e9)) {
    active <- single_channel_plan(1, population)
    empty <- single_channel_plan(0, population)
    # One contact on one person: the reach cannot be zero
    expect_error(plan_metrics(active, reach = 0), "smaller than the largest audience")
    # No contact at all: the reach cannot be one person
    expect_error(plan_metrics(empty, reach = 1), "larger than")
    # The controls are valid
    expect_equal(plan_metrics(active, reach = 1)$totals$average_frequency, 1)
    expect_equal(plan_metrics(empty, reach = 0)$totals$reach, 0)
  }
  # The original cases of the first audit are still rejected
  plan <- media_plan(
    data.frame(channel = c("A", "B"), audience = c(50, 20),
               insertions = c(1, 1), cost_per_insertion = c(10, 5)),
    population = 100
  )
  expect_error(plan_metrics(plan, reach = 90), "larger than the number of impressions")
  expect_error(plan_metrics(plan, reach = 10), "smaller than the largest audience")
})

test_that("H03: the audience outside the target must fit, to the last person", {
  for (universe in c(100, 1e6, 1e9)) {
    # One person outside the target in a universe that has none outside it
    expect_error(audience_metrics(1, 0, universe, universe),
                 "audience outside the target")
    # A whole universe that is the target: every audience belongs to it
    ok <- audience_metrics(1, 1, universe, universe)
    expect_equal(ok$target_composition, 1)
    expect_equal(ok$affinity_index, 100)
  }
  # Limit case: the audience outside the target fills the universe outside it
  expect_no_error(audience_metrics(90, 10, 100, 20))
  expect_error(audience_metrics(91, 10, 100, 20), "audience outside the target")
})

test_that("H01: one person beyond a bound of a duplication is also rejected", {
  # The duplication of two vehicles cannot exceed the smaller audience
  audiences <- c(5e8, 5e8)
  population <- 1e9
  too_large <- matrix(c(NA, 5e8 + 1, 5e8 + 1, NA), 2)
  expect_error(calc_agostini_duplication(audiences, population, too_large),
               "bounds")
  valid <- matrix(c(NA, 5e8, 5e8, NA), 2)
  expect_no_error(calc_agostini_duplication(audiences, population, valid))
})

test_that("H02: precision measures the fit and never widens the bounds of the schedule", {
  for (population in c(1, 100, 1e6)) {
    audiences <- c(0.5, 0.1) * population
    # A vehicle alone reaches 50%: a reach of 30% is impossible, whatever
    # the precision
    for (precision in c(1e-6, 1e-3, 1) * population) {
      expect_error(
        fit_bbd_to_reach(c(1, 1), audiences, 0.3 * population, population,
                         precision = precision),
        "smaller than the largest audience of a vehicle"
      )
    }
    # 50% is possible for the schedule and for the family: the fit reaches it
    # instead of selecting the polarized limit (30%)
    for (precision in c(1e-6, 1e-3, 1) * population) {
      fit <- fit_bbd_to_reach(c(1, 1), audiences, 0.5 * population, population,
                              precision = precision)
      expect_gte(fit$reach$fitted, 0.5 * population * (1 - 1e-12))
      expect_lt(abs(fit$reach$fitted - 0.5 * population), 1e-6 * population)
      expect_identical(fit$parameters$fit_type, "beta_binomial")
      # The family interval and the interval of the schedule are reported apart
      expect_equal(unname(fit$parameters$feasible_reach), c(0.3, 0.51) * population)
      expect_equal(unname(fit$parameters$schedule_bounds), c(0.5, 0.6) * population)
    }
  }
})

test_that("H02: no fit is returned outside the bounds of the schedule", {
  # Audiences of 90 and 10: the family has a mean of 0.5 and reaches at most
  # 75 people, below the 90 people of the larger vehicle. A large precision
  # must not turn the binomial limit (75) into the fit of a reach of 90
  expect_error(
    fit_bbd_to_reach(c(1, 1), c(90, 10), 90, 100, precision = 100),
    "compatible with the schedule"
  )
  # The fitted reach of any admissible target lies inside the schedule bounds
  set.seed(20261008)
  for (i in 1:200) {
    population <- 10^sample(c(0, 2, 6, 9), 1)
    insertions <- sample(1:5, 3, replace = TRUE)
    audiences <- stats::runif(3, 0.01, 0.9) * population
    lower <- max(audiences)
    upper <- min(population, sum(insertions * audiences))
    target <- stats::runif(1, lower, upper)
    fit <- tryCatch(
      fit_bbd_to_reach(insertions, audiences, target, population),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      slack <- 64 * .Machine$double.eps * population
      expect_gte(fit$reach$fitted, lower - slack)
      expect_lte(fit$reach$fitted, upper + slack)
    }
  }
})

test_that("N01: the Binomial optimization states when its reach is not the plan's reach", {
  plan <- media_plan(
    data.frame(channel = c("A", "B"), audience = c(90, 10),
               insertions = c(1, 1), cost_per_insertion = c(1, 1)),
    population = 100
  )
  expect_warning(
    binomial <- optimize_media_plan(
      plan, budget = 2, effective_frequency = 2, max_insertions = c(1, 1),
      model = "binomial", method = "exact"
    ),
    class = "mediaPlanR_homogenized_reach_incompatible"
  )
  expect_equal(unname(binomial$allocation), c(1, 1))
  # Valid for the homogenized model: p = 0.5 and two trials
  expect_equal(binomial$effective_reach, 0.25)
  expect_equal(binomial$reach$reach$people, 75)
  expect_false(binomial$reach_compatible_with_plan)
  # The metrics that need a reach of the plan itself are not built on it
  expect_null(binomial$metrics$totals$reach)
  expect_null(binomial$metrics$totals$average_frequency)
  expect_equal(binomial$metrics$totals$spend, 2)
  expect_output(print(binomial), "below the largest audience")
  # Sainsbury keeps both audiences: reach 91% and two exposures 9%
  sainsbury <- optimize_media_plan(
    plan, budget = 2, effective_frequency = 2, max_insertions = c(1, 1),
    model = "sainsbury", method = "exact"
  )
  expect_true(sainsbury$reach_compatible_with_plan)
  expect_equal(sainsbury$reach$reach$people, 91)
  expect_equal(sainsbury$effective_reach, 0.09)
  expect_equal(sainsbury$metrics$totals$average_frequency, 100 / 91)
  # Equal audiences: both models coincide and the reach is compatible
  equal <- plan
  equal$data$audience <- c(50, 50)
  expect_no_warning(
    same <- optimize_media_plan(equal, budget = 2, effective_frequency = 2,
                                max_insertions = c(1, 1), model = "binomial",
                                method = "exact")
  )
  expect_true(same$reach_compatible_with_plan)
  expect_equal(same$metrics$totals$average_frequency, 100 / 75)
  # Called directly, the physical check of plan_metrics() is unchanged
  expect_error(
    plan_metrics(plan, reach = estimate_reach(plan, "binomial")$reach$people),
    "smaller than the largest audience"
  )
})
