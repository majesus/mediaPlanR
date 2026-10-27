# Regressions from the follow-up audit of the corrected 2.0.0 manual
# (2026-10-08, second review). Each case was reproduced against the version
# that the review examined. The expected values are logical bounds or
# identities, not snapshots of the previous implementation.

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
