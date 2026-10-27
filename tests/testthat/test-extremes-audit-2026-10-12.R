# Regressions for the definitive audit of 2026-10-12 (findings E02 and E03; E01
# concerned the allocation optimizer, which is not part of this package). Each
# case has a witness that does not use the code under test.

collect_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = messages)
}

test_that("the Negative-Binomial reach survives a quotient that underflows (E02)", {
  # mean / size is about 1e-325, below the smallest double. The reach of a
  # Gamma-Poisson mixture with that mean is 1 - (1 + mean / size)^(-size),
  # which is mean to the precision of a double.
  z <- nbd_exposure_distribution(mean_contacts = 1e-17, size = 1e308,
                                 report_max = 10)
  expect_equal(z$reach$probability, 1e-17, tolerance = 1e-12)
  expect_equal(z$average_frequency, 1, tolerance = 1e-12)
  expect_equal(sum(z$distribution$probability[-1]), 1e-17, tolerance = 1e-9)

  # A denormal quotient (about 1e-317) is the same limit.
  z <- nbd_exposure_distribution(mean_contacts = 1e-17, size = 1e300,
                                 report_max = 10)
  expect_equal(z$reach$probability, 1e-17, tolerance = 1e-12)

  # The series and the logarithm agree at the switch (ratio of 1e-8).
  direct <- function(mean, size) -expm1(-size * log1p(mean / size))
  for (size in c(1e8 * (1 - 1e-7), 1e8, 1e8 * (1 + 1e-7))) {
    z <- nbd_exposure_distribution(mean_contacts = 1, size = size,
                                   report_max = 10)
    expect_equal(z$reach$probability, direct(1, size), tolerance = 1e-14)
  }
  # Ordinary sizes are untouched, against the complement of the zero cell.
  for (size in c(0.25, 1, 40)) {
    z <- nbd_exposure_distribution(mean_contacts = 2.5, size = size,
                                   report_max = 10)
    expect_equal(z$reach$probability, 1 - stats::dnbinom(0, size = size, mu = 2.5),
                 tolerance = 1e-13)
  }
})

test_that("opportunities beyond the integer range is kept as a number (E03)", {
  out <- collect_warnings(
    nbd_exposure_distribution(mean_contacts = 1, size = 1, report_max = 10,
                              opportunities = 3e9)
  )
  expect_length(out$warnings, 0L)
  d <- out$value$diagnostics
  expect_equal(d$opportunities, 3e9)
  expect_equal(d$probability_above_opportunities,
               stats::pnbinom(3e9, size = 1, mu = 1, lower.tail = FALSE))
  expect_true(d$finite_opportunity_compatible)

  # Within the integer range the value is still an integer, as before.
  z <- nbd_exposure_distribution(mean_contacts = 1, size = 1, report_max = 10,
                                 opportunities = 9)
  expect_type(z$diagnostics$opportunities, "integer")
})
