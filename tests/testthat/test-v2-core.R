test_that("sainsbury reach is a complete exact Poisson-binomial distribution", {
  # Three exposure opportunities with probabilities .2, .2 (two insertions of
  # the first vehicle) and .3. P(0) = .8 * .8 * .7, P(3) = .2 * .2 * .3.
  result <- calc_sainsbury(c(200, 300), 1000, c(2, 1))

  expect_equal(result$reach$percent, 100 * (1 - .8 * .8 * .7), tolerance = 1e-12)
  expect_equal(sum(result$distribution$percent), result$reach$percent,
               tolerance = 1e-12)
  expect_equal(result$distribution$percent[3L], 100 * .2 * .2 * .3,
               tolerance = 1e-12)
  expect_equal(result$cumulative$percent[1L], result$reach$percent,
               tolerance = 1e-12)
  expect_true(all(diff(result$cumulative$percent) <= 0))
  expect_equal(sum(1:3 * result$distribution$percent) / 100, 0.2 + 0.2 + 0.3,
               tolerance = 1e-12)
})

test_that("binomial replaces the audiences by their insertion-weighted mean", {
  # Mean probability (2 * .2 + .3) / 3 over three opportunities.
  p <- (2 * .2 + .3) / 3
  result <- calc_binomial(c(200, 300), 1000, c(2, 1))

  expect_equal(result$mean_probability, p, tolerance = 1e-12)
  expect_equal(result$reach$percent, 100 * (1 - (1 - p)^3), tolerance = 1e-12)
  expect_equal(result$distribution$percent,
               100 * stats::dbinom(1:3, 3, p), tolerance = 1e-12)
})

test_that("calc_sainsbury/calc_binomial handle all-zero insertions without NaNs", {
  zero_sainsbury <- calc_sainsbury(c(1000, 2000), 10000, insertions = c(0, 0))
  expect_equal(zero_sainsbury$reach$percent, 0)
  expect_length(zero_sainsbury$distribution$percent, 0)

  zero_binomial <- calc_binomial(c(1000, 2000), 10000, insertions = c(0, 0))
  expect_equal(zero_binomial$reach$percent, 0)
  expect_length(zero_binomial$distribution$percent, 0)
})

test_that("Sainsbury handles many vehicles without exponential enumeration", {
  result <- calc_sainsbury(rep(1000, 100), 100000)
  expect_true(result$reach$percent > 0)
  expect_length(result$distribution$percent, 100)
})
