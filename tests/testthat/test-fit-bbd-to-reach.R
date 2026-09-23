test_that("the BBD-to-reach fit has an honest primary class", {
  fit <- fit_bbd_to_reach(c(5, 7, 4), c(500000, 550000, 600000),
                          reach = 850000, universe = 1000000)
  expect_s3_class(fit, "bbd_reach_fit")
  expect_false(inherits(fit, "list"))
})

test_that("the fit reproduces the external reach and preserves the mean probability", {
  fit <- fit_bbd_to_reach(c(5, 7, 4), c(500000, 550000, 600000),
                          reach = 850000, universe = 1000000)
  p <- fit$parameters
  expect_true(p$converged)
  expect_identical(p$fit_type, "beta_binomial")
  expect_equal(fit$reach$fitted, 850000, tolerance = 100 / 850000)
  # The distribution and the reported reach use identical final parameters
  expect_equal((1 - fit$distribution$probability[1]) * 1e6, fit$reach$fitted, tolerance = 1e-8)
  expect_equal(p$alpha / (p$alpha + p$beta), p$mean_probability, tolerance = 1e-12)
  expect_equal(p$mean_probability, (5 * 0.5 + 7 * 0.55 + 4 * 0.6) / 16)
  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-12)
  # Mean exposures per person equal the plan's gross exposures per person
  expect_equal(sum(fit$distribution$contacts * fit$distribution$probability),
               sum(c(5, 7, 4) * c(0.5, 0.55, 0.6)), tolerance = 1e-10)
  expect_equal(fit$distribution$cumulative_probability[1], 1, tolerance = 1e-12)
})

test_that("the BBD-to-reach fit handles both theoretical boundary models", {
  insertions <- 2
  audiences <- 200000
  universe <- 1000000

  polarized <- fit_bbd_to_reach(insertions, audiences, reach = 200000, universe = universe)
  binomial <- fit_bbd_to_reach(insertions, audiences, reach = 360000, universe = universe)

  expect_identical(polarized$parameters$fit_type, "polarized_limit")
  expect_equal(polarized$distribution$probability, c(0.8, 0, 0.2))
  expect_identical(binomial$parameters$fit_type, "binomial_limit")
  expect_equal(binomial$distribution$probability, stats::dbinom(0:2, 2, 0.2))
  expect_error(
    fit_bbd_to_reach(insertions, audiences, reach = 150000, universe = universe),
    "feasible Beta-Binomial interval"
  )
})

test_that("the BBD-to-reach fit validates its arguments and prints", {
  expect_error(fit_bbd_to_reach(c(2, NA), c(1e5, 2e5), 3e5, 1e6), "insertions")
  expect_error(fit_bbd_to_reach(c(2, 2), c(1e5), 3e5, 1e6), "audiences")
  expect_error(fit_bbd_to_reach(c(2, 2), c(1e5, 2e6), 3e5, 1e6), "cannot exceed")
  expect_error(fit_bbd_to_reach(c(2, 2), c(1e5, 2e5), NA, 1e6), "reach")
  expect_error(fit_bbd_to_reach(c(2, 2), c(1e5, 2e5), 3e5, 1e6, precision = 0), "precision")
  # The initial value of alpha of the original iterative procedure is not an argument
  expect_error(fit_bbd_to_reach(c(2, 2), c(1e5, 2e5), 3e5, 1e6, A0 = 0.1), "unused argument")
  fit <- do.call(fit_bbd_to_reach, bbd_reach_example)
  expect_output(print(fit), "Beta-Binomial fit to an external reach")
})
