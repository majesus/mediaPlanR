test_that("CANEX handles the binomial and polarized BBD limits exactly", {
  independent <- calc_canex(
    data.frame(k = 2, R1 = 0.2, R2 = 0.36), matrix(1, 1, 1), 1000
  )
  polarized <- calc_canex(
    data.frame(k = 2, R1 = 0.2, R2 = 0.2), matrix(1, 1, 1), 1000
  )
  expect_equal(independent$total_reach, 0.36, tolerance = 1e-12)
  expect_equal(polarized$total_reach, 0.2, tolerance = 1e-12)
  expect_named(independent$diagnostics,
               c("negative_mass_truncated", "mass_before_renormalization",
                 "correlation_min_eigenvalue"))
  expect_error(
    calc_canex(data.frame(k = 2, R1 = 0.2, R2 = 0.5), matrix(1, 1, 1)),
    "independence limit"
  )
})

test_that("CANEX rejects impossible pairwise duplication", {
  vehicles <- data.frame(k = c(2, 2), R1 = c(0.2, 0.1), R2 = c(0.3, 0.15))
  impossible <- matrix(c(1, 0.15, 0.15, 1), 2, 2)
  expect_error(calc_canex(vehicles, impossible), "Frechet bounds")
})

test_that("BBD-to-reach distribution and reported coverage use identical final parameters", {
  result <- fit_bbd_to_reach(c(5, 7, 4), c(500000, 550000, 600000),
                             RM = 550000, universe = 1000000, A0 = 0.1)
  distribution_reach <- (1 - result$contact_distribution[1]) * 1000000
  expect_true(result$parameters$converged)
  expect_equal(result$coverage$BBD, 550000, tolerance = 100)
  expect_equal(distribution_reach, result$coverage$BBD, tolerance = 1e-8)
})

test_that("Hofmans return value matches its documented contract", {
  result <- calc_hofmans(0.06, 0.103, 5, show_steps = FALSE)
  expect_s3_class(result, "reach_hofmans")
  expect_named(result$parameters, c("k", "d", "alpha"))
})

test_that("one-dimensional BBD calibration preserves R1 and reaches its target", {
  truth <- extraDistr::dbbinom(0:6, size = 6, alpha = 0.8, beta = 1.2)
  target <- sum(truth[4:7])
  fit <- calibrate_bbd(first_reach = 0.4, target_reach = target,
                       frequency = 3, max_insertions = 6,
                       type = "at_least", tolerance = 1e-5)
  expect_s3_class(fit, "bbd_calibration")
  expect_true(fit$converged)
  expect_equal(fit$alpha / (fit$alpha + fit$beta), 0.4, tolerance = 1e-12)
  expect_equal(fit$predicted_reach, target, tolerance = 1e-5)
})
