test_that("CANEX handles the binomial and polarized BBD limits exactly", {
  independent <- calc_canex(
    data.frame(k = 2, R1 = 0.2, R2 = 0.36), matrix(1, 1, 1), 1000
  )
  polarized <- calc_canex(
    data.frame(k = 2, R1 = 0.2, R2 = 0.2), matrix(1, 1, 1), 1000
  )
  expect_equal(independent$reach$probability, 0.36, tolerance = 1e-12)
  expect_equal(polarized$reach$probability, 0.2, tolerance = 1e-12)
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

test_that("Hofmans accumulation return value matches its documented contract", {
  result <- calc_hofmans_accumulation(0.06, 0.103, 5)
  expect_s3_class(result, "reach_hofmans_accumulation")
  expect_named(result, c("results", "parameters", "plot"))
  expect_named(result$parameters, c("k", "d", "alpha", "R3"))
  expect_named(result$results, c("N", "RN"))
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

test_that("BBD calibration validates its arguments", {
  expect_error(calibrate_bbd(NA, 0.5, 2, 6), "first_reach")
  expect_error(calibrate_bbd(0.3, 1, 2, 6), "target_reach")
  expect_error(calibrate_bbd(0.3, 0.5, NA, 6), "frequency")
  expect_error(calibrate_bbd(0.3, 0.5, 3, NA), "max_insertions")
  expect_error(calibrate_bbd(0.3, 0.5, 3, 2), "max_insertions")
  expect_output(print(calibrate_bbd(0.3, 0.2, 2, 4)), "Beta-Binomial calibration")
})
