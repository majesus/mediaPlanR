test_that("NBD exposure distribution keeps an explicit open tail", {
  result <- nbd_exposure_distribution(
    mean_contacts = 2.5, size = 1.7, report_max = 9, opportunities = 9
  )

  expect_s3_class(result, "nbd_exposure")
  expect_equal(sum(result$distribution$probability), 1, tolerance = 1e-12)
  expect_equal(result$distribution$label[10], "9+")
  expect_true(result$distribution$open_tail[10])
  expect_equal(result$reach$probability,
               1 - dnbinom(0, size = 1.7, mu = 2.5), tolerance = 1e-12)
  expect_equal(result$average_frequency * result$reach$probability,
               2.5, tolerance = 1e-12)
  expect_gt(result$diagnostics$probability_above_opportunities, 0)
  expect_false(result$diagnostics$finite_opportunity_compatible)
})

test_that("NBD maximum-likelihood fit recovers overdispersion", {
  set.seed(20260817)
  counts <- rnbinom(5000, size = 2, mu = 3)
  fit <- fit_nbd_exposure(counts)

  expect_s3_class(fit, "nbd_exposure_fit")
  expect_false(fit$diagnostics$poisson_boundary)
  expect_true(fit$diagnostics$converged)
  expect_equal(fit$estimates$mean_contacts, mean(counts), tolerance = 1e-12)
  expect_equal(fit$estimates$size, 2, tolerance = 0.25)
  expect_true(is.finite(fit$diagnostics$log_likelihood))
})

test_that("underdispersed counts use the Poisson boundary", {
  fit <- fit_nbd_exposure(c(1, 1, 1, 2, 2, 2))
  expect_true(fit$diagnostics$poisson_boundary)
  expect_identical(fit$estimates$size, Inf)
})

