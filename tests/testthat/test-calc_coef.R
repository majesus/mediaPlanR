test_that("calc_R1_R2 recovers R1 exactly and returns coefficients in (0,1)", {
  result <- calc_R1_R2(0.5, 0.3)

  expect_type(result, "list")
  expect_equal(result$R1, 0.5 / (0.5 + 0.3))
  expect_true(result$R1 >= 0 && result$R1 <= 1)
  expect_true(result$R2 >= 0 && result$R2 <= 1)
  expect_gte(result$R2, result$R1)
})

test_that("calc_R1_R2 is the inverse of calculate_bbd_params's alpha/beta recovery", {
  # Round-tripping alpha/beta -> R1/R2 -> alpha/beta should return to the
  # same shape parameters (calculate_bbd_params is internal, reached via :::).
  original <- list(alpha = 0.8, beta = 1.4)
  coefficients <- calc_R1_R2(original$alpha, original$beta)
  recovered <- mediaPlanR:::calculate_bbd_params(coefficients$R1, coefficients$R2)

  # calc_R1_R2() solves for R2 via stats::optimize(), so the round trip is
  # only accurate to its optimizer tolerance, not to machine precision.
  expect_equal(recovered$alpha, original$alpha, tolerance = 1e-3)
  expect_equal(recovered$beta, original$beta, tolerance = 1e-3)
})

test_that("calc_R1_R2 validates its inputs", {
  expect_error(calc_R1_R2(-1, 2), "positive")
  expect_error(calc_R1_R2(1, 0), "positive")
  expect_error(calc_R1_R2("a", 2), "positive")
})
