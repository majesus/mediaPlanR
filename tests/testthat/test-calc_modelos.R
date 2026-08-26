test_that("calc_sainsbury, calc_binomial and calc_beta_binomial return coherent reach values (0-100%)", {
  audiences <- c(300000, 400000, 200000)
  population <- 1000000

  res_s <- calc_sainsbury(audiences, population)
  res_b <- calc_binomial(audiences, population)

  expect_s3_class(res_s, "reach_sainsbury")
  expect_s3_class(res_b, "reach_binomial")
  expect_true(res_s$reach$percent > 0 && res_s$reach$percent <= 100)
  expect_true(res_b$reach$percent > 0 && res_b$reach$percent <= 100)
  expect_equal(sum(res_s$distribution$percent), res_s$reach$percent, tolerance = 1e-6)

  res_bb <- calc_beta_binomial(A1 = 500000, A2 = 550000, P = 1000000, n = 5)
  expect_s3_class(res_bb, "reach_beta_binomial")
  expect_true(res_bb$reach$percent > 0 && res_bb$reach$percent <= 100)
})

test_that("calc_metheringham computes coherent A1, D and A2 and validates its inputs", {
  duplication_matrix <- matrix(c(
    150000, 200000, 180000,
    200000, 120000, 140000,
    180000, 140000, 170000
  ), nrow = 3, byrow = TRUE)

  res <- calc_metheringham(
    audiences = c(1500000, 800000, 1200000),
    insertions = c(4, 3, 5),
    duplication_matrix = duplication_matrix
  )

  expect_s3_class(res, "reach_metheringham")
  expect_true(res$second_audience > res$mean_audience)

  expect_error(
    calc_metheringham(audiences = c(0, 0), insertions = c(0, 0), duplication_matrix = matrix(0, 2, 2)),
    "insertions"
  )
})

test_that("calc_hofmans produces monotonically increasing reach and returns a ggplot object", {
  res <- calc_hofmans(0.06, 0.103, N = 5, show_steps = FALSE)
  expect_true(all(diff(res$results$RN) >= 0))
  expect_s3_class(res$plot, "ggplot")

  expect_error(calc_hofmans(0.1, 0.2, N = 5, show_steps = FALSE), "division by zero")
})
