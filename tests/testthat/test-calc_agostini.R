test_that("calc_agostini with k=1 exactly matches calc_sainsbury's independence assumption", {
  audiences <- c(300000, 400000, 200000)
  population <- 1000000

  res_agostini <- calc_agostini(audiences, population, k = 1)
  res_sainsbury <- calc_sainsbury(audiences, population)

  expect_equal(res_agostini$reach$percent, res_sainsbury$reach$percent, tolerance = 1e-9)
})

test_that("calc_agostini returns a reach_agostini object with a working print method", {
  res <- calc_agostini(c(300000, 400000, 200000), population = 1000000, k = 0.9)
  expect_s3_class(res, "reach_agostini")
  expect_output(print(res), "AGOSTINI MODEL")
  expect_length(res$cumulative$people, 3)
  expect_true(res$reach$people <= 1000000)
})

test_that("calc_agostini validates its inputs", {
  expect_error(calc_agostini(c(-1, 2), 100), "positive")
  expect_error(calc_agostini(c(1, 2), 0), "positive")
  expect_error(calc_agostini(c(1, 2), 100, k = -1), "non-negative")
})
