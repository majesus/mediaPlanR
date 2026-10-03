test_that("MSAD reproduces the Morgensztern reach and preserves probability", {
  vehicles <- data.frame(
    insertions = c(2, 2),
    R1 = c(0.4902, 0.0330),
    R2 = c(0.5805, 0.0502)
  )
  duplications <- matrix(c(NA, 0.0157, 0.0157, NA), 2, 2)
  result <- calc_msad(vehicles, duplications, population = 1e6)

  expect_s3_class(result, "reach_msad")
  expect_equal(result$reach$probability, 0.602830914351716,
               tolerance = 1e-12)
  expect_equal(sum(result$distribution$probability), 1, tolerance = 1e-12)
  expect_gte(min(result$distribution$probability), 0)
  expect_equal(result$distribution$probability[1],
               1 - result$reach$probability, tolerance = 1e-12)
  expect_equal(result$diagnostics$distribution_mean_contacts,
               sum(vehicles$insertions * vehicles$R1), tolerance = 1e-12)
  expect_lt(max(result$steps$row_margin_error), 1e-12)
  expect_lt(max(result$steps$column_margin_error), 1e-12)
})

test_that("MSAD final reach is order invariant but its frequency shape is not", {
  vehicles <- data.frame(
    insertions = c(3, 2, 4), R1 = c(0.3, 0.2, 0.1),
    R2 = c(0.45, 0.32, 0.17)
  )
  duplications <- matrix(
    c(NA, 0.08, 0.04, 0.08, NA, 0.03, 0.04, 0.03, NA),
    3, byrow = TRUE
  )
  forward <- calc_msad(vehicles, duplications, "audience_desc")
  backward <- calc_msad(vehicles, duplications, c(3, 2, 1))

  expect_equal(forward$reach$probability, backward$reach$probability,
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(forward$distribution$probability,
                                backward$distribution$probability,
                                tolerance = 1e-12)))
  expect_equal(forward$diagnostics$distribution_mean_contacts,
               backward$diagnostics$distribution_mean_contacts,
               tolerance = 1e-12)
})

test_that("MSAD rejects probabilistically impossible inputs", {
  vehicles <- data.frame(
    insertions = c(2, 2), R1 = c(0.2, 0.3), R2 = c(0.32, 0.45)
  )
  expect_error(
    calc_msad(vehicles, matrix(c(NA, 0.25, 0.25, NA), 2)),
    "Frechet"
  )
  expect_error(
    calc_msad(vehicles, matrix(c(NA, 0.05, 0.04, NA), 2)),
    "symmetric"
  )
  expect_error(
    calc_msad(vehicles, matrix(c(NA, 0.05, 0.05, NA), 2), c(1.5, 2)),
    "integer row indices"
  )
})

test_that("MSAD reproduces the worked example of Lee (1988, pp. 81-93)", {
  # Three vehicles with two insertions each, aggregated in the order of Lee's
  # example. Lee's table is rounded to three decimals.
  vehicles <- data.frame(insertions = c(2, 2, 2),
                         R1 = c(0.121, 0.132, 0.088),
                         R2 = c(0.162, 0.182, 0.121))
  duplications <- matrix(c(NA, 0.046, 0.037,
                           0.046, NA, 0.033,
                           0.037, 0.033, NA), nrow = 3, byrow = TRUE)
  fit <- calc_msad(vehicles, duplications, aggregation_order = "given")
  published <- c(0.702, 0.085, 0.106, 0.062, 0.035, 0.009, 0.002)
  expect_length(fit$distribution$probability, length(published))
  expect_lt(max(abs(fit$distribution$probability - published)), 0.0015)
  expect_lt(abs(fit$reach$probability - (1 - 0.702)), 0.0015)
})
