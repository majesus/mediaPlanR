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
