kim_csd_inputs <- function() {
  list(
    vehicles_data = data.frame(
      insertions = c(2, 2, 2),
      R1 = c(0.4902, 0.0333, 0.0300),
      R2 = c(0.5805, 0.0502, 0.0371)
    ),
    duplications = matrix(
      c(NA, 0.0157, 0.0139,
        0.0157, NA, 0.0003,
        0.0139, 0.0003, NA),
      nrow = 3, byrow = TRUE
    )
  )
}

test_that("CSD reproduces Kim's complete three-vehicle example", {
  inputs <- kim_csd_inputs()
  fit <- calc_csd(
    inputs$vehicles_data, inputs$duplications,
    aggregation_order = 1:3
  )

  expect_s3_class(fit, "reach_csd")
  expect_equal(fit$steps$target_reach, c(0.6022075, 0.6180583),
               tolerance = 1e-7)
  expect_equal(
    fit$distribution$percent,
    c(38.20, 18.57, 39.18, 2.49, 1.51, 0.05, 0.01),
    tolerance = 0.03
  )
  expect_equal(fit$reach$probability, 0.6180583, tolerance = 1e-7)
})

test_that("CSD preserves probability, exposure mean, and non-negative cells", {
  inputs <- kim_csd_inputs()
  fit <- calc_csd(inputs$vehicles_data, inputs$duplications,
                  aggregation_order = 1:3, population = 1e6)

  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-12)
  expect_gte(min(fit$distribution$probability), 0)
  expect_equal(fit$diagnostics$mean_error, 0, tolerance = 1e-12)
  expect_lt(max(fit$steps$row_margin_error), 1e-12)
  expect_lt(max(fit$steps$column_margin_error), 1e-12)
  expect_equal(sum(fit$distribution$people), 1e6, tolerance = 1e-6)
})

test_that("CSD exposes sequential order sensitivity without changing canonical reach", {
  inputs <- kim_csd_inputs()
  forward <- calc_csd(inputs$vehicles_data, inputs$duplications,
                      aggregation_order = 1:3)
  reverse <- calc_csd(inputs$vehicles_data, inputs$duplications,
                      aggregation_order = 3:1)

  expect_equal(forward$reach$probability, reverse$reach$probability,
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    forward$distribution$probability,
    reverse$distribution$probability,
    tolerance = 1e-12
  )))
})

test_that("CSD validates duplication and aggregation inputs", {
  inputs <- kim_csd_inputs()
  asymmetric <- inputs$duplications
  asymmetric[1, 2] <- 0.02

  expect_error(
    calc_csd(inputs$vehicles_data, asymmetric),
    "symmetric"
  )
  expect_error(
    calc_csd(inputs$vehicles_data, inputs$duplications,
             aggregation_order = c(1, 1, 3)),
    "permutation"
  )
  expect_error(
    calc_csd(inputs$vehicles_data, inputs$duplications, population = 0),
    "population must be one finite positive number"
  )
})

test_that("CSD has a concise print method", {
  inputs <- kim_csd_inputs()
  fit <- calc_csd(inputs$vehicles_data, inputs$duplications,
                  aggregation_order = 1:3)
  expect_output(print(fit), "Canonical Sequential")
  expect_output(print(fit), "Probability sum")
})
