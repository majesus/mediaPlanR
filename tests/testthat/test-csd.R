csd_inputs <- function() {
  data(csd_example, package = "mediaPlanR", envir = environment())
  csd_example
}

test_that("CSD returns its steps, the canonical reach and a valid distribution", {
  inputs <- csd_inputs()
  fit <- calc_csd(
    inputs$vehicles_data, inputs$duplications,
    aggregation_order = 1:3
  )

  expect_s3_class(fit, "reach_csd")
  # One conformed reach per aggregation step after the first vehicle, and the
  # final reach is the last step's reach
  expect_length(fit$steps$target_reach, 2)
  expect_equal(fit$reach$probability, fit$steps$target_reach[2],
               tolerance = 1e-12)
  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-12)
  expect_length(fit$distribution$probability,
                sum(inputs$vehicles_data$insertions) + 1L)
  # The reach is at least the reach of the largest vehicle (one insertion)
  # and a probability
  expect_gte(fit$reach$probability, max(inputs$vehicles_data$R1))
  expect_lte(fit$reach$probability, 1)
})

test_that("CSD preserves probability, exposure mean, and non-negative cells", {
  inputs <- csd_inputs()
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
  inputs <- csd_inputs()
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
  inputs <- csd_inputs()
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
  inputs <- csd_inputs()
  fit <- calc_csd(inputs$vehicles_data, inputs$duplications,
                  aggregation_order = 1:3)
  expect_output(print(fit, full = FALSE), "Canonical Sequential")
  expect_output(print(fit, full = FALSE), "Probability sum")
})

test_that("a logical NA R2 is accepted only for vehicles with a single insertion", {
  duplications <- matrix(c(NA, 0.25, 0.25, NA), 2)
  single <- data.frame(insertions = c(1, 1), R1 = c(0.5, 0.5), R2 = c(NA, NA))
  expect_true(is.logical(single$R2))
  expect_s3_class(calc_cbd(single, duplications), "reach_cbd")
  expect_s3_class(calc_csd(single, duplications), "reach_csd")

  needed <- data.frame(insertions = c(2, 1), R1 = c(0.5, 0.5), R2 = c(NA, NA))
  expect_error(calc_csd(needed, duplications), "at least two insertions")

  wrong_type <- data.frame(insertions = c(1, 1), R1 = c(0.5, 0.5), R2 = c("a", "b"))
  expect_error(calc_csd(wrong_type, duplications), "R2 must be numeric")
})
