# Original three-vehicle schedule with 3, 1 and 2 insertions (not taken from
# any publication).
mbd_v3_inputs <- function() {
  list(
    vehicles_data = data.frame(
      insertions = c(3, 1, 2),
      R1 = c(0.20, 0.08, 0.12),
      R2 = c(0.28, NA, 0.18)
    ),
    duplications = matrix(
      c(NA, 0.02, 0.03,
        0.02, NA, 0.015,
        0.03, 0.015, NA),
      nrow = 3, byrow = TRUE
    )
  )
}

test_that("MBD returns a valid distribution for a three-vehicle schedule", {
  inputs <- mbd_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)

  expect_s3_class(fit, "reach_mbd")
  expect_length(fit$distribution$probability,
                sum(inputs$vehicles_data$insertions) + 1L)
  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-9)
  expect_gte(min(fit$distribution$probability), 0)
  expect_equal(fit$reach$probability, 1 - fit$distribution$probability[1],
               tolerance = 1e-12)
  # The reach is at least that of the vehicle with the largest reach
  expect_gte(fit$reach$probability, max(inputs$vehicles_data$R1))
  expect_equal(fit$diagnostics$negative_mass_adjusted, 0)
})

test_that("MBD preserves probability mass and reports diagnostics", {
  inputs <- mbd_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications,
                  aggregation_order = 1:3, population = 1e6)

  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-9)
  expect_gte(min(fit$distribution$probability), 0)
  expect_equal(sum(fit$distribution$people), 1e6, tolerance = 1e-6)
  expect_true(all(c("negative_mass_adjusted", "cells_adjusted") %in% names(fit$diagnostics)))
})

test_that("MBD's exclusive grid is a distribution whose margins are the vehicles' reaches", {
  inputs <- mbd_v3_inputs()
  S <- mbd_coexposure_sums(inputs$vehicles_data$R1, inputs$duplications, 1e-8)
  grid <- mbd_exclusive_grid(S, 3, 1e-8)
  cells <- vapply(names(grid), function(key) as.numeric(grid[[key]]), numeric(1))
  expect_equal(sum(cells), 1, tolerance = 1e-9)
  # The margin of vehicle v is the sum of the cells whose key contains v
  for (v in 1:3) {
    in_v <- vapply(strsplit(names(grid), "_"), function(k) as.character(v) %in% k,
                   logical(1))
    expect_equal(sum(cells[in_v]), inputs$vehicles_data$R1[v], tolerance = 1e-9)
  }
})

test_that("MBD's first-order shrink never raises the estimate above its sub-tuples", {
  trips <- c(0.002, 0.0004, 0.0003, 0.0015)
  shrunk <- mbd_shrink_to_subtuples(0.01, trips)
  expect_gte(shrunk, 0)
  expect_lte(shrunk, 0.01)
})

test_that("MBD's safety net removes negative cells and keeps unit mass", {
  raw <- c(0.55, 0.22, 0.12, 0.06, -0.01, 0.04, 0.015, -0.005, 0.02, 0.01)
  safety <- mbd_safety_net(raw, 1e-9)
  expect_equal(safety$cells_adjusted, 2L)
  expect_gt(safety$negative_mass_adjusted, 0)
  expect_gte(min(safety$distribution), 0)
  expect_equal(sum(safety$distribution), 1, tolerance = 1e-9)
})

test_that("MBD validates inputs and the vehicle-count cap", {
  inputs <- mbd_v3_inputs()
  expect_error(
    calc_mbd(inputs$vehicles_data, inputs$duplications[1:2, 1:2]),
    "square matrix"
  )
  expect_error(
    calc_mbd(inputs$vehicles_data, inputs$duplications, aggregation_order = c(1, 1, 3)),
    "permutation"
  )
  bad <- inputs$duplications
  bad[1, 2] <- bad[2, 1] <- 0.9
  expect_error(
    calc_mbd(inputs$vehicles_data, bad),
    "Frechet bounds"
  )

  n <- 13
  big_vehicles <- data.frame(insertions = rep(2, n), R1 = rep(.08, n), R2 = rep(.12, n))
  big_dup <- matrix(.08 * 0.35, n, n)
  diag(big_dup) <- NA
  expect_error(calc_mbd(big_vehicles, big_dup), "at most 12 vehicles")
})

test_that("MBD warns for four or more vehicles", {
  R1 <- c(.15, .13, .10, .09)
  R2 <- c(.20, .18, .14, .12)
  n <- 4
  dup <- matrix(0, n, n)
  for (i in 1:(n - 1)) for (j in (i + 1):n) dup[i, j] <- dup[j, i] <- min(R1[i], R1[j]) * 0.35
  vehicles <- data.frame(insertions = rep(2, n), R1 = R1, R2 = R2)
  expect_warning(calc_mbd(vehicles, dup), "first-order consistency check")
})

test_that("MBD has a concise print method", {
  inputs <- mbd_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)
  expect_output(print(fit, full = FALSE), "Multivariate Beta Binomial")
  expect_output(print(fit, full = FALSE), "Probability sum")
})

test_that("MBD does not error or return NaN when a vehicle's own R1/R2 sit at the binomial or polarized limit (regression test)", {
  # Peeling a vehicle whose own Beta-Binomial parameters are at alpha=beta=Inf
  # (binomial limit) or alpha=beta=0 (polarized limit) must not pass those
  # non-finite or degenerate values to extraDistr::dbbinom(), which returns NaN
  # there and would break the conditional allocation. Both limits are computed
  # from their closed-form limiting distributions instead.
  dup <- matrix(c(NA, 0.05, 0.05, NA), nrow = 2, byrow = TRUE)

  R1 <- 0.3
  binomial_limit_vehicles <- data.frame(
    insertions = c(2, 2),
    R1 = c(R1, 0.20),
    R2 = c(2 * R1 - R1^2, 0.35)
  )
  fit_binomial <- calc_mbd(binomial_limit_vehicles, dup, aggregation_order = 1:2)
  expect_false(anyNA(fit_binomial$distribution$probability))
  expect_equal(sum(fit_binomial$distribution$probability), 1, tolerance = 1e-9)

  polarized_vehicles <- data.frame(
    insertions = c(2, 2),
    R1 = c(0.30, 0.20),
    R2 = c(0.30, 0.35)
  )
  fit_polarized <- calc_mbd(polarized_vehicles, dup, aggregation_order = 1:2)
  expect_false(anyNA(fit_polarized$distribution$probability))
  expect_equal(sum(fit_polarized$distribution$probability), 1, tolerance = 1e-9)
})
