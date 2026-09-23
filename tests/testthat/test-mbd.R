cheong_v3_inputs <- function() {
  list(
    vehicles_data = data.frame(
      insertions = c(2, 1, 3),
      R1 = c(0.146, 0.110, 0.252),
      R2 = c(0.191, NA, 0.318)
    ),
    duplications = matrix(
      c(NA, 0.032, 0.063,
        0.032, NA, 0.041,
        0.063, 0.041, NA),
      nrow = 3, byrow = TRUE
    )
  )
}

test_that("MBD reproduces Cheong's complete three-vehicle example", {
  inputs <- cheong_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)

  expect_s3_class(fit, "reach_mbd")
  # Cheong (2007), Ch. 4.2, final collapsed distribution (p.75): the only
  # example in the dissertation that is fully traceable end to end without
  # engaging the negative-probability safety net. Cheong's own intermediate
  # values are rounded to three decimals, so a small tolerance is expected
  # (the same practice used for Kim's CSD example elsewhere in this package).
  expect_equal(
    fit$distribution$probability,
    c(0.515, 0.164, 0.112, 0.121, 0.046, 0.034, 0.008),
    tolerance = 0.01
  )
  expect_equal(fit$reach$probability, 1 - 0.515, tolerance = 0.01)
  expect_equal(fit$diagnostics$negative_mass_adjusted, 0)
})

test_that("MBD preserves probability mass and reports diagnostics", {
  inputs <- cheong_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications,
                  aggregation_order = 1:3, population = 1e6)

  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-9)
  expect_gte(min(fit$distribution$probability), 0)
  expect_equal(sum(fit$distribution$people), 1e6, tolerance = 1e-6)
  expect_true(all(c("negative_mass_adjusted", "cells_adjusted") %in% names(fit$diagnostics)))
})

test_that("MBD's exclusive grid matches Cheong's published 2x2x2 table", {
  inputs <- cheong_v3_inputs()
  S <- mbd_coexposure_sums(inputs$vehicles_data$R1, inputs$duplications, 1e-8)
  grid <- mbd_exclusive_grid(S, 3, 1e-8)
  # Table on p.64-65 of Cheong (2007): (A,B,C) exclusive cells.
  published <- c("0" = 0.612, "3" = 0.164, "2" = 0.053, "2_3" = 0.025,
                 "1" = 0.068, "1_3" = 0.047, "1_2" = 0.016, "1_2_3" = 0.016)
  for (key in names(published)) {
    diff <- abs(as.numeric(grid[[key]]) - published[[key]])
    expect_lt(diff, 0.0015, label = sprintf("cell %s (diff=%.5f)", key, diff))
  }
})

test_that("MBD's first-order shrink reproduces Cheong's #84 quad check exactly", {
  # Cheong (2007), Ch. 4.3, p.77: the four triple ("trip") values and the
  # sequence of shrinks applied to the raw quad estimate for schedule #84.
  trips <- c(0.00115, 0.00015, 0.00006, 0.00107)
  shrunk <- mbd_shrink_to_subtuples(0.01623, trips)
  expect_equal(shrunk, 0.0000594, tolerance = 1e-8)
})

test_that("MBD's safety net matches Cheong's MBD-ADJ direction on schedule #151", {
  # Cheong (2007), Ch. 4.4, Table 4.4.1: the raw (unadjusted) univariate
  # distribution for schedule #151, including its published negative cells.
  raw_ud <- c(.2817, .0077, .1314, .0569, .0136, .0034, .0025, -.0001,
              .2410, .0612, .1583, .0097, .0096, .00049, .0047, .0021,
              -.0070, .0030, .0142, .0017, .00069, -.00013, -.00013, .000128,
              .0074, -.00068, .0024, .0017, -.000027, .00013, .00034, .000001)
  safety <- mbd_safety_net(raw_ud, 1e-9)

  expect_equal(safety$cells_adjusted, 6L)
  expect_gt(safety$negative_mass_adjusted, 0)
  expect_gte(min(safety$distribution), 0)
  expect_equal(sum(safety$distribution), 1, tolerance = 1e-9)
  # Cheong's own MBD-ADJ table (4.4.3): close but not identical, since
  # Cheong's raw table itself sums to 1.008 (rounding), not 1.
  expect_equal(safety$distribution[1:7],
               c(.2792, .0082, .1306, .0566, .0135, .0034, .0027),
               tolerance = 0.01)
})

test_that("MBD validates inputs and the vehicle-count cap", {
  inputs <- cheong_v3_inputs()
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
  inputs <- cheong_v3_inputs()
  fit <- calc_mbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)
  expect_output(print(fit), "Multivariate Beta Binomial")
  expect_output(print(fit), "Probability sum")
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
