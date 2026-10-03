test_that("the Beta-Binomial limits are recognized relative to the variance, not in absolute terms", {
  for (scale in c(1e-1, 1e-4, 1e-8)) {
    R1 <- 0.3 * scale
    independence <- 2 * R1 - R1^2
    # A genuinely over-dispersed pair stays a Beta-Binomial at any scale.
    params <- mediaPlanR:::calculate_bbd_params(R1, 1.5 * R1)
    expect_identical(params$type, "beta_binomial")
    expect_lt(abs(params$alpha / (params$alpha + params$beta) - R1) / R1, 1e-9)
    expect_identical(mediaPlanR:::calculate_bbd_params(R1, independence)$type,
                     "binomial_limit")
    expect_identical(mediaPlanR:::calculate_bbd_params(R1, R1)$type,
                     "polarized_limit")
  }
  expect_error(mediaPlanR:::calculate_bbd_params(0.3, 0.52), "independence limit")
})

test_that("the canonical correlation does not mistake small audiences for independence", {
  # Two vehicles that reach exactly the same tiny audience are perfectly
  # correlated, not independent.
  expect_lt(abs(mediaPlanR:::calculate_duplication(1e-12, 1e-12, 1e-12) - 1), 1e-9)
  expect_identical(mediaPlanR:::calculate_duplication(0.1, 0, 0.5), 0)
  expect_identical(mediaPlanR:::calculate_duplication(0.1, 1, 0.5), 0)
})

test_that("the local Beta-Binomial probability function is stable and matches extraDistr", {
  expect_lt(abs(sum(mediaPlanR:::dbetabinom(0:2000, 2000, 0.4, 1.6)) - 1), 1e-9)
  expect_true(all(is.finite(mediaPlanR:::dbetabinom(0:5000, 5000, 1e-3, 5e-3))))
  skip_if_not_installed("extraDistr")
  for (case in list(c(6, 0.8, 1.2), c(20, 0.05, 0.4), c(3, 25, 40))) {
    x <- 0:case[1]
    expect_lt(max(abs(mediaPlanR:::dbetabinom(x, case[1], case[2], case[3]) -
                        extraDistr::dbbinom(x, case[1], case[2], case[3]))),
              1e-12)
  }
})

infeasible_triple <- function() {
  # Three exposures with audience 0.5 and pairwise co-exposure 0.1375 (a
  # canonical correlation of -0.45) satisfy the Frechet bounds and give a
  # positive definite correlation matrix, but no joint distribution has these
  # pairs: P(all three) would have to be negative.
  list(
    vehicles_data = data.frame(insertions = c(2, 2, 2), R1 = rep(0.5, 3),
                               R2 = rep(0.7, 3)),
    duplications = matrix(c(NA, 0.1375, 0.1375,
                            0.1375, NA, 0.1375,
                            0.1375, 0.1375, NA), 3)
  )
}

test_that("jointly impossible duplications are rejected by all five multivariate models", {
  input <- infeasible_triple()
  for (model in c("calc_csd", "calc_cbd", "calc_msad", "calc_mbd")) {
    expect_error(do.call(model, c(input, aggregation_order = "given")),
                 "jointly impossible", info = model)
  }
  canex_dup <- input$duplications
  diag(canex_dup) <- 1
  expect_error(
    calc_canex(data.frame(k = 2, R1 = rep(0.5, 3), R2 = rep(0.7, 3)), canex_dup),
    "jointly impossible"
  )
})

test_that("feasible triples are accepted", {
  expect_silent(mediaPlanR:::check_triple_feasibility(
    rep(0.5, 3), matrix(0.3, 3, 3), 1e-10))
  expect_error(mediaPlanR:::check_triple_feasibility(
    rep(0.5, 3), matrix(0.1375, 3, 3), 1e-10), "jointly impossible")
})

test_that("a duplication well below the random one is reported, a rounding-level one is not", {
  vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.3), R2 = c(0.45, 0.45))
  expect_warning(
    calc_csd(vehicles, matrix(c(NA, 0.05, 0.05, NA), 2)),
    class = "mediaPlanR_low_duplication"
  )
  expect_warning(
    calc_msad(vehicles, matrix(c(NA, 0, 0, NA), 2)),
    class = "mediaPlanR_low_duplication"
  )
  expect_warning(calc_csd(vehicles, matrix(c(NA, 0.085, 0.085, NA), 2)), NA)
  expect_warning(calc_csd(vehicles, matrix(c(NA, 0.09, 0.09, NA), 2)), NA)
})

test_that("the published reference examples do not trigger the low-duplication warning", {
  plans <- utils::read.csv(test_path("fixtures", "cbd_published_plans.csv"))
  rho <- with(plans, (dup - R1_1 * R1_2) /
                sqrt(R1_1 * (1 - R1_1) * R1_2 * (1 - R1_2)))
  expect_gt(min(rho), mediaPlanR:::low_duplication_correlation)
})

test_that("calc_canex reports the mean it preserves and warns when truncation is material", {
  vehicles <- data.frame(k = c(2, 2), R1 = c(0.3, 0.3), R2 = c(0.45, 0.45))
  ok <- calc_canex(vehicles, matrix(c(1, 0.09, 0.09, 1), 2))
  expect_equal(ok$diagnostics$mean_exposures_result,
               ok$diagnostics$mean_exposures_expected, tolerance = 1e-9)
  expect_equal(ok$diagnostics$mean_exposures_expected, 2 * 2 * 0.3,
               tolerance = 1e-9)

  # A markedly low duplication between vehicles with several insertions drives
  # cells of the second-order expansion below zero; with unequal vehicles the
  # truncation also changes the mean, and the call reports it. (The
  # low-duplication warning is muffled here.)
  strong <- data.frame(k = c(5, 5, 5), R1 = c(0.4, 0.3, 0.2), R2 = c(0.6, 0.45, 0.3))
  dup <- matrix(c(1, 0.02, 0.01, 0.02, 1, 0.01, 0.01, 0.01, 1), 3)
  withCallingHandlers(
    expect_warning(calc_canex(strong, dup), class = "mediaPlanR_canex_truncation"),
    mediaPlanR_low_duplication = function(w) invokeRestart("muffleWarning")
  )
  fit <- suppressWarnings(calc_canex(strong, dup))
  expect_gt(fit$diagnostics$negative_mass_truncated,
            mediaPlanR:::canex_negative_mass_threshold)
  expect_gt(fit$diagnostics$mean_exposures_result -
              fit$diagnostics$mean_exposures_expected, 0.1)
  # With the random duplication there is no truncation and no warning.
  expect_warning(calc_canex(
    data.frame(k = c(6, 6), R1 = c(0.5, 0.5), R2 = c(0.7, 0.7)),
    matrix(c(1, 0.25, 0.25, 1), 2)), NA)
})

test_that("schedules with extreme audience scales keep valid distributions", {
  for (scale in c(1e-3, 1e-6)) {
    vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.2) * scale,
                           R2 = c(0.45, 0.3) * scale)
    dup <- matrix(c(NA, 0.06 * scale, 0.06 * scale, NA), 2)
    # The random duplication at this scale is 0.06 * scale^2, far below the
    # observed one: the pair is strongly associated, which is feasible.
    for (model in c("calc_csd", "calc_cbd", "calc_msad", "calc_mbd")) {
      fit <- suppressWarnings(do.call(model, list(vehicles, dup, "given")))
      expect_true(all(is.finite(fit$distribution$probability)), info = model)
      expect_lt(abs(sum(fit$distribution$probability) - 1), 1e-9)
      expect_gte(min(fit$distribution$probability), 0)
    }
  }
})
