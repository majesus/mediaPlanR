# Regressions from the independent audit of 2026-10-03. Expected values are
# analytical identities, not snapshots of the previous implementation.

test_that("audit F08: classical models preserve scale and reject impossible overlaps", {
  for (p in c(1e-11, 1e-16)) {
    expect_lt(abs(calc_sainsbury(p, 1)$reach$percent / (100 * p) - 1), 1e-12)
    expect_lt(abs(calc_binomial(p, 1)$reach$percent / (100 * p) - 1), 1e-12)
    expect_lt(abs(calc_beta_binomial(p, p, 2)$reach$percent / (100 * p) - 1), 1e-12)
    h <- calc_hofmans_accumulation(p, 1.5 * p, 3)
    expect_lt(abs(h$results$RN[3] / p - 1.8), 1e-12)
    m <- matrix(c(p, 0, p, p), 2)
    expect_error(calc_metheringham(c(p, p), c(2, 2), m, 1), "symmetric")
    m <- matrix(c(NA, 2 * p, 2 * p, NA), 2)
    expect_error(calc_agostini_duplication(c(p, p), 1, m), "bounds")
    expect_error(calc_hofmans_duplication(c(p, p), 1, m), "bounds")
  }
})

test_that("audit F09: large finite counts keep scale or fail explicitly", {
  m <- matrix(c(NA, 0.12, 0.12, NA), 2)
  for (f in c(calc_agostini_duplication, calc_hofmans_duplication)) {
    ordinary <- f(c(0.3, 0.4), 1, m)
    huge <- f(c(0.3, 0.4) * 1e308, 1e308, m * 1e308)
    expect_equal(huge$reach$percent, ordinary$reach$percent, tolerance = 1e-12)
    expect_equal(huge$reach$people / 1e308, ordinary$reach$people, tolerance = 1e-12)
  }
  m <- matrix(c(0.15, 0.12, 0.12, 0.2), 2)
  ordinary <- calc_metheringham(c(0.3, 0.4), c(4, 3), m, 1)
  huge <- calc_metheringham(c(0.3, 0.4) * 1e308, c(4, 3), m * 1e308, 1e308)
  expect_equal(huge$reach$percent, ordinary$reach$percent, tolerance = 1e-12)
  plan <- media_plan(data.frame(channel = c("A", "B"), audience = 1e308,
                                insertions = 2, cost_per_insertion = 1), 1e308)
  expect_error(plan_metrics(plan), "finite|representable")
  expect_error(calc_beta_binomial(0.3, 0.45, 1e308), "insertions.*integer|insertions.*supported")
})

test_that("audit F01: asymmetric duplications are rejected at every scale", {
  for (p in c(0.1, 1e-8, 1e-11, 1e-12)) {
    vehicles <- data.frame(insertions = c(1, 1), R1 = c(p, p), R2 = NA_real_)
    asymmetric <- matrix(c(NA, 0, p, NA), 2)
    for (model in c("calc_csd", "calc_cbd", "calc_msad", "calc_mbd")) {
      expect_error(do.call(model, list(vehicles, asymmetric)), "symmetric")
      expect_error(do.call(model, list(vehicles, t(asymmetric))), "symmetric")
    }
    vehicles <- data.frame(k = c(1, 1), R1 = c(p, p), R2 = c(p, p))
    expect_error(calc_canex(vehicles, asymmetric), "symmetric")
    expect_error(calc_canex(vehicles, t(asymmetric)), "symmetric")
  }
  # Small relative rounding differences are still allowed, including large
  # counts; a large entry must not hide asymmetry in a different pair.
  for (scale in c(1e-12, 1, 1e308)) {
    m <- matrix(c(NA, scale, scale * (1 - 1e-10), NA), 2)
    expect_silent(assert_symmetric_matrix(m, "m", 2L))
  }
  m <- matrix(1, 3, 3); m[1, 2] <- 0; m[2, 1] <- 1e-12
  expect_error(assert_symmetric_matrix(m, "m", 3L), "symmetric")
})

test_that("audit F02: CANEX keeps reach and frequency for tiny identical audiences", {
  for (p in c(1e-3, 1e-8, 1e-11, 1e-12, 1e-16)) {
    fit <- calc_canex(data.frame(k = c(1, 1), R1 = c(p, p), R2 = c(p, p)),
                      matrix(c(NA, p, p, NA), 2), population = 1 / p)
    expect_lt(abs(fit$reach$people - 1), 1e-10)
    expect_lt(abs(fit$average_frequency - 2), 1e-10)
    expect_lt(abs(fit$distribution$probability[3] / p - 1), 1e-10)
    expect_lt(abs(fit$reach$probability -
                    fit$distribution$cumulative_probability[2]), p * 1e-10)
  }
})

test_that("audit F06: CANEX normalizes even a small amount of truncated mass", {
  vehicles <- data.frame(k = c(2, 2), R1 = c(0.5, 0.5), R2 = c(0.75, 0.75))
  fit <- calc_canex(vehicles, matrix(c(NA, 0.3750001, 0.3750001, NA), 2))
  expect_gt(fit$diagnostics$negative_mass_truncated, 0)
  expect_lt(abs(sum(fit$distribution$probability) - 1), 1e-14)
  expect_lt(abs(fit$distribution$cumulative_probability[1] - 1), 1e-14)
  expect_lt(abs(fit$reach$probability -
                  fit$distribution$cumulative_probability[2]), 1e-14)
  expect_lt(abs(fit$diagnostics$mean_exposures_result - 2), 1e-14)
})

test_that("audit F07: truncation reports both means without asserting a change", {
  vehicles <- data.frame(k = c(2, 2), R1 = c(0.5, 0.5), R2 = c(0.75, 0.75))
  warnings <- character()
  fit <- withCallingHandlers(
    calc_canex(vehicles, matrix(c(NA, 0.5, 0.5, NA), 2)),
    mediaPlanR_canex_truncation = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(warnings, 1L)
  expect_false(grepl("changed from", warnings))
  expect_equal(fit$diagnostics$mean_exposures_result, 2, tolerance = 1e-12)
})

test_that("audit section 44: CBD and MBD have their own duplication limits", {
  vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.5, 0.5),
                         R2 = c(0.75, 0.75))
  for (d in c(0, 0.25, 0.5)) {
    duplication <- matrix(c(NA, d, d, NA), 2)
    mbd <- suppressWarnings(calc_mbd(vehicles, duplication))
    expect_equal(mbd$reach$probability, 15 / 16, tolerance = 1e-12)
    if (d == 0) {
      expect_error(suppressWarnings(calc_cbd(vehicles, duplication)),
                   "invalid zero-exposure")
    } else {
      cbd <- calc_cbd(vehicles, duplication)
      expect_equal(cbd$reach$probability, if (d == 0.25) 15 / 16 else 13 / 16,
                   tolerance = 1e-12)
    }
  }
})
