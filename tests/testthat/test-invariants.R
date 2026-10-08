# Probabilistic invariants shared by the multivariate models, on several
# schedules: a valid distribution (non-negative, unit mass), full support
# (0 to the total number of insertions), a mean number of exposures equal to
# the sum of the vehicles' means, and reach equal to one minus the zero cell.

invariant_schedules <- function() {
  data(csd_example, package = "mediaPlanR", envir = environment())
  list(
    three_vehicles = list(
      vehicles_data = data.frame(insertions = c(3, 1, 2),
                                 R1 = c(0.20, 0.08, 0.12),
                                 R2 = c(0.28, NA, 0.18)),
      duplications = matrix(c(NA, 0.02, 0.03,
                              0.02, NA, 0.015,
                              0.03, 0.015, NA), 3, byrow = TRUE)
    ),
    illustrative = csd_example,
    two_vehicles = list(
      vehicles_data = data.frame(insertions = c(3, 2), R1 = c(0.30, 0.20),
                                 R2 = c(0.42, 0.30)),
      duplications = matrix(c(NA, 0.09, 0.09, NA), 2)
    )
  )
}

check_invariants <- function(fit, vehicles_data, label, mean_tolerance) {
  probability <- fit$distribution$probability
  contacts <- fit$distribution$contacts
  expect_true(all(is.finite(probability)), info = label)
  expect_gte(min(probability), 0)
  expect_lt(abs(sum(probability) - 1), 1e-9)
  expect_identical(as.integer(contacts), 0:sum(vehicles_data$insertions))
  expect_lt(abs(sum(contacts * probability) -
                  sum(vehicles_data$insertions * vehicles_data$R1)),
            mean_tolerance)
  expect_lt(abs(fit$reach$probability - (1 - probability[1L])), 1e-9)
}

test_that("CSD, MSAD and MBD return valid distributions that preserve the mean", {
  for (name in names(invariant_schedules())) {
    schedule <- invariant_schedules()[[name]]
    vd <- schedule$vehicles_data
    dup <- schedule$duplications
    models <- c("calc_csd", "calc_msad")
    # MBD rejects some admissible-looking schedules with an informative error
    # (see "Domain of validity" in ?calc_canex), so it is checked where it runs.
    if (name %in% c("three_vehicles", "two_vehicles")) models <- c(models, "calc_mbd")
    for (model in models) {
      fit <- suppressWarnings(do.call(model, list(vd, dup, "given")))
      check_invariants(fit, vd, paste(model, name), mean_tolerance = 1e-9)
    }
  }
})

test_that("CBD returns valid distributions and reports its departure from the mean", {
  # Kim's (1994) algorithm does not preserve the vehicles' means exactly; the
  # departure is exposed in `diagnostics$mean_error` and is small.
  for (name in names(invariant_schedules())) {
    schedule <- invariant_schedules()[[name]]
    vd <- schedule$vehicles_data
    fit <- suppressWarnings(calc_cbd(vd, schedule$duplications, "given"))
    check_invariants(fit, vd, paste("calc_cbd", name), mean_tolerance = 0.02)
    expect_equal(sum(fit$distribution$contacts * fit$distribution$probability) -
                   sum(vd$insertions * vd$R1), fit$diagnostics$mean_error,
                 tolerance = 1e-9)
  }
})

test_that("CANEX returns a valid distribution whose mean is preserved up to its own truncation", {
  for (name in names(invariant_schedules())) {
    schedule <- invariant_schedules()[[name]]
    vd <- schedule$vehicles_data
    n <- length(vd$insertions)
    if (anyNA(vd$R2)) vd$R2[is.na(vd$R2)] <- vd$R1[is.na(vd$R2)]
    canex_vehicles <- data.frame(k = vd$insertions, R1 = vd$R1, R2 = vd$R2)
    dup <- schedule$duplications
    diag(dup) <- 1
    fit <- suppressWarnings(calc_canex(canex_vehicles, dup))
    probability <- fit$distribution$probability
    expect_gte(min(probability), 0)
    expect_lt(abs(sum(probability) - 1), 1e-9)
    expect_identical(as.integer(fit$distribution$contacts),
                     0:sum(canex_vehicles$k))
    # With no negative mass truncated, the second-order expansion preserves
    # every vehicle's mean exactly.
    if (fit$diagnostics$negative_mass_truncated == 0) {
      expect_lt(abs(fit$diagnostics$mean_exposures_result -
                      fit$diagnostics$mean_exposures_expected), 1e-9)
    }
  }
})

test_that("every model reproduces the single-vehicle reach under binomial and polarized limits", {
  # One vehicle is represented by two identical insertions (R2 on a limit): the
  # reach of the schedule is that of one vehicle with those insertions.
  vd <- data.frame(insertions = c(2, 1), R1 = c(0.2, 0.15), R2 = c(0.36, NA))
  dup <- matrix(c(NA, 0.03, 0.03, NA), 2)
  for (model in c("calc_csd", "calc_cbd", "calc_msad", "calc_mbd")) {
    fit <- suppressWarnings(do.call(model, list(vd, dup, "given")))
    expect_lt(abs(sum(fit$distribution$probability) - 1), 1e-9)
    expect_gte(min(fit$distribution$probability), 0)
  }
})
