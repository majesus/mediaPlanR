# Regressions for the audit of mediaPlanR of 2026-10-12 (findings MP-01 to
# MP-10). Every case has a witness that does not use the code under test: a
# closed form, a recurrence written here, or the base R distribution functions.

with_options <- function(new, code) {
  old <- options(new)
  on.exit(options(old))
  force(code)
}

collect_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = messages)
}

# Beta-Binomial probability function by the recurrence of consecutive
# probabilities in logarithms, normalized (the witness of the audit).
reference_bbd <- function(n, alpha, beta) {
  k <- 0:(n - 1)
  logs <- c(0, cumsum(log(n - k) - log(k + 1) + log(alpha + k) -
                        log(beta + n - 1 - k)))
  q <- exp(logs - max(logs))
  q / sum(q)
}

near_independence_R2 <- function(R1, theta) {
  R1 + (1 - theta) * R1 * (1 - R1)
}

test_that("MP-02: the Beta-Binomial keeps its mass and its mean near independence", {
  for (theta in c(1e-7, 1e-8, 2e-9)) {
    R1 <- 0.3
    R2 <- near_independence_R2(R1, theta)
    params <- mediaPlanR:::calculate_bbd_params(R1, R2)
    expect_identical(params$type, "beta_binomial")
    fit <- calc_beta_binomial(R1, R2, insertions = 500)
    probability <- c(fit$parameters$zero_contact_probability / 100,
                     fit$distribution$percent / 100)
    expect_equal(sum(probability), 1, tolerance = 1e-13)
    expect_lte(fit$reach$percent, 100)
    expect_equal(sum(0:500 * probability), 150, tolerance = 1e-11)
    expect_lt(max(abs(probability -
                        reference_bbd(500, params$alpha, params$beta))), 1e-12)
    expect_true(all(probability >= 0))
  }
})

test_that("MP-02: mass one, mean n * R1 and a valid reach at any size and audience", {
  for (insertions in c(1, 2, 50, 500)) {
    for (R1 in c(1e-9, 0.02, 0.5, 0.98, 1 - 1e-9)) {
      for (theta in c(1e-8, 1e-3, 0.4, 0.999)) {
        R2 <- near_independence_R2(R1, theta)
        params <- mediaPlanR:::calculate_bbd_params(R1, R2)
        if (params$type != "beta_binomial") next
        distribution <- mediaPlanR:::dbetabinom(0:insertions, insertions,
                                                params$alpha, params$beta)
        label <- sprintf("n=%d R1=%g theta=%g", insertions, R1, theta)
        expect_true(all(distribution >= 0), info = label)
        expect_equal(sum(distribution), 1, tolerance = 1e-13, info = label)
        expect_equal(sum(0:insertions * distribution) / (insertions * R1), 1,
                     tolerance = 1e-9, info = label)
        reach <- sum(distribution[-1L])
        expect_true(reach >= 0 && reach <= 1, info = label)
      }
    }
  }
})

test_that("MP-02: the two evaluations of the probability function meet at their switch", {
  # The log-beta form is used below a concentration of 10 and the recurrence
  # above it; both give the same distribution at the switch.
  below <- mediaPlanR:::bbd_distribution(40, 0.3 * 9.999999, 0.7 * 9.999999)
  above <- mediaPlanR:::bbd_distribution(40, 0.3 * 10.000001, 0.7 * 10.000001)
  expect_equal(below, above, tolerance = 1e-5)
  s <- 10
  lbeta_form <- exp(lchoose(40, 0:40) + lbeta(0:40 + 0.3 * s, 40 - 0:40 + 0.7 * s) -
                      lbeta(0.3 * s, 0.7 * s))
  expect_equal(mediaPlanR:::bbd_distribution(40, 0.3 * s, 0.7 * s), lbeta_form,
               tolerance = 1e-12)
  expect_equal(mediaPlanR:::bbd_distribution(40, 0.3 * s, 0.7 * s),
               reference_bbd(40, 0.3 * s, 0.7 * s), tolerance = 1e-12)
  # Shape parameters that are not finite and positive are an internal error.
  expect_error(mediaPlanR:::bbd_distribution(5, Inf, Inf), "finite")
})

test_that("MP-02: the variance of the expansion does not overflow for a large concentration", {
  mv <- mediaPlanR:::calculate_mean_variance(10, alpha = 1e200, beta = 2e200)
  expect_equal(mv$mean, 10 / 3)
  expect_equal(mv$variance, 10 * (1 / 3) * (2 / 3), tolerance = 1e-12)
  # The usual form agrees where it can be evaluated.
  k <- 7; a <- 0.8; b <- 2.4
  expect_equal(mediaPlanR:::calculate_mean_variance(k, a, b)$variance,
               k * a * b * (a + b + k) / ((a + b)^2 * (a + b + 1)),
               tolerance = 1e-14)
})

test_that("MP-02: a CSD plan near independence is no longer rejected for its margins", {
  vehicles <- data.frame(insertions = c(50, 2), R1 = c(0.3, 0.2),
                         R2 = c(0.51 - 2.1e-9, 0.3))
  duplications <- matrix(c(NA, 0.06, 0.06, NA), 2)
  # (MSAD rejects this plan on its own terms: with fifty insertions the
  # first vehicle reaches almost everyone, and the Morgensztern reach of the
  # pair exceeds one.)
  fit <- calc_csd(vehicles, duplications)
  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-13)
  expect_equal(fit$diagnostics$distribution_mean_contacts,
               sum(vehicles$insertions * vehicles$R1), tolerance = 1e-11)
  expect_lt(max(fit$steps$row_margin_error, fit$steps$column_margin_error),
            1e-12)
  expect_lte(fit$reach$probability, 1)
  expect_error(calc_msad(vehicles, duplications), "Morgensztern reach")
})

test_that("MP-01: CSD and MSAD keep a tiny reach through the whole chain", {
  # Two vehicles, one insertion each, in a population of 1 / p people. The
  # union is 2p under independence, p under coincidence and 1.5p when half of
  # the audience is shared: closed forms that do not depend on the models.
  for (p in c(1e-8, 1e-12, 1e-16, 1e-17, 1e-100)) {
    vehicles <- data.frame(insertions = c(1, 1), R1 = c(p, p), R2 = c(p, p))
    cases <- list(independence = list(p * p, 2 * p - p * p),
                  coincidence = list(p, p),
                  partial = list(p / 2, 1.5 * p))
    for (case in names(cases)) {
      duplications <- matrix(c(NA, cases[[case]][[1]], cases[[case]][[1]], NA), 2)
      for (model in c("calc_csd", "calc_msad")) {
        label <- sprintf("%s p=%g %s", model, p, case)
        fit <- do.call(model, list(vehicles, duplications, population = 1 / p))
        expected <- cases[[case]][[2]]
        expect_true(is.finite(fit$reach$probability), info = label)
        expect_gt(fit$reach$probability, 0)
        expect_equal(fit$reach$probability / expected, 1, tolerance = 1e-9,
                     info = label)
        expect_equal(fit$reach$people * p / expected, 1, tolerance = 1e-9,
                     info = label)
        expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-14,
                     info = label)
        expect_gte(fit$average_frequency, 1 - 1e-12)
        # The mean of the distribution is the gross mean of the inputs, in
        # relative terms.
        expect_equal(fit$diagnostics$distribution_mean_contacts / (2 * p), 1,
                     tolerance = 1e-12, info = label)
        expect_equal(fit$average_frequency, 2 * p / expected, tolerance = 1e-8,
                     info = label)
      }
    }
  }
})

test_that("MP-01: a tiny audience combined with a large one keeps its margins", {
  # Independent vehicles (a duplication equal to the product of the
  # audiences): the union has the closed form ra + rb - ra * rb, and the
  # distribution has the convolution of the two margins when it is
  # independent.
  tiny <- 1e-17
  vehicles <- data.frame(insertions = c(3, 2), R1 = c(0.3, tiny),
                         R2 = c(0.5, 1.8 * tiny))
  duplications <- matrix(c(NA, 0.3 * tiny, 0.3 * tiny, NA), 2)
  fit <- calc_csd(vehicles, duplications, aggregation_order = "given")
  ra <- sum(calc_beta_binomial(0.3, 0.5, 3)$distribution$percent) / 100
  rb <- sum(calc_beta_binomial(tiny, 1.8 * tiny, 2)$distribution$percent) / 100
  expect_equal(fit$reach$probability, ra + rb - ra * rb, tolerance = 1e-12)
  expect_equal(fit$diagnostics$distribution_mean_contacts,
               3 * 0.3 + 2 * tiny, tolerance = 1e-12)
  expect_equal(sum(fit$distribution$probability), 1, tolerance = 1e-14)
  # The same schedule aggregated in the other order has the same reach.
  reversed <- calc_csd(vehicles, duplications, aggregation_order = 2:1)
  expect_equal(reversed$reach$probability, fit$reach$probability,
               tolerance = 1e-12)
  expect_lt(max(fit$steps$row_margin_error, fit$steps$column_margin_error),
            1e-12)
})

test_that("MP-01: the ordinary results of CSD and MSAD are those of the reach formulas", {
  data(csd_example)
  csd <- do.call(calc_csd, csd_example)
  # Danaher's canonical zero probability, from the marginals, as a witness.
  fit <- calc_csd(csd_example$vehicles_data, csd_example$duplications,
                  aggregation_order = "given")
  expect_equal(fit$reach$probability, fit$steps$target_reach[2], tolerance = 1e-13)
  expect_equal(1 - fit$steps$zero_probability[2], fit$steps$target_reach[2],
               tolerance = 1e-12)
  expect_equal(csd$reach$probability, fit$reach$probability, tolerance = 1e-12)
})

test_that("MP-03: the binomial bound of the fit survives a tiny audience", {
  fit <- fit_bbd_to_reach(insertions = 2, audiences = 1, reach = 2,
                          population = 1e17, precision = 0.01)
  expect_identical(fit$parameters$fit_type, "binomial_limit")
  # The binomial limit of two insertions of one person in 1e17 is, in
  # people, 2 - 1e-17: about two.
  expect_equal(fit$parameters$feasible_reach[["max"]], 2, tolerance = 1e-12)
  expect_equal(fit$parameters$feasible_reach[["min"]], 1, tolerance = 1e-12)
  expect_gte(fit$parameters$feasible_reach[["max"]],
             fit$parameters$feasible_reach[["min"]])
  # Interior targets and small populations: the interval is never inverted and
  # the bounds agree with the closed forms.
  for (case in list(list(c(2, 3), c(1, 2), 3.5, 1e17),
                    list(5, 7, 9, 100), list(3, 1, 2.5, 10),
                    list(1, 30, 30, 100))) {
    fit <- fit_bbd_to_reach(case[[1]], case[[2]], case[[3]], case[[4]],
                            precision = 1e-6 * case[[4]])
    N <- sum(case[[1]])
    p <- sum(case[[1]] * case[[2]] / case[[4]]) / N
    feasible <- fit$parameters$feasible_reach
    expect_gte(feasible[["max"]], feasible[["min"]])
    expect_equal(feasible[["min"]], p * case[[4]], tolerance = 1e-12)
    # 1 - (1 - p)^N, by its first terms when p is so small that the direct
    # subtraction is zero.
    binomial <- if (p < 1e-8) N * p - choose(N, 2) * p^2 else 1 - (1 - p)^N
    expect_equal(feasible[["max"]], binomial * case[[4]], tolerance = 1e-9)
  }
  # The bounds are not widened by `precision`.
  expect_error(
    fit_bbd_to_reach(2, 30, 52, 100, precision = 0.5),
    "feasible Beta-Binomial interval")
})

test_that("MP-04: the Negative-Binomial reach and variance do not overflow", {
  # A size so small that mean / size overflows. The cumulant is
  # size * (log(mean + size) - log(size)), here about 7.1e-306.
  z <- nbd_exposure_distribution(10, 1e-308, report_max = 10)
  cumulant <- 1e-308 * (log(10 + 1e-308) - log(1e-308))
  expect_equal(z$reach$probability, -expm1(-cumulant), tolerance = 1e-12)
  expect_lt(z$reach$probability, 1e-300)
  expect_equal(z$average_frequency, 10 / z$reach$probability, tolerance = 1e-12)
  # The table of base R agrees: its positive cells add up to the reach.
  expect_equal(sum(z$distribution$probability[-1L]), z$reach$probability,
               tolerance = 1e-2)
  expect_equal(z$distribution$probability[1L], 1)
  # The variance is a modest number although mean^2 is not representable.
  z <- nbd_exposure_distribution(1e200, 1e200, report_max = 10)
  expect_equal(z$variance, 2e200, tolerance = 1e-12)
  expect_equal(z$reach$probability, 1)
  # A variance beyond the largest double is infinite, not a wrong number.
  z <- nbd_exposure_distribution(1e200, 1e-200, report_max = 10)
  expect_identical(z$variance, Inf)
  expect_equal(z$reach$probability, -expm1(-1e-200 * (log(1e200) - log(1e-200))),
               tolerance = 1e-12)
  # A quotient that overflows with a mean below one: the variance is finite.
  z <- nbd_exposure_distribution(1e-10, 1e-320, report_max = 10)
  expect_equal(z$variance, 1e-10 + 1e-20 / 1e-320, tolerance = 1e-3)
  # The cases already corrected keep their values.
  z <- nbd_exposure_distribution(1e-17, 1e308, report_max = 10)
  expect_equal(z$reach$probability, 1e-17, tolerance = 1e-12)
  expect_equal(nbd_exposure_distribution(2.5, 1.7, report_max = 10)$variance,
               2.5 + 2.5^2 / 1.7)
})

test_that("MP-05: the product of two small variances does not turn into 0 / 0", {
  for (p in c(1e-100, 1e-200, 1e-300)) {
    vehicles <- data.frame(insertions = c(1, 1), R1 = c(p, p), R2 = c(NA, NA))
    independent <- matrix(c(NA, 0, 0, NA), 2)
    for (model in c("calc_csd", "calc_cbd", "calc_msad", "calc_mbd")) {
      fit <- do.call(model, list(vehicles, independent))
      expect_equal(fit$reach$probability / (2 * p), 1, tolerance = 1e-12,
                   info = paste(model, p))
    }
  }
  # Correlated tiny audiences: a perfect coincidence and a half-shared one.
  p <- 1e-200
  vehicles <- data.frame(insertions = c(1, 1), R1 = c(p, p), R2 = c(NA, NA))
  for (model in c("calc_csd", "calc_cbd", "calc_mbd")) {
    coincident <- do.call(model, list(vehicles, matrix(c(NA, p, p, NA), 2)))
    expect_equal(coincident$reach$probability / p, 1, tolerance = 1e-10,
                 info = model)
    half <- do.call(model, list(vehicles, matrix(c(NA, p / 2, p / 2, NA), 2)))
    expect_equal(half$reach$probability / p, 1.5, tolerance = 1e-10,
                 info = model)
  }
})

test_that("MP-06: levels beyond the integer range are rejected without a coercion", {
  observed <- data.frame(contacts = c(0, 2147483648), observed = c(0.5, 0.5))
  predicted <- data.frame(contacts = c(0, 2147483648), predicted = c(0.5, 0.5))
  out <- collect_warnings(tryCatch(
    evaluate_exposure_model(observed, predicted, observed_scale = "probability",
                            predicted_scale = "probability"),
    error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_s3_class(out$value, "error")
  expect_match(conditionMessage(out$value), "observed schedule '1'")
  expect_match(conditionMessage(out$value), "every exposure level")
  expect_match(conditionMessage(out$value), "2147483648")

  # The predicted table is the one that fails when only it is wrong.
  ok <- data.frame(contacts = 0:1, observed = c(0.5, 0.5))
  out <- collect_warnings(tryCatch(
    evaluate_exposure_model(ok, predicted, observed_scale = "probability",
                            predicted_scale = "probability"),
    error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_match(conditionMessage(out$value), "predicted schedule '1'")

  # A level far beyond what a double holds exactly.
  huge <- data.frame(contacts = c(0, 1e300), observed = c(0.5, 0.5))
  out <- collect_warnings(tryCatch(
    evaluate_exposure_model(huge, data.frame(contacts = c(0, 1e300),
                                             predicted = c(0.5, 0.5)),
                            observed_scale = "probability",
                            predicted_scale = "probability"),
    error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_match(conditionMessage(out$value), "every exposure level")

  # Empty tables are rejected explicitly, not summarized as NA with warnings.
  empty <- data.frame(contacts = numeric(), observed = numeric())
  out <- collect_warnings(tryCatch(
    evaluate_exposure_model(empty, data.frame(contacts = numeric(),
                                              predicted = numeric()),
                            observed_scale = "probability",
                            predicted_scale = "probability"),
    error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_match(conditionMessage(out$value), "no rows")

  # Incomplete supports and duplicated cells keep being rejected.
  expect_error(
    evaluate_exposure_model(data.frame(contacts = c(0, 2), observed = c(.5, .5)),
                            data.frame(contacts = c(0, 2), predicted = c(.5, .5)),
                            observed_scale = "probability",
                            predicted_scale = "probability"),
    "every exposure level")
  expect_error(
    evaluate_exposure_model(data.frame(contacts = c(0, 0, 1),
                                       observed = c(.2, .3, .5)),
                            data.frame(contacts = 0:1, predicted = c(.5, .5)),
                            observed_scale = "probability",
                            predicted_scale = "probability"),
    "duplicate")
  # Several schedules, each complete, are still accepted.
  two <- data.frame(schedule = rep(c("a", "b"), each = 3), contacts = rep(0:2, 2),
                    observed = c(.5, .3, .2, .6, .3, .1))
  evaluation <- evaluate_exposure_model(
    two, transform(two, predicted = observed)[c("schedule", "contacts", "predicted")],
    observed_scale = "probability", predicted_scale = "probability",
    schedule_col = "schedule")
  expect_equal(evaluation$summary$kim_aer, 0)
})

test_that("MP-07: the default Negative-Binomial table is refused without a coercion warning", {
  out <- collect_warnings(tryCatch(nbd_exposure_distribution(3e9, Inf),
                                   error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_s3_class(out$value, "error")
  expect_match(conditionMessage(out$value), "report_max")
  expect_match(conditionMessage(out$value), "1,000,000")
  # The same process is summarized by an open tail when the table is short.
  out <- collect_warnings(nbd_exposure_distribution(3e9, Inf, report_max = 10))
  expect_length(out$warnings, 0L)
  expect_equal(out$value$reach$probability, 1)
  expect_true(out$value$distribution$open_tail[11L])
  # An infinite quantile is refused in the same way.
  out <- collect_warnings(tryCatch(
    nbd_exposure_distribution(1e300, 1e-5), error = function(e) e))
  expect_length(out$warnings, 0L)
  expect_s3_class(out$value, "error")
  # An ordinary default table is unchanged.
  z <- nbd_exposure_distribution(2.5, 1.7)
  expect_gte(nrow(z$distribution), 11L)
})

test_that("MP-08: the limits of the family are candidates of the calibration", {
  # With two insertions, the effective reach of one or more exposures of an
  # audience of 0.3 is at most 0.51, the binomial limit; the interval of
  # concentrations stops short of it by 6e-8.
  fit <- calibrate_bbd(0.3, 0.51, frequency = 1, max_insertions = 2,
                       tolerance = 1e-12)
  expect_identical(fit$fit_type, "binomial_limit")
  expect_true(fit$converged)
  expect_equal(fit$predicted_reach, 0.51, tolerance = 1e-14)
  expect_identical(fit$concentration, Inf)
  expect_identical(c(fit$alpha, fit$beta), c(Inf, Inf))
  expect_equal(fit$distribution$probability, dbinom(0:2, 2, 0.3))
  expect_identical(fit$candidates$fit_type[fit$candidates$n == 2L],
                   "binomial_limit")

  # The polarized limit: effective reach of two or more exposures equal to
  # R1 with two insertions, which no finite concentration reaches exactly.
  fit <- calibrate_bbd(0.3, 0.3, frequency = 2, max_insertions = 2,
                       tolerance = 1e-12)
  expect_identical(fit$fit_type, "polarized_limit")
  expect_true(fit$converged)
  expect_identical(c(fit$alpha, fit$beta, fit$concentration), c(0, 0, 0))
  expect_equal(fit$distribution$probability, c(0.7, 0, 0.3))

  # An interior target is still resolved by the interval, as before, and one
  # reach that the family cannot take is still reported as not reached.
  fit <- calibrate_bbd(0.3, 0.2, frequency = 2, max_insertions = 6)
  expect_identical(fit$fit_type, "beta_binomial")
  expect_true(fit$converged)
  expect_true(is.finite(fit$concentration) && fit$concentration > 0)
  expect_equal(fit$alpha / (fit$alpha + fit$beta), 0.3, tolerance = 1e-12)
  fit <- calibrate_bbd(0.3, 0.99, frequency = 1, max_insertions = 1)
  expect_false(fit$converged)
  expect_equal(fit$error, 0.69)
  expect_identical(fit$fit_type, "beta_binomial")
})

test_that("MP-09: CANEX does not need R2 for a single insertion, but needs it for two", {
  single <- data.frame(k = 1, R1 = 0.3, R2 = NA_real_)
  for (R2 in list(NA_real_, 0.55, 0.3)) {
    single$R2 <- R2
    fit <- calc_canex(single, matrix(NA_real_, 1))
    expect_equal(fit$distribution$probability, c(0.7, 0.3))
    expect_equal(fit$reach$probability, 0.3)
  }
  # An all-NA logical column, as data.frame(R2 = NA) produces.
  single$R2 <- NA
  expect_equal(calc_canex(single, matrix(NA_real_, 1))$reach$probability, 0.3)
  # A mixed schedule uses the Bernoulli of the single insertion, and gives
  # what the vehicle with a valid R2 gave before: the Beta-Binomial with one
  # insertion has the same marginal.
  vehicles <- data.frame(insertions = c(2, 1), R1 = c(0.3, 0.2),
                         R2 = c(0.45, 0.35))
  duplications <- matrix(c(NA, 0.08, 0.08, NA), 2)
  with_valid <- calc_canex(vehicles, duplications)
  vehicles$R2[2] <- NA
  expect_equal(calc_canex(vehicles, duplications)$distribution$probability,
               with_valid$distribution$probability, tolerance = 1e-12)
  # Two or more insertions: R2 is required and validated.
  two <- data.frame(k = 2, R1 = 0.3, R2 = NA_real_)
  expect_error(calc_canex(two, matrix(NA_real_, 1)), "R2")
  two$R2 <- 0.55
  expect_error(calc_canex(two, matrix(NA_real_, 1)), "independence limit")
  two$R2 <- 0.2
  expect_error(calc_canex(two, matrix(NA_real_, 1)), "smaller than R1")
})

test_that("MP-10: an oversized schedule is refused before its tables are allocated", {
  vehicles <- data.frame(insertions = c(3000, 3000, 3000), R1 = c(0.3, 0.2, 0.1),
                         R2 = c(0.45, 0.3, 0.15))
  duplications <- matrix(0.05, 3, 3)
  diag(duplications) <- NA
  for (model in c("calc_csd", "calc_msad")) {
    expect_error(do.call(model, list(vehicles, duplications)),
                 "joint table of 1.8e\\+07 cells")
    expect_error(do.call(model, list(vehicles, duplications)),
                 "mediaPlanR.max_table_cells")
  }
  # The limit is an option, and a schedule inside it is computed as before.
  small <- data.frame(insertions = c(4, 3), R1 = c(0.3, 0.2), R2 = c(0.45, 0.3))
  small_duplications <- matrix(c(NA, 0.07, 0.07, NA), 2)
  reference <- calc_csd(small, small_duplications)
  with_options(list(mediaPlanR.max_table_cells = 19), {
    # Largest table: (4 + 1) x (3 + 1) = 20 cells (audience_desc order).
    expect_error(calc_csd(small, small_duplications), "joint table of 20 cells")
    expect_error(calc_msad(small, small_duplications), "joint table of 20 cells")
  })
  with_options(list(mediaPlanR.max_table_cells = 20), {
    expect_equal(calc_csd(small, small_duplications)$distribution$probability,
                 reference$distribution$probability)
  })
  # An invalid option falls back to the default.
  with_options(list(mediaPlanR.max_table_cells = "many"), {
    expect_equal(calc_csd(small, small_duplications)$reach$probability,
                 reference$reach$probability)
  })

  # CBD and MBD: the work is estimated from the insertions, before anything
  # is built.
  twelve <- data.frame(insertions = rep(2000, 12), R1 = rep(0.2, 12),
                       R2 = rep(0.32, 12))
  dup12 <- matrix(0.044, 12, 12)
  diag(dup12) <- NA
  expect_error(suppressWarnings(calc_cbd(twelve, dup12)),
               "elementary operations")
  expect_error(suppressWarnings(calc_mbd(twelve, dup12)),
               "mediaPlanR.max_work")
  expect_gt(mediaPlanR:::cbd_work_estimate(rep(2000, 12)), 1e9)
  expect_equal(mediaPlanR:::cbd_work_estimate(c(2, 3)),
               4 * ((1 + 0) * 3 + (1 + 2) * 4))
  expect_equal(mediaPlanR:::mbd_work_estimate(c(2, 3), 1:2),
               2^(2 - 1) * 1 * (3 + 1) + 2^(1 - 1) * (1 + 3) * (2 + 1))
  three <- data.frame(insertions = c(3, 2), R1 = c(0.3, 0.2), R2 = c(0.45, 0.3))
  three_dup <- matrix(c(NA, 0.07, 0.07, NA), 2)
  with_options(list(mediaPlanR.max_work = 5), {
    expect_error(calc_cbd(three, three_dup), "elementary operations")
    expect_error(suppressWarnings(calc_mbd(three, three_dup)),
                 "elementary operations")
  })
  expect_s3_class(calc_cbd(three, three_dup), "reach_cbd")
})

test_that("MP-12: the manual build stops on a non-zero exit code of a step", {
  script <- file.path("..", "..", "tools", "build-manual.R")
  skip_if_not(file.exists(script),
              "the build script is not part of the package archive")
  env <- new.env()
  sys.source(script, envir = env)
  expect_true(is.function(env$build_manual))
  program <- file.path(R.home("bin"), "Rscript")
  output <- env$run_checked("a step that works", program, c("-e", "cat(42)"))
  expect_identical(output, "42")
  failure <- tryCatch(
    env$run_checked("a failing step", program, c("-e", "quit(status=3)")),
    manual_build_error = function(e) e)
  expect_s3_class(failure, "manual_build_error")
  expect_equal(as.integer(failure$status), 3L)
  expect_match(conditionMessage(failure),
               "a failing step failed (exit code 3)", fixed = TRUE)
})
