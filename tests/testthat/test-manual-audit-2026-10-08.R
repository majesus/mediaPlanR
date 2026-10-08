# Regressions from the academic and technical audit of the 2.0.0 manual
# (2026-10-08). Each case was reproduced against the audited version; the
# expected values are logical bounds or identities, not snapshots.

two_channel_plan <- function() {
  media_plan(
    data.frame(channel = c("A", "B"), audience = c(50, 20),
               insertions = c(1, 1), cost_per_insertion = c(10, 5)),
    population = 100
  )
}

test_that("H01: plan_metrics() rejects a reach that the plan cannot produce", {
  plan <- two_channel_plan()
  # 90 people cannot be reached with 70 contacts
  expect_error(plan_metrics(plan, reach = 90), "larger than the number of impressions")
  # A single channel already reaches 50 people
  expect_error(plan_metrics(plan, reach = 10), "smaller than the largest audience")
  totals <- plan_metrics(plan, reach = 60)$totals
  expect_equal(totals$average_frequency, 70 / 60)
  # Boundaries are admissible: the largest audience, and every contact on a
  # different person
  expect_equal(plan_metrics(plan, reach = 50)$totals$average_frequency, 70 / 50)
  expect_equal(plan_metrics(plan, reach = 70)$totals$average_frequency, 1)
  # The frequency stays between one and the number of insertions
  for (r in c(50, 55, 60, 65, 70)) {
    f <- plan_metrics(plan, reach = r)$totals$average_frequency
    expect_gte(f, 1)
    expect_lte(f, 2)
  }
  # A reach estimated by the package is compatible with its own plan
  big <- media_plan(
    data.frame(channel = c("TV", "Radio"), audience = c(300000, 180000),
               insertions = c(4, 6), cost_per_insertion = c(18000, 3500)),
    population = 1000000
  )
  expect_no_error(plan_metrics(big, reach = estimate_reach(big)$reach$people))
  # No contacts: the reach can only be zero
  none <- media_plan(
    data.frame(channel = "A", audience = 10, insertions = 0,
               cost_per_insertion = 1), population = 100
  )
  expect_error(plan_metrics(none, reach = 5), "larger than")
  expect_equal(plan_metrics(none, reach = 0)$totals$reach, 0)
})

test_that("H02 and H16: the BBD fit separates impossible reaches from family limits", {
  # A vehicle alone reaches 50 people: a reach of 30 is impossible
  expect_error(
    fit_bbd_to_reach(c(1, 1), c(50, 10), 30, 100, precision = 0.001),
    "smaller than the largest audience of a vehicle"
  )
  expect_error(
    fit_bbd_to_reach(c(1, 1), c(50, 10), 70, 100, precision = 0.001),
    "larger than the population or the total number of contacts"
  )
  # Possible for the schedule but outside the Beta-Binomial family
  expect_error(
    fit_bbd_to_reach(c(1, 1), c(50, 10), 55, 100, precision = 0.001),
    "possible for the schedule, but the Beta-Binomial family"
  )
  # A reach inside both intervals is fitted
  fit <- fit_bbd_to_reach(c(1, 1), c(50, 10), 50, 100, precision = 0.001)
  expect_true(fit$parameters$converged)
  expect_lt(abs(fit$reach$fitted - 50), 0.001)

  # The default tolerance follows the scale of the universe (H16)
  small <- fit_bbd_to_reach(c(1, 1), c(0.3, 0.4), 0.5, 1)
  expect_lt(abs(small$reach$difference), 1e-4)
  expect_equal(small$reach$fitted, 0.5, tolerance = 1e-4)
  # An explicit absolute tolerance in people is still honoured
  loose <- fit_bbd_to_reach(c(1, 1), c(0.3, 0.4), 0.5, 1, precision = 100)
  expect_true(loose$parameters$converged)
  # Small reaches are printed with enough digits
  expect_output(print(small), "Fitted reach: +0\\.5")
})

test_that("H03: the non-target audience must fit in the non-target universe", {
  expect_error(audience_metrics(90, 10, 100, 80), "audience outside the target")
  ok <- audience_metrics(90, 70, 100, 80)
  expect_equal(ok$affinity_index, (70 / 90) / (80 / 100) * 100)
  # Boundary: the non-target audience equals the non-target universe
  expect_no_error(audience_metrics(100, 80, 100, 80))
  expect_no_error(audience_metrics(c(50, 90), c(40, 70), 100, 80))
  expect_error(audience_metrics(c(50, 90), c(40, 10), 100, 80),
               "audience outside the target")
})

test_that("H04, H08: the evaluator validates cells before it applies the mass tolerance", {
  obs <- data.frame(contacts = 0:1, observed = c(0.5, 0.5))
  bad <- data.frame(contacts = 0:1, predicted = c(1.001, 0))
  expect_error(
    evaluate_exposure_model(obs, bad, observed_scale = "probability",
                            predicted_scale = "probability"),
    "cannot exceed 1 in any cell"
  )
  bad_percent <- data.frame(contacts = 0:1, predicted = c(100.1, 0))
  expect_error(
    evaluate_exposure_model(obs, bad_percent, observed_scale = "probability",
                            predicted_scale = "percent"),
    "cannot exceed 100 in any cell"
  )
  # Rounded tables with valid cells and a total within the tolerance are used
  # as supplied, without renormalization
  deficit <- data.frame(contacts = 0:1, predicted = c(0.5, 0.496))
  result <- evaluate_exposure_model(obs, deficit, observed_scale = "probability",
                                    predicted_scale = "probability")
  expect_equal(result$by_schedule$predicted_reach, 0.5)
  expect_equal(result$input_mass$predicted_input_mass, 0.996)
  rounded <- data.frame(contacts = 0:2, predicted = c(0.4999, 0.4999, 0.0001))
  expect_no_error(evaluate_exposure_model(
    data.frame(contacts = 0:2, observed = c(0.5, 0.5, 0)), rounded,
    observed_scale = "probability", predicted_scale = "probability"))
})

test_that("H06: a collapsed last level does not present its mean as the real mean", {
  z <- nbd_exposure_distribution(mean_contacts = 2, size = 1, report_max = 3)
  expect_equal(z$mean_contacts, 2)
  table_mean <- with(z$distribution, sum(contacts * probability))
  expect_equal(table_mean, 1.407407, tolerance = 1e-6)

  observed <- data.frame(contacts = 0:3, observed = c(0.30, 0.25, 0.20, 0.25))
  predicted <- data.frame(contacts = 0:3, predicted = z$distribution$probability)
  exact <- evaluate_exposure_model(observed, predicted,
                                   observed_scale = "probability",
                                   predicted_scale = "probability")
  expect_false(is.na(exact$summary$mean_contact_bias))
  expect_null(exact$summary$mean_censored_contact_bias)

  censored <- evaluate_exposure_model(observed, predicted,
                                      observed_scale = "probability",
                                      predicted_scale = "probability",
                                      censored_last_level = TRUE)
  expect_true(is.na(censored$summary$mean_contact_bias))
  expect_true(all(is.na(censored$by_schedule$observed_mean_contacts)))
  expect_true(all(is.na(censored$by_schedule$predicted_mean_contacts)))
  expect_equal(censored$by_schedule$predicted_censored_mean, table_mean)
  expect_equal(censored$summary$mean_censored_contact_bias,
               exact$summary$mean_contact_bias)
  # AER and APE of the collapsed cells are unaffected by the flag
  expect_equal(censored$summary$kim_ape, exact$summary$kim_ape)
  expect_output(print(censored), "last level censored")
  expect_error(evaluate_exposure_model(observed, predicted,
                                       observed_scale = "probability",
                                       predicted_scale = "probability",
                                       censored_last_level = NA),
               "censored_last_level")
  # An open-tail object is still rejected as an exact prediction
  expect_error(evaluate_exposure_model(observed, z, observed_scale = "probability"),
               "open tail")
})

test_that("H19: the average frequency of an empty plan is NA in both interfaces", {
  empty <- media_plan(
    data.frame(channel = "A", audience = 0, insertions = 1,
               cost_per_insertion = 1), population = 100
  )
  expect_true(is.na(estimate_reach(empty)$average_frequency))
  expect_true(is.na(plan_metrics(empty, reach = 0)$totals$average_frequency))
  expect_true(is.na(compare_reach_models(empty)$average_frequency[1]))
  expect_output(print(estimate_reach(empty)), "Average frequency: NA")
})

test_that("H21: CANEX needs R2 for every vehicle, and a single insertion ignores its value", {
  vehicles <- data.frame(insertions = c(2, 1), R1 = c(0.3, 0.2),
                         R2 = c(0.45, NA))
  duplications <- matrix(c(NA, 0.08, 0.08, NA), 2)
  expect_error(calc_canex(vehicles, duplications), "R2")
  low <- vehicles
  low$R2[2] <- low$R1[2]
  high <- vehicles
  high$R2[2] <- 2 * high$R1[2] - high$R1[2]^2
  expect_equal(calc_canex(low, duplications)$distribution$probability,
               calc_canex(high, duplications)$distribution$probability,
               tolerance = 1e-10)
})

test_that("H05: the MBD safety net is reported as a package correction, not as MBD-ADJ", {
  result <- do.call(calc_mbd, mbd_cheong2007)
  result$diagnostics$negative_mass_adjusted <- 1e-3
  result$diagnostics$cells_adjusted <- 1L
  expect_output(print(result, full = FALSE), "not Cheong's UD-stage MBD-ADJ")
})
