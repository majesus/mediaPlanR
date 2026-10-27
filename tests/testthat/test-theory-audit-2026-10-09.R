# Regression tests for the statements added to the help pages after the
# theoretical audit of the manual (2026-10-09). Each block checks, with
# independent arithmetic, a claim that the documentation now makes.

test_that("the two Hofmans models coincide for equal vehicles and equal duplications", {
  # Help pages of calc_hofmans_accumulation() and calc_hofmans_duplication():
  # p = 0.30, R2 = 0.45 (d = 0.15) and N = 4 give a reach of 0.60 in both.
  p <- 0.30
  R2 <- 0.45
  d <- 2 * p - R2
  for (N in 2:6) {
    accumulation <- calc_hofmans_accumulation(p, R2, N)$results$RN[N]
    duplication <- calc_hofmans_duplication(
      audiences = rep(p, N), population = 1,
      duplication_matrix = matrix(d, N, N)
    )$reach$people
    expect_equal(accumulation, duplication, tolerance = 1e-12)
  }
  expect_equal(calc_hofmans_accumulation(p, R2, 4)$results$RN[4], 0.60,
               tolerance = 1e-12)
  # Pairwise coefficient of the duplication model, 2p / (2p - d), equals the
  # constant coefficient 2 R1 / R2 of the accumulation model.
  expect_equal(2 * p / (2 * p - d), calc_hofmans_accumulation(p, R2, 3)$parameters$k,
               tolerance = 1e-12)
})

test_that("MBD can move the mean of the contacts without any negative probability", {
  # Help page of calc_mbd(), section "What the result conserves". The data are
  # compatible: the joint distribution of the numbers of exposures (K_A, K_B)
  # below has exactly these audiences, reaches after two insertions and
  # duplications, with the exposed insertion chosen with equal probability
  # when a vehicle has a single exposure.
  joint <- data.frame(
    A = c(0, 0, 1, 2, 2), B = c(0, 1, 0, 1, 2),
    p = c(0.50, 0.10, 0.20, 0.10, 0.10)
  )
  expect_equal(sum(joint$p), 1)
  expect_equal(sum(joint$A * joint$p) / 2, 0.30)
  expect_equal(sum(joint$B * joint$p) / 2, 0.20)
  expect_equal(sum(joint$p[joint$A > 0]), 0.40)
  expect_equal(sum(joint$p[joint$B > 0]), 0.30)
  expect_equal(sum(joint$A * joint$B * joint$p) / 4, 0.15)
  expect_gt(0.15, 0.30 * 0.20)

  vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.30, 0.20),
                         R2 = c(0.40, 0.30))
  duplications <- matrix(c(0, 0.15, 0.15, 0), nrow = 2)
  mbd <- calc_mbd(vehicles, duplications, aggregation_order = "given")

  expect_equal(mbd$diagnostics$negative_mass_adjusted, 0)
  expect_equal(mbd$diagnostics$cells_adjusted, 0)
  expect_equal(mbd$diagnostics$gross_mean_contacts, 1, tolerance = 1e-12)
  expect_equal(mbd$diagnostics$distribution_mean_contacts, 1.0537878788,
               tolerance = 1e-9)
  expect_equal(mbd$diagnostics$mean_error, 0.0537878788, tolerance = 1e-8)
  expect_equal(mbd$distribution$probability,
               c(0.5041193182, 0.1893465909, 0.1214488636, 0.1187973485,
                 0.0662878788), tolerance = 1e-9)
  expect_equal(mbd$reach$probability, 0.4958806818, tolerance = 1e-9)
  expect_equal(mbd$average_frequency, 2.1250835482, tolerance = 1e-9)
  # The contacts of the input per person reached are not the average frequency
  # of the result.
  expect_equal(1 / mbd$reach$probability, 2.0166141507, tolerance = 1e-9)

  # CSD and MSAD conform each step to the margins they combine and conserve
  # the mean on the same data.
  expect_equal(calc_csd(vehicles, duplications,
                        aggregation_order = "given")$diagnostics$mean_error,
               0, tolerance = 1e-12)
  expect_equal(calc_msad(vehicles, duplications,
                         aggregation_order = "given")$diagnostics$mean_error,
               0, tolerance = 1e-12)
})

test_that("a duplication below the random one can be valid data that CANEX reproduces", {
  # Help page of calc_canex(), section "Domain of validity": audiences 0.30 and
  # 0.20, one insertion each, duplication 0.03 (random: 0.06).
  joint <- c(neither = 0.53, only_second = 0.17, only_first = 0.27, both = 0.03)
  expect_equal(sum(joint), 1)
  expect_equal(unname(joint["only_first"] + joint["both"]), 0.30)
  expect_equal(unname(joint["only_second"] + joint["both"]), 0.20)
  expect_lt(0.03, 0.30 * 0.20)

  vehicles <- data.frame(insertions = c(1, 1), R1 = c(0.30, 0.20),
                         R2 = c(0.30, 0.20))
  duplications <- matrix(c(0, 0.03, 0.03, 0), nrow = 2)
  expect_warning(
    result <- calc_canex(vehicles, duplications),
    class = "mediaPlanR_low_duplication"
  )
  expect_equal(result$distribution$probability, c(0.53, 0.44, 0.03),
               tolerance = 1e-12)
  expect_equal(result$diagnostics$negative_mass_truncated, 0)
  expect_equal(result$diagnostics$mean_exposures_result,
               result$diagnostics$mean_exposures_expected, tolerance = 1e-12)
})

test_that("the bound of the Beta-Binomial family is narrower than the logical bound", {
  # Help page of calc_beta_binomial(): p = 0.30, R2 = 0.55 are logically
  # possible but outside the family, whose largest R2 is 2p - p^2 = 0.51.
  joint <- c(both = 0.05, only_first = 0.25, only_second = 0.25, neither = 0.45)
  expect_equal(sum(joint), 1)
  expect_equal(unname(joint["both"] + joint["only_first"]), 0.30)
  expect_equal(unname(joint["both"] + joint["only_second"]), 0.30)
  expect_equal(1 - unname(joint["neither"]), 0.55)
  expect_lte(0.55, min(1, 2 * 0.30))
  expect_equal(2 * 0.30 - 0.30^2, 0.51)
  expect_error(calc_beta_binomial(0.30, 0.55, 2), "independence limit")
  expect_silent(calc_beta_binomial(0.30, 0.51, 2))
})

test_that("the APE denominator is the observed reach, equal to Kim's sum for complete tables", {
  # Help page of evaluate_exposure_model(): observed 0.500 and 0.496 (mass
  # 0.996), predicted 0.800 and 0.200.
  observed <- data.frame(contacts = 0:1, observed = c(0.500, 0.496))
  predicted <- data.frame(contacts = 0:1, predicted = c(0.800, 0.200))
  rounded <- evaluate_exposure_model(observed, predicted, "probability",
                                     "probability")
  expect_equal(rounded$summary$kim_ape, 0.296 / 0.500, tolerance = 1e-12)
  expect_equal(0.296 / 0.496, 0.5967741935, tolerance = 1e-9)

  complete <- data.frame(contacts = 0:2, observed = c(0.5, 0.3, 0.2))
  other <- data.frame(contacts = 0:2, predicted = c(0.4, 0.4, 0.2))
  full <- evaluate_exposure_model(complete, other, "probability", "probability")
  kim_denominator <- sum(complete$observed[-1L])
  expect_equal(full$summary$kim_ape,
               sum(abs(complete$observed[-1L] - other$predicted[-1L])) /
                 kim_denominator, tolerance = 1e-12)
})

test_that("the mean departure of CBD quoted for csd_example is reproducible", {
  # Help page of calc_cbd(): -0.0044 exposures per person for csd_example.
  data(csd_example, package = "mediaPlanR", envir = environment())
  fit <- do.call(calc_cbd, csd_example)
  expect_equal(round(fit$diagnostics$mean_error, 4), -0.0044)
})
