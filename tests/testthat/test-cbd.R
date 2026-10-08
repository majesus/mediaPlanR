kim_cbd_inputs <- function() {
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

test_that("calc_cbd runs on Kim's (2005) CSD example inputs and returns a valid distribution", {
  inputs <- kim_cbd_inputs()
  result <- calc_cbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)

  expect_s3_class(result, "reach_cbd")
  expect_equal(sum(result$distribution$probability), 1, tolerance = 1e-9)
  expect_true(result$reach$percent > 0 && result$reach$percent < 100)
  expect_equal(result$diagnostics$negative_mass_adjusted, 0)
  # The zero-exposure probability is the canonical expansion of the zero cell
  # with all the insertions (Kim 1994, p. 124), the same one calc_canex()
  # computes.
  canex <- calc_canex(data.frame(k = inputs$vehicles_data$insertions,
                                 R1 = inputs$vehicles_data$R1,
                                 R2 = inputs$vehicles_data$R2),
                      inputs$duplications, population = 1)
  expect_equal(result$distribution$probability[1],
               canex$distribution$probability[1], tolerance = 1e-9)
})

test_that("calc_cbd reproduces the three-vehicle worked example of Kim (1994, p. 139)", {
  # SMRB 1979 data of Kim (1994, pp. 125-139): vehicles with 2, 1 and 3
  # insertions (the same example as Cheong, 2007, p. 75).
  vehicles <- data.frame(insertions = c(2, 1, 3),
                         R1 = c(0.146, 0.110, 0.252),
                         R2 = c(0.191, NA, 0.318))
  duplications <- matrix(c(NA, 0.032, 0.063,
                           0.032, NA, 0.041,
                           0.063, 0.041, NA), nrow = 3, byrow = TRUE)
  fit <- calc_cbd(vehicles, duplications, aggregation_order = 1:3)

  # Canonical correlations and (0,1) grid printed by Kim, in the order of the
  # exposure patterns 000, 001, 010, 011, 100, 101, 110, 111.
  R1 <- vehicles$R1
  correlation <- diag(3)
  for (i in 1:2) for (j in (i + 1):3) {
    correlation[i, j] <- correlation[j, i] <-
      calculate_duplication(duplications[i, j], R1[i], R1[j])
  }
  expect_lt(max(abs(c(correlation[1, 2], correlation[1, 3], correlation[2, 3]) -
                      c(0.144, 0.171, 0.098))), 0.0005)
  grid <- cbd_binary_grid(R1, correlation)
  patterns <- list(integer(0), 3, 2, c(2, 3), 1, c(1, 3), c(1, 2), c(1, 2, 3))
  expect_lt(max(abs(vapply(patterns, function(t) grid[[mbd_key(t)]], numeric(1)) -
                      c(0.6151, 0.1609, 0.0499, 0.0281, 0.0639, 0.0501, 0.0191, 0.0129))),
            0.00006)

  # Published distribution (Kim 1994, p. 139). Its printed zero of the
  # canonical expansion, 0.5066, differs from the one that the formula gives
  # with the printed inputs (about 0.510), which this function evaluates; the
  # zero and one-contact classes therefore differ by up to about 0.4
  # percentage points and the other classes by less than 0.15.
  published <- c(0.5066, 0.1620, 0.1205, 0.1329, 0.0453, 0.0275, 0.0055)
  deviation <- abs(fit$distribution$probability - published)
  expect_length(deviation, 7L)
  expect_lt(max(deviation), 0.004)
  expect_lt(max(deviation[-(1:3)]), 0.0005)
  expect_lt(abs(fit$diagnostics$canonical_zero_probability - 0.5102), 0.0001)
  expect_equal(fit$diagnostics$negative_mass_adjusted, 0)
})

test_that("calc_cbd reproduces the published CBD distributions of Kim (2005, Appendix B) and Hong (1998, Appendix E)", {
  # tests/testthat/fixtures/cbd_published_plans.csv holds the one-insertion and
  # two-insertion reaches (as proportions) of each two-vehicle, two-insertion
  # plan and the published CBD distribution (percent, zero to four contacts).
  # Neither source prints the duplication between the two vehicles; `dup` is
  # the duplication that reproduces the source's own CANX column (Kim) or MSAD
  # column (Hong) with the corresponding model of this package, so the check
  # is a consistency check of the CBD column with the other published
  # columns, with a resolution of 0.01 percentage points.
  skip_if_no_fixture("cbd_published_plans.csv")
  plans <- utils::read.csv(test_path("fixtures", "cbd_published_plans.csv"),
                           stringsAsFactors = FALSE)
  expect_equal(nrow(plans), 80L)
  for (source in unique(plans$source)) {
    expect_equal(sort(plans$plan[plans$source == source]), 1:40)
  }
  deviation <- vapply(seq_len(nrow(plans)), function(i) {
    plan <- plans[i, ]
    vehicles <- data.frame(insertions = c(2, 2),
                           R1 = c(plan$R1_1, plan$R1_2),
                           R2 = c(plan$R2_1, plan$R2_2))
    duplications <- matrix(c(NA, plan$dup, plan$dup, NA), nrow = 2)
    probability <- calc_cbd(vehicles, duplications,
                            aggregation_order = "given")$distribution$probability
    modelled <- 100 * c(probability, rep(0, 5))[1:5]
    max(abs(modelled - unlist(plan[paste0("pub", 0:4)])))
  }, numeric(1))
  expect_lt(max(deviation[plans$source == "Kim2005_AppB"]), 0.02)
  expect_lt(max(deviation[plans$source == "Hong1998_AppE"]), 0.02)
})

test_that("calc_cbd does not depend on the aggregation order", {
  inputs <- kim_cbd_inputs()
  forward <- calc_cbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)
  backward <- calc_cbd(inputs$vehicles_data, inputs$duplications, aggregation_order = 3:1)
  expect_equal(forward$distribution$probability, backward$distribution$probability,
               tolerance = 1e-12)
})

test_that("cbd_binary_grid recovers each vehicle's own R1 as its marginal", {
  inputs <- kim_cbd_inputs()
  R1 <- inputs$vehicles_data$R1
  n <- length(R1)
  correlation_matrix <- diag(1, n)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      correlation_matrix[i, j] <- correlation_matrix[j, i] <-
        calculate_duplication(inputs$duplications[i, j], R1[i], R1[j])
    }
  }
  grid <- cbd_binary_grid(R1, correlation_matrix)
  all_subsets <- mbd_all_subsets(seq_len(n))

  total <- sum(vapply(all_subsets, function(t) grid[[mbd_key(t)]], numeric(1)))
  expect_equal(total, 1, tolerance = 1e-9)

  for (v in seq_len(n)) {
    marginal <- sum(vapply(all_subsets, function(t) {
      if (v %in% t) grid[[mbd_key(t)]] else 0
    }, numeric(1)))
    expect_equal(marginal, R1[v], tolerance = 1e-9)
  }
})

test_that("calc_cbd reduces to the exact independent convolution at zero correlation", {
  skip_if_not_installed("extraDistr")
  vehicles <- data.frame(insertions = c(3, 2), R1 = c(0.2, 0.15), R2 = c(0.35, 0.27))
  independent_duplication <- vehicles$R1[1] * vehicles$R1[2]
  duplications <- matrix(c(NA, independent_duplication, independent_duplication, NA), nrow = 2)

  result <- calc_cbd(vehicles, duplications, aggregation_order = 1:2)

  bb1 <- calculate_bbd_params(vehicles$R1[1], vehicles$R2[1])
  bb2 <- calculate_bbd_params(vehicles$R1[2], vehicles$R2[2])
  d1 <- extraDistr::dbbinom(0:3, size = 3, alpha = bb1$alpha, beta = bb1$beta)
  d2 <- extraDistr::dbbinom(0:2, size = 2, alpha = bb2$alpha, beta = bb2$beta)
  expected <- numeric(6)
  for (a in 0:3) for (b in 0:2) expected[a + b + 1] <- expected[a + b + 1] + d1[a + 1] * d2[b + 1]

  expect_equal(result$distribution$probability, expected, tolerance = 1e-8)
})

test_that("calc_cbd validates inputs and caps the number of vehicles", {
  inputs <- kim_cbd_inputs()
  expect_error(
    calc_cbd(inputs$vehicles_data, matrix(0.9, 3, 3), aggregation_order = 1:3),
    "Frechet"
  )

  many <- data.frame(insertions = rep(2, 13), R1 = rep(0.1, 13), R2 = rep(0.15, 13))
  dup <- matrix(0.01, 13, 13); diag(dup) <- NA
  expect_error(calc_cbd(many, dup), "at most 12 vehicles")
})

test_that("calc_cbd does not error or return NaN when a vehicle's own R1/R2 sit at the binomial or polarized limit (regression test)", {
  # The conditional Beta-Binomial updating of calc_cbd() has the same two
  # limits as calc_mbd()'s peeling step, and both must be handled explicitly.
  dup <- matrix(c(NA, 0.05, 0.05, NA), nrow = 2, byrow = TRUE)

  R1 <- 0.3
  binomial_limit_vehicles <- data.frame(
    insertions = c(2, 2),
    R1 = c(R1, 0.20),
    R2 = c(2 * R1 - R1^2, 0.35)
  )
  fit_binomial <- calc_cbd(binomial_limit_vehicles, dup, aggregation_order = 1:2)
  expect_false(anyNA(fit_binomial$distribution$probability))
  expect_equal(sum(fit_binomial$distribution$probability), 1, tolerance = 1e-9)

  polarized_vehicles <- data.frame(
    insertions = c(2, 2),
    R1 = c(0.30, 0.20),
    R2 = c(0.30, 0.35)
  )
  fit_polarized <- calc_cbd(polarized_vehicles, dup, aggregation_order = 1:2)
  expect_false(anyNA(fit_polarized$distribution$probability))
  expect_equal(sum(fit_polarized$distribution$probability), 1, tolerance = 1e-9)
})

test_that("CBD does not depend on the aggregation order, and the report says so", {
  data(csd_kim2005)
  forward <- do.call(calc_cbd, csd_kim2005)
  backward <- calc_cbd(csd_kim2005$vehicles_data, csd_kim2005$duplications,
                       aggregation_order = c(3, 2, 1))
  expect_equal(forward$distribution, backward$distribution)
  expect_match(paste(capture.output(print(forward)), collapse = "\n"),
               "does not affect CBD")
})
