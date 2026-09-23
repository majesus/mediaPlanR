test_that("Sainsbury and Binomial return coherent reach values", {
  audiences <- c(300000, 400000, 200000)
  population <- 1000000

  res_s <- calc_sainsbury(audiences, population)
  res_b <- calc_binomial(audiences, population)

  expect_s3_class(res_s, "reach_sainsbury")
  expect_s3_class(res_b, "reach_binomial")
  expect_equal(res_s$reach$percent, 66.4, tolerance = 1e-12)
  expect_equal(sum(res_s$distribution$percent), res_s$reach$percent, tolerance = 1e-9)
  expect_equal(sum(res_b$distribution$percent), res_b$reach$percent, tolerance = 1e-9)
  expect_equal(res_b$mean_probability, 0.3)
  expect_equal(res_b$reach$percent, 100 * (1 - 0.7^3), tolerance = 1e-12)
})

test_that("Sainsbury equals exhaustive enumeration and Binomial equals dbinom", {
  audiences <- c(0.3, 0.4, 0.2, 0.1)
  insertions <- c(2, 1, 2, 1)
  probabilities <- rep(audiences, insertions)
  n <- length(probabilities)
  grid <- as.matrix(expand.grid(rep(list(0:1), n)))
  weights <- apply(grid, 1, function(x) prod(ifelse(x == 1, probabilities, 1 - probabilities)))
  exact <- as.vector(tapply(weights, rowSums(grid), sum))

  sainsbury <- calc_sainsbury(audiences * 1e6, 1e6, insertions)
  expect_equal(sainsbury$distribution$percent / 100, exact[-1], tolerance = 1e-12)

  binomial <- calc_binomial(audiences * 1e6, 1e6, insertions)
  expect_equal(binomial$distribution$percent / 100,
               stats::dbinom(1:n, n, mean(probabilities)), tolerance = 1e-12)
})

test_that("Binomial uses the insertion-weighted mean audience", {
  # Aldas Manzano (1998, 3.3.1.1) uses the simple mean with equal insertions
  equal <- calc_binomial(c(2e5, 6e5), 1e6, insertions = c(3, 3))
  expect_equal(equal$mean_probability, mean(c(0.2, 0.6)))
  # With unequal insertions the weighted mean preserves expected exposures
  unequal <- calc_binomial(c(2e5, 6e5), 1e6, insertions = c(1, 3))
  expect_equal(unequal$mean_probability, (0.2 * 1 + 0.6 * 3) / 4)
  expect_equal(sum(seq_along(unequal$distribution$percent) * unequal$distribution$percent) / 100,
               4 * unequal$mean_probability, tolerance = 1e-12)
})

test_that("Sainsbury and Binomial handle audiences of zero and of the whole population", {
  expect_equal(calc_sainsbury(c(0, 1e6), 1e6)$reach$percent, 100)
  expect_equal(calc_binomial(c(0, 0), 1e6)$reach$percent, 0)
})

test_that("plan-level classical models validate every argument", {
  expect_error(calc_sainsbury(c(1e5, NA), 1e6), "audiences")
  expect_error(calc_sainsbury(c(1e5, -1), 1e6), "audiences")
  expect_error(calc_sainsbury(c(2e6), 1e6), "cannot exceed population")
  expect_error(calc_binomial(1e5, NA), "population")
  expect_error(calc_binomial(1e5, 0), "population")
  expect_error(calc_sainsbury(c(1e5, 2e5), 1e6, insertions = c(1, 1.5)), "insertions")
  expect_error(calc_sainsbury(c(1e5, 2e5), 1e6, insertions = 1), "insertions")
})

test_that("calc_beta_binomial reproduces R1 and R2 through its own distribution", {
  fit <- calc_beta_binomial(A1 = 4e5, A2 = 5.2e5, P = 1e6, n = 6)
  a <- fit$parameters$alpha
  b <- fit$parameters$beta
  expect_equal(1 - extraDistr::dbbinom(0, 1, a, b), 0.4, tolerance = 1e-12)
  expect_equal(1 - extraDistr::dbbinom(0, 2, a, b), 0.52, tolerance = 1e-12)
  expect_equal(fit$parameters$mean_probability, 0.4)
  expect_equal(fit$parameters$type, "beta_binomial")
  expect_equal(sum(fit$distribution$percent) + fit$parameters$zero_contact_probability, 100)
  expect_equal(fit$cumulative$percent[1], fit$reach$percent, tolerance = 1e-12)
})

test_that("calc_beta_binomial handles the binomial and polarized limits instead of returning NaN", {
  # R2 exactly at the independence (binomial) limit 2*R1 - R1^2: alpha and
  # beta are formally Inf, which extraDistr::dbbinom() turns into an all-NaN
  # distribution unless the limit is routed through the binomial branch.
  R1 <- 0.3
  R2 <- 2 * R1 - R1^2
  limit <- calc_beta_binomial(A1 = R1 * 1e6, A2 = R2 * 1e6, P = 1e6, n = 5)
  expect_false(anyNA(limit$distribution$percent))
  expect_equal(limit$parameters$alpha, Inf)
  expect_equal(limit$parameters$beta, Inf)
  expect_equal(limit$parameters$type, "binomial_limit")
  expect_equal(limit$distribution$percent,
               stats::dbinom(1:5, size = 5, prob = R1) * 100, tolerance = 1e-9)

  # R2 == R1: alpha == beta == 0, the polarized (all-or-nothing) limit.
  polarized <- calc_beta_binomial(A1 = 3e5, A2 = 3e5, P = 1e6, n = 5)
  expect_false(anyNA(polarized$distribution$percent))
  expect_equal(polarized$parameters$alpha, 0)
  expect_equal(polarized$parameters$type, "polarized_limit")
  expect_equal(polarized$reach$percent, 30, tolerance = 1e-9)
  expect_equal(polarized$distribution$percent[5], 30, tolerance = 1e-9)
  expect_equal(sum(polarized$distribution$percent[1:4]), 0)
})

test_that("the Beta-Binomial print method reports the mean of the Beta at the limits", {
  limit <- calc_beta_binomial(A1 = 3e5, A2 = 3e5, P = 1e6, n = 5)
  printed <- paste(capture.output(print(limit)), collapse = "\n")
  expect_match(printed, "Mean of the Beta distribution: 0.300")
  expect_false(grepl("NaN", printed))
})

test_that("calc_beta_binomial validates its arguments", {
  expect_error(calc_beta_binomial(NA, 5e5, 1e6, 5), "A1")
  expect_error(calc_beta_binomial(c(1, 2), 5e5, 1e6, 5), "A1")
  expect_error(calc_beta_binomial(5e5, 5e5, NA, 5), "P")
  expect_error(calc_beta_binomial(5e5, 5e5, 1e6, 2.5), "n must be one finite positive integer")
  expect_error(calc_beta_binomial(5e5, 5e5, 1e6, Inf), "n")
  expect_error(calc_beta_binomial(2e6, 2e6, 1e6, 3), "cannot exceed")
  expect_error(calc_beta_binomial(5e5, 4e5, 1e6, 3), "non-decreasing")
  expect_error(calc_beta_binomial(3e5, 6e5, 1e6, 3), "independence limit")
})

# Independent implementation of Aldas Manzano (1998, 3.3.1.5): enumerate
# every pair of insertions and average the audience they reach.
metheringham_reference <- function(audiences, insertions, duplication, population) {
  vehicle <- rep(seq_along(audiences), insertions)
  N <- length(vehicle)
  pairs <- utils::combn(N, 2)
  reach_of_pair <- apply(pairs, 2, function(p) {
    i <- vehicle[p[1]]
    j <- vehicle[p[2]]
    audiences[i] + audiences[j] - duplication[i, j]
  })
  A1 <- mean(audiences[vehicle])
  A2 <- mean(reach_of_pair)
  list(N = N, A1 = A1, A2 = A2,
       fit = calc_beta_binomial(A1, A2, population, N))
}

test_that("calc_metheringham follows Aldas Manzano's model for several insertions per vehicle", {
  data(metheringham_example)
  ex <- metheringham_example
  result <- do.call(calc_metheringham, ex)
  reference <- metheringham_reference(ex$audiences, ex$insertions,
                                      ex$duplication_matrix, ex$population)

  expect_s3_class(result, "reach_metheringham")
  expect_equal(result$total_insertions, 12)
  expect_equal(result$mean_audience, reference$A1, tolerance = 1e-12)
  expect_equal(result$second_audience, reference$A2, tolerance = 1e-12)
  expect_equal(result$mean_duplication, 2 * reference$A1 - reference$A2, tolerance = 1e-12)
  expect_equal(result$parameters$alpha, reference$fit$parameters$alpha, tolerance = 1e-12)
  expect_equal(result$reach$percent, reference$fit$reach$percent, tolerance = 1e-12)

  # The distribution spans zero to N = sum(insertions) exposures, and its
  # mean is the plan's gross number of exposures per person.
  expect_length(result$distribution$percent, 12)
  expect_equal(sum(seq_len(12) * result$distribution$percent) / 100,
               sum(ex$audiences * ex$insertions) / ex$population, tolerance = 1e-12)
  expect_equal(sum(result$opportunity_vector), choose(12, 2))
})

test_that("calc_metheringham with one insertion per vehicle reduces to Aldas Manzano 3.2.2.9", {
  audiences <- c(3e5, 4e5, 2e5)
  duplication <- matrix(c(NA, 150000, 90000, 150000, NA, 110000, 90000, 110000, NA), 3)
  result <- calc_metheringham(audiences, c(1, 1, 1), duplication, 1e6)

  expect_equal(result$mean_audience, mean(audiences))
  expect_equal(result$mean_duplication, mean(c(150000, 90000, 110000)))
  expect_equal(result$second_audience, 2 * mean(audiences) - mean(c(150000, 90000, 110000)))
  expect_length(result$distribution$percent, 3)
  reference <- calc_beta_binomial(result$mean_audience, result$second_audience, 1e6, 3)
  expect_equal(result$reach$percent, reference$reach$percent, tolerance = 1e-12)
})

test_that("calc_metheringham checks only the duplication entries it uses", {
  audiences <- c(3e5, 4e5, 2e5)
  duplication <- matrix(c(NA, 150000, NA, 150000, NA, NA, NA, NA, NA), 3)
  # Vehicle 3 has no insertions: its row and column are ignored
  result <- calc_metheringham(audiences, c(1, 1, 0), duplication, 1e6)
  expect_equal(result$mean_duplication, 150000)
  # A missing value where a pair of insertions exists is an error
  expect_error(calc_metheringham(audiences, c(1, 1, 1), duplication, 1e6),
               "finite wherever a pair of insertions exists")
})

test_that("calc_metheringham validates its inputs", {
  audiences <- c(3e5, 4e5)
  duplication <- matrix(c(1e5, 1.5e5, 1.5e5, 1e5), 2)
  expect_error(calc_metheringham(audiences, c(0, 1), duplication, 1e6), "At least two insertions")
  expect_error(calc_metheringham(audiences, c(2, 2), duplication, NA), "population")
  expect_error(calc_metheringham(audiences, c(2, 1.5), duplication, 1e6), "insertions")
  expect_error(calc_metheringham(audiences, c(2, 2), matrix(1, 3, 3), 1e6), "2 x 2")
  asymmetric <- duplication
  asymmetric[1, 2] <- 1e5
  expect_error(calc_metheringham(audiences, c(2, 2), asymmetric, 1e6), "symmetric")
  too_high <- duplication
  too_high[1, 2] <- too_high[2, 1] <- 5e5
  expect_error(calc_metheringham(audiences, c(2, 2), too_high, 1e6), "must lie between")
  # Duplication below random duplication cannot come from a Beta-Binomial
  low <- matrix(c(1e4, 1e4, 1e4, 1e4), 2)
  expect_error(calc_metheringham(audiences, c(2, 2), low, 1e6),
               "incompatible with a Beta-Binomial")
})

test_that("calc_hofmans_accumulation follows Aldas Manzano equations 3.11 and 3.12", {
  R1 <- 0.06
  R2 <- 0.103
  d <- 2 * R1 - R2
  k <- 2 * R1 / R2

  constant <- calc_hofmans_accumulation(R1, R2, N = 6)
  expect_s3_class(constant, "reach_hofmans_accumulation")
  expect_equal(constant$parameters$alpha, 1)
  expect_equal(constant$parameters$k, k)
  expect_equal(constant$parameters$d, d)
  expect_equal(constant$results$RN[1:2], c(R1, R2))
  N <- 1:6
  # Equation 3.11: R_N = (N R1)^2 / (N R1 + k (2 R1 - R2) choose(N, 2))
  expect_equal(constant$results$RN[3:6],
               ((N * R1)^2 / (N * R1 + k * d * choose(N, 2)))[3:6], tolerance = 1e-12)
  expect_true(all(diff(constant$results$RN) > 0))
  expect_true(all(constant$results$RN <= N * R1))
  expect_s3_class(constant$plot, "ggplot")

  # Equation 3.12: with an observed R3 the exponent is estimated and the
  # curve reproduces R3 exactly.
  R3 <- 0.135
  varying <- calc_hofmans_accumulation(R1, R2, N = 6, R3 = R3)
  expected_alpha <- log((3 * R1 - R3) * R2 / ((2 * R1 - R2) * R3)) / log(2)
  expect_equal(varying$parameters$alpha, expected_alpha, tolerance = 1e-12)
  expect_equal(varying$results$RN[3], R3, tolerance = 1e-12)
  expect_equal(varying$results$RN[5],
               (5 * R1)^2 / (5 * R1 + k * 4^expected_alpha * (5 / 2) * d), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(varying$results$RN[6], constant$results$RN[6])))
})

test_that("calc_hofmans_accumulation validates its inputs and prints on request", {
  expect_error(calc_hofmans_accumulation(0.1, 0.2, N = 5), "must be smaller than 2 \\* R1")
  expect_error(calc_hofmans_accumulation(0.06, 0.13, N = 5), "must be smaller than 2 \\* R1")
  expect_error(calc_hofmans_accumulation(0.1, 0.1, N = 5), "greater than R1")
  expect_error(calc_hofmans_accumulation(NA, 0.1, N = 5), "R1")
  expect_error(calc_hofmans_accumulation(0.06, 0.103, N = 1.5), "N")
  expect_error(calc_hofmans_accumulation(0.06, 0.103, N = c(3, 4)), "N")
  expect_error(calc_hofmans_accumulation(0.06, 0.103, N = 5, R3 = 0.2), "R3 must lie between")
  expect_error(calc_hofmans_accumulation(0.06, 0.103, N = 2, R3 = 0.13), "at least 3")
  expect_output(calc_hofmans_accumulation(0.06, 0.103, N = 4, show_steps = TRUE),
                "HOFMANS MODEL")
  expect_silent(calc_hofmans_accumulation(0.06, 0.103, N = 4))
})

test_that("calc_agostini_duplication follows Aldas Manzano equation 3.57", {
  data(duplication_example)
  ex <- duplication_example
  A <- sum(ex$audiences)
  D <- sum(ex$duplication_matrix[upper.tri(ex$duplication_matrix)])

  default <- do.call(calc_agostini_duplication, ex)
  expect_s3_class(default, "reach_agostini_duplication")
  expect_equal(default$k, 1.125)
  expect_equal(default$reach$people, A^2 / (A + 1.125 * D), tolerance = 1e-12)
  expect_equal(default$reach$percent, 100 * default$reach$people / ex$population)
  expect_equal(default$total_duplication, D)

  # With k = 0 there is no duplication correction: reach is the gross audience
  expect_equal(calc_agostini_duplication(ex$audiences, ex$population,
                                         ex$duplication_matrix, k = 0)$reach$people, A)
  # For two vehicles, k = (A1 + A2) / (A1 + A2 - A12) makes the formula exact
  audiences <- c(3e5, 4e5)
  A12 <- 1.4e5
  k12 <- sum(audiences) / (sum(audiences) - A12)
  exact <- calc_agostini_duplication(audiences, 1e6, matrix(c(NA, A12, A12, NA), 2), k = k12)
  expect_equal(exact$reach$people, sum(audiences) - A12, tolerance = 1e-9)
  expect_output(print(default), "AGOSTINI MODEL")
})

test_that("calc_agostini_duplication validates inputs and warns about illogical reach", {
  dup <- matrix(c(NA, 1e5, 1e5, NA), 2)
  expect_error(calc_agostini_duplication(c(3e5, 4e5), 1e6, dup, k = NA), "k")
  expect_error(calc_agostini_duplication(c(3e5, 4e5), 1e6, dup, k = Inf), "k")
  expect_error(calc_agostini_duplication(c(3e5, 4e5), 1e6, dup, k = -1), "k")
  expect_error(calc_agostini_duplication(3e5, 1e6, matrix(NA, 1, 1)), "at least 2")
  expect_error(calc_agostini_duplication(c(3e5, 0), 1e6, dup), "audiences")
  expect_error(calc_agostini_duplication(c(3e5, 4e5), 1e6, matrix(1, 3, 3)), "2 x 2")
  # A duplication above its Frechet upper bound min(A_i, A_j)
  expect_error(calc_agostini_duplication(c(3e5, 4e5), 1e6,
                                         matrix(c(NA, 5e5, 5e5, NA), 2)), "must lie between")
  # Audiences of 60% each cannot overlap less than 20% of the population
  expect_error(calc_agostini_duplication(c(6e5, 6e5), 1e6,
                                         matrix(c(NA, 0, 0, NA), 2)), "must lie between 200000")
  # At the feasible lower bound, k = 1.125 pushes the formula above the population
  expect_warning(calc_agostini_duplication(c(6e5, 6e5), 1e6,
                                           matrix(c(NA, 2e5, 2e5, NA), 2)),
                 "outside the logical range")
})

test_that("calc_hofmans_duplication follows Aldas Manzano equations 3.61 and 3.62", {
  data(duplication_example)
  ex <- duplication_example
  res <- do.call(calc_hofmans_duplication, ex)
  expect_s3_class(res, "reach_hofmans_duplication")

  audiences <- ex$audiences
  dup <- ex$duplication_matrix
  Kij <- function(i, j) (audiences[i] + audiences[j]) / (audiences[i] + audiences[j] - dup[i, j])
  kD <- Kij(1, 2) * dup[1, 2] + Kij(1, 3) * dup[1, 3] + Kij(2, 3) * dup[2, 3]
  A <- sum(audiences)
  expect_equal(res$reach$people, A^2 / (A + kD), tolerance = 1e-12)
  expect_equal(res$weighted_duplication, kD, tolerance = 1e-12)

  # For two vehicles the formula is exact: A1 + A2 - A12
  two <- calc_hofmans_duplication(c(3e5, 4e5), 1e6, matrix(c(NA, 1.4e5, 1.4e5, NA), 2))
  expect_equal(two$reach$people, 3e5 + 4e5 - 1.4e5, tolerance = 1e-9)
  # Without duplication, the reach is the sum of the audiences
  none <- calc_hofmans_duplication(c(3e5, 4e5), 1e6, matrix(0, 2, 2))
  expect_equal(none$reach$people, 7e5, tolerance = 1e-9)
  expect_output(print(res), "HOFMANS MODEL")
})

test_that("calc_hofmans_duplication enforces the Frechet bounds of every duplication", {
  expect_error(calc_hofmans_duplication(c(6e5, 6e5), 1e6, matrix(0, 2, 2)),
               "must lie between 200000")
  expect_error(calc_hofmans_duplication(c(3e5, 4e5), 1e6, matrix(9e5, 2, 2)),
               "must lie between")
  expect_error(calc_hofmans_duplication(c(3e5, 4e5), 1e6, matrix(c(0, 1, 2, 0), 2)),
               "symmetric")
  expect_error(calc_hofmans_duplication(c(3e5, NA), 1e6, matrix(0, 2, 2)), "audiences")
})
