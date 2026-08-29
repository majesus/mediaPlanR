test_that("calc_sainsbury, calc_binomial and calc_beta_binomial return coherent reach values (0-100%)", {
  audiences <- c(300000, 400000, 200000)
  population <- 1000000

  res_s <- calc_sainsbury(audiences, population)
  res_b <- calc_binomial(audiences, population)

  expect_s3_class(res_s, "reach_sainsbury")
  expect_s3_class(res_b, "reach_binomial")
  expect_true(res_s$reach$percent > 0 && res_s$reach$percent <= 100)
  expect_true(res_b$reach$percent > 0 && res_b$reach$percent <= 100)
  expect_equal(sum(res_s$distribution$percent), res_s$reach$percent, tolerance = 1e-6)

  res_bb <- calc_beta_binomial(A1 = 500000, A2 = 550000, P = 1000000, n = 5)
  expect_s3_class(res_bb, "reach_beta_binomial")
  expect_true(res_bb$reach$percent > 0 && res_bb$reach$percent <= 100)
})

test_that("calc_metheringham computes A1/D/A2, derives alpha/beta, and returns reach", {
  duplication_matrix <- matrix(c(
    150000, 200000, 180000,
    200000, 120000, 140000,
    180000, 140000, 170000
  ), nrow = 3, byrow = TRUE)

  res <- calc_metheringham(
    audiences = c(1500000, 800000, 1200000),
    insertions = c(4, 3, 5),
    duplication_matrix = duplication_matrix,
    population = 10000000
  )

  expect_s3_class(res, "reach_metheringham")
  expect_true(res$second_audience > res$mean_audience)

  # A1 and A2 (already computed from audiences/duplications) must be the
  # exact R1/R2 calc_beta_binomial() needs: 2*R1 - E_2^2 = R2 is an identity
  # of the Beta-Binomial distribution, so calling it with A1/A2 directly must
  # reproduce its own alpha/beta estimator.
  legacy_bb <- calc_beta_binomial(A1 = res$mean_audience, A2 = res$second_audience,
                                  P = 10000000, n = 3)
  expect_equal(res$parameters$alpha, legacy_bb$parameters$alpha, tolerance = 1e-9)
  expect_equal(res$parameters$beta, legacy_bb$parameters$beta, tolerance = 1e-9)
  expect_equal(res$reach$percent, legacy_bb$reach$percent, tolerance = 1e-9)
  expect_length(res$distribution$percent, 3)
  expect_true(res$reach$percent > 0 && res$reach$percent <= 100)

  expect_error(
    calc_metheringham(audiences = c(0, 0), insertions = c(0, 0),
                      duplication_matrix = matrix(0, 2, 2), population = 1000),
    "insertions"
  )
})

test_that("calc_hofmans_accumulation produces monotonically increasing reach and returns a ggplot object", {
  res <- calc_hofmans_accumulation(0.06, 0.103, N = 5, show_steps = FALSE)
  expect_true(all(diff(res$results$RN) >= 0))
  expect_s3_class(res$plot, "ggplot")

  expect_error(calc_hofmans_accumulation(0.1, 0.2, N = 5, show_steps = FALSE), "division by zero")
})

test_that("calc_hofmans_duplication matches its closed form and reduces to a simple union at zero duplication", {
  audiences <- c(300000, 400000, 200000)
  population <- 1000000
  duplication_matrix <- matrix(c(
       0, 60000, 40000,
   60000,     0, 50000,
   40000, 50000,     0
  ), nrow = 3, byrow = TRUE)

  res <- calc_hofmans_duplication(audiences, population, duplication_matrix)
  expect_s3_class(res, "reach_hofmans_duplication")

  Kij <- function(i, j) (audiences[i] + audiences[j]) /
    (audiences[i] + audiences[j] - duplication_matrix[i, j])
  kD <- Kij(1, 2) * 60000 + Kij(1, 3) * 40000 + Kij(2, 3) * 50000
  A <- sum(audiences)
  expect_equal(res$reach$people, A^2 / (A + kD), tolerance = 1e-9)

  # Zero observed duplication: coverage reduces to the simple sum of audiences
  zero_dup <- matrix(0, 3, 3)
  zero_res <- calc_hofmans_duplication(audiences, population, zero_dup)
  expect_equal(zero_res$reach$people, sum(audiences), tolerance = 1e-9)

  expect_error(
    calc_hofmans_duplication(audiences, population,
                          matrix(c(0, 9e5, 0, 9e5, 0, 0, 0, 0, 0), nrow = 3)),
    "Frechet|min\\(audience"
  )
})
