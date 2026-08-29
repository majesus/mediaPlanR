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
  # Same canonical-expansion between-vehicle step as CSD on identical inputs:
  # close, though not required to be identical, since the two differ in how
  # they combine it with the within-vehicle expansion.
  csd_result <- calc_csd(inputs$vehicles_data, inputs$duplications, aggregation_order = 1:3)
  expect_equal(result$reach$percent, csd_result$reach$percent, tolerance = 0.2)
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
