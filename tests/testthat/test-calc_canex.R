two_vehicles <- function() {
  list(
    vehicles = data.frame(k = c(2, 2), R1 = c(0.40, 0.05), R2 = c(0.50, 0.08)),
    duplications = matrix(c(1, 0.02, 0.02, 1), nrow = 2, byrow = TRUE)
  )
}

test_that("calc_canex returns a consistent reach and distribution", {
  inputs <- two_vehicles()
  res <- calc_canex(inputs$vehicles, inputs$duplications, population = 1000000)

  expect_s3_class(res, "reach_canex")
  expect_equal(res$reach$percent, 100 * res$reach$probability)
  expect_equal(res$reach$probability * 1e6, res$reach$people, tolerance = 1e-9)
  expect_equal(res$reach$probability, 1 - res$distribution$probability[1],
               tolerance = 1e-12)
  # The reach lies between the largest single-vehicle reach after two
  # insertions and the sum of the reaches
  expect_gte(res$reach$probability, max(inputs$vehicles$R2))
  expect_lte(res$reach$probability, sum(inputs$vehicles$R2))
  # Average frequency among the people reached: contacts per person / reach
  contacts <- sum(res$distribution$contacts * res$distribution$probability)
  expect_equal(res$average_frequency, contacts / res$reach$probability,
               tolerance = 1e-12)
})

test_that("calc_canex's distribution sums to 100% even after truncating negative probabilities", {
  vehicles <- data.frame(k = c(2, 2, 2),
                         R1 = c(0.40, 0.05, 0.04),
                         R2 = c(0.50, 0.08, 0.06))
  duplications <- matrix(c(1, 0.02, 0.016,
                           0.02, 1, 0.0005,
                           0.016, 0.0005, 1), nrow = 3, byrow = TRUE)

  res <- calc_canex(vehicles, duplications, population = 500000)
  expect_equal(sum(res$distribution$percent), 100, tolerance = 1e-9)
  expect_equal(sum(res$distribution$people), 500000, tolerance = 1e-6)
  expect_equal(res$distribution$cumulative_probability[1], 1, tolerance = 1e-12)
  expect_true(all(diff(res$distribution$cumulative_probability) <= 0))
})

test_that("calc_canex does not round people, so every scale stays consistent", {
  inputs <- two_vehicles()
  res <- calc_canex(inputs$vehicles, inputs$duplications, population = 50)

  expect_equal(res$distribution$people, 50 * res$distribution$probability, tolerance = 1e-12)
  expect_equal(res$reach$people, 50 * res$reach$probability, tolerance = 1e-12)
  expect_false(all(res$distribution$people == round(res$distribution$people)))
  expect_equal(res$distribution$percent, 100 * res$distribution$probability)
  # Cumulative probability at one exposure is exactly the reach
  expect_equal(res$distribution$cumulative_probability[2], res$reach$probability,
               tolerance = 1e-12)
})

test_that("print.reach_canex reports the average over the people reached", {
  inputs <- two_vehicles()
  res <- calc_canex(inputs$vehicles, inputs$duplications, population = 1000000)

  expect_output(print(res), "CANEX MODEL")
  # The distribution includes the zero-exposure row, so the printed average
  # must divide by the people reached and not by the population.
  expect_output(print(res),
                sprintf("Average exposures per person reached: %.2f", res$average_frequency))
})

test_that("calc_canex validates its inputs", {
  vehicles_ok <- data.frame(k = c(2, 2), R1 = c(0.40, 0.05), R2 = c(0.50, 0.08))
  dup_ok <- matrix(c(1, 0.02, 0.02, 1), nrow = 2)

  expect_error(calc_canex(data.frame(k = c(0, 2), R1 = c(0.1, 0.2), R2 = c(0.2, 0.3)),
                          matrix(c(1, 0, 0, 1), 2)), "positive integers")
  expect_error(calc_canex(vehicles_ok, matrix(1, nrow = 3, ncol = 2)), "2 x 2 matrix")
  expect_error(calc_canex(vehicles_ok, dup_ok, population = -10), "population")
  expect_error(calc_canex(vehicles_ok, dup_ok, population = NA), "population")
  expect_error(calc_canex(vehicles_ok[, c("k", "R1")], dup_ok), "R1 and R2")
  expect_error(calc_canex(transform(vehicles_ok, R1 = c(NA, 0.05)), dup_ok), "R1")
  asymmetric <- dup_ok
  asymmetric[1, 2] <- 0.03
  expect_error(calc_canex(vehicles_ok, asymmetric), "symmetric")
  expect_error(calc_canex(vehicles_ok, matrix(c(1, 1.5, 1.5, 1), 2)), "between 0 and 1")
})

test_that("calc_canex ignores the diagonal of the duplication matrix", {
  inputs <- two_vehicles()
  with_na <- inputs$duplications
  diag(with_na) <- NA
  expect_equal(calc_canex(inputs$vehicles, with_na)$reach$percent,
               calc_canex(inputs$vehicles, inputs$duplications)$reach$percent)
})

test_that("calc_canex stops with an informative error if the combination grid is excessive", {
  big_vehicles <- data.frame(k = rep(20, 5), R1 = rep(0.3, 5), R2 = rep(0.4, 5))
  big_dup <- matrix(0.1, 5, 5)
  diag(big_dup) <- 1

  expect_error(calc_canex(big_vehicles, big_dup), "exposure combinations")
})

test_that("the canonical correlation inverts to the joint probability and CANEX uses it", {
  # Equation 1 of Danaher (1991) and its inverse, for arbitrary marginals
  p1 <- 0.30; p2 <- 0.25; p12 <- 0.10
  rho <- calculate_duplication(p12, p1, p2)
  expect_equal(rho, (p12 - p1 * p2) / sqrt(p1 * (1 - p1) * p2 * (1 - p2)),
               tolerance = 1e-12)
  p11 <- p1 * p2 * (1 + rho * sqrt((1 - p1) * (1 - p2) / (p1 * p2)))
  expect_equal(p11, p12, tolerance = 1e-12)
  # With one insertion per vehicle the expansion returns the bivariate table
  fit <- calc_canex(data.frame(k = c(1, 1), R1 = c(p1, p2), R2 = c(p1, p2)),
                    matrix(c(NA, p12, p12, NA), 2))
  expect_lt(max(abs(fit$distribution$probability -
                      c(1 - p1 - p2 + p12, p1 + p2 - 2 * p12, p12))), 1e-12)
})

test_that("calc_canex accepts the insertions column as an alias of k", {
  duplications <- matrix(c(NA, 0.02, 0.02, NA), 2)
  with_k <- data.frame(k = c(2, 2), R1 = c(0.40, 0.05), R2 = c(0.50, 0.08))
  with_insertions <- data.frame(insertions = c(2, 2), R1 = c(0.40, 0.05),
                                R2 = c(0.50, 0.08))
  expect_equal(calc_canex(with_insertions, duplications)$distribution,
               calc_canex(with_k, duplications)$distribution)
  both <- cbind(with_k, insertions = c(2, 2))
  expect_equal(calc_canex(both, duplications)$reach, calc_canex(with_k, duplications)$reach)
  both$insertions <- c(3, 2)
  expect_error(calc_canex(both, duplications), "both insertions and k")
  expect_error(calc_canex(with_k[, c("R1", "R2")], duplications), "insertions [(]or k[)]")
})

test_that("the example data frame of the sequential models also runs CANEX", {
  data(csd_example)
  duplications <- csd_example$duplications
  expect_s3_class(calc_canex(csd_example$vehicles_data, duplications), "reach_canex")
})

test_that("calc_canex reports proportions unless a population is supplied", {
  vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.2), R2 = c(0.4, 0.3))
  duplications <- matrix(c(NA, 0.06, 0.06, NA), 2)
  default <- calc_canex(vehicles, duplications)
  expect_equal(default$population, 1)
  expect_equal(default$reach$people, default$reach$probability)
  expect_false(any(grepl("people", capture.output(print(default)))))
  scaled <- calc_canex(vehicles, duplications, population = 5e5)
  expect_equal(scaled$reach$people, 5e5 * scaled$reach$probability)
  expect_true(any(grepl("people", capture.output(print(scaled)))))
})
