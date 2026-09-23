kim_two_vehicles <- function() {
  # First two vehicles of Kim (2005), Tables 4.2.2.1 and 4.2.2.2
  list(
    vehicles = data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502)),
    duplications = matrix(c(1, 0.0157, 0.0157, 1), nrow = 2, byrow = TRUE)
  )
}

test_that("calc_canex reproduces the reference value of the canonical expansion", {
  inputs <- kim_two_vehicles()
  res <- calc_canex(inputs$vehicles, inputs$duplications, population = 1000000)

  expect_s3_class(res, "reach_canex")
  expect_equal(res$reach$percent, 60.2057, tolerance = 1e-3)
  expect_equal(res$reach$probability * 1e6, res$reach$people, tolerance = 1e-9)
  expect_equal(res$reach$people, 602057, tolerance = 1)
  expect_equal(res$average_frequency, 1.7380, tolerance = 1e-3)
})

test_that("calc_canex's distribution sums to 100% even after truncating negative probabilities", {
  vehicles <- data.frame(k = c(2, 2, 2),
                         R1 = c(0.4902, 0.033, 0.03),
                         R2 = c(0.5805, 0.0502, 0.0371))
  duplications <- matrix(c(1, 0.0157, 0.0139,
                           0.0157, 1, 0.0003,
                           0.0139, 0.0003, 1), nrow = 3, byrow = TRUE)

  res <- calc_canex(vehicles, duplications, population = 500000)
  expect_equal(sum(res$distribution$percent), 100, tolerance = 1e-9)
  expect_equal(sum(res$distribution$people), 500000, tolerance = 1e-6)
  expect_equal(res$distribution$cumulative_probability[1], 1, tolerance = 1e-12)
  expect_true(all(diff(res$distribution$cumulative_probability) <= 0))
})

test_that("calc_canex does not round people, so every scale stays consistent", {
  inputs <- kim_two_vehicles()
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
  inputs <- kim_two_vehicles()
  res <- calc_canex(inputs$vehicles, inputs$duplications, population = 1000000)

  expect_output(print(res), "CANEX MODEL")
  # The distribution includes the zero-exposure row, so the printed average
  # must divide by the people reached and not by the population.
  expect_output(print(res),
                sprintf("Average exposures per person reached: %.2f", res$average_frequency))
})

test_that("calc_canex validates its inputs", {
  vehicles_ok <- data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502))
  dup_ok <- matrix(c(1, 0.0157, 0.0157, 1), nrow = 2)

  expect_error(calc_canex(data.frame(k = c(0, 2), R1 = c(0.1, 0.2), R2 = c(0.2, 0.3)),
                          matrix(c(1, 0, 0, 1), 2)), "positive integers")
  expect_error(calc_canex(vehicles_ok, matrix(1, nrow = 3, ncol = 2)), "2 x 2 matrix")
  expect_error(calc_canex(vehicles_ok, dup_ok, population = -10), "population")
  expect_error(calc_canex(vehicles_ok, dup_ok, population = NA), "population")
  expect_error(calc_canex(vehicles_ok[, c("k", "R1")], dup_ok), "columns k, R1 and R2")
  expect_error(calc_canex(transform(vehicles_ok, R1 = c(NA, 0.033)), dup_ok), "R1")
  asymmetric <- dup_ok
  asymmetric[1, 2] <- 0.02
  expect_error(calc_canex(vehicles_ok, asymmetric), "symmetric")
  expect_error(calc_canex(vehicles_ok, matrix(c(1, 1.5, 1.5, 1), 2)), "between 0 and 1")
})

test_that("calc_canex ignores the diagonal of the duplication matrix", {
  inputs <- kim_two_vehicles()
  with_na <- inputs$duplications
  diag(with_na) <- NA
  expect_equal(calc_canex(inputs$vehicles, with_na)$reach$percent,
               calc_canex(inputs$vehicles, inputs$duplications)$reach$percent)
})

test_that("calc_canex stops with an informative error if the combination grid is excessive", {
  big_vehicles <- data.frame(k = rep(20, 5), R1 = rep(0.3, 5), R2 = rep(0.4, 5))
  big_dup <- matrix(0.05, 5, 5)
  diag(big_dup) <- 1

  expect_error(calc_canex(big_vehicles, big_dup), "exposure combinations")
})
