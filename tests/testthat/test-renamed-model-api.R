test_that("the BBD-to-reach fit has an honest primary class", {
  fit <- fit_bbd_to_reach(
    c(5, 7, 4), c(500000, 550000, 600000),
    RM = 550000, universe = 1000000, A0 = 0.1
  )
  legacy <- calc_MBBD(
    c(5, 7, 4), c(500000, 550000, 600000),
    RM = 550000, universe = 1000000, A0 = 0.1
  )
  expect_s3_class(fit, "bbd_reach_fit")
  expect_s3_class(fit, "MBBD")
  expect_equal(fit$contact_distribution, legacy$contact_distribution)
})

test_that("the BBD-to-reach fit handles both theoretical boundary models", {
  insertions <- 2
  audiences <- 200000
  universe <- 1000000

  polarized <- fit_bbd_to_reach(
    insertions, audiences, RM = 200000, universe = universe, A0 = 0.1
  )
  binomial <- fit_bbd_to_reach(
    insertions, audiences, RM = 360000, universe = universe, A0 = 0.1
  )

  expect_identical(polarized$parameters$fit_type, "polarized_limit")
  expect_equal(polarized$contact_distribution, c(0.8, 0, 0.2))
  expect_identical(binomial$parameters$fit_type, "binomial_limit")
  expect_equal(binomial$contact_distribution, stats::dbinom(0:2, 2, 0.2))
  expect_error(
    fit_bbd_to_reach(
      insertions, audiences, RM = 150000, universe = universe, A0 = 0.1
    ),
    "feasible Beta-Binomial interval"
  )
})

test_that("descriptive example datasets execute their matching models", {
  data(canex_example)
  data(nbd_example)
  data(mbbd_example)
  data(msad_example)
  data(csd_example)

  expect_s3_class(do.call(calc_canex, canex_example), "reach_canex")
  expect_s3_class(do.call(calc_nbd, nbd_example), "reach_nbd")
  expect_s3_class(do.call(fit_bbd_to_reach, mbbd_example), "bbd_reach_fit")
  expect_s3_class(do.call(calc_msad, msad_example), "reach_msad")
  expect_s3_class(do.call(calc_csd, csd_example), "reach_csd")
})
