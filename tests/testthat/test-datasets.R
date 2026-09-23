test_that("every example dataset works with do.call() on its matching function", {
  data(ratings_example)
  data(beta_binomial_example)
  data(metheringham_example)
  data(hofmans_accumulation_example)
  data(duplication_example)
  data(canex_example)
  data(bbd_reach_example)
  data(csd_example)
  data(msad_example)
  data(mbd_example)

  expect_s3_class(do.call(calc_sainsbury, ratings_example), "reach_sainsbury")
  expect_s3_class(do.call(calc_binomial, ratings_example), "reach_binomial")
  expect_s3_class(do.call(calc_agostini_duplication, duplication_example),
                  "reach_agostini_duplication")
  expect_s3_class(do.call(calc_hofmans_duplication, duplication_example),
                  "reach_hofmans_duplication")
  expect_s3_class(do.call(calc_beta_binomial, beta_binomial_example), "reach_beta_binomial")
  expect_s3_class(do.call(calc_metheringham, metheringham_example), "reach_metheringham")
  expect_s3_class(do.call(calc_hofmans_accumulation, hofmans_accumulation_example),
                  "reach_hofmans_accumulation")
  expect_s3_class(do.call(calc_canex, canex_example), "reach_canex")
  expect_s3_class(do.call(fit_bbd_to_reach, bbd_reach_example), "bbd_reach_fit")
  expect_s3_class(do.call(calc_csd, csd_example), "reach_csd")
  expect_s3_class(do.call(calc_cbd, csd_example), "reach_cbd")
  expect_s3_class(do.call(calc_msad, msad_example), "reach_msad")
  expect_s3_class(do.call(calc_mbd, mbd_example), "reach_mbd")
})

test_that("the literature-validation datasets feed their matching functions", {
  data(csd_kim2005)
  data(msad_kim2005)
  data(mbd_cheong2007)
  expect_s3_class(do.call(calc_csd, csd_kim2005), "reach_csd")
  expect_s3_class(do.call(calc_msad, msad_kim2005), "reach_msad")
  expect_s3_class(do.call(calc_mbd, mbd_cheong2007), "reach_mbd")
})

test_that("the CANEX example is original, not Kim's published inputs", {
  data(canex_example)
  data(csd_kim2005)
  expect_false(any(canex_example$vehicles_data$R1 %in% csd_kim2005$vehicles_data$R1))
})

test_that("mediaPlanR does not export anything called binomial (it would mask stats::binomial)", {
  expect_false(exists("binomial", where = asNamespace("mediaPlanR"), inherits = FALSE))
  expect_true(is.function(stats::binomial))
})

test_that("dataset names that are not part of the package do not exist", {
  for (name in c("hofmans_duplication_example", "sainsbury_example", "agostini_example")) {
    expect_false(exists(name, envir = asNamespace("mediaPlanR"), inherits = FALSE), info = name)
  }
})
