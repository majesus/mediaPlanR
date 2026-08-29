test_that("every example dataset works with do.call() on its matching function", {
  data(ratings_example); data(beta_binomial_example)
  data(metheringham_example); data(hofmans_accumulation_example)
  data(hofmans_duplication_example)

  expect_s3_class(do.call(calc_sainsbury, ratings_example), "reach_sainsbury")
  expect_s3_class(do.call(calc_binomial, ratings_example), "reach_binomial")
  expect_s3_class(do.call(calc_agostini_duplication, ratings_example), "reach_agostini_duplication")
  expect_s3_class(do.call(calc_beta_binomial, beta_binomial_example), "reach_beta_binomial")
  expect_s3_class(do.call(calc_metheringham, metheringham_example), "reach_metheringham")
  expect_type(do.call(calc_hofmans_accumulation,
                       c(hofmans_accumulation_example, list(show_steps = FALSE))), "list")
  expect_s3_class(do.call(calc_hofmans_duplication, hofmans_duplication_example),
                   "reach_hofmans_duplication")
})

test_that("mediaPlanR does not export anything called 'binomial' (would mask stats::binomial)", {
  expect_false(exists("binomial", where = asNamespace("mediaPlanR"), inherits = FALSE))
  expect_true(is.function(stats::binomial))
})
