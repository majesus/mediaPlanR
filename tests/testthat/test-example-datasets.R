test_that("cada dataset de ejemplo funciona con do.call() sobre su función correspondiente", {
  data(sainsbury_example); data(binomial_plan); data(beta_binomial_example)
  data(metheringham_example); data(hofmans_example); data(agostini_example)

  expect_s3_class(do.call(calc_sainsbury, sainsbury_example), "reach_sainsbury")
  expect_s3_class(do.call(calc_binomial, binomial_plan), "reach_binomial")
  expect_s3_class(do.call(calc_beta_binomial, beta_binomial_example), "reach_beta_binomial")
  expect_s3_class(do.call(calc_metheringham, metheringham_example), "reach_metheringham")
  expect_type(do.call(calc_hofmans, c(hofmans_example, list(show_steps = FALSE))), "list")
  expect_s3_class(do.call(calc_agostini, agostini_example), "reach_agostini")
})

test_that("'binomial_plan' no se llama 'binomial' (evita enmascarar stats::binomial)", {
  expect_false(exists("binomial", where = asNamespace("mediaPlanR"), inherits = FALSE))
  data(binomial_plan)
  expect_true(is.function(stats::binomial))
})
