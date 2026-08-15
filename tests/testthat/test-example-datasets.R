test_that("cada dataset de ejemplo funciona con do.call() sobre su función correspondiente", {
  data(sainsbury); data(binomial_plan); data(beta_binomial); data(metheringham)
  data(hofmans); data(agostini); data(MBBD); data(canex); data(nbd)
  data(grps); data(cpm); data(roas)

  expect_s3_class(do.call(calc_sainsbury, sainsbury), "reach_sainsbury")
  expect_s3_class(do.call(calc_binomial, binomial_plan), "reach_binomial")
  expect_s3_class(do.call(calc_beta_binomial, beta_binomial), "reach_beta_binomial")
  expect_s3_class(do.call(calc_metheringham, metheringham), "reach_metheringham")
  expect_type(do.call(calc_hofmans, c(hofmans, list(show_steps = FALSE))), "list")
  expect_s3_class(do.call(calc_agostini, agostini), "reach_agostini")
  expect_s3_class(do.call(calc_MBBD, MBBD), "MBBD")
  expect_s3_class(do.call(calc_canex, canex), "reach_canex")
  expect_s3_class(do.call(calc_nbd, nbd), "reach_nbd")
  expect_type(do.call(calc_grps, grps), "list")
  expect_type(do.call(calc_cpm, cpm), "list")
  expect_s3_class(do.call(calcular_roas, c(roas, list(imprimir_resultados = FALSE))), "data.frame")
})

test_that("'binomial_plan' no se llama 'binomial' (evita enmascarar stats::binomial)", {
  expect_false(exists("binomial", where = asNamespace("mediaPlanR"), inherits = FALSE))
  data(binomial_plan)
  expect_true(is.function(stats::binomial))
})
