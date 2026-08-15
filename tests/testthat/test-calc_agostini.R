test_that("calc_agostini con k=1 coincide exactamente con el supuesto de independencia de calc_sainsbury", {
  audiencias <- c(300000, 400000, 200000)
  pob_total <- 1000000

  res_agostini <- calc_agostini(audiencias, pob_total, k = 1)
  res_sainsbury <- calc_sainsbury(audiencias, pob_total)

  expect_equal(res_agostini$reach$porcentaje, res_sainsbury$reach$porcentaje, tolerance = 1e-9)
})

test_that("calc_agostini devuelve un objeto de clase reach_agostini con print funcional", {
  res <- calc_agostini(c(300000, 400000, 200000), pob_total = 1000000, k = 0.9)
  expect_s3_class(res, "reach_agostini")
  expect_output(print(res), "MODELO DE AGOSTINI")
  expect_length(res$acumulada$personas, 3)
  expect_true(res$reach$personas <= 1000000)
})

test_that("calc_agostini valida los datos de entrada", {
  expect_error(calc_agostini(c(-1, 2), 100), "positivas")
  expect_error(calc_agostini(c(1, 2), 0), "positiv")
  expect_error(calc_agostini(c(1, 2), 100, k = -1), "no negativo")
})
