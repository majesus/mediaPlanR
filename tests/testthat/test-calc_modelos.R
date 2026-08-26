test_that("calc_sainsbury, calc_binomial y calc_beta_binomial devuelven coberturas coherentes (0-100%)", {
  audiencias <- c(300000, 400000, 200000)
  pob_total <- 1000000

  res_s <- calc_sainsbury(audiencias, pob_total)
  res_b <- calc_binomial(audiencias, pob_total)

  expect_s3_class(res_s, "reach_sainsbury")
  expect_s3_class(res_b, "reach_binomial")
  expect_true(res_s$reach$porcentaje > 0 && res_s$reach$porcentaje <= 100)
  expect_true(res_b$reach$porcentaje > 0 && res_b$reach$porcentaje <= 100)
  expect_equal(sum(res_s$distribucion$porcentaje), res_s$reach$porcentaje, tolerance = 1e-6)

  res_bb <- calc_beta_binomial(A1 = 500000, A2 = 550000, P = 1000000, n = 5)
  expect_s3_class(res_bb, "reach_beta_binomial")
  expect_true(res_bb$reach$porcentaje > 0 && res_bb$reach$porcentaje <= 100)
})

test_that("calc_metheringham calcula A1, D y A2 coherentes y valida sus inputs", {
  matriz_dup <- matrix(c(
    150000, 200000, 180000,
    200000, 120000, 140000,
    180000, 140000, 170000
  ), nrow = 3, byrow = TRUE)

  res <- calc_metheringham(
    audiencias = c(1500000, 800000, 1200000),
    inserciones = c(4, 3, 5),
    matriz_duplicacion = matriz_dup
  )

  expect_s3_class(res, "reach_metheringham")
  expect_true(res$audiencia_segunda > res$audiencia_media)

  expect_error(
    calc_metheringham(audiencias = c(0, 0), inserciones = c(0, 0), matriz_duplicacion = matrix(0, 2, 2)),
    "inserciones"
  )
})

test_that("calc_hofmans produce una cobertura monotona creciente y devuelve un grafico ggplot", {
  res <- calc_hofmans(0.06, 0.103, N = 5, show_steps = FALSE)
  expect_true(all(diff(res$results$RN) >= 0))
  expect_s3_class(res$plot, "ggplot")

  expect_error(calc_hofmans(0.1, 0.2, N = 5, show_steps = FALSE), "division por cero")
})
