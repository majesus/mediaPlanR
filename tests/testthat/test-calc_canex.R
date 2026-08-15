test_that("calc_canex reproduce el valor de referencia de la implementacion original del modelo", {
  # Valores de referencia (misma parametrizacion que la calculadora original
  # del modelo CANEX): 2 vehiculos, k=2 cada uno, con duplicacion 0.0157.
  vehicles <- data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502))
  duplications <- matrix(c(1, 0.0157, 0.0157, 1), nrow = 2, byrow = TRUE)

  res <- calc_canex(vehicles, duplications, poblacion = 1000000)

  expect_s3_class(res, "reach_canex")
  expect_equal(res$total_reach * 100, 60.2057, tolerance = 1e-3)
  expect_equal(res$total_reach_people, 602057, tolerance = 1)
  expect_equal(res$stats$avg_contacts, 1.7380, tolerance = 1e-3)
})

test_that("la distribucion de calc_canex siempre suma 100% (incluso tras truncar probabilidades negativas)", {
  vehicles <- data.frame(k = c(2, 2, 2),
                          R1 = c(0.4902, 0.033, 0.03),
                          R2 = c(0.5805, 0.0502, 0.0371))
  duplications <- matrix(c(1, 0.0157, 0.0139,
                            0.0157, 1, 0.0003,
                            0.0139, 0.0003, 1), nrow = 3, byrow = TRUE)

  res <- calc_canex(vehicles, duplications, poblacion = 500000)
  expect_equal(sum(res$distribution$percentage), 100, tolerance = 1e-6)
  expect_equal(sum(res$distribution$people), 500000, tolerance = 1)
})

test_that("print.reach_canex se dispara correctamente vía print() (regresion del bug de clase S3 ausente)", {
  vehicles <- data.frame(k = c(1, 1), R1 = c(0.3, 0.2), R2 = c(0.4, 0.3))
  duplications <- matrix(c(1, 0.05, 0.05, 1), nrow = 2)
  res <- calc_canex(vehicles, duplications)

  expect_output(print(res), "MODELO CANEX")
})

test_that("calc_canex valida los datos de entrada", {
  vehicles_ok <- data.frame(k = c(2, 2), R1 = c(0.4902, 0.033), R2 = c(0.5805, 0.0502))
  dup_ok <- matrix(c(1, 0.0157, 0.0157, 1), nrow = 2)

  expect_error(calc_canex(data.frame(k = c(0, 2), R1 = c(0.1, 0.2), R2 = c(0.2, 0.3)),
                           matrix(c(1, 0, 0, 1), 2)),
               "entero positivo")
  expect_error(calc_canex(vehicles_ok, matrix(1, nrow = 3, ncol = 2)),
               "cuadrada")
  expect_error(calc_canex(vehicles_ok, dup_ok, poblacion = -10),
               "positivo")
})

test_that("calc_canex se detiene con un error informativo si la rejilla de combinaciones es excesiva", {
  big_vehicles <- data.frame(k = rep(20, 5), R1 = rep(0.3, 5), R2 = rep(0.4, 5))
  big_dup <- matrix(0.05, 5, 5)
  diag(big_dup) <- 1

  expect_error(calc_canex(big_vehicles, big_dup), "combinaciones de exposicion")
})
