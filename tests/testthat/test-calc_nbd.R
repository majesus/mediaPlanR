test_that("calc_nbd con k directo devuelve una distribucion que suma 100% y una cobertura coherente", {
  res <- calc_nbd(c(300000, 400000, 200000), c(3, 2, 4), 1000000, k = 1.7)

  expect_s3_class(res, "reach_nbd")
  expect_equal(sum(res$distribucion$porcentaje), 100, tolerance = 1e-9)
  expect_equal(sum(res$distribucion$personas), 1000000, tolerance = 1)
  expect_true(res$reach$porcentaje > 0 && res$reach$porcentaje < 100)
})

test_that("la calibracion via reach_conocida reproduce EXACTAMENTE la cobertura objetivo (no una aproximacion)", {
  # Regresion: una version anterior renormalizaba la distribucion truncada,
  # lo que desplazaba la cobertura resultante lejos del objetivo (57.5% en
  # vez de 60%). El diseño correcto acumula la cola en el ultimo tramo sin
  # tocar P(X=0), así que la cobertura debe coincidir de forma exacta.
  res <- calc_nbd(c(300000, 400000, 200000), c(3, 2, 4), 1000000, reach_conocida = 0.60)
  expect_equal(res$reach$porcentaje / 100, 0.60, tolerance = 1e-9)
})

test_that("el limite k=1 reproduce la formula geometrica cerrada P(X=0) = 1/(1+m)", {
  # m = (300000*5)/1000000 = 1.5 -> con k=1 (geometrica), reach = 1 - 1/(1+m) = 0.6
  res <- calc_nbd(300000, 5, 1000000, k = 1)
  expect_equal(res$reach$porcentaje / 100, 0.6, tolerance = 1e-9)
})

test_that("calc_nbd valida sus argumentos", {
  audiencias <- c(300000, 400000, 200000)
  inserciones <- c(3, 2, 4)
  expect_error(calc_nbd(audiencias, inserciones, 1000000), "'k' o 'reach_conocida'")
  expect_error(calc_nbd(audiencias, inserciones, 1000000, k = 1, reach_conocida = 0.5), "no ambos")
  expect_error(calc_nbd(audiencias, inserciones, 1000000, reach_conocida = 0.999999), "no es alcanzable")
  expect_error(calc_nbd(audiencias, inserciones, 1000000, k = -1), "positivo")
  expect_error(calc_nbd(audiencias, inserciones, 1000000, reach_conocida = 1.5), "entre 0 y 1")
})

test_that("max_contactos por defecto es sum(inserciones) y el ultimo tramo agrupa la cola ('N o mas')", {
  res <- calc_nbd(c(300000, 400000, 200000), c(3, 2, 4), 1000000, k = 0.5)
  expect_equal(length(res$distribucion$porcentaje), sum(c(3, 2, 4)) + 1)
  expect_equal(res$parametros$max_contactos, sum(c(3, 2, 4)))
})

test_that("print.reach_nbd se dispara via print() y etiqueta el ultimo tramo como 'o mas'", {
  res <- calc_nbd(c(1e5, 2e5), c(2, 3), 1e6, k = 1)
  expect_output(print(res), "MODELO NBD")
  expect_output(print(res), "o más contactos")
})
