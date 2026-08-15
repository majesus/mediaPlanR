test_that("calcular_roas calcula ROAS/ROI coherentes a partir de una audiencia efectiva", {
  res <- calcular_roas(
    audiencia_efectiva = 51000,
    precio_unidad = 2.50,
    margen_unidad = 1.14,
    inversion = 45000,
    imprimir_resultados = FALSE
  )

  expect_true(res$ROAS_Bruto > 0)
  expect_true(res$ROAS_Neto > 0)
  expect_true(res$ROAS_Bruto > res$ROAS_Neto)
})

test_that("el analisis de sensibilidad de calcular_roas no fuga variables al entorno global (regresion)", {
  # Nos aseguramos de que no exista ya una variable con ese nombre en el
  # entorno global antes del test.
  if (exists("tasa_visita", envir = .GlobalEnv, inherits = FALSE)) {
    skip("Ya existe 'tasa_visita' en .GlobalEnv; se omite para no interferir")
  }

  resultados <- calcular_roas(
    audiencia_efectiva = 55000,
    precio_unidad = 2.50,
    margen_unidad = 1.14,
    inversion = 45000,
    imprimir_resultados = FALSE,
    variaciones = list(tasa_visita = c(0.15, 0.20, 0.25))
  )

  expect_false(exists("tasa_visita", envir = .GlobalEnv, inherits = FALSE))
  expect_true(nrow(resultados$Analisis_Sensibilidad) == 3)
})
