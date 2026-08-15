test_that("ningun archivo de R/ ejecuta codigo a nivel de modulo (evita lanzar apps Shiny o calculos al cargar el paquete)", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "No se encuentra el directorio R/ (se omite fuera del arbol fuente)")

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)

  es_top_level_seguro <- function(expr) {
    if (!is.call(expr)) return(TRUE)
    fn <- as.character(expr[[1]])[1]
    fn %in% c("<-", "=", "<<-", "{", "if", "for", "while",
              "library", "require", "suppressPackageStartupMessages",
              "options", "::", ":::")
  }

  ofensores <- character(0)
  for (f in r_files) {
    exprs <- parse(f, keep.source = FALSE)
    for (e in as.list(exprs)) {
      if (!es_top_level_seguro(e)) {
        ofensores <- c(ofensores, sprintf("%s: %s", basename(f), substring(deparse(e)[1], 1, 60)))
      }
    }
  }

  expect_length(ofensores, 0)
})
