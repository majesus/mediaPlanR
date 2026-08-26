test_that("no file under R/ executes module-level code (avoids launching Shiny apps or calculations on package load)", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ directory not found (skipped outside the source tree)")

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)

  is_safe_top_level <- function(expr) {
    if (!is.call(expr)) return(TRUE)
    fn <- as.character(expr[[1]])[1]
    fn %in% c("<-", "=", "<<-", "{", "if", "for", "while",
              "library", "require", "suppressPackageStartupMessages",
              "options", "::", ":::")
  }

  offenders <- character(0)
  for (f in r_files) {
    exprs <- parse(f, keep.source = FALSE)
    for (e in as.list(exprs)) {
      if (!is_safe_top_level(e)) {
        offenders <- c(offenders, sprintf("%s: %s", basename(f), substring(deparse(e)[1], 1, 60)))
      }
    }
  }

  expect_length(offenders, 0)
})
