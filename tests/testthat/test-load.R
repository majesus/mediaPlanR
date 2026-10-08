# Installed-package checks in a fresh process. This measures specific state
# changes; it does not claim that a syntax whitelist proves absence of effects.
test_that("installed loading preserves process state and optional packages stay unloaded", {
  skip_on_cran()
  installed <- find.package("mediaPlanR")
  skip_if_not(file.exists(file.path(installed, "Meta", "package.rds")),
              "the package is not installed (running from the source tree)")
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  expect_true(file.exists(rscript), info = rscript)
  if (!file.exists(rscript)) return(invisible(NULL))
  script <- tempfile(fileext = ".R")
  writeLines(c(
    paste0(".libPaths(", paste(deparse(.libPaths()), collapse = ""), ")"),
    "workspace <- tempfile('mediaPlanR-load-'); dir.create(workspace); setwd(workspace)",
    "set.seed(7401)",
    "snapshot <- function() list(wd = getwd(), options = options(), seed = .Random.seed, files = list.files('.', all.files = TRUE, recursive = TRUE), connections = showConnections(all = TRUE))",
    "before <- snapshot()",
    sprintf("library(mediaPlanR, lib.loc = %s)", deparse(dirname(installed))),
    "after <- snapshot()",
    "stopifnot(identical(before, after))",
    "cat('STATE_UNCHANGED: TRUE\\n')",
    "detach('package:mediaPlanR')",
    "messages <- character()",
    sprintf(paste0("withCallingHandlers(suppressPackageStartupMessages(library(mediaPlanR, lib.loc = %s)), ",
                    "message = function(m) messages <<- c(messages, conditionMessage(m)))"),
            deparse(dirname(installed))),
    "stopifnot(length(messages) == 0L)",
    "cat('STARTUP_SUPPRESSIBLE: TRUE\\n')",
    "d <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.2), R2 = c(0.45, 0.3))",
    "m <- matrix(c(NA, 0.08, 0.08, NA), 2)",
    "for (f in c(calc_csd, calc_cbd, calc_msad, calc_mbd)) stopifnot(abs(sum(f(d, m)$distribution$probability) - 1) < 1e-9)",
    "names(d)[1] <- 'k'; stopifnot(is.finite(calc_canex(d, m)$average_frequency))",
    "stopifnot(is.finite(calc_beta_binomial(R1 = 0.5, R2 = 0.55, insertions = 5, population = 1e6)$reach$probability))",
    "cat('OPTIONAL_LOADED:', any(c('extraDistr', 'ggplot2') %in% loadedNamespaces()), '\\n')",
    sprintf(".libPaths(c(%s, .Library))", deparse(dirname(installed))),
    "if (!requireNamespace('ggplot2', quietly = TRUE)) stopifnot(is.null(calc_hofmans_accumulation(0.06, 0.103, 5)$plot))"
  ), script)
  output <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(script)),
                                     stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  info <- paste(output, collapse = "\n")
  expect_true(is.null(status) || status == 0, info = info)
  expect_true(any(grepl("^mediaPlanR [0-9]", output)), info = info)
  expect_true(any(grepl("STATE_UNCHANGED: TRUE", output)), info = info)
  expect_true(any(grepl("STARTUP_SUPPRESSIBLE: TRUE", output)), info = info)
  expect_true(any(grepl("OPTIONAL_LOADED: FALSE", output)), info = info)
})
