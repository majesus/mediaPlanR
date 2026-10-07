sequential_fits <- function(population = 1) {
  data(csd_example, package = "mediaPlanR", envir = environment())
  data(mbd_example, package = "mediaPlanR", envir = environment())
  list(
    csd = do.call(calc_csd, c(csd_example, population = population)),
    msad = do.call(calc_msad, c(csd_example, population = population)),
    cbd = do.call(calc_cbd, c(csd_example, population = population)),
    mbd = do.call(calc_mbd, c(mbd_example, population = population))
  )
}

test_that("the sequential models print a full report with full = TRUE", {
  for (fit in sequential_fits()) {
    out <- capture.output(returned <- print(fit, full = TRUE))
    expect_identical(returned, fit)
    expect_true(any(grepl("^HEADLINE METRICS:", out)))
    expect_true(any(grepl("^MODEL PARAMETERS:", out)))
    expect_true(any(grepl("^EXPOSURE DISTRIBUTION:", out)))
    expect_true(any(grepl("^CUMULATIVE DISTRIBUTION:", out)))
    expect_true(any(grepl("^DIAGNOSTICS:", out)))
    expect_true(any(grepl(
      sprintf("^Total reach: %.2f%%$", fit$reach$percent), out
    )))
    # Every exposure level from 1 to N is listed (population 1: no people).
    n <- nrow(fit$distribution) - 1L
    expect_true(any(grepl(sprintf("^%d exposures?: ", n), out)))
    expect_false(any(grepl("people", out)))
  }
})

test_that("the full report expresses cells as people when a population is given", {
  fit <- sequential_fits(population = 2e6)$csd
  out <- capture.output(print(fit, full = TRUE))
  expect_true(any(grepl(
    sprintf("^Total reach: %.2f%% [(]%.0f people[)]$",
            fit$reach$percent, fit$reach$people), out
  )))
  expect_true(any(grepl("^1 exposure: .* people[)]$", out)))
})

test_that("max_rows cuts long distributions with an explicit note", {
  fit <- sequential_fits()$cbd
  levels <- nrow(fit$distribution) - 1L
  out <- capture.output(print(fit, full = TRUE, max_rows = 2))
  expect_true(any(grepl(
    sprintf("%d further exposure levels [(]3 to %d[)] not shown", levels - 2L, levels),
    out
  )))
  expect_false(any(grepl(sprintf("^%d exposures: ", levels), out)))
  all_rows <- capture.output(print(fit, full = TRUE, max_rows = Inf))
  expect_false(any(grepl("not shown", all_rows)))
  expect_error(print(fit, full = TRUE, max_rows = 0), "max_rows")
})

test_that("the default print stays the compact summary", {
  for (fit in sequential_fits()) {
    out <- capture.output(print(fit))
    expect_lt(length(out), 8L)
    expect_false(any(grepl("HEADLINE METRICS", out)))
  }
})
