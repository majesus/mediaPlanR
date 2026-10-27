# Regressions for the final audit of 2026-10-09 (findings H03 and H04; H01 and
# H02 concerned the allocation optimizer, which is not part of this package).
# Each case carries a witness that does not use the code under test.

test_that("a tiny mean keeps its reach in the Negative-Binomial table (H03)", {
  for (size in c(1, 0.25, 40, Inf)) {
    tiny <- nbd_exposure_distribution(mean_contacts = 1e-17, size = size,
                                      report_max = 10)
    # P(at least one exposure) = mean to first order (1 - exp(-mean) with Poisson).
    expect_equal(tiny$reach$probability, 1e-17, tolerance = 1e-12)
    expect_equal(tiny$reach$probability, sum(tiny$distribution$probability[-1L]),
                 tolerance = 1e-12)
    expect_equal(tiny$reach$percent, 1e-15, tolerance = 1e-12)
    expect_equal(tiny$average_frequency, 1, tolerance = 1e-12)
    expect_true(is.finite(tiny$average_frequency))
  }
})

test_that("the stable reach agrees with the direct formula for ordinary means (H03)", {
  for (mean_contacts in c(0.05, 1, 2.5, 12)) {
    for (size in c(0.5, 1.7, 30, Inf)) {
      fit <- nbd_exposure_distribution(mean_contacts, size, report_max = 60)
      zero <- if (is.infinite(size)) stats::dpois(0, mean_contacts) else
        stats::dnbinom(0, size = size, mu = mean_contacts)
      expect_equal(fit$reach$probability, 1 - zero, tolerance = 1e-12)
      expect_equal(fit$reach$probability, sum(fit$distribution$probability[-1L]),
                   tolerance = 1e-12)
    }
  }
})

test_that("MBD and CBD report the reach as the sum of the positive cells (H03)", {
  data(csd_example, package = "mediaPlanR", envir = environment())
  cbd <- do.call(calc_cbd, csd_example)
  expect_equal(cbd$reach$probability, sum(cbd$distribution$probability[-1L]),
               tolerance = 1e-14)
  vehicles <- data.frame(insertions = c(2, 2), R1 = c(0.30, 0.20),
                         R2 = c(0.40, 0.30))
  mbd <- calc_mbd(vehicles, matrix(c(0, 0.15, 0.15, 0), nrow = 2),
                  aggregation_order = "given")
  expect_equal(mbd$reach$probability, sum(mbd$distribution$probability[-1L]),
               tolerance = 1e-14)
})

test_that("truncating negative probabilities need not change the mean (H04)", {
  # The vignette says that the mean *may* change, not that it did. Four
  # vehicles, one insertion each, audiences .5 and every pair duplication
  # .475. The data are compatible: a 90/10 mixture of four perfectly
  # coinciding Bernoulli(.5) variables and four independent ones has audience
  # .5 and pair duplication .9 * .5 + .1 * .25 = .475. Yet the canonical
  # expansion has negative cells; truncating and normalizing them leaves the
  # mean of the contacts at 2, because the data are symmetric.
  expect_equal(0.9 * 0.5 + 0.1 * 0.25, 0.475)
  vehicles <- data.frame(insertions = rep(1, 4), R1 = rep(0.5, 4),
                         R2 = rep(0.5, 4))
  duplications <- matrix(0.475, 4, 4)
  diag(duplications) <- 0
  expect_warning(
    fit <- calc_canex(vehicles, duplications),
    class = "mediaPlanR_canex_truncation"
  )
  expect_gt(fit$diagnostics$negative_mass_truncated, 0)
  expect_equal(fit$diagnostics$mean_exposures_expected, 2)
  expect_equal(fit$diagnostics$mean_exposures_result, 2, tolerance = 1e-9)
})

test_that("the vignette states the effect of truncation on the mean as a possibility (H04)", {
  skip_if_not(file.exists(test_path("..", "..", "vignettes", "mediaPlanR-intro.Rmd")),
              "vignette sources not available")
  vignette <- readLines(test_path("..", "..", "vignettes", "mediaPlanR-intro.Rmd"),
                        warn = FALSE)
  row <- grep("Warns\\*\\* about truncated probabilities", vignette, value = TRUE)
  expect_length(row, 1L)
  expect_false(grepl("and the mean number of exposures changed", row, fixed = TRUE))
  expect_match(row, "may have changed", fixed = TRUE)
})
