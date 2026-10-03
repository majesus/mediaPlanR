# Kim (2005), Appendix B: 40 two-vehicle, two-insertion plans with published
# CANX, MSAD and CSD distributions (percent, zero to four contacts). The
# duplication is not printed; `dup` in the fixture is the one that reproduces
# the published CANX column, so the CANEX check is a consistency check of the
# fitted duplication and the MSAD and CSD checks are independent comparisons.
# Bounds are absolute, in percentage points, per cell.

kim_appendix_b_deviation <- function(model) {
  skip_if_no_fixture("kim2005_appB_models.csv")
  plans <- utils::read.csv(test_path("fixtures", "kim2005_appB_models.csv"))
  vapply(seq_len(nrow(plans)), function(i) {
    plan <- plans[i, ]
    R1 <- c(plan$R1_1, plan$R1_2)
    R2 <- c(plan$R2_1, plan$R2_2)
    dup <- matrix(c(plan$R1_1, plan$dup, plan$dup, plan$R1_2), 2)
    distribution <- suppressWarnings(switch(
      model,
      canex = calc_canex(data.frame(k = 2, R1 = R1, R2 = R2), dup)$distribution$probability,
      csd = calc_csd(data.frame(insertions = 2, R1 = R1, R2 = R2), dup,
                     "given")$distribution$probability,
      msad = calc_msad(data.frame(insertions = 2, R1 = R1, R2 = R2), dup,
                       "given")$distribution$probability
    ))
    modelled <- 100 * c(distribution, rep(0, 5))[1:5]
    max(abs(modelled - unlist(plan[paste0(model, 0:4)])))
  }, numeric(1))
}

test_that("CANEX reproduces the published CANX column of Kim (2005, Appendix B)", {
  expect_lt(max(kim_appendix_b_deviation("canex")), 0.02)
})

test_that("CSD reproduces the published CSD column of Kim (2005, Appendix B)", {
  expect_lt(max(kim_appendix_b_deviation("csd")), 0.02)
})

test_that("MSAD reproduces 39 Kim schedules using the common reconstructed inputs", {
  deviation <- kim_appendix_b_deviation("msad")
  # Plan 19 is tested separately: its printed MSAD row matches random
  # duplication, not the duplication reconstructed from the CANX column.
  expect_lt(max(deviation[-19]), 0.02)
  expect_gt(deviation[19], 0.09)
})

test_that("Kim plan 19's printed MSAD row matches random duplication", {
  skip_if_no_fixture("kim2005_appB_models.csv")
  plans <- utils::read.csv(test_path("fixtures", "kim2005_appB_models.csv"))
  plan <- plans[plans$plan == 19, ]
  R1 <- c(plan$R1_1, plan$R1_2)
  R2 <- c(plan$R2_1, plan$R2_2)
  independent_dup <- prod(R1)
  dup <- matrix(c(NA, independent_dup, independent_dup, NA), 2)
  fit <- calc_msad(data.frame(insertions = 2, R1 = R1, R2 = R2), dup, "given")
  published <- as.numeric(plan[paste0("msad", 0:4)])
  expect_equal(round(100 * fit$distribution$probability, 2), published)
  expect_lt(max(abs(100 * fit$distribution$probability - published)), 0.005)
  # This is evidence for a different input in the printed MSAD calculation,
  # not proof of why the source would have used it. Keep the common fixture.
})
