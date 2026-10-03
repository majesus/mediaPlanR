# Run after R CMD INSTALL, using base/recommended packages only.
# This compatibility check is deliberately separate from the full testthat suite.
suppressPackageStartupMessages(library(mediaPlanR))
cat(R.version.string, "\n")
stopifnot(getRversion() >= "4.0.0")
d <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.2), R2 = c(0.45, 0.3))
m <- matrix(c(NA, 0.08, 0.08, NA), 2)
for (f in c(calc_csd, calc_msad, calc_cbd, calc_mbd)) {
  p <- f(d, m)$distribution$probability
  stopifnot(all(is.finite(p)), all(p >= 0), abs(sum(p) - 1) < 1e-12)
}
names(d)[1] <- "k"
stopifnot(abs(sum(calc_canex(d, m)$distribution$probability) - 1) < 1e-12)
stopifnot(abs(calc_sainsbury(c(0.3, 0.4), 1)$reach$percent - 58) < 1e-10)
stopifnot(abs(calc_binomial(c(0.3, 0.4), 1)$reach$percent - 57.75) < 1e-10)
stopifnot(abs(calc_beta_binomial(0.3, 0.45, 1, 2)$reach$percent - 45) < 1e-10)
for (dataset in list(metheringham_example, duplication_example)) {
  f <- if ("insertions" %in% names(dataset)) calc_metheringham else calc_hofmans_duplication
  stopifnot(is.finite(do.call(f, dataset)$reach$percent))
}
stopifnot(is.finite(do.call(calc_agostini_duplication, duplication_example)$reach$percent))
stopifnot(all(is.finite(calc_hofmans_accumulation(0.06, 0.103, 5)$results$RN)))
plan <- media_plan(data.frame(channel = c("A", "B"), audience = c(300, 200),
                              insertions = c(2, 2), cost_per_insertion = c(10, 5)), 1000)
stopifnot(plan_metrics(plan)$totals$impressions == 1000,
          is.finite(estimate_reach(plan)$reach$people))
stopifnot(optimize_media_plan(plan, budget = 40, max_insertions = c(2, 2),
                             method = "exact")$global_optimum)
tiny <- calc_canex(data.frame(k = c(1, 1), R1 = 1e-11, R2 = 1e-11),
                   matrix(c(NA, 1e-11, 1e-11, NA), 2), 1e11)
stopifnot(abs(tiny$reach$people - 1) < 1e-10,
          abs(tiny$average_frequency - 2) < 1e-10)
cat("Base-dependency compatibility checks passed.\n")
