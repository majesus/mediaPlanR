test_that("the internal validators word their messages clearly", {
  expect_error(mediaPlanR:::assert_number(NA, "x"), "x must be one finite number\\.")
  expect_error(mediaPlanR:::assert_number(-1, "x", min = 0), "x must be one finite non-negative number\\.")
  expect_error(mediaPlanR:::assert_number(0, "x", min = 0, min_open = TRUE),
               "x must be one finite positive number\\.")
  expect_error(mediaPlanR:::assert_number(0, "x", min = 1, integer = TRUE),
               "x must be one finite positive integer\\.")
  expect_error(mediaPlanR:::assert_number(2, "x", min = 0, max = 1),
               "x must be one finite number in \\[0, 1\\]\\.")
  expect_error(mediaPlanR:::assert_number(1.5, "x", integer = TRUE), "x must be one finite integer\\.")
  expect_error(mediaPlanR:::assert_number(Inf, "x"), "finite")
  expect_silent(mediaPlanR:::assert_number(Inf, "x", allow_inf = TRUE))
  expect_error(mediaPlanR:::assert_numeric_vector(c(1, NA), "v"), "v must be a numeric vector of finite numbers\\.")
  expect_error(mediaPlanR:::assert_numeric_vector(1, "v", length = 3),
               "v must be a numeric vector of length 3 with finite numbers\\.")
  expect_error(mediaPlanR:::assert_numeric_vector(1, "v", min_length = 2),
               "of length at least 2")
  expect_error(mediaPlanR:::assert_flag(NA, "f"), "f must be TRUE or FALSE")
  expect_error(mediaPlanR:::assert_symmetric_matrix(matrix(1:6, 2), "m", 2), "2 x 2")
})

test_that("missing or malformed numeric arguments give package errors, never cryptic base R errors", {
  plan <- media_plan(data.frame(channel = c("A", "B"), audience = c(300, 200),
                                insertions = c(2, 3), cost_per_insertion = c(10, 5)),
                     population = 1000)
  vd <- data.frame(insertions = c(2, 2), R1 = c(0.3, 0.2), R2 = c(0.4, 0.3))
  dup <- matrix(c(NA, 0.08, 0.08, NA), 2)
  dup_people <- matrix(c(NA, 1.4e5, 1.4e5, NA), 2)
  duplication_metheringham <- matrix(c(1.5e5, 2e5, 2e5, 1.2e5), 2)

  calls <- list(
    list("calc_sainsbury", calc_sainsbury, list(audiences = c(3e5, 4e5), population = 1e6, insertions = c(1, 2))),
    list("calc_binomial", calc_binomial, list(audiences = c(3e5, 4e5), population = 1e6, insertions = c(1, 2))),
    list("calc_beta_binomial", calc_beta_binomial, list(A1 = 5e5, A2 = 5.5e5, P = 1e6, n = 5)),
    list("calc_metheringham", calc_metheringham, list(audiences = c(1.5e6, 8e5), insertions = c(2, 2),
                                 duplication_matrix = duplication_metheringham, population = 1e7)),
    list("calc_agostini_duplication", calc_agostini_duplication, list(audiences = c(3e5, 4e5), population = 1e6,
                                         duplication_matrix = dup_people, k = 1.125)),
    list("calc_hofmans_duplication", calc_hofmans_duplication, list(audiences = c(3e5, 4e5), population = 1e6,
                                        duplication_matrix = dup_people)),
    list("calc_hofmans_accumulation", calc_hofmans_accumulation, list(R1 = 0.06, R2 = 0.103, N = 5)),
    list("calc_canex", calc_canex, list(vehicles_data = data.frame(k = c(2, 2), R1 = c(0.3, 0.2), R2 = c(0.4, 0.3)),
                          duplications = dup, population = 1e6)),
    list("calc_csd", calc_csd, list(vehicles_data = vd, duplications = dup, population = 1, tolerance = 1e-10)),
    list("calc_msad", calc_msad, list(vehicles_data = vd, duplications = dup, population = 1, tolerance = 1e-10)),
    list("calc_cbd", calc_cbd, list(vehicles_data = vd, duplications = dup, population = 1, tolerance = 1e-8)),
    list("calc_mbd", calc_mbd, list(vehicles_data = vd, duplications = dup, population = 1, tolerance = 1e-8)),
    list("fit_bbd_to_reach", fit_bbd_to_reach, list(insertions = c(5, 7), audiences = c(5e5, 5.5e5), reach = 8e5,
                                universe = 1e6, precision = 100, max_iter = 100)),
    list("calibrate_bbd", calibrate_bbd, list(first_reach = 0.3, target_reach = 0.2, frequency = 2,
                             max_insertions = 6, tolerance = 1e-6)),
    list("nbd_exposure_distribution", nbd_exposure_distribution, list(mean_contacts = 2.5, size = 1.7, report_max = 10,
                                         tail_tolerance = 1e-6)),
    list("fit_nbd_exposure", fit_nbd_exposure, list(counts = c(0, 0, 1, 2, 5, 0), conf_level = 0.95)),
    list("audience_metrics", audience_metrics, list(gross_audience = c(300, 200), target_audience = c(150, 80),
                                gross_universe = 1000, target_universe = 400)),
    list("media_plan", media_plan, list(data = plan$data, population = 1000)),
    list("plan_metrics", plan_metrics, list(plan = plan, reach = 400)),
    list("optimize_media_plan", optimize_media_plan, list(plan = plan, budget = 40, effective_frequency = 1,
                                   max_insertions = c(2, 3), max_combinations = 1e5))
  )
  package_message <- paste0("must|cannot|requires|Missing|supports|incompatible|",
                            "not logically|outside|exceeds|between|at least|not consistent")
  bad_values <- list(NA_real_, NaN, "a", numeric(0))

  for (call in calls) {
    fn_name <- call[[1]]
    fn <- call[[2]]
    args <- call[[3]]
    for (argument in names(args)) {
      if (is.data.frame(args[[argument]]) || is.matrix(args[[argument]]) ||
          inherits(args[[argument]], "media_plan")) next
      for (bad in bad_values) {
        modified <- args
        modified[[argument]] <- bad
        error <- tryCatch({
          suppressWarnings(do.call(fn, modified))
          NULL
        }, error = function(e) conditionMessage(e))
        label <- sprintf("%s(%s = %s)", fn_name, argument, deparse(bad))
        info <- paste(label, "->", if (is.null(error)) "no error" else error)
        expect_false(is.null(error), info = info)
        if (!is.null(error)) expect_match(error, package_message, info = info)
      }
    }
  }
})
