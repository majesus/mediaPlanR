# Illustrative percentages (not data from any publication), complete to the
# rounding of the third decimal.
observed_distribution <- function() {
  data.frame(
    contacts = 0:6,
    observed = c(40.00, 18.00, 38.00, 2.50, 1.50, 0, 0)
  )
}

predicted_distribution <- function() {
  data.frame(
    contacts = 0:6,
    predicted = c(39.50, 18.60, 37.80, 2.40, 1.40, 0.20, 0.10)
  )
}

test_that("AER and APE follow their definitions on a worked example", {
  observed <- observed_distribution()
  predicted <- predicted_distribution()
  evaluation <- evaluate_exposure_model(
    observed, predicted, observed_scale = "percent", predicted_scale = "percent"
  )

  expect_s3_class(evaluation, "exposure_model_evaluation")
  # AER = |R_obs - R_pred| / R_obs and APE = sum_{j >= 1} |p_obs - p_pred| / R_obs,
  # computed here directly from the probabilities
  p_obs <- observed$observed / 100
  p_pred <- predicted$predicted / 100
  reach_obs <- 1 - p_obs[1]
  reach_pred <- 1 - p_pred[1]
  expect_equal(evaluation$summary$kim_aer, abs(reach_obs - reach_pred) / reach_obs,
               tolerance = 1e-12)
  expect_equal(evaluation$summary$kim_ape,
               sum(abs(p_obs[-1] - p_pred[-1])) / reach_obs, tolerance = 1e-12)
  expect_equal(evaluation$summary$schedules, 1)
  expect_equal(evaluation$input_mass$observed_input_mass, 1)
  expect_equal(evaluation$input_mass$predicted_input_mass, 1)
  expect_output(print(evaluation), "Kim AER")
})

test_that("observations can be evaluated directly against a CSD result", {
  data(csd_example)
  fit <- do.call(calc_csd, csd_example)
  observed <- data.frame(
    contacts = 0:9,
    observed = c(520, 150, 130, 80, 50, 40, 20, 5, 3, 2)
  )
  evaluation <- evaluate_exposure_model(observed, fit, observed_scale = "count")

  expect_identical(evaluation$inputs$prediction_source, "reach_csd")
  expect_equal(evaluation$by_schedule$predicted_reach,
               fit$reach$probability, tolerance = 1e-12)
  expect_true(evaluation$summary$kim_aer > 0)
  expect_true(evaluation$summary$kim_ape > 0)
})

test_that("count observations are normalized explicitly within schedule", {
  observed <- data.frame(
    contacts = 0:2,
    observed = c(70, 20, 10)
  )
  predicted <- data.frame(
    contacts = 0:2,
    predicted = c(0.65, 0.25, 0.10)
  )
  evaluation <- evaluate_exposure_model(
    observed, predicted,
    observed_scale = "count", predicted_scale = "probability"
  )

  expect_equal(evaluation$by_schedule$observed_reach, 0.3)
  expect_equal(sum(evaluation$aligned_distribution$observed_probability), 1)
})

test_that("multiple schedules produce averaged Kim metrics", {
  observed <- data.frame(
    schedule_id = rep(c("A", "B"), each = 3),
    contacts = rep(0:2, 2),
    observed = c(70, 20, 10, 50, 30, 20)
  )
  predicted <- data.frame(
    schedule_id = rep(c("A", "B"), each = 3),
    contacts = rep(0:2, 2),
    predicted = c(68, 22, 10, 55, 25, 20)
  )
  evaluation <- evaluate_exposure_model(
    observed, predicted,
    observed_scale = "percent", predicted_scale = "percent",
    schedule_col = "schedule_id"
  )

  expect_equal(evaluation$summary$schedules, 2)
  expect_equal(evaluation$summary$kim_aer,
               mean(evaluation$by_schedule$kim_relative_reach_error))
  expect_equal(evaluation$summary$kim_ape,
               mean(evaluation$by_schedule$kim_distribution_error))
})

test_that("evaluation rejects ambiguous or incomplete distributions", {
  observed <- observed_distribution()
  predicted <- predicted_distribution()

  expect_error(
    evaluate_exposure_model(observed, predicted,
                            observed_scale = "percent"),
    "predicted_scale must be declared"
  )
  expect_error(
    evaluate_exposure_model(observed[-1, ], predicted[-1, ],
                            observed_scale = "percent",
                            predicted_scale = "percent"),
    "from 0"
  )
  expect_error(
    evaluate_exposure_model(observed, predicted[-7, ],
                            observed_scale = "percent",
                            predicted_scale = "percent"),
    "supports differ"
  )
  incomplete <- observed
  incomplete$observed <- incomplete$observed / 10
  expect_error(
    evaluate_exposure_model(incomplete, predicted,
                            observed_scale = "percent",
                            predicted_scale = "percent"),
    "complete distribution"
  )
})

test_that("open NBD tails are not mistaken for exact contact cells", {
  nbd <- nbd_exposure_distribution(2, 1.5, report_max = 6)
  expect_error(
    evaluate_exposure_model(
      observed_distribution(), nbd, observed_scale = "percent"
    ),
    "open tail"
  )
})

test_that("count weights whose sum overflows are rejected instead of normalized to zero", {
  observed <- data.frame(contacts = 0:1, observed = c(1e308, 1e308))
  predicted <- data.frame(contacts = 0:1, predicted = c(0.5, 0.5))
  expect_error(
    evaluate_exposure_model(observed, predicted, observed_scale = "count",
                            predicted_scale = "probability"),
    "too large to be summed"
  )
})
