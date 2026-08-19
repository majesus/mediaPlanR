kim_observed_distribution <- function() {
  data.frame(
    contacts = 0:6,
    observed = c(38.41, 17.89, 39.66, 2.67, 1.36, 0, 0)
  )
}

kim_predicted_distribution <- function() {
  data.frame(
    contacts = 0:6,
    predicted = c(38.20, 18.57, 39.18, 2.49, 1.51, 0.05, 0.01)
  )
}

test_that("Kim's published example reproduces AER and APE", {
  evaluation <- evaluate_exposure_model(
    kim_observed_distribution(), kim_predicted_distribution(),
    observed_scale = "percent", predicted_scale = "percent"
  )

  expect_s3_class(evaluation, "exposure_model_evaluation")
  expect_equal(evaluation$summary$kim_aer, 0.003409644422795712,
               tolerance = 1e-12)
  expect_equal(evaluation$summary$kim_ape, 0.02516642312063641,
               tolerance = 1e-12)
  expect_equal(evaluation$summary$schedules, 1)
  expect_equal(evaluation$input_mass$observed_input_mass, 0.9999)
  expect_equal(evaluation$input_mass$predicted_input_mass, 1.0001)
  expect_output(print(evaluation), "Kim AER")
})

test_that("observations can be evaluated directly against a CSD result", {
  data(csd_kim2005)
  fit <- do.call(calc_csd, csd_kim2005)
  evaluation <- evaluate_exposure_model(
    kim_observed_distribution(), fit, observed_scale = "percent"
  )

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
  observed <- kim_observed_distribution()
  predicted <- kim_predicted_distribution()

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
      kim_observed_distribution(), nbd, observed_scale = "percent"
    ),
    "open tail"
  )
})
