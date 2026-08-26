test_that("media_plan validates units and plan_metrics uses consistent denominators", {
  plan <- media_plan(data.frame(
    channel = c("TV", "Digital"),
    audience = c(300000, 200000),
    insertions = c(2, 3),
    cost_per_insertion = c(10000, 3000),
    target = c(150000, 100000)
  ), population = 1000000, target_audience = "target")

  metrics <- plan_metrics(plan)
  expect_s3_class(plan, "media_plan")
  expect_equal(metrics$totals$impressions, 1200000)
  expect_equal(metrics$totals$spend, 29000)
  expect_equal(metrics$totals$grps, 120)
  expect_equal(metrics$by_channel$cost_per_rating_point[1], 20000 / 60)
  expect_true(all(metrics$by_channel$target_audience <= metrics$by_channel$audience))
})

test_that("independent reach is a complete exact Poisson-binomial distribution", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(200, 300),
    insertions = c(2, 1), cost_per_insertion = c(1, 1)
  ), population = 1000)
  result <- estimate_reach(plan, "independent")

  expect_s3_class(result, "media_reach")
  expect_equal(sum(result$distribution$probability), 1, tolerance = 1e-12)
  expect_equal(result$cumulative$probability[1], 1)
  expect_true(all(diff(result$cumulative$probability) <= 0))
  expect_equal(sum(result$distribution$contacts * result$distribution$probability),
               0.2 + 0.2 + 0.3, tolerance = 1e-12)
})

test_that("v2 independent model preserves legacy Sainsbury results", {
  audiences <- c(300000, 400000, 200000)
  legacy <- calc_sainsbury(audiences, 1000000)
  plan <- media_plan(data.frame(
    channel = LETTERS[1:3], audience = audiences,
    insertions = 1L, cost_per_insertion = 0
  ), 1000000)
  modern <- estimate_reach(plan)

  expect_equal(modern$reach$percent, legacy$reach$percent, tolerance = 1e-12)
  expect_equal(modern$distribution$percent[-1],
               legacy$distribution$percent, tolerance = 1e-12)
})

test_that("Sainsbury handles many vehicles without exponential enumeration", {
  result <- calc_sainsbury(rep(1000, 100), 100000)
  expect_true(result$reach$percent > 0)
  expect_length(result$distribution$percent, 100)
})

test_that("audience metrics distinguish composition from affinity", {
  result <- audience_metrics(300000, 180000, 1000000, 400000)
  expect_equal(result$target_composition, 0.6)
  expect_equal(result$affinity_index, 150)
  expect_equal(result$target_rating, 0.45)
  expect_error(audience_metrics(300000, 350000, 1000000, 400000),
               "not logically compatible")
})

test_that("NBD average frequency uses the analytic mean despite its open tail", {
  plan <- media_plan(data.frame(
    channel = "A", audience = 300, insertions = 2,
    cost_per_insertion = 1
  ), population = 1000)
  result <- estimate_reach(plan, "nbd", k = 0.5)
  expect_equal(result$average_frequency,
               result$parameters$mean_contacts / result$reach$probability,
               tolerance = 1e-12)
})

test_that("compare_reach_models aligns one row per model with consistent reach figures", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(300000, 200000),
    insertions = c(2, 3), cost_per_insertion = c(1, 1)
  ), population = 1000000)

  comparison <- compare_reach_models(plan, c("independent", "binomial"))

  expect_s3_class(comparison, "data.frame")
  expect_equal(comparison$model, c("independent", "binomial"))
  expect_equal(comparison$reach_percent, comparison$reach_probability * 100)
  expect_equal(comparison$reach_people,
               comparison$reach_probability * plan$population)
  expect_true(all(comparison$reach_probability > 0 &
                     comparison$reach_probability <= 1))

  expect_error(compare_reach_models(plan, "not_a_model"), "Unknown reach model")
})
