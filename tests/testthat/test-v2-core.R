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

test_that("sainsbury reach is a complete exact Poisson-binomial distribution", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(200, 300),
    insertions = c(2, 1), cost_per_insertion = c(1, 1)
  ), population = 1000)
  result <- estimate_reach(plan, "sainsbury")

  expect_s3_class(result, "media_reach")
  expect_equal(sum(result$distribution$probability), 1, tolerance = 1e-12)
  expect_equal(result$cumulative$probability[1], 1)
  expect_true(all(diff(result$cumulative$probability) <= 0))
  expect_equal(sum(result$distribution$contacts * result$distribution$probability),
               0.2 + 0.2 + 0.3, tolerance = 1e-12)
})

test_that("estimate_reach with the sainsbury model matches calc_sainsbury", {
  audiences <- c(300000, 400000, 200000)
  direct <- calc_sainsbury(audiences, 1000000)
  plan <- media_plan(data.frame(
    channel = LETTERS[1:3], audience = audiences,
    insertions = 1L, cost_per_insertion = 0
  ), 1000000)
  modern <- estimate_reach(plan)

  expect_equal(modern$reach$percent, direct$reach$percent, tolerance = 1e-12)
  expect_equal(modern$distribution$percent[-1],
               direct$distribution$percent, tolerance = 1e-12)
})

test_that("estimate_reach with the binomial model matches calc_binomial", {
  audiences <- c(300000, 400000, 200000)
  direct <- calc_binomial(audiences, 1000000)
  plan <- media_plan(data.frame(
    channel = LETTERS[1:3], audience = audiences,
    insertions = 1L, cost_per_insertion = 0
  ), 1000000)
  modern <- estimate_reach(plan, "binomial")

  expect_equal(modern$reach$percent, direct$reach$percent, tolerance = 1e-12)
  expect_equal(modern$distribution$percent[-1],
               direct$distribution$percent, tolerance = 1e-12)
})

test_that("calc_sainsbury/calc_binomial with insertions match estimate_reach exactly", {
  plan <- media_plan(data.frame(
    channel = c("TV", "Radio", "Digital"),
    audience = c(300000, 180000, 120000),
    insertions = c(4, 6, 10), cost_per_insertion = 1
  ), population = 1000000)

  sainsbury_direct <- calc_sainsbury(plan$data$audience, plan$population, plan$data$insertions)
  sainsbury_via_plan <- estimate_reach(plan, "sainsbury")
  expect_equal(sainsbury_via_plan$reach$percent, sainsbury_direct$reach$percent, tolerance = 1e-12)

  binomial_direct <- calc_binomial(plan$data$audience, plan$population, plan$data$insertions)
  binomial_via_plan <- estimate_reach(plan, "binomial")
  expect_equal(binomial_via_plan$reach$percent, binomial_direct$reach$percent, tolerance = 1e-12)
})

test_that("calc_sainsbury/calc_binomial handle all-zero insertions without NaNs", {
  zero_sainsbury <- calc_sainsbury(c(1000, 2000), 10000, insertions = c(0, 0))
  expect_equal(zero_sainsbury$reach$percent, 0)
  expect_length(zero_sainsbury$distribution$percent, 0)

  zero_binomial <- calc_binomial(c(1000, 2000), 10000, insertions = c(0, 0))
  expect_equal(zero_binomial$reach$percent, 0)
  expect_length(zero_binomial$distribution$percent, 0)

  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(1000, 2000),
    insertions = c(0, 0), cost_per_insertion = 1
  ), population = 10000)
  expect_equal(estimate_reach(plan, "sainsbury")$reach$percent, 0)
  expect_equal(estimate_reach(plan, "binomial")$reach$percent, 0)
})

test_that("Sainsbury handles many vehicles without exponential enumeration", {
  result <- calc_sainsbury(rep(1000, 100), 100000)
  expect_true(result$reach$percent > 0)
  expect_length(result$distribution$percent, 100)
})

test_that("audience metrics distinguish composition, rating and affinity", {
  result <- audience_metrics(300000, 180000, 1000000, 400000)
  expect_equal(result$target_composition, 0.6)
  expect_equal(result$affinity_index, 150)
  expect_equal(result$target_rating, 0.45)
  expect_equal(result$gross_rating, 0.3)
  # Affinity is the target rating over the gross rating, in index points
  expect_equal(result$affinity_index, result$target_rating / result$gross_rating * 100)
  # The former selectivity column was identical to affinity and is gone
  expect_false("selectivity_index" %in% names(result))
  expect_error(audience_metrics(300000, 350000, 1000000, 400000),
               "not logically compatible")
})

test_that("audience metrics recycle scalars, validate inputs and handle empty audiences", {
  result <- audience_metrics(c(300000, 180000, 0), c(180000, 90000, 0), 1000000, 400000)
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$target_composition[3]))
  expect_true(is.na(result$affinity_index[3]))
  expect_error(audience_metrics(c(1, 2), c(1, 2, 3), 10, 5), "common length")
  expect_error(audience_metrics(NA, 1, 10, 5), "gross_audience")
  expect_error(audience_metrics(5, 1, 10, "a"), "target_universe")
})

test_that("media_plan validates its columns, labels and target audience", {
  data <- data.frame(channel = c("A", "B"), audience = c(300, 200),
                     insertions = c(2, 3), cost_per_insertion = c(10, 5),
                     t1 = c(100, 50), t2 = c(10, 10))
  expect_s3_class(media_plan(data, 1000, target_audience = "t1"), "media_plan")
  expect_error(media_plan(data, 1000, target_audience = c("t1", "t2")), "target_audience")
  expect_error(media_plan(data, 1000, target_audience = "missing"), "Missing required columns")
  expect_error(media_plan(data, 1000, audience = 5), "audience must be the name")
  expect_error(media_plan(data, NA), "population")
  expect_error(media_plan(transform(data, channel = c("A", "A")), 1000), "unique")
  expect_error(media_plan(transform(data, audience = c(300, 2000)), 1000), "between zero and population")
  expect_error(media_plan(transform(data, insertions = c(2, 1.5)), 1000), "non-negative integers")
  expect_error(media_plan(transform(data, cost_per_insertion = c(-1, 5)), 1000), "cannot be negative")
  expect_error(media_plan(transform(data, t1 = c(500, 50)), 1000, target_audience = "t1"),
               "target audience")
  expect_error(media_plan(data, 1000, currency = NA), "currency")
})

test_that("the plan currency appears in printed output", {
  plan <- media_plan(data.frame(channel = "A", audience = 500, insertions = 1,
                                cost_per_insertion = 5), population = 1000, currency = "USD")
  expect_output(print(plan), "Currency: USD")
  expect_output(print(plan_metrics(plan)), "currency: USD")
  expect_output(print(optimize_media_plan(plan, budget = 5)), "USD")
})

test_that("plan_metrics adds reach-based totals only when reach is supplied", {
  plan <- media_plan(data.frame(channel = c("A", "B"), audience = c(300, 200),
                                insertions = c(2, 3), cost_per_insertion = c(10, 5)),
                     population = 1000)
  base <- plan_metrics(plan)
  expect_null(base$totals$reach)
  with_reach <- plan_metrics(plan, reach = estimate_reach(plan)$reach$people)
  expect_equal(with_reach$totals$average_frequency,
               with_reach$totals$impressions / with_reach$totals$reach)
  expect_equal(with_reach$totals$cost_per_thousand_reached,
               with_reach$totals$spend / with_reach$totals$reach * 1000)
  expect_error(plan_metrics(plan, reach = 2000), "reach")
  expect_error(plan_metrics(list()), "media_plan")
})

test_that("compare_reach_models aligns one row per model with consistent reach figures", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(300000, 200000),
    insertions = c(2, 3), cost_per_insertion = c(1, 1)
  ), population = 1000000)

  comparison <- compare_reach_models(plan, c("sainsbury", "binomial"))

  expect_s3_class(comparison, "data.frame")
  expect_equal(comparison$model, c("sainsbury", "binomial"))
  expect_equal(comparison$reach_percent, comparison$reach_probability * 100)
  expect_equal(comparison$reach_people,
               comparison$reach_probability * plan$population)
  expect_true(all(comparison$reach_probability > 0 &
                     comparison$reach_probability <= 1))

  expect_error(compare_reach_models(plan, "not_a_model"), "Unknown reach model")
})
