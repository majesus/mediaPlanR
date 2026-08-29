test_that("exact optimizer reports a global optimum", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(500, 400),
    insertions = c(1, 1), cost_per_insertion = c(10, 6)
  ), population = 1000)

  max_reach <- optimize_media_plan(plan, budget = 10, objective = "max_reach")
  expect_s3_class(max_reach, "media_optimization")
  expect_true(max_reach$global_optimum)
  expect_equal(unname(max_reach$allocation), c(1, 0))
  expect_lte(max_reach$spend, 10)

  min_cost <- optimize_media_plan(plan, budget = 10, objective = "min_cost",
                                  target_reach = 0.35)
  expect_equal(unname(min_cost$allocation), c(0, 1))
  expect_equal(min_cost$spend, 6)
  expect_true(min_cost$target_met)
})

test_that("optimizer never silently exceeds budget", {
  plan <- media_plan(data.frame(
    channel = c("A", "B", "C"), audience = c(600, 500, 400),
    insertions = c(1, 1, 1), cost_per_insertion = c(11, 7, 5)
  ), population = 1000)
  fit <- optimize_media_plan(plan, budget = 10)
  expect_lte(fit$spend, 10)
  expect_true(fit$metrics$totals$spend <= fit$budget)
})

