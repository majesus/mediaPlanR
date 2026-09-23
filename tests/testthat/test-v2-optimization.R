readme_plan <- function() {
  media_plan(data.frame(
    channel = c("TV", "Radio", "Digital"),
    audience = c(300000, 180000, 120000),
    insertions = c(4, 6, 10),
    cost_per_insertion = c(18000, 3500, 1200)
  ), population = 1000000)
}

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

test_that("the fast allocation evaluator equals estimate_reach for both models", {
  plan <- readme_plan()
  for (model in c("sainsbury", "binomial")) {
    for (allocation in list(c(0, 0, 0), c(1, 0, 0), c(2, 3, 5), c(4, 6, 10))) {
      for (frequency in c(1, 2, 4)) {
        fast <- mediaPlanR:::allocation_reach(plan, allocation, model, frequency)
        candidate <- plan
        candidate$data$insertions <- allocation
        full <- estimate_reach(candidate, model)
        idx <- match(frequency, full$cumulative$min_contacts)
        expected <- if (is.na(idx)) 0 else full$cumulative$probability[idx]
        expect_equal(fast$reach, full$reach$probability, tolerance = 1e-12)
        expect_equal(fast$effective_reach, expected, tolerance = 1e-12)
      }
    }
  }
})

test_that("exact search matches an independent enumeration for effective frequencies above one", {
  plan <- media_plan(data.frame(
    channel = c("A", "B", "C"), audience = c(400, 250, 150),
    insertions = c(3, 3, 3), cost_per_insertion = c(7, 3, 1)
  ), population = 1000)
  budget <- 20
  for (frequency in c(1, 2, 3)) {
    fit <- optimize_media_plan(plan, budget = budget, effective_frequency = frequency,
                               method = "exact")
    grid <- as.matrix(expand.grid(0:3, 0:3, 0:3))
    grid <- grid[as.vector(grid %*% c(7, 3, 1)) <= budget, , drop = FALSE]
    best <- max(apply(grid, 1, function(a) {
      candidate <- plan
      candidate$data$insertions <- a
      result <- estimate_reach(candidate)
      idx <- match(frequency, result$cumulative$min_contacts)
      if (is.na(idx)) 0 else result$cumulative$probability[idx]
    }))
    expect_equal(fit$effective_reach, best, tolerance = 1e-12)
    expect_true(fit$global_optimum)
  }
})

test_that("greedy optimizer skips zero-cost channels with no reach gain", {
  plan <- media_plan(data.frame(
    channel = c("zero", "useful"), audience = c(0, 500),
    insertions = c(1, 1), cost_per_insertion = c(0, 5)
  ), population = 1000)

  fit <- optimize_media_plan(
    plan, budget = 5, max_insertions = c(1, 1), method = "greedy"
  )

  expect_equal(unname(fit$allocation), c(0, 1))
  expect_equal(fit$reach$reach$probability, 0.5)
})

test_that("greedy optimizer finds effective reach when the effective frequency is above one", {
  plan <- readme_plan()
  for (frequency in c(2, 3)) {
    greedy <- optimize_media_plan(plan, budget = 60000, effective_frequency = frequency,
                                  max_insertions = c(4, 8, 12), method = "greedy")
    exact <- optimize_media_plan(plan, budget = 60000, effective_frequency = frequency,
                                 max_insertions = c(4, 8, 12), method = "exact")
    expect_false(greedy$global_optimum)
    expect_gt(sum(greedy$allocation), 0)
    expect_lte(greedy$spend, 60000)
    expect_gt(greedy$effective_reach, 0)
    # A heuristic cannot beat the verified optimum, and should come close
    expect_lte(greedy$effective_reach, exact$effective_reach + 1e-12)
    expect_gt(greedy$effective_reach, 0.9 * exact$effective_reach)
  }
})

test_that("greedy min_cost reaches an attainable target and warns about an unattainable one", {
  plan <- readme_plan()
  reached <- optimize_media_plan(plan, budget = 60000, objective = "min_cost",
                                 target_reach = 0.30, effective_frequency = 2,
                                 max_insertions = c(4, 8, 12), method = "greedy")
  expect_true(reached$target_met)
  expect_gte(reached$effective_reach, 0.30)
  expect_lte(reached$spend, 60000)

  expect_warning(
    unreached <- optimize_media_plan(plan, budget = 2000, objective = "min_cost",
                                     target_reach = 0.90, effective_frequency = 2,
                                     max_insertions = c(4, 8, 12), method = "greedy"),
    "below target_reach"
  )
  expect_false(unreached$target_met)
  expect_lte(unreached$spend, 2000)
})

test_that("method auto switches to the greedy heuristic beyond max_combinations", {
  plan <- readme_plan()
  big <- optimize_media_plan(plan, budget = 60000, effective_frequency = 2,
                             max_insertions = c(4, 8, 12), max_combinations = 100)
  expect_identical(big$method, "greedy")
  expect_false(big$global_optimum)
  small <- optimize_media_plan(plan, budget = 60000, max_insertions = c(2, 2, 2))
  expect_identical(small$method, "exact")
  expect_error(optimize_media_plan(plan, budget = 60000, max_insertions = c(4, 8, 12),
                                   method = "exact", max_combinations = 100),
               "Exact search requires")
  expect_output(print(big), "greedy heuristic")
})

test_that("exact search prefers the cheaper allocation when effective reach ties", {
  plan <- media_plan(data.frame(
    channel = c("A", "B"), audience = c(400, 400),
    insertions = c(1, 1), cost_per_insertion = c(5, 9)
  ), population = 1000)
  fit <- optimize_media_plan(plan, budget = 9, max_insertions = c(1, 1))
  # Same audience: the allocation using the cheaper channel wins
  expect_equal(unname(fit$allocation), c(1, 0))
})

test_that("optimizer rejects non-finite search controls", {
  plan <- media_plan(data.frame(
    channel = "A", audience = 500, insertions = 1,
    cost_per_insertion = 5
  ), population = 1000)

  expect_error(
    optimize_media_plan(plan, 5, max_insertions = Inf),
    "finite non-negative integer"
  )
  expect_error(
    optimize_media_plan(plan, 5, effective_frequency = Inf),
    "finite positive integer"
  )
  expect_error(
    optimize_media_plan(plan, 5, objective = "min_cost", target_reach = NA_real_),
    "target_reach"
  )
  expect_error(
    optimize_media_plan(plan, 5, objective = "min_cost"),
    "requires a target_reach"
  )
  expect_error(
    optimize_media_plan(plan, 5, max_combinations = NA_real_),
    "max_combinations"
  )
  expect_error(optimize_media_plan(plan, NA), "budget")
  expect_error(optimize_media_plan(plan, -1), "budget")
})
