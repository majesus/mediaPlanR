# Reach and effective reach (at least `effective_frequency` exposures) of one
# allocation of insertions. It evaluates the same Sainsbury or Binomial
# distribution as estimate_reach() without building the full result object, so
# that the search can evaluate many allocations quickly.
allocation_reach <- function(plan, allocation, model, effective_frequency = 1L) {
  probabilities <- rep(plan$data$audience / plan$population, allocation)
  n <- length(probabilities)
  if (n == 0L) return(list(reach = 0, effective_reach = 0))
  distribution <- if (model == "sainsbury") {
    poisson_binomial_distribution(probabilities)
  } else {
    stats::dbinom(0:n, size = n, prob = mean(probabilities))
  }
  effective <- if (effective_frequency > n) 0 else
    sum(distribution[(effective_frequency + 1L):(n + 1L)])
  list(reach = sum(distribution[-1L]), effective_reach = effective)
}

enumerate_allocations <- function(max_insertions) {
  grid <- expand.grid(lapply(max_insertions, function(x) 0:x),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  as.matrix(grid)
}

# Greedy search. Each step adds the move with the best gain in effective reach
# per unit of cost. A move adds between one and `effective_frequency`
# insertions to a single channel: with an effective frequency of f, an empty
# allocation with fewer than f insertions in total has no effective reach, so
# single-insertion moves alone would see no gain and stop at once. After the
# start, a further insertion can raise the probability of reaching f.
greedy_allocation <- function(plan, budget, max_insertions, model,
                              effective_frequency, target_reach = NULL) {
  cost <- plan$data$cost_per_insertion
  allocation <- integer(nrow(plan$data))
  trace <- list()
  current <- allocation_reach(plan, allocation, model, effective_frequency)
  repeat {
    if (!is.null(target_reach) && current$effective_reach >= target_reach - 1e-12) break
    spend <- sum(allocation * cost)
    best <- NULL
    for (i in seq_along(allocation)) {
      room <- max_insertions[i] - allocation[i]
      for (q in seq_len(min(room, effective_frequency))) {
        step_cost <- q * cost[i]
        if (spend + step_cost > budget + 1e-10) break
        proposal <- allocation
        proposal[i] <- proposal[i] + q
        evaluated <- allocation_reach(plan, proposal, model, effective_frequency)
        gain <- evaluated$effective_reach - current$effective_reach
        if (gain <= 1e-15) next
        score <- if (step_cost > 0) gain / step_cost else Inf
        if (is.null(best) || score > best$score) {
          best <- list(i = i, q = q, gain = gain, score = score,
                       evaluated = evaluated)
        }
      }
    }
    if (is.null(best)) break
    allocation[best$i] <- allocation[best$i] + best$q
    current <- best$evaluated
    trace[[length(trace) + 1L]] <- c(channel = best$i, insertions = best$q,
                                      gain = best$gain)
  }
  list(allocation = allocation, trace = trace)
}

#' Optimize a cross-media allocation
#'
#' Finds an allocation of insertions across channels under a budget, using
#' either exhaustive search (with a verifiable global optimum) or an explicitly
#' labeled greedy heuristic. The objective can maximize effective reach or
#' minimize spend for a required effective reach.
#'
#' @param plan A `media_plan` object. Its current insertions are the default
#'   upper bounds of the search. The optimization uses `audience` and
#'   `population` only: a `target_audience` column of the plan is ignored, so
#'   the quantity optimized is the effective reach of the whole population,
#'   not of the target. To optimize for the target, supply a plan whose
#'   `population` is the target universe and whose `audience` is the part of
#'   each audience that belongs to the target.
#' @param budget Maximum total spend, in the plan's currency.
#' @param objective `"max_reach"` maximizes effective reach within the budget
#'   (despite its name, the quantity maximized is the effective reach defined by
#'   `effective_frequency`; with the default `effective_frequency = 1` it is the
#'   ordinary reach). `"min_cost"` minimizes spend, within the budget, subject to
#'   effective reach being at least `target_reach`.
#' @param target_reach Required effective reach, as a proportion of the
#'   population between zero and one (0.40 for 40%, not 40), for
#'   `objective = "min_cost"`.
#' @param effective_frequency Effective frequency: the minimum number of
#'   exposures, \eqn{f}, that a person needs to count as effectively reached
#'   (a positive integer, default 1). Effective reach is the proportion of the
#'   population exposed `effective_frequency` times or more, that is, \eqn{f} or
#'   more times; it is not the proportion exposed exactly \eqn{f} times. The
#'   threshold \eqn{f} is a planning criterion that the analyst justifies for the
#'   objective, the creative, the medium and the period (it is worth comparing
#'   several values). Reaching it is a delivery result: the function does not
#'   estimate that people exposed \eqn{f} times remember, are persuaded or
#'   buy, and cost per thousand or coverage are delivery and efficiency
#'   measures, not business outcomes.
#' @param max_insertions Integer vector with the maximum number of insertions
#'   allowed in each channel, in the order of the rows of the plan.
#' @param model `"sainsbury"` or `"binomial"`; see [estimate_reach()]. These are
#'   the models that the optimizer offers because they need only the audiences
#'   and the insertions that a `media_plan` contains. The other models are not
#'   excluded because every allocation would have to be observed again: with
#'   parameters held fixed for each vehicle (stationary), a model that uses
#'   observed duplications or the reach after two insertions could in principle
#'   be evaluated for any allocation. Offering them would need more
#'   information in the plan (the duplications or `R2` of each vehicle), a
#'   justification that those parameters do not change when the number of
#'   insertions changes, and a policy for allocations in which the model fails.
#'   The Negative-Binomial approximation, which is scoped to continuous
#'   exposure processes and not to finite schedules, is not offered, since
#'   candidate allocations routinely place several insertions in the same
#'   vehicle.
#' @param method `"exact"`, `"greedy"` or `"auto"`, which uses exhaustive
#'   search when the number of allocations does not exceed `max_combinations`
#'   and the greedy heuristic otherwise.
#' @param max_combinations Maximum number of allocations allowed for exhaustive
#'   search. The number of allocations is the product, over the channels, of
#'   one plus the maximum insertions of the channel (`prod(max_insertions + 1)`).
#'
#' @details
#' Exhaustive search evaluates every allocation within `max_insertions` whose
#' spend does not exceed the budget and returns the best one, so its result is
#' a global optimum for the chosen reach `model` and within those bounds (the
#' optimum is relative to the model: a different model can rank allocations
#' differently). Ties in effective reach are broken in favor of lower
#' spend and then higher reach. For `objective = "min_cost"` the cheapest
#' allocation that reaches `target_reach` is returned, and an error is raised
#' when none does.
#'
#' The `"binomial"` model evaluates every allocation as a homogenized plan in
#' which each insertion has the insertion-weighted mean audience. Its reach is
#' therefore the reach of that homogenized plan, and it can be smaller than the
#' audience of a single channel of the original plan. With audiences of 90 and
#' 10 people in a universe of 100 and one insertion in each channel, the
#' Binomial reach is 75 people and the probability of two exposures is 0.25,
#' while the first channel alone already reaches 90 people (the Sainsbury model,
#' which keeps both audiences, gives 91 people and 0.09). A reach below the
#' largest audience cannot be the reach of the plan, and [plan_metrics()] rejects
#' it. When the selected allocation has this property, the function still
#' returns the allocation and the effective reach, which are results of the
#' homogenized model and of nothing else, but it issues a warning (class
#' `mediaPlanR_homogenized_reach_incompatible`), sets
#' `reach_compatible_with_plan` to `FALSE` and leaves out of `metrics` the
#' quantities that need a reach of the plan itself (`reach`, `reach_percent`,
#' `average_frequency` and `cost_per_thousand_reached`). The model `"sainsbury"`
#' keeps the audience of each channel and does not have this limitation.
#'
#' The greedy heuristic repeatedly adds the move with the largest gain in
#' effective reach per unit of cost. A move adds between one and
#' `effective_frequency` insertions to one channel. Starting from an empty plan,
#' fewer insertions in total than the effective frequency cannot create any
#' effective reach, so a search that added one insertion at a time would see no
#' gain and stop at once; this is why the first moves may add up to \eqn{f}
#' insertions together. Once insertions have been assigned, one further
#' insertion can raise the probability of reaching \eqn{f} exposures, and the
#' later moves of one or a few insertions capture that smaller marginal gain.
#' The result is not guaranteed to be optimal and is always reported as a
#' heuristic. With `objective = "min_cost"`, a warning is issued if the target
#' cannot be reached within the budget.
#'
#' @return A `media_optimization` object: a list with the optimized `plan`, the
#'   named `allocation`, the `reach` result of [estimate_reach()], the plan
#'   `metrics`, the `effective_frequency` and `effective_reach` (a proportion of the
#'   population exposed `effective_frequency` times or more), `spend`,
#'   `budget`, `target_reach` and `target_met`, the `method` used, the reach
#'   `model`, `reach_compatible_with_plan` (`FALSE` when the model's reach is
#'   below the largest audience of the channels with insertions; see Details),
#'   `global_optimum` (`TRUE` only for exhaustive search),
#'   `combinations_evaluated` and, for exhaustive search, a `search_table` with
#'   the spend, reach and effective reach of each evaluated allocation.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la
#' distribución de contactos en la planificación de medios publicitarios
#' impresos (Models for determining reach and exposure distribution in print
#' media planning). Doctoral dissertation, Universidad de Valencia, Spain.
#' Sections 3.3.1.1 and 3.3.1.2, the reach models this function evaluates.
#'
#' @examples
#' plan <- media_plan(
#'   data.frame(channel = c("TV", "Radio", "Digital"),
#'              audience = c(300000, 180000, 120000),
#'              insertions = c(4, 6, 10),
#'              cost_per_insertion = c(18000, 3500, 1200)),
#'   population = 1000000
#' )
#' optimized <- optimize_media_plan(
#'   plan, budget = 60000, objective = "max_reach",
#'   effective_frequency = 2, max_insertions = c(4, 8, 12)
#' )
#' optimized
#' optimized$global_optimum
#'
#' # Cheapest allocation reaching 40% effective reach
#' optimize_media_plan(
#'   plan, budget = 60000, objective = "min_cost", target_reach = 0.40,
#'   effective_frequency = 2, max_insertions = c(4, 8, 12)
#' )$allocation
#'
#' @seealso [media_plan()] and [estimate_reach()].
#' @export
optimize_media_plan <- function(plan, budget,
                                objective = c("max_reach", "min_cost"),
                                target_reach = NULL,
                                effective_frequency = 1L,
                                max_insertions = plan$data$insertions,
                                model = c("sainsbury", "binomial"),
                                method = c("auto", "exact", "greedy"),
                                max_combinations = 1e6) {
  assert_media_plan(plan)
  objective <- match.arg(objective)
  model <- match.arg(model)
  method <- match.arg(method)
  assert_number(budget, "budget", min = 0)
  assert_numeric_vector(max_insertions, "max_insertions", min = 0,
                        integer = TRUE, length = nrow(plan$data))
  assert_number(effective_frequency, "effective_frequency", min = 1,
                integer = TRUE)
  if (objective == "min_cost") {
    if (is.null(target_reach)) {
      stop("min_cost requires a target_reach between zero and one.",
           call. = FALSE)
    }
    assert_number(target_reach, "target_reach", min = 0, max = 1,
                  proportion = TRUE)
  }
  assert_number(max_combinations, "max_combinations", min = 1)

  combinations <- prod(max_insertions + 1)
  if (!is.finite(combinations)) combinations <- Inf
  selected_method <- if (method == "auto") {
    if (combinations <= max_combinations) "exact" else "greedy"
  } else method
  if (selected_method == "exact" && combinations > max_combinations) {
    stop(sprintf("Exact search requires %.0f allocations; raise max_combinations or use method = \"greedy\".",
                 combinations), call. = FALSE)
  }

  if (selected_method == "exact") {
    allocations <- enumerate_allocations(max_insertions)
    spend <- as.vector(allocations %*% plan$data$cost_per_insertion)
    feasible_budget <- which(spend <= budget + 1e-10)
    allocations <- allocations[feasible_budget, , drop = FALSE]
    spend <- spend[feasible_budget]
    effective <- numeric(nrow(allocations))
    reach <- numeric(nrow(allocations))
    for (i in seq_len(nrow(allocations))) {
      evaluated <- allocation_reach(plan, allocations[i, ], model, effective_frequency)
      effective[i] <- evaluated$effective_reach
      reach[i] <- evaluated$reach
    }
    # Values are compared after rounding so that floating-point noise cannot
    # override the tie-breaking rules (lower spend first).
    if (objective == "max_reach") {
      ordering <- order(-round(effective, 12), round(spend, 8), -round(reach, 12))
    } else {
      feasible_target <- which(effective >= target_reach - 1e-12)
      if (!length(feasible_target)) {
        stop("No allocation reaches target_reach within budget.", call. = FALSE)
      }
      ordering <- feasible_target[order(round(spend[feasible_target], 8),
                                        -round(effective[feasible_target], 12),
                                        -round(reach[feasible_target], 12))]
    }
    best <- ordering[1L]
    allocation <- as.integer(allocations[best, ])
    search_table <- data.frame(spend = spend, reach = reach,
                               effective_reach = effective)
  } else {
    greedy <- greedy_allocation(plan, budget, max_insertions, model,
                                effective_frequency,
                                if (objective == "min_cost") target_reach else NULL)
    allocation <- greedy$allocation
    search_table <- NULL
  }

  optimized_plan <- plan
  optimized_plan$data$insertions <- allocation
  reach_result <- estimate_reach(optimized_plan, model)
  # The Binomial model replaces the audience of every channel by their
  # insertion-weighted mean, so its reach describes that homogenized plan. It
  # can fall below the largest audience of the original plan, a reach that the
  # plan itself cannot have. The search and the effective reach are valid for
  # the model, but the physical metrics that use the reach (average frequency
  # and cost per thousand people reached) are not built on an incompatible
  # figure: they are left out and the result says so.
  reach_bounds <- plan_reach_bounds(optimized_plan$data, plan$population,
                                    sum(optimized_plan$data$audience *
                                          optimized_plan$data$insertions))
  reach_compatible <- reach_result$reach$people >=
    reach_bounds[["lower"]] - exact_constraint_slack(plan$population) &&
    reach_result$reach$people <=
    reach_bounds[["upper"]] + exact_constraint_slack(plan$population)
  metrics <- plan_metrics(
    optimized_plan,
    reach = if (reach_compatible) reach_result$reach$people
  )
  if (!reach_compatible) {
    warning(warningCondition(
      paste0("The ", model, " model gives a reach of ",
             format(signif(reach_result$reach$people, 7), scientific = FALSE,
                    trim = TRUE),
             " people for the selected allocation, below the largest audience ",
             "of the channels with insertions (",
             format(signif(reach_bounds[["lower"]], 7), scientific = FALSE,
                    trim = TRUE),
             " people): a figure that the plan itself cannot have, because the ",
             "model replaces every audience by the insertion-weighted mean. ",
             "The allocation and the effective reach are valid for that ",
             "homogenized model only; the average frequency and the cost per ",
             "thousand people reached are not reported. Use model = ",
             "\"sainsbury\" to keep the audience of each channel."),
      class = "mediaPlanR_homogenized_reach_incompatible", call = NULL))
  }
  idx <- match(effective_frequency, reach_result$cumulative$min_contacts)
  effective_reach <- if (is.na(idx)) 0 else reach_result$cumulative$probability[idx]
  target_met <- is.null(target_reach) || effective_reach >= target_reach - 1e-12
  if (objective == "min_cost" && !target_met) {
    warning("The greedy search stopped at an effective reach of ",
            format(round(effective_reach, 4)), ", below target_reach (",
            target_reach, "). Raise the budget or max_insertions.",
            call. = FALSE)
  }

  structure(list(
    plan = optimized_plan,
    allocation = stats::setNames(allocation, plan$data$channel),
    reach = reach_result,
    metrics = metrics,
    effective_frequency = effective_frequency,
    effective_reach = effective_reach,
    spend = metrics$totals$spend,
    budget = budget,
    target_reach = target_reach,
    target_met = target_met,
    method = selected_method,
    model = model,
    reach_compatible_with_plan = reach_compatible,
    global_optimum = identical(selected_method, "exact"),
    combinations_evaluated = if (selected_method == "exact") nrow(search_table) else NA_integer_,
    search_table = search_table
  ), class = "media_optimization")
}

#' @export
print.media_optimization <- function(x, ...) {
  label <- if (x$global_optimum) "verified global optimum" else "greedy heuristic"
  cat(sprintf("Media optimization (%s)\n", label))
  cat(sprintf("Spend: %.2f / %.2f %s | Reach: %.2f%% | Effective reach (%d or more exposures): %.2f%%\n",
              x$spend, x$budget, x$plan$currency, x$reach$reach$percent,
              as.integer(x$effective_frequency), 100 * x$effective_reach))
  print(data.frame(channel = names(x$allocation), insertions = x$allocation),
        row.names = FALSE)
  if (identical(x$reach_compatible_with_plan, FALSE)) {
    cat("Note: the reach of the homogenized", x$model, "model is below the",
        "largest audience of the plan; see ?optimize_media_plan.\n")
  }
  invisible(x)
}
