allocation_reach <- function(plan, allocation, model, effective_frequency = 1L) {
  candidate <- plan
  candidate$data$insertions <- allocation
  result <- estimate_reach(candidate, model = model)
  idx <- match(effective_frequency, result$cumulative$min_contacts)
  effective <- if (is.na(idx)) 0 else result$cumulative$probability[idx]
  list(result = result, effective_reach = effective)
}

enumerate_allocations <- function(max_insertions) {
  grid <- expand.grid(lapply(max_insertions, function(x) 0:x),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  as.matrix(grid)
}

greedy_allocation <- function(plan, budget, max_insertions, model,
                              effective_frequency, target_reach = NULL) {
  allocation <- integer(nrow(plan$data))
  trace <- list()
  repeat {
    current <- allocation_reach(plan, allocation, model, effective_frequency)
    if (!is.null(target_reach) && current$effective_reach >= target_reach) break
    current_spend <- sum(allocation * plan$data$cost_per_insertion)
    candidates <- lapply(seq_along(allocation), function(i) {
      if (allocation[i] >= max_insertions[i] ||
          current_spend + plan$data$cost_per_insertion[i] > budget) return(NULL)
      proposal <- allocation
      proposal[i] <- proposal[i] + 1L
      evaluated <- allocation_reach(plan, proposal, model, effective_frequency)
      gain <- evaluated$effective_reach - current$effective_reach
      cost <- plan$data$cost_per_insertion[i]
      list(i = i, gain = gain, score = if (cost > 0) gain / cost else Inf,
           evaluated = evaluated)
    })
    candidates <- Filter(Negate(is.null), candidates)
    if (!length(candidates)) break
    scores <- vapply(candidates, function(x) x$score, numeric(1))
    best <- candidates[[which.max(scores)]]
    if (best$gain <= 0) break
    allocation[best$i] <- allocation[best$i] + 1L
    trace[[length(trace) + 1L]] <- c(channel = best$i, gain = best$gain)
  }
  list(allocation = allocation, trace = trace)
}

#' Optimize a cross-media allocation
#'
#' Finds an insertion allocation under a budget using either exhaustive search
#' (with a verifiable global optimum) or an explicitly labelled greedy
#' heuristic. The objective can maximize effective reach or minimize spend for
#' a required effective reach.
#'
#' @param plan A `media_plan` object. Its current insertions are used as the
#'   default upper bounds.
#' @param budget Maximum total spend.
#' @param objective `max_reach` or `min_cost`.
#' @param target_reach Required effective reach, as a proportion, for
#'   `min_cost`.
#' @param effective_frequency Minimum exposures defining effective reach.
#' @param max_insertions Integer upper bound per channel.
#' @param model `sainsbury` or `binomial`; see `estimate_reach()`. Candidate
#'   allocations routinely place several insertions in the same vehicle, so
#'   the experimental NBD approximation (scoped to continuous exposure
#'   processes, not finite schedules) is not offered here.
#' @param method `exact`, `greedy`, or `auto`.
#' @param max_combinations Maximum allocations allowed for exact enumeration.
#'
#' @return A `media_optimization` object. `global_optimum` is `TRUE` only for
#'   exhaustive search.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la
#' distribucion de contactos en la planificacion de medios publicitarios
#' impresos. Tesis doctoral, Universidad de Valencia, Espana. (Sections
#' 3.3.1.1-3.3.1.2.)
#'
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
  if (!is.numeric(budget) || length(budget) != 1L || !is.finite(budget) || budget < 0) {
    stop("budget must be one non-negative finite number", call. = FALSE)
  }
  if (!is.numeric(max_insertions) || length(max_insertions) != nrow(plan$data) ||
      anyNA(max_insertions) || any(max_insertions < 0) ||
      any(max_insertions != round(max_insertions))) {
    stop("max_insertions must provide one non-negative integer per channel", call. = FALSE)
  }
  if (!is.numeric(effective_frequency) || length(effective_frequency) != 1L ||
      effective_frequency < 1 || effective_frequency != round(effective_frequency)) {
    stop("effective_frequency must be a positive integer", call. = FALSE)
  }
  if (objective == "min_cost" &&
      (is.null(target_reach) || !is.numeric(target_reach) ||
       length(target_reach) != 1L || target_reach < 0 || target_reach > 1)) {
    stop("min_cost requires target_reach between zero and one", call. = FALSE)
  }

  combinations <- prod(max_insertions + 1)
  if (!is.finite(combinations)) combinations <- Inf
  selected_method <- if (method == "auto") {
    if (combinations <= max_combinations) "exact" else "greedy"
  } else method
  if (selected_method == "exact" && combinations > max_combinations) {
    stop(sprintf("Exact search requires %.0f allocations; raise max_combinations or use method='greedy'",
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
      reach[i] <- evaluated$result$reach$probability
    }
    if (objective == "max_reach") {
      ordering <- order(-effective, spend, -reach)
    } else {
      feasible_target <- which(effective >= target_reach - 1e-12)
      if (!length(feasible_target)) {
        stop("No allocation reaches target_reach within budget", call. = FALSE)
      }
      ordering <- feasible_target[order(spend[feasible_target],
                                        -effective[feasible_target],
                                        -reach[feasible_target])]
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
  metrics <- plan_metrics(optimized_plan, reach = reach_result$reach$people)
  idx <- match(effective_frequency, reach_result$cumulative$min_contacts)
  effective_reach <- if (is.na(idx)) 0 else reach_result$cumulative$probability[idx]
  target_met <- is.null(target_reach) || effective_reach >= target_reach - 1e-12

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
    global_optimum = identical(selected_method, "exact"),
    combinations_evaluated = if (selected_method == "exact") nrow(search_table) else NA_integer_,
    search_table = search_table
  ), class = "media_optimization")
}

#' @export
print.media_optimization <- function(x, ...) {
  label <- if (x$global_optimum) "verified global optimum" else "greedy heuristic"
  cat(sprintf("Media optimization (%s)\n", label))
  cat(sprintf("Spend: %.2f / %.2f | Reach: %.2f%% | Reach %d+: %.2f%%\n",
              x$spend, x$budget, x$reach$reach$percent,
              x$effective_frequency, 100 * x$effective_reach))
  print(data.frame(channel = names(x$allocation), insertions = x$allocation),
        row.names = FALSE)
  invisible(x)
}

