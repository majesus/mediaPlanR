#__________________________________________________________#
# Shared console formatting for the print methods of the classical reach
# models (Sainsbury, Binomial, Beta-Binomial, Metheringham, CANEX and the ad
# hoc reach-only formulas of Agostini and Hofmans), so every model reports the
# same way: a title, the headline reach, model-specific parameters and, when
# the model returns them, the full exposure distribution, the cumulative
# distribution and the average exposures per person reached. The
# sequential-aggregation models (CSD, MSAD, CBD, MBD), whose distributions can
# run into the hundreds of cells, print a compact summary by default and the
# same kind of report through print_sequential_report(), which truncates long
# distributions.
print_reach_report <- function(title, description, reach_percent, reach_people,
                               distribution = NULL, cumulative = NULL,
                               parameters = NULL, notes = character(0),
                               show_people = TRUE) {
  people_text <- function(people) {
    if (show_people) sprintf(" (%.0f people)", people) else ""
  }
  cat(title, "\n", sep = "")
  cat(strrep("=", nchar(title)), "\n", sep = "")
  cat("Description: ", description, "\n\n", sep = "")

  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%%%s\n", reach_percent, people_text(reach_people)))

  if (!is.null(parameters) && length(parameters)) {
    cat("\nMODEL PARAMETERS:\n")
    cat("-----------------\n")
    for (nm in names(parameters)) {
      val <- parameters[[nm]]
      if (is.numeric(val) && length(val) == 1L) {
        cat(sprintf("%s: %s\n", nm, format(val, digits = 4, big.mark = ",", scientific = FALSE)))
      }
    }
  }

  if (!is.null(distribution)) {
    contacts <- if (!is.null(distribution$contacts)) distribution$contacts else seq_along(distribution$percent)
    cat("\nEXPOSURE DISTRIBUTION:\n")
    cat("----------------------\n")
    cat("(Percentage of the population receiving exactly N exposures)\n")
    for (i in seq_along(distribution$percent)) {
      cat(sprintf("%d exposure%s: %.2f%%%s\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  distribution$percent[i], people_text(distribution$people[i])))
    }
  }

  if (!is.null(cumulative)) {
    contacts <- if (!is.null(cumulative$min_contacts)) cumulative$min_contacts else seq_along(cumulative$percent)
    cat("\nCUMULATIVE DISTRIBUTION:\n")
    cat("-------------------------\n")
    cat("(Percentage of the population receiving N or more exposures)\n")
    for (i in seq_along(cumulative$percent)) {
      cat(sprintf(">= %d exposure%s: %.2f%%%s\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  cumulative$percent[i], people_text(cumulative$people[i])))
    }
  }

  if (!is.null(distribution)) {
    contacts <- if (!is.null(distribution$contacts)) distribution$contacts else seq_along(distribution$percent)
    # Divide by reach_people, not sum(distribution$people): for Sainsbury,
    # Binomial, Beta-Binomial and Metheringham the distribution already
    # excludes the zero-exposure row, so the two are identical; for CANEX
    # (and any future model) whose distribution includes a zero-exposure row,
    # summing distribution$people would divide by the whole population
    # instead of by those actually reached.
    average_contacts <- sum(contacts * distribution$people) / reach_people
    cat("\nSUMMARY STATISTICS:\n")
    cat("--------------------\n")
    cat(sprintf("Average exposures per person reached: %.2f\n", average_contacts))
  }

  for (note in notes) cat(note, "\n", sep = "")

  invisible(NULL)
}

# Full report for the sequential-aggregation models (CSD, MSAD, CBD, MBD). It
# keeps the layout of print_reach_report() but works on the result object
# itself: `x$distribution` includes the zero-exposure row, so reach is read from
# the object rather than recomputed, and distributions longer than `max_rows`
# exposure levels are cut with an explicit note instead of flooding the console.
# `parameters` is a named list of scalars, `tables` a named list of data frames
# or matrices printed under their name, and `diagnostics` a character vector of
# ready-made lines.
print_sequential_report <- function(x, title, description, parameters = NULL,
                                    tables = NULL, diagnostics = character(0),
                                    max_rows = 30L) {
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
      max_rows < 1) {
    stop("max_rows must be a single number of at least 1 (use Inf for all).",
         call. = FALSE)
  }
  distribution <- x$distribution
  population <- x$reach$people / x$reach$probability
  show_people <- !isTRUE(all.equal(population, 1))
  people_text <- function(people) {
    if (show_people) sprintf(" (%.0f people)", people) else ""
  }
  exposed <- distribution[distribution$contacts >= 1L, , drop = FALSE]
  shown <- seq_len(min(nrow(exposed), max_rows))
  omitted <- nrow(exposed) - length(shown)
  omitted_note <- function() {
    if (omitted > 0L) {
      cat(sprintf(paste0("... %d further exposure level%s (%d to %d) not shown; ",
                         "use max_rows = Inf to list them all.
"),
                  omitted, if (omitted == 1L) "" else "s",
                  exposed$contacts[length(shown) + 1L],
                  exposed$contacts[nrow(exposed)]))
    }
  }
  exposure_label <- function(n) sprintf("%d exposure%s", n, if (n == 1L) "" else "s")

  cat(title, "
", strrep("=", nchar(title)), "
", sep = "")
  cat("Description: ", description, "

", sep = "")

  cat("HEADLINE METRICS:
-----------------
")
  cat(sprintf("Total reach: %.2f%%%s
", x$reach$percent,
              people_text(x$reach$people)))
  cat(sprintf("Average exposures per person reached: %.2f
",
              x$average_frequency))

  cat("
MODEL PARAMETERS:
-----------------
")
  cat(sprintf("Probability of 0 exposures (%%): %.2f
", distribution$percent[1L]))
  cat(sprintf("Total insertions (N): %d
", nrow(distribution) - 1L))
  for (name in names(parameters)) {
    cat(sprintf("%s: %s
", name, paste(parameters[[name]], collapse = " -> ")))
  }

  for (name in names(tables)) {
    cat("
", toupper(name), ":
", strrep("-", nchar(name) + 1L), "
", sep = "")
    if (is.matrix(tables[[name]])) {
      print(tables[[name]], digits = 4)
    } else {
      print(tables[[name]], row.names = FALSE, digits = 4)
    }
  }

  cat("
EXPOSURE DISTRIBUTION:
----------------------
")
  cat("(Percentage of the population receiving exactly N exposures)
")
  for (i in shown) {
    cat(sprintf("%s: %.2f%%%s
", exposure_label(exposed$contacts[i]),
                exposed$percent[i], people_text(exposed$people[i])))
  }
  omitted_note()

  cat("
CUMULATIVE DISTRIBUTION:
-------------------------
")
  cat("(Percentage of the population receiving N or more exposures)
")
  for (i in shown) {
    cat(sprintf(">= %s: %.2f%%%s
", exposure_label(exposed$contacts[i]),
                100 * exposed$cumulative_probability[i],
                people_text(population * exposed$cumulative_probability[i])))
  }
  omitted_note()

  if (length(diagnostics)) {
    cat("
DIAGNOSTICS:
------------
")
    for (line in diagnostics) cat(line, "
", sep = "")
  }
  invisible(NULL)
}

# Lines shared by the diagnostics block of the four sequential models.
sequential_diagnostic_lines <- function(x) {
  lines <- sprintf("Probability sum: %.12f | Smallest probability: %.3g | Mean error: %.3g",
                   x$diagnostics$probability_sum,
                   x$diagnostics$minimum_probability,
                   x$diagnostics$mean_error)
  if (!is.null(x$steps) && all(c("row_margin_error", "column_margin_error") %in% names(x$steps))) {
    lines <- c(lines, sprintf("Largest margin error across aggregation steps: %.3g",
                              max(abs(c(x$steps$row_margin_error,
                                        x$steps$column_margin_error)))))
  }
  if (!is.null(x$diagnostics$negative_mass_adjusted)) {
    lines <- c(lines, if (x$diagnostics$negative_mass_adjusted > 0) {
      sprintf("Safety net engaged: %d cell(s), %.6g probability mass adjusted.",
              x$diagnostics$cells_adjusted, x$diagnostics$negative_mass_adjusted)
    } else {
      "Safety net: not engaged (no negative probabilities)."
    })
  }
  lines
}

# Per-vehicle table of the CSD and MSAD results: insertions, own reach and
# position in the aggregation order.
sequential_vehicle_table <- function(x) {
  data.frame(
    vehicle = seq_along(x$vehicle_reach),
    insertions = vapply(x$vehicle_marginals, nrow, integer(1)) - 1L,
    own_reach_percent = 100 * x$vehicle_reach,
    aggregation_position = match(seq_along(x$vehicle_reach), x$aggregation_order)
  )
}

# Aggregation steps without the margin-error columns (summarised in the
# diagnostics instead), with the target reach also shown as a percentage.
sequential_step_table <- function(steps) {
  steps <- steps[setdiff(names(steps), c("row_margin_error", "column_margin_error"))]
  if ("target_reach" %in% names(steps)) {
    steps$target_reach <- 100 * steps$target_reach
    names(steps)[names(steps) == "target_reach"] <- "target_reach_percent"
  }
  steps
}
