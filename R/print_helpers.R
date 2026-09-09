#__________________________________________________________#
# Shared console formatting for the "classical reach models" print methods
# (Sainsbury, Binomial, Beta-Binomial, Metheringham, CANEX), so every model
# that returns reach/distribution/cumulative reports it the same way: a
# title, the headline reach, the full contact distribution, the cumulative
# distribution, model-specific parameters, and average contacts per person
# reached. Models with a different return shape (e.g. Agostini, which has no
# per-contact distribution, or the sequential-aggregation models, whose
# distributions can run into the hundreds of cells) keep their own printing.
print_reach_report <- function(title, description, reach_percent, reach_people,
                               distribution = NULL, cumulative = NULL,
                               parameters = NULL, notes = character(0)) {
  cat(title, "\n", sep = "")
  cat(strrep("=", nchar(title)), "\n", sep = "")
  cat("Description: ", description, "\n\n", sep = "")

  cat("HEADLINE METRICS:\n")
  cat("-----------------\n")
  cat(sprintf("Total reach: %.2f%% (%.0f people)\n", reach_percent, reach_people))

  if (!is.null(parameters) && length(parameters)) {
    cat("\nMODEL PARAMETERS:\n")
    cat("-----------------\n")
    for (nm in names(parameters)) {
      val <- parameters[[nm]]
      if (is.numeric(val) && length(val) == 1L) {
        cat(sprintf("%s: %s\n", nm, format(val, digits = 4, nsmall = 0, big.mark = ",")))
      }
    }
  }

  if (!is.null(distribution)) {
    contacts <- if (!is.null(distribution$contacts)) distribution$contacts else seq_along(distribution$percent)
    cat("\nCONTACT DISTRIBUTION:\n")
    cat("----------------------\n")
    cat("(Percentage of the population receiving exactly N contacts)\n")
    for (i in seq_along(distribution$percent)) {
      cat(sprintf("%d contact%s: %.2f%% (%.0f people)\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  distribution$percent[i], distribution$people[i]))
    }
  }

  if (!is.null(cumulative)) {
    contacts <- if (!is.null(cumulative$min_contacts)) cumulative$min_contacts else seq_along(cumulative$percent)
    cat("\nCUMULATIVE DISTRIBUTION:\n")
    cat("-------------------------\n")
    cat("(Percentage of the population receiving N or more contacts)\n")
    for (i in seq_along(cumulative$percent)) {
      cat(sprintf(">= %d contact%s: %.2f%% (%.0f people)\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  cumulative$percent[i], cumulative$people[i]))
    }
  }

  if (!is.null(distribution)) {
    contacts <- if (!is.null(distribution$contacts)) distribution$contacts else seq_along(distribution$percent)
    # Divide by reach_people, not sum(distribution$people): for Sainsbury,
    # Binomial, Beta-Binomial and Metheringham the distribution already
    # excludes the zero-contact row, so the two are identical; for CANEX
    # (and any future model) whose distribution includes a zero-contact row,
    # summing distribution$people would divide by the whole population
    # instead of by those actually reached.
    average_contacts <- sum(contacts * distribution$people) / reach_people
    cat("\nSUMMARY STATISTICS:\n")
    cat("--------------------\n")
    cat(sprintf("Average contacts per person reached: %.2f\n", average_contacts))
  }

  for (note in notes) cat(note, "\n", sep = "")

  invisible(NULL)
}
