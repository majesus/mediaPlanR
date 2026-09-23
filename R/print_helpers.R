#__________________________________________________________#
# Shared console formatting for the print methods of the classical reach
# models (Sainsbury, Binomial, Beta-Binomial, Metheringham, CANEX and the ad
# hoc reach-only formulas of Agostini and Hofmans), so every model reports the
# same way: a title, the headline reach, model-specific parameters and, when
# the model returns them, the full exposure distribution, the cumulative
# distribution and the average exposures per person reached. The
# sequential-aggregation models (CSD, MSAD, CBD, MBD), whose distributions can
# run into the hundreds of cells, keep their own compact printing.
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
      cat(sprintf("%d exposure%s: %.2f%% (%.0f people)\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  distribution$percent[i], distribution$people[i]))
    }
  }

  if (!is.null(cumulative)) {
    contacts <- if (!is.null(cumulative$min_contacts)) cumulative$min_contacts else seq_along(cumulative$percent)
    cat("\nCUMULATIVE DISTRIBUTION:\n")
    cat("-------------------------\n")
    cat("(Percentage of the population receiving N or more exposures)\n")
    for (i in seq_along(cumulative$percent)) {
      cat(sprintf(">= %d exposure%s: %.2f%% (%.0f people)\n",
                  contacts[i], if (contacts[i] == 1) "" else "s",
                  cumulative$percent[i], cumulative$people[i]))
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
