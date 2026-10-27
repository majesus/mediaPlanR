# Builds the PDF reference manual of the package from its help pages.
#
# Run it from the package root:
#
#   Rscript tools/build-manual.R [output.pdf]
#
# The default output is mediaPlanR_manual.pdf (not part of the package
# archive; see .Rbuildignore). It needs a LaTeX installation with pdflatex and
# makeindex, and the LaTeX packages that `R CMD Rd2pdf` uses.
#
# The script is `R CMD Rd2pdf` plus changes to the LaTeX file that it
# generates, made after the Rd style is loaded:
#
# * \emergencystretch=4em. Without it, about 75 lines of the manual run into the
#   right margin, because long function and argument names in running text
#   cannot be hyphenated; with it, LaTeX stretches the spaces of those lines in
#   an additional pass instead of letting them overflow.
# * A break opportunity after each underscore of a name (calc_beta_binomial,
#   report_max, ...) in the running text: \_ is made a robust command that
#   typesets the underscore and allows a line break after it. It is written
#   unexpanded to the index, so the index and the bookmarks are not affected.
#
# Nothing else is changed: the text comes from man/*.Rd.
#
# Every step is checked. A non-zero exit code of Rd2pdf, pdflatex or makeindex
# stops the build with the end of its output, a PDF left by an earlier pass is
# never taken for the result of this one, and the output of all the steps is
# kept, with the overfull boxes of the final pass, in <output>.build.log (next
# to the PDF; see .Rbuildignore).

# Runs an external program with its output captured. It returns the lines of
# output, and stops (with a condition of class "manual_build_error" that carries
# the output and the exit code) when the exit code is not zero. A program that
# cannot be started gives a non-zero code as well.
run_checked <- function(label, program, args = character()) {
  step_log <- tempfile("step-", fileext = ".txt")
  on.exit(unlink(step_log))
  status <- suppressWarnings(
    system2(program, args, stdout = step_log, stderr = step_log)
  )
  lines <- if (file.exists(step_log)) {
    readLines(step_log, warn = FALSE)
  } else {
    character()
  }
  if (!identical(as.integer(status), 0L)) {
    stop(structure(
      class = c("manual_build_error", "error", "condition"),
      list(message = sprintf(
             "%s failed (exit code %s). End of its output:\n%s", label, status,
             paste(utils::tail(lines, 25L), collapse = "\n")),
           call = NULL, output = lines, status = status)
    ))
  }
  lines
}

build_manual <- function(output = "mediaPlanR_manual.pdf") {
  stopifnot(file.exists("DESCRIPTION"), dir.exists("man"))
  for (program in c("pdflatex", "makeindex")) {
    if (!nzchar(Sys.which(program))) {
      stop(program, " was not found; install a LaTeX distribution.",
           call. = FALSE)
    }
  }
  output <- normalizePath(output, mustWork = FALSE)
  log_path <- sub("[.]pdf$", ".build.log", output)
  if (identical(log_path, output)) log_path <- paste0(output, ".build.log")

  work <- NULL
  # Rd2pdf on Windows can fail when the expanded temp path has spaces or accents.
  if (.Platform$OS.type == "windows" &&
      grepl("[^!-~]", normalizePath(tempdir(), winslash = "/"))) {
    public <- Sys.getenv("PUBLIC")
    if (nzchar(public) && dir.exists(public)) {
      candidate <- tempfile("manual-", tmpdir = public)
      if (dir.create(candidate, showWarnings = FALSE)) work <- candidate
    }
  }
  if (is.null(work)) {
    work <- tempfile("manual-")
    dir.create(work)
  }
  package_copy <- file.path(work, "mediaPlanR")
  dir.create(package_copy)
  file.copy(c("DESCRIPTION", "NAMESPACE", "man"), package_copy, recursive = TRUE)

  log <- character()
  old <- setwd(work)
  on.exit({
    setwd(old)
    unlink(work, recursive = TRUE)
    if (length(log)) writeLines(log, log_path, useBytes = TRUE)
  }, add = TRUE)

  # Runs one step, keeps its output in the log (also when it fails) and stops
  # if its exit code is not zero.
  run <- function(label, program, args) {
    lines <- tryCatch(
      run_checked(label, program, args),
      manual_build_error = function(e) {
        log <<- c(log, sprintf("==== %s: %s (exit code %s)", label, program,
                               e$status), e$output)
        stop(e)
      }
    )
    log <<- c(log, sprintf("==== %s: %s (exit code 0)", label, program), lines)
    invisible(lines)
  }

  r_program <- file.path(R.home("bin"), "R")
  run("R CMD Rd2pdf", r_program,
      c("CMD", "Rd2pdf", "--no-preview", "--batch", "--force", "--no-clean",
        "--output=first-pass.pdf", "mediaPlanR"))
  build_dir <- list.files(work, pattern = "^[.]Rd2pdf", all.files = TRUE,
                          full.names = TRUE)
  if (length(build_dir) != 1L) {
    stop("R CMD Rd2pdf did not leave exactly one build directory.",
         call. = FALSE)
  }

  setwd(build_dir)
  tex <- readLines("Rd2.tex", encoding = "UTF-8")
  style_line <- grep("^\\\\usepackage\\[.*\\]\\{Rd\\}", tex)
  if (length(style_line) != 1L) {
    stop("The Rd style line was not found in Rd2.tex.", call. = FALSE)
  }
  tex <- append(tex, c(
    "\\emergencystretch=4em",
    "\\DeclareRobustCommand{\\_}{\\textunderscore\\hspace{0pt}}"
  ), after = style_line)
  writeLines(tex, "Rd2.tex", useBytes = TRUE)

  # The first pass of Rd2pdf may have left a PDF: only one produced by the
  # passes below counts.
  unlink("Rd2.pdf")
  pdflatex_arguments <- c("-interaction=nonstopmode", "-halt-on-error",
                          "Rd2.tex")
  run("pdflatex, pass 1", "pdflatex", pdflatex_arguments)
  run("pdflatex, pass 2", "pdflatex", pdflatex_arguments)
  run("makeindex", "makeindex", "Rd2.idx")
  run("pdflatex, final pass", "pdflatex", pdflatex_arguments)
  if (!file.exists("Rd2.pdf")) {
    stop("LaTeX did not produce a PDF.", call. = FALSE)
  }

  # Lines that run into the margin in the final pass.
  latex_log <- readLines("Rd2.log", warn = FALSE)
  overfull <- grep("^Overfull \\\\hbox", latex_log, value = TRUE)
  excess <- suppressWarnings(as.numeric(sub(
    "^Overfull \\\\hbox \\(([0-9.]+)pt too wide\\).*$", "\\1", overfull)))
  log <- c(log,
           sprintf("==== Overfull boxes of the final pass: %d", length(overfull)),
           overfull)

  if (!isTRUE(file.copy("Rd2.pdf", output, overwrite = TRUE))) {
    stop("Could not write the manual to ", output, ".", call. = FALSE)
  }
  message("Wrote ", output)
  message(sprintf("Overfull boxes in the final pass: %d%s (build log: %s)",
                  length(overfull),
                  if (length(overfull)) {
                    sprintf(", the widest %.1f pt too wide",
                            max(excess, na.rm = TRUE))
                  } else {
                    ""
                  },
                  log_path))
  invisible(output)
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  build_manual(if (length(args)) args[1L] else "mediaPlanR_manual.pdf")
}
