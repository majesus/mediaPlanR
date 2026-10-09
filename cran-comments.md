## Submission

mediaPlanR 2.0.0, a new submission (first release on CRAN). Date field 2026-10-09.

Source archive: `mediaPlanR_2.0.0.tar.gz`, built with `R CMD build` (R 4.6.1,
Windows) on 2026-10-09. It is the only archive to submit; the earlier archives
of the same name (SHA-256 `53ee4de2...` of 2026-10-07 and `b7d20cea...` of
2026-10-08) are superseded and were moved to `superseded/`.
SHA-256: `4d1cf0c5d31dc40d1f62fb751212195fa50566b6115f3230d6b36a40046e50c8`.
Later commits may change only this file, which is excluded from the archive
through `.Rbuildignore`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Windows 11, R Under development (unstable) 2026-10-08 r90650 (ucrt), local:
  `R CMD check --as-cran --run-donttest` with `_R_CHECK_FORCE_SUGGESTS_` and
  `_R_CHECK_CRAN_INCOMING_` set to true, on the archive above, including the
  PDF and HTML manuals, the examples, the tests (2978 expectations pass and 8
  are skipped in the archive: 6 because the local-only validation tables are
  absent, 1 because it does not run on CRAN, 1 because a directory is absent
  in that context) and the rebuilt vignette. The notes about the current time
  and about V8 that appeared on the earlier local machine do not appear here.
* GitHub Actions, run 37903187686, commit `9312c72` (it differs from the source
  of the archive only in this file): `R CMD check --as-cran --run-donttest` on
  Ubuntu (R release, oldrel-1 and devel), macOS (R release) and Windows (R
  release), plus a job with R 4.0.5 on Ubuntu that installs the package and
  runs a smoke test using only base and recommended packages (it checks the
  declared minimum, `Depends: R (>= 4.0)`). All six jobs succeeded; the
  workflow treats warnings as failures. The individual notes of this run were
  not read; those of the previous revision (run 37844750201), reviewed in the
  re-audit of 2026-10-09, were only the new-submission note in each job.

Not run: win-builder and R-hub.

## Other checks

* `spelling::spell_check_package(vignettes = TRUE)`: no findings.
* `urlchecker::url_check()`: 14 URLs, all correct on 2026-10-09. The DOIs
  cited in DESCRIPTION are written as `<doi:...>`. The identity of the nine DOIs
  cited was verified against Crossref metadata in the review of 2026-10-03.
* mediaPlanR was absent from the CRAN package index and from the archive on
  2026-10-09 (HTTP 404). This is a dated availability check, not a reservation.

## Notes for the reviewer

* The package implements published reach and exposure-distribution models. Where
  it differs from a published table, `inst/DATA-PROVENANCE.md` states by how
  much, with the comparison cell by cell: the zero cell of Kim (1994, p. 139),
  the three-vehicle example of Cheong (2007, p. 75; largest absolute difference
  0.0020348568 in probability) and plan 19 of Kim (2005, Appendix B). The cause
  of these differences is not established: the documentation records the
  evidence that bears on each one (arithmetic inconsistencies in the printed
  tables, a duplication input that reproduces a printed row) and the regression
  bounds, which are above the real maxima. The comparisons assess numerical
  replication and consistency, not predictive accuracy against independent
  measurements.
* The safety net of `calc_mbd()` is a correction of the final distribution that
  the package adds; the help page states that it is not Cheong's MBD-ADJ.
* All packaged datasets are original illustrative examples. The inputs of
  published worked examples (Kim 2005, Cheong 2007) are not distributed in the
  archive or tracked in the repository: the comparisons with them are
  local-only tests of the maintainer, skipped when the files are absent. The
  earlier datasets that held them were removed because the basis for
  redistributing them was not established.
* The three doctoral dissertations cited in DESCRIPTION (Kim 1994, Kim 2005 and
  Cheong 2007) have no DOI; DESCRIPTION gives their type and institution
  (doctoral dissertation, University of Texas at Austin) and refers to the help
  pages for the complete references. The repository handles of Kim (2005) and
  Cheong (2007) appear, as plain text, in
  the references of the help pages: the repository answers HTTP 403 to automated
  requests, so a link would fail the URL check. No repository identifier was
  found for Kim (1994).
* The DOI of Cheong, Leckenby and Eakin (2011) is written as plain text in two
  help pages instead of a `\doi{}` link, because that macro drops the hyphen of
  this DOI when the PDF manual is typeset.

## Before submitting (maintainer's checklist)

* The tag `v2.0.0` already exists on GitHub and points to an old commit
  (`846c336`, 2026-09-24). Decide whether to move it to the submitted commit or
  to tag the release only after CRAN accepts it (recommended: tag after
  acceptance).
* The redistribution basis in `inst/DATA-PROVENANCE.md` is the maintainer's
  position, not a permission granted by the authors or their institutions;
  attribution does not establish permission.
* Submit through <https://cran.r-project.org/submit.html> with the archive above
  (`cran_submission/`), then confirm the e-mail sent to the maintainer address. Paste the sections
  "R CMD check results" and "Notes for the reviewer" in the comments field.
* Optional, before submitting: `devtools::check_win_devel()`.
