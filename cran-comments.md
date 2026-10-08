## Submission

mediaPlanR 2.0.0, a new submission (first release on CRAN). Date field 2026-10-08.

Source archive: `mediaPlanR_2.0.0.tar.gz`, built with `R CMD build` (R 4.4.1,
Windows) from commit `39197063e458af18ab2fd34ebf5e6be48f3226f0`.
SHA-256: `2b662491fcccacf916179536376cc39a9febfc79aaba15f333d24be5b06d89ff`.
Later commits change only this file, which is excluded from the archive
through `.Rbuildignore`.

## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new submission.
* "unable to verify current time" (checking for future file timestamps). It
  appears only on the local Windows machine, which could not reach a time
  server; it does not depend on the package.
* "Skipping checking math rendering: package 'V8' unavailable" (checking HTML
  version of manual). V8 is not installed on the local machine.

## Test environments

* Windows 11, R 4.4.1 (2024-06-14 ucrt), local: `R CMD check --as-cran
  --run-donttest` with `_R_CHECK_FORCE_SUGGESTS_` and
  `_R_CHECK_CRAN_INCOMING_` set to true, on the archive above, including the
  PDF manual, the examples, the tests (2537 expectations pass and 8 are
  skipped in the archive) and the rebuilt vignette. The complete suite in the
  source tree (2553 expectations, 0 failures) also passes locally.
* GitHub Actions, run 37813128785, commit `3919706` (the source of the archive):
  `R CMD check --as-cran --run-donttest` on Ubuntu (R release, oldrel-1 and
  devel), macOS (R release) and Windows (R release), plus a job with R 4.0.5
  on Ubuntu that installs the package and runs a smoke test using only base
  and recommended packages (it checks the declared minimum, `Depends: R
  (>= 4.0)`). All six jobs succeeded; the workflow treats warnings as
  failures. The notes reported by each job were not reviewed in this revision.

Not run: win-builder and R-hub.

## Other checks

* `spelling::spell_check_package(vignettes = TRUE)`: no findings.
* `urlchecker::url_check()`: 14 URLs; 6 correct and 8 `https://doi.org/` links
  (README.md and the vignette) answered HTTP 403 to automated requests from the
  local machine, as publishers commonly do. The DOIs cited in DESCRIPTION are
  written as `<doi:...>`. The identity of the nine DOIs cited was verified
  against Crossref metadata in the review of 2026-10-03.
* mediaPlanR was absent from the CRAN package index and from the archive on
  2026-10-08 (HTTP 404). This is a dated availability check, not a reservation.

## Notes for the reviewer

* The package implements published reach and exposure-distribution models. Where
  it differs from a published table, the vignette and `inst/DATA-PROVENANCE.md`
  state by how much and why: the zero cell of Kim (1994, p. 139), the cells of the
  three-vehicle example of Cheong (2007, p. 75; up to 0.002) and plan 19 of Kim
  (2005, Appendix B). Each has an identified cause and explicit test bounds.
* The safety net of `calc_mbd()` is a correction of the final distribution that
  the package adds; the help page states that it is not Cheong's MBD-ADJ.
* Three datasets (`csd_kim2005`, `msad_kim2005`, `mbd_cheong2007`) reproduce the
  numeric inputs of one worked example each (reach and duplication figures)
  published in two doctoral dissertations and one article, with attribution, so
  that users can check the package against the sources. No text, figures or
  complete tables are included. `inst/DATA-PROVENANCE.md` records their origin.
  Two larger published-plan tables used only for validation are neither in the
  archive nor in the public repository; the tests that read them are skipped
  when the files are absent.
* The three doctoral dissertations cited in DESCRIPTION (Kim 1994, Kim 2005 and
  Cheong 2007) have no DOI and are listed as author (year) only. The
  repository handles of Kim (2005) and Cheong (2007) appear, as plain text, in
  the references of the help pages: the repository answers HTTP 403 to automated
  requests, so a link would fail the URL check. No repository identifier was
  found for Kim (1994).
* The DOI of Cheong, Leckenby and Eakin (2011) is written as plain text in two
  help pages instead of a `\doi{}` link, because that macro drops the hyphen of
  this DOI when the PDF manual is typeset.

## Before submitting (maintainer's checklist)

* The tag `v2.0.0` already exists on GitHub and points to an old commit
  (`846c336`, 2026-09-24). Decide whether to move it to the submitted commit or
  to tag the release only after CRAN accepts it.
* The redistribution basis in `inst/DATA-PROVENANCE.md` is the maintainer's
  position, not a permission granted by the authors or their institutions;
  attribution does not establish permission.
* Submit through <https://cran.r-project.org/submit.html> with the archive above,
  then confirm the e-mail sent to the maintainer address. Paste the sections
  "R CMD check results" and "Notes for the reviewer" in the comments field.
* Optional, before submitting: `devtools::check_win_devel()`.
