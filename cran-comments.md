## Submission

mediaPlanR 2.0.0, a new submission (first release on CRAN). Date field 2026-10-07.

Source archive: `mediaPlanR_2.0.0.tar.gz`, built with `R CMD build` from commit
`ad73fa684a4814d8531e6759ff1f9aca240d1182` (R 4.6.1).
SHA-256: `53ee4de2af66824761b0816732f1d315dcf4ceded9ebaf07f16f985fa9b38413`.
Later commits change only this file, which is excluded from the archive
through `.Rbuildignore`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

All with `R CMD check --as-cran --run-donttest`, with `_R_CHECK_FORCE_SUGGESTS_`
and `_R_CHECK_CRAN_INCOMING_` set to true.

* Windows 11, R 4.6.1 (2026-06-24 ucrt), local. The archive above, including
  the PDF manual, the HTML manual (with V8 and tidy), the examples, the tests
  (2472 expectations in the source tree) and the rebuilt vignette. Only the
  new-submission note.
* GitHub Actions, run 37665171464, commit `798ad88` (it differs from the
  archive only by the `Date` field and the spelling word list): macOS arm64 R 4.6.1,
  Windows Server 2022 R 4.6.1, Ubuntu 24.04 R 4.6.1 (with the PDF manual),
  R 4.5.3 and R-devel (r90643, 2026-10-06). Each reports 0 errors, 0 warnings
  and only the new-submission note. The complete logs were reviewed.
* GitHub Actions, same run: R 4.0.5 on Ubuntu, installation and a smoke test
  that uses only base and recommended packages (it checks the declared minimum,
  `Depends: R (>= 4.0)`).

Not run: win-builder and R-hub.

## Other checks

* `spelling::spell_check_package(vignettes = TRUE)`: no findings.
* `urlchecker::url_check()`: all 14 URLs correct. The identity of the nine DOIs
  cited was verified against Crossref metadata in the review of 2026-10-03; some
  publishers answer HTTP 403 to automated requests.
* mediaPlanR was absent from the CRAN package index and from the archive on
  2026-10-07 (HTTP 404). This is a dated availability check, not a reservation.

## Notes for the reviewer

* The package implements published reach and exposure-distribution models. Where
  it differs from a published table, the vignette and `inst/DATA-PROVENANCE.md`
  state by how much and why: the zero cell of Kim (1994, p. 139), the cells of the
  three-vehicle example of Cheong (2007, p. 75; up to 0.002) and plan 19 of Kim
  (2005, Appendix B). Each has an identified cause and explicit test bounds.
* Three datasets (`csd_kim2005`, `msad_kim2005`, `mbd_cheong2007`) reproduce the
  minimal numeric inputs (reach and duplication figures) published in two
  doctoral dissertations and one article, with attribution, so that users can
  check the package against the sources. `inst/DATA-PROVENANCE.md` records their
  origin and the maintainer's redistribution basis. Two larger published-plan
  fixtures used only for validation are not distributed in the archive; the tests
  that read them are skipped when they are absent.

## Before submitting (maintainer's checklist)

* Confirm that the Actions run for the last pushed commit is green and that its
  `Date` and word-list changes are the only difference with `798ad88`.
* The redistribution basis in `inst/DATA-PROVENANCE.md` is the maintainer's
  position, not a permission granted by the authors or their institutions;
  attribution does not establish permission. Decide whether to keep the three
  datasets as they are.
* Persistent identifiers for the Kim (1994, 2005) and Cheong (2007) theses are not
  all verified (candidate Texas repository handles returned HTTP 403), so they were
  not added to the references.
* Submit through <https://cran.r-project.org/submit.html> with the archive above,
  then confirm the e-mail sent to the maintainer address. Paste the sections
  "R CMD check results" and "Notes for the reviewer" in the comments field.
* Optional, before submitting: `devtools::check_win_devel()`.
