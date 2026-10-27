## Submission preparation

mediaPlanR 2.0.0, first CRAN submission. Prepared 2026-10-27.
This package has NOT been submitted to CRAN.

## Validation

The complete source-tree test suite passed locally on Windows with R 4.2.3
on 2026-10-27 (one installed-package-only test skipped).
The PDF manual was regenerated and visually checked: 61 pages, no overfull boxes.
The final source archive was built from commit
8356ec7b748caa2a4d29a42ced374e838d391a13.
SHA-256: 987c2e406e59b87ed5ad94c9a2a68865def6a0b5b323bfc5bf10079367299aef.

Windows / R 4.2.3: R CMD check --as-cran --run-donttest completed with
0 ERRORs, 0 WARNINGs and 2 NOTEs. Examples, tests, vignette rebuilding,
and PDF manual passed.

The incoming-feasibility NOTE reports a new submission and HTTP 403 responses
for eight doi.org URLs (six of these are also checked as DESCRIPTION DOIs).
These are access-denied responses; they do not establish that the identifiers
are missing. The original references are retained. A separate URL check
reproduced the same access restrictions. Crossref metadata retrieval succeeded
for five of the eight identifiers; the other three returned HTTP 429.
The second NOTE is "unable to verify current time" from the timestamp check.
These network-dependent checks should be repeated immediately before submission.

The Windows, macOS and Linux CI checks additionally report HTTP 404
for 10.1080/01633392.1984.10505351 in fit_bbd_to_reach.Rd. The publisher's
indexed article page identifies the same DOI, authors, title, journal and
pages for Leckenby and Boyd (1984):
https://www.tandfonline.com/doi/abs/10.1080/01633392.1984.10505351
Direct retrieval of that publisher page returns HTTP 403 in this environment.
The bibliographic identifier is retained; the resolver discrepancy is recorded
for review rather than treated as a successful DOI check.

GitHub Actions validation of the same source commit:
https://github.com/majesus/mediaPlanR/actions/runs/40579841697

- Windows / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE (new submission and DOI 404).
- macOS / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE (same incoming check).
- Linux / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE (same incoming check).
  The PDF and HTML versions of the manual both passed validation.
- Linux / R 4.5.3: 0 ERRORs, 0 WARNINGs, 1 NOTE (same incoming check).
- Linux / R-devel (2026-10-26 r90766): 0 ERRORs, 0 WARNINGs, 1 NOTE
  (same incoming check).
- Linux / R 4.0.5: installation and base-dependency compatibility checks passed.

The complete source-tree suite also passed in each of the five full-check
jobs above (0 failures, 0 warnings; local-only fixtures were skipped).
All six GitHub Actions jobs completed successfully. Every check NOTE was
reviewed; the network-dependent findings are explained above.

## Notes for the reviewer

The package implements classical reach and exposure-distribution models.
The help pages distinguish assumptions, algorithmic extensions, numerical
replication of published examples, and independent predictive validation.
Packaged datasets are original illustrative examples. Local published-plan
fixtures are excluded from the source archive and from the repository;
checks requiring those fixtures skip when they are absent.
The manual identifies corrections of negative probabilities and their effects
on marginal distributions and means. Negative-Binomial functions are scoped
as univariate, unbounded count approximations.

## Maintainer checklist

- Repeat network-dependent link and time checks immediately before submission.
- Submit only the archive identified by its SHA-256 in the final validation record.
- Leave the existing historical v2.0.0 tag unchanged; tag the accepted release later.
- Upload to CRAN only after an explicit maintainer decision.
