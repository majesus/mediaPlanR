## Submission preparation

mediaPlanR 2.0.0, first CRAN submission. Prepared 2026-10-27.
This package has NOT been submitted to CRAN.

## Validation

The complete source-tree test suite passed locally on Windows with R 4.2.3
on 2026-10-27 (one installed-package-only test skipped).
The PDF manual was regenerated and visually checked: 61 pages, no overfull boxes.
Current archive checks and GitHub Actions results are recorded below after completion.
Earlier archive hashes and CI results do not certify this revision.

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

- Review the final R CMD check logs, including every NOTE.
- Confirm current-release and R-devel CI, Windows, Linux and macOS.
- Submit only the archive identified by its SHA-256 in the final validation record.
- Leave the existing historical v2.0.0 tag unchanged; tag the accepted release later.
- Upload to CRAN only after an explicit maintainer decision.
