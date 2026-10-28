## Submission preparation

mediaPlanR 2.0.0, first CRAN submission. Prepared 2026-10-28.
This package has NOT been submitted to CRAN.

## Validation of the corrected source

The complete source-tree suite passed locally on Windows with R 4.4.1:
3442 expectations passed, 0 failures, 0 errors, 0 warnings, and 1 test skipped
because it requires an installed package. That installed-package loading test
was then run separately: all 6 expectations passed. An additional check of
105 two-insertion scenarios against the independent closed-form reach passed;
the maximum absolute reach error was 6.23e-14 in probability.

The correction expands the numerical bracket of fit_bbd_to_reach() for valid
interior targets close to either family limit. Regression tests cover the
reported near-binomial failure and a near-polarized interior solution.
The help page and NEWS describe the correction.

Current-R CI and a complete check of the corrected source archive, including
the PDF manual, are still PENDING. The earlier six successful CI jobs concern
the previous source and do not clear this correction for submission. The old
archive with SHA-256 987c2e406e59b87ed5ad94c9a2a68865def6a0b5b323bfc5bf10079367299aef
is superseded and must not be submitted.

The updated vignette has been rebuilt with Pandoc 3.9.0.2 and a draft archive
has been built with R 4.4.1. The local full check is blocked at dependency
discovery: file.access() in the Windows sandbox reports -1 even for readable
files, so installed Suggests appear unavailable. Network checks also fail to
resolve hosts. These are recorded limitations, not successful checks.

The CI configuration now builds and checks the exact submission archive with
current release R and preserves the archive, SHA-256, check log and PDF manual.
Publishing the correction is currently blocked by GitHub integration HTTP 403;
the updated CI has not yet run.

## Previously observed incoming NOTE

The previous CI checks reported a new submission and HTTP 404 for
10.1080/01633392.1984.10505351 in fit_bbd_to_reach.Rd.
The publisher's indexed article page identifies that same DOI, John D.
Leckenby and Marsha M. Boyd, "An Improved Beta Binomial Reach/Frequency Model
for Magazines", Current Issues and Research in Advertising, 7(1), 1-24:
https://www.tandfonline.com/doi/abs/10.1080/01633392.1984.10505351
The publisher record was checked again on 2026-10-28. No replacement DOI was
verified. The bibliographic identifier is retained and the discrepancy is
reported; it is not represented as a successful resolver check. All incoming
checks and their NOTEs must be reviewed again on the corrected archive.

## Notes for the reviewer

The package implements classical reach and exposure-distribution models.
The help pages distinguish assumptions, algorithmic extensions, numerical
replication of published examples, and independent predictive validation.
Packaged datasets are original illustrative examples. Local published-plan
fixtures are excluded from the source archive and repository; their tests
skip when the fixtures are absent. Corrections of negative probabilities and
their effects on margins and means are documented. Negative-Binomial
functions are scoped as univariate, unbounded count approximations.

## Maintainer checklist

- Complete current-R CI and review every NOTE before submission.
- Record the source commit and SHA-256 of the exact checked archive.
- Repeat network-dependent checks immediately before submission.
- Do not submit either the superseded archive or the unvalidated local draft.
- Leave the historical v2.0.0 tag unchanged; tag the accepted release later.
- Upload to CRAN only after an explicit maintainer decision.
