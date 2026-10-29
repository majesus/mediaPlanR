## Submission preparation

mediaPlanR 2.0.0, first CRAN submission. Prepared 2026-10-28.
This package has NOT been submitted to CRAN.

## Validated source and archive

Source commit: 29903414a274d0aa79898dbe39d6e81320d858c1.
Archive: mediaPlanR_2.0.0.tar.gz.
SHA-256: f688c9239eb818eb0e32447b5aec70bb722bf534774c00c7ba97f5996747f8e2.

The exact archive above was built and checked on Linux with R 4.6.1 using
R CMD check --as-cran --run-donttest: 0 ERRORs, 0 WARNINGs, 1 NOTE.
Examples, tests, vignette rebuilding, PDF manual and HTML manual passed.
The archive, checksum, check log and check-generated PDF were preserved in
GitHub Actions and downloaded locally; the downloaded checksum matches.

GitHub Actions: https://github.com/majesus/mediaPlanR/actions/runs/40698607806
All six jobs completed successfully:
- Windows / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE.
- macOS / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE.
- Linux / R 4.6.1: 0 ERRORs, 0 WARNINGs, 1 NOTE.
- Linux / R 4.5.3: 0 ERRORs, 0 WARNINGs, 1 NOTE.
- Linux / R-devel (2026-10-26 r90766): 0 ERRORs, 0 WARNINGs, 1 NOTE.
- Linux / R 4.0.5: installation and base-dependency compatibility passed.

Each full-check job passed 3379 expectations with no failures or warnings.
The additional source-tree suites passed 3383 expectations, with local-only
fixtures and installed-package-only checks skipped as appropriate.
The complete local source-tree suite also passed on Windows / R 4.2.3,
including the local published-plan fixtures; one installed-only test skipped.
The independent PDF manual was regenerated, visually inspected on the title
page and changed page 47, and has 61 pages with no overfull boxes.

The correction expands the numerical bracket of fit_bbd_to_reach() for valid
interior targets near either family limit. Regression tests include an
independent two-insertion reach identity, mass and expected exposures.
The help page and NEWS describe the correction.

## Changes after the validated archive (2026-10-29)

The archive identified above is superseded. The sources changed after it was
built (documentation and metadata only; no function code was touched), and the
archive to submit is the one built from source commit
b6dc4f0 (main) by GitHub Actions run #39:
https://github.com/majesus/mediaPlanR/actions/runs/40823414107
The run completed successfully on all its jobs (Linux release, oldrel-1 and
devel, macOS, Windows, and the R 4.0.5 base-dependency job) and published the
artifact CRAN-submission-b6dc4f0 together with the per-platform check results.
Still to do by the maintainer before submission: download that artifact, read
the check logs (0 ERRORs and 0 WARNINGs are required; list every NOTE), and
record here the file name and SHA-256 of the archive actually uploaded. Until
that is done, the numbers of the superseded section below must not be quoted
for this archive.

- Date field set to 2026-10-29; set it to the actual submission date.
- The DOI of Leckenby and Boyd (1984) was removed from the help page and the
  README because the resolver returns HTTP 404 and the identifier could not be
  verified; the full bibliographic reference is retained.
- The DOI of Cheong, Leckenby and Eakin (2011) in calc_mbd() now uses \doi{}.
- References added to the help pages where the original source was missing:
  Metheringham (1964), Chandon (1976) and Leckenby and Boyd (1984) for
  calc_beta_binomial(); Danaher (1992) for calc_csd(); Morgensztern (1970), as
  listed by Kim (2005), for calc_msad(); Danaher (2007) and Ehrenberg (1959)
  for fit_nbd_exposure(); Metheringham (1964) and Chandon (1976) for
  calibrate_bbd().

## Incoming NOTE (before these changes)

Every incoming check reports a new submission and HTTP 404 for
10.1080/01633392.1984.10505351 in fit_bbd_to_reach.Rd.
The publisher's indexed article page identifies that same DOI, John D.
Leckenby and Marsha M. Boyd, An Improved Beta Binomial Reach/Frequency Model
for Magazines, Current Issues and Research in Advertising, 7(1), 1-24:
https://www.tandfonline.com/doi/abs/10.1080/01633392.1984.10505351
The publisher record was checked on 2026-10-28. A Crossref title search
(rows=3) returned unrelated records and no replacement DOI. This limited
search does not prove that no alternative exists. The bibliographic identifier
is retained; the resolver discrepancy is not treated as a successful check.

The earlier local 403 responses for eight other DOI URLs were access-denied
responses, not evidence of invalid identifiers. They did not appear as
findings in the current final archive check. All current NOTEs were reviewed.

## Notes for the reviewer

The help pages distinguish model assumptions, algorithmic extensions,
numerical replication and independent predictive validation. Packaged datasets
are original illustrative examples. Local published-plan fixtures are excluded
from the repository and archive; tests needing them skip when absent.
Corrections of negative probabilities and their effects on margins and means
are documented. Negative-Binomial functions are scoped as univariate,
unbounded count approximations.

## Before eventual submission

- Repeat network-dependent checks near the actual submission date.
- Submit only the archive identified above; previous archives are superseded.
- Leave the historical v2.0.0 tag unchanged; tag the accepted release later.
- Upload to CRAN only after an explicit maintainer decision.
