## Submission

This is a new submission (mediaPlanR 2.0.0).

## Test environments

* Local: Windows 11, R 4.4.1 (`R CMD check --as-cran`).
* GitHub Actions (see `.github/workflows/R-CMD-check.yaml`): macOS (release),
  Windows (release), Ubuntu (devel, release and oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "New submission".

## Notes for the reviewers

* The DESCRIPTION cites Kim (2005), Kim (1994) and Cheong (2007), which are
  unpublished doctoral dissertations without a DOI; all other references carry a
  DOI.
* Reference data in `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` are the
  minimal numeric inputs of the worked examples published in those
  dissertations, included with attribution so that users can verify the models;
  every other dataset is original.
