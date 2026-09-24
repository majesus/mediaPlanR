## Submission

This is a new submission (mediaPlanR 2.0.0).

## Test environments

* Local: Windows 11, R 4.4.1 (`R CMD check --as-cran`).
* win-builder: R Under development (2026-09-21 r90579 ucrt), Windows.
* GitHub Actions (see `.github/workflows/R-CMD-check.yaml`): macOS (release),
  Windows (release), Ubuntu (devel, release and oldrel-1).

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "New submission".
* The same note lists "Possibly misspelled words in DESCRIPTION": Agostini,
  Cheong, Danaher, Hofmans, Leckenby, Metheringham, Morgensztern and Sainsbury.
  They are surnames of the authors of the cited methods, spelled correctly.

## Notes for the reviewers

* The DESCRIPTION cites Kim (2005), Kim (1994) and Cheong (2007), which are
  doctoral dissertations without a DOI (Kim, 1994, is unpublished); all other
  references carry a DOI.
* Reference data in `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` are the
  minimal numeric inputs of the worked examples published in those
  dissertations, included with attribution so that users can verify the models;
  every other dataset is original.
