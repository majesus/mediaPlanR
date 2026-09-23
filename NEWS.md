# mediaPlanR 2.0.0

First CRAN release. It is a complete redesign of the development snapshots
0.1.x and 0.2.0, which were distributed only through GitHub, and it is not
backward compatible with them: function names, arguments, example datasets and
returned objects changed. The Shiny explorers and the budget/KPI helpers of the
earlier snapshots are not part of this package.

## Planning workflow

* `media_plan()` creates a validated, unit-explicit media plan; `plan_metrics()`
  and `audience_metrics()` compute impressions, spend, rating points, cost
  ratios, target composition, target rating and affinity.
* `estimate_reach()` and `compare_reach_models()` return complete exposure
  distributions (including zero exposures) under the Sainsbury and Binomial
  models, with reach in probability, percent and people.
* `optimize_media_plan()` maximizes effective reach for a budget, or minimizes
  spend for a target effective reach. Exhaustive search verifies a global
  optimum; the greedy heuristic is labeled as such and evaluates moves of up to
  `effective_frequency` insertions, so it also works when effective reach needs
  several exposures.

## Reach and exposure-distribution models

* Random duplication: `calc_sainsbury()` (exact Poisson-binomial distribution by
  dynamic convolution) and `calc_binomial()`, both with an `insertions`
  argument for several insertions per vehicle.
* One vehicle, several insertions: `calc_beta_binomial()`, with the binomial and
  polarized limits handled explicitly, and `calc_hofmans_accumulation()`
  (Hofmans' equations 3.11 and 3.12 in Aldás Manzano, 1998; an optional third
  observation `R3` estimates the variable-coefficient exponent).
* Several vehicles, one insertion each: `calc_agostini_duplication()`
  (`R = A^2 / (A + k D)` with Agostini's `k = 1.125`) and
  `calc_hofmans_duplication()` (pairwise coefficients). Both take the
  duplication matrix and return reach only.
* Several vehicles and insertions: `calc_metheringham()` (a Beta-Binomial for
  `N = sum(insertions)` insertions, from audiences and duplications averaged
  over all pairs of insertions, following Aldás Manzano, 1998, Section
  3.3.1.5), `calc_canex()`, `calc_csd()`, `calc_msad()`, `calc_cbd()` and
  `calc_mbd()`.
* `fit_bbd_to_reach()` and `calibrate_bbd()` fit a Beta-Binomial to an external
  reach or to a target effective reach.
* `nbd_exposure_distribution()` and `fit_nbd_exposure()` provide an explicitly
  scoped Negative-Binomial approximation for unbounded exposure-count
  processes.
* `evaluate_exposure_model()` compares predicted and observed exposure
  distributions with declared scales and identical supports, reproducing Kim's
  (2005) average error in reach and in the exposure distribution.

## Validation and reproducibility

* Every exported function validates its arguments and reports `NA`, `NaN`,
  `Inf`, wrong types and wrong lengths with informative errors.
* The ad hoc formulas of Agostini and Hofmans (duplication) check the
  duplication matrix against the Fréchet bounds implied by the audiences and
  the population, and warn when the reach they compute leaves its logical
  range.
* `calc_csd()` reproduces the worked example of Kim (2005) within the rounding of
  its published tables, and `calc_mbd()` the worked example of Cheong (2007).
* `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` contain the published
  numeric inputs of those examples, with attribution. All other example
  datasets are original illustrative data, ready for `do.call()`.

## Methodological notes

* CANEX truncates the negative probabilities that its second-order expansion can
  produce and reports the truncated mass in `diagnostics`; CSD and MSAD stop
  instead of altering an invalid target. MBD and CBD zero negative cells after
  peeling and redistribute their mass proportionally (Cheong's MBD-ADJ), and
  `calc_mbd()` warns for four or more vehicles.
* `calc_mbd()` and `calc_cbd()` support at most 12 vehicles because of the
  exponential cost of their exposure grid.
* The affinity index equals the target rating divided by the gross rating, so
  `audience_metrics()` returns it once.
