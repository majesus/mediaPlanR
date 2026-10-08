# mediaPlanR 2.0.0

First CRAN release. It is a complete redesign of the development snapshots
0.1.x and 0.2.0, which were distributed only through GitHub, and it is not
backward compatible with them: function names, arguments, example datasets and
returned objects changed. The Shiny explorers and the budget/KPI helpers of the
earlier snapshots are not part of this package.

## Corrections from the academic audit of the manual (2026-10-08)

Validation:

* `plan_metrics(reach =)` stops when the reach cannot be produced by the plan:
  below the largest audience among the channels with insertions, or above the
  smaller of the population and the plan's impressions.
* `fit_bbd_to_reach()` separates two cases: a reach that is logically impossible
  for the schedule (below the largest audience or above its contacts) and a
  reach that is possible but that the Beta-Binomial family cannot represent.
  Its `precision` default is now `NULL`, meaning `1e-4 * population` (the former
  100 people for a universe of one million, and a scale-appropriate tolerance
  for small universes); the printed fit shows the reaches with enough digits.
* `audience_metrics()` also requires that the audience outside the target fits
  in the universe outside the target.
* `evaluate_exposure_model()` rejects any probability above one (or percentage
  above 100) before applying the tolerance of the total, and has a new argument
  `censored_last_level` for tables whose last level is a collapsed tail: the
  exact mean and `mean_contact_bias` are then `NA`, and the means of the
  censored codes are reported separately.
* The average frequency of a plan with no reach is `NA` in `estimate_reach()`
  and `calc_canex()` (as it already was in `plan_metrics()`), instead of zero.

Documentation:

* The safety net of `calc_mbd()` is described as a package correction of the
  final distribution, not as Cheong's MBD-ADJ, which acts on the joint-exposure
  table before it is expanded; the study's accuracy figures are attributed to
  its own variants (MBD, MBD-ADJ, MBD-ADJ2).
* The help pages state the unit and scope of every quantity that the audit found
  ambiguous: the exceptions to the percentage rule, contacts versus people versus
  pairs, the limits of the Beta-Binomial family (`alpha`, `beta` of `0` and
  `Inf` are indicators of a limit), the homogenized Binomial, the target
  columns (descriptive; they do not change `estimate_reach()` or the
  optimizer), effective frequency as an analyst's threshold, calibration as a
  search that can fail, the Negative-Binomial result and evaluation result structures,
  the open-tail mean, the sample required by `fit_nbd_exposure()`, normalization
  and the positive observed reach of the evaluator, and the contract
  differences of `calc_canex()` (`R2` for every vehicle).
* The general statement about errors and warnings distinguishes violated data
  constraints from approximations that fail on feasible data.
* The replication of published tables is separated from validation against
  independent observations, the datasets state what is and is not distributed,
  and a suggested study path is added to `?mediaPlanR`.
* The DOI of Cheong, Leckenby and Eakin (2011) in `calc_mbd()` and
  `mbd_cheong2007` is written as plain text (`doi:10.2753/...`) instead of a
  `https://doi.org/` link: `R CMD check --as-cran` asks for the `\doi{}` macro
  for links, but that macro drops the hyphen of this particular DOI when the
  PDF manual is typeset.

## Harmonized argument names (2026-10-08)

* `calc_beta_binomial()` now takes the reach proportions `R1` and `R2`, the
  number of `insertions` and the `population` (default 1), instead of the
  audiences `A1`, `A2` in people, `n` and `P`. This matches
  `calc_hofmans_accumulation()`, `calc_canex()` and the sequential models.
* `calc_hofmans_accumulation()` takes `insertions` instead of `N`, and its
  results table names that column `insertions`.
* `fit_bbd_to_reach()` takes `population` instead of `universe` (and reports it
  as `parameters$population`); `calibrate_bbd()` takes `R1` instead of
  `first_reach`.
* The `vehicles_data` of `calc_canex()` accepts a column named `insertions`,
  as the sequential models do, besides the original `k`.
* New "Notation and units" section in `?mediaPlanR`.
* `calc_canex()` now uses `population = 1` by default, like the sequential
  models and `calc_beta_binomial()`, so that the `people` columns equal the
  probabilities unless a population is supplied. The printed reports leave out
  the number of people when the population is 1.
* Invalid reach arguments are reported with the admissible range of `R2`, and a
  hint when values look like percentages instead of proportions. The Metheringham
  error about the minimum number of insertions is more explicit.
* `?mediaPlanR` has new "Assumptions of the models" and "Notation and units"
  sections; `?calc_canex` opens its "Domain of validity" section in plain
  words, and the vignette adds a table of what to do when a function stops or
  warns. The help pages state the unit of every argument (people, proportion
  between 0 and 1) and that `aggregation_order` does not affect `calc_cbd()`.
* The titles of the example datasets follow one pattern ("Illustrative inputs
  for ..." or "Published inputs for ... (Author, year)").

## Full report for the sequential models (2026-10-07)

* The print methods of `calc_csd()`, `calc_msad()`, `calc_cbd()` and `calc_mbd()`
  now show the same kind of report as the classical models: headline metrics,
  parameters, aggregation steps, exposure and cumulative distributions, and
  diagnostics. `max_rows` cuts long distributions with a note, and
  `print(x, full = FALSE)` gives the previous four-line summary.
* The README now explains how to run the models with the analyst's own data and
  lists the inputs and units of every model.

## Audit corrections (2026-10-03)

* Pairwise symmetry is checked relative to each duplicated audience, including
  very small audiences and Metheringham's used matrix entries. Fréchet bounds
  in the ad hoc models and Metheringham use the smaller audience as their scale.
* CANEX always normalizes after truncation and computes positive-exposure reach
  without subtracting nearly equal numbers. Tiny positive reach retains its
  conditional average frequency. Truncation warnings report both means without
  claiming that they must differ.
* Sainsbury, Binomial and Beta-Binomial sum positive-exposure probabilities to
  retain tiny reach. Hofmans accumulation accepts positive small duplications;
  algebraically equivalent evaluations avoid unnecessary squares and products.
  Agostini, Hofmans duplication and Metheringham avoid intermediate overflow
  for large finite audience counts. Unrepresentable totals in plan metrics and
  unsupported Beta-Binomial insertion counts fail explicitly.
* Regression tests now distinguish malformed numeric matrices from wrong matrix
  storage types. Optional extraDistr comparisons are skipped when it is absent.
  The installed-package load test runs on Windows and checks process state.
  CBD benchmarks cover all 40 two-vehicle Hong schedules (previously 32) and
  all 40 Kim schedules, retaining the 0.02 percentage-point per-cell bound.
  Tests use no-warning expectations supported by older testthat releases.
  Kim's plan 19 MSAD row is now tested against random duplication, which
  reproduces every printed percentage after rounding. The common reconstructed
  fixture is preserved; the former 0.11 percentage-point exception is removed.
* CI includes a manual PDF job, archived check logs and NOTE summaries, plus a
  base-dependency R 4.0.5 compatibility job. README is included in the tarball.
  `DATA-PROVENANCE.md` records dataset/fixture sources, the redistribution
  basis and numerical discrepancies. The two published-plan fixtures (whole
  numeric appendices of Kim 2005 and Hong 1998) are excluded from the package
  archive; the tests that read them are skipped when absent and the CI workflow
  runs the complete suite from the source tree. HBBD/DMD descriptions no longer
  claim that the available literature lacks their specifications.

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
  `Inf`, logical, factor and list values, wrong types and wrong lengths with
  informative errors; `media_plan()` accepts only numeric columns for audience,
  insertions, cost and target audience.
* The ad hoc formulas of Agostini and Hofmans (duplication) check the
  duplication matrix against the Fréchet bounds implied by the audiences and
  the population, and warn when the reach they compute leaves its logical
  range.
* `calc_csd()` reproduces the worked example of Kim (2005) within the rounding of
  its published tables; `calc_msad()` the numerical example of Lee (1988,
  pp. 81-93); `calc_cbd()` the worked example of Kim (1994, p. 139) except for
  its zero cell (about 0.4 percentage points) and the CBD columns of Kim (2005,
  Appendix B) and Hong (1998, Appendix E); and `calc_mbd()` the worked example of
  Cheong (2007, p. 75; also Cheong, Leckenby and Eakin, 2011), to within 0.002
  per cell. `calc_canex()`, `calc_csd()` and `calc_msad()` also reproduce the 40
  plans of Kim (2005, Appendix B) within 0.02 percentage points per cell
  (MSAD plan 19, 0.099 with the common reconstructed duplication; its printed
  row is reproduced with random duplication). The three remaining differences
  with published values (Kim 1994 zero cell, Cheong MBD 0.002, Kim 2005 MSAD
  plan 19) have identified causes recorded in `inst/DATA-PROVENANCE.md`. The
  test suite checks all of these.
* `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` contain the published
  numeric inputs of those examples, with attribution. All other example
  datasets are original illustrative data, ready for `do.call()`.

## Dependencies

* `extraDistr` and `ggplot2` are suggested, not imported: the Beta-Binomial
  probability function is computed internally (and cross-checked against
  `extraDistr` in the tests), and the plot of `calc_hofmans_accumulation()` is
  `NULL` when `ggplot2` is not installed.

## Methodological notes

* CANEX truncates the negative probabilities that its second-order expansion can
  produce and reports the truncated mass in `diagnostics`; Danaher (1991)
  justifies this because that mass is negligible, and the documentation says
  what happens to the mean exposure when it is not. CSD, MSAD and CBD stop
  instead of altering an invalid canonical target. MBD and CBD zero any negative
  cell that remains and redistribute its mass proportionally (Cheong's
  MBD-ADJ), and `calc_mbd()` warns for four or more vehicles.
* `calc_cbd()` follows Kim's (1994) specification: the vehicles are
  conditionally independent given the (0,1) grid, the zero cell is conformed to
  the canonical expansion of all the insertions, and the result does not depend
  on `aggregation_order`. The returned object reports the within-vehicle
  distribution used for each vehicle in `vehicle_expansion`.
* The documentation of the multivariate models states their domain of validity:
  duplications below the random one, pairwise duplications that are jointly
  impossible for three or more vehicles, and the inputs for which CSD, MSAD and
  CBD stop. The five models stop when some triple of vehicles has no non-negative
  joint exposure probability compatible with its audiences and pairwise
  duplications (a necessary condition; for four or more vehicles it is not
  sufficient), and warn (class `mediaPlanR_low_duplication`) when the canonical
  correlation of a pair is below -0.1. `calc_canex()` warns (class
  `mediaPlanR_canex_truncation`) when the truncated negative mass exceeds 1e-4
  and reports the expected and returned mean number of exposures. Both
  thresholds are decisions of this package, not of the cited authors.
* The limits of the Beta-Binomial (binomial and polarized) and the degeneracy of
  the canonical correlation are recognized relative to the variance of the
  one-insertion exposure, not with absolute thresholds, so that schedules with
  very small audiences keep their parameters. The Frechet checks use a
  tolerance relative to the smaller audience.
* `calc_mbd()` and `calc_cbd()` support at most 12 vehicles because of the
  exponential cost of their exposure grid.
* The affinity index equals the target rating divided by the gross rating, so
  `audience_metrics()` returns it once.
