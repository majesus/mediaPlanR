# mediaPlanR 2.0.0

First CRAN release. It is a complete redesign of the development snapshots
0.1.x and 0.2.0, which were distributed only through GitHub, and it is not
backward compatible with them: function names, arguments, example datasets and
returned objects changed. The Shiny explorers and the budget/KPI helpers of the
earlier snapshots are not part of this package. This release contains the reach
and exposure-distribution models, the fitting of the Beta-Binomial model to an
observed reach, a scoped Negative-Binomial approximation and the evaluation of
predicted distributions against observed ones.

## Correction from the pre-submission review of 2026-10-28

* `fit_bbd_to_reach()` now expands its log-concentration search interval to
  bracket interior targets near the binomial or polarized limit. Previously,
  valid targets outside the initial numerical bracket could cause a raw
  `uniroot()` error. Regression checks include an independent two-insertion
  identity, probability mass and the mean exposure implied by the inputs.
  Exact boundary handling and checks against the schedule bounds are retained.

## Documentation of the Hofmans Beta-Binomial procedure (HBBD)

* The help page of `fit_bbd_to_reach()` and the vignette now name the
  two-step Hofmans Beta-Binomial (HBBD) procedure explicitly and give a worked
  example: the reach of `calc_hofmans_duplication()` is passed to
  `fit_bbd_to_reach()` with one insertion per vehicle. There is no single
  `calc_hbbd()` function, and the composition is not validated against a
  published numeric example. A test checks that the composition preserves the
  Hofmans reach and the mean exposure implied by the audiences.

## Corrections from the numerical audit of 2026-10-12 (MP-01 to MP-12)

Each correction is checked by `tests/testthat/test-audit-2026-10-12-followup.R`,
whose witnesses (closed forms, a recurrence written in the test, base R
distributions) do not use the code under test. Results that were correct do not
change beyond rounding (about 1e-13 in a cell).

* `calc_csd()` and `calc_msad()` (MP-01): the reach of every marginal, of every
  step and of the result is now the sum of the positive cells, not `1 - P(0)`,
  and the joint table of a step is built from the probability of being
  reached by both parts, so that a small audience combined with a large one
  keeps its digits. With one insertion in each of two vehicles of audience
  `p = 1e-17`, both models stopped (`missing value where TRUE/FALSE needed`); for
  `p = 1e-16` they reported a reach of `2.22e-16` and an average frequency of
  0.90, below one; for `p = 1e-12` in a population of `1e12` they reported
  1.99996 people instead of 2. The average frequency is computed as one plus the
  excess over one, which cannot fall below one. The Morgensztern reach forms the
  ratio of reach to audience of each vehicle before the product, so that it does
  not underflow. The collapse of the joint table by total exposures is linear in
  its cells (it was quadratic).
* Beta-Binomial probability function (MP-02, used by every model that expands a
  vehicle): the distribution is now normalized and is evaluated by the log-beta
  form below a concentration `alpha + beta` of 10 and by the recurrence of
  consecutive probabilities above it. Near the independence limit the former
  difference of log-beta functions summed to one only to about 1e-8 (a reach of
  1.0000000081 for 500 insertions, `R1 = 0.3` and `R2` at 2e-9 of the limit, and
  a mean of 150.0000012 instead of 150), which also made `calc_csd()` reject a
  valid plan (insertions 50 and 2, `R1` 0.3 and 0.2, duplication 0.06). The mass
  is now one and the mean is `n * R1` to about 1e-13. The variance used by the
  canonical expansion is written so that it does not overflow for a large
  concentration. `calc_canex()` evaluates the marginals once per vehicle, not
  once per exposure level.
* `fit_bbd_to_reach()` (MP-03): the binomial bound of the feasible interval is
  `-expm1(N * log1p(-mean))` times the population; `(1 - (1 - mean)^N)` lost the
  whole bound for one person in `1e17` and reported the interval `[1, 0]`. The
  bounds are not widened by `precision`.
* `nbd_exposure_distribution()` (MP-04, MP-07): the reach for a `size` so small
  that `mean_contacts / size` overflows (`1e-308`) is no longer one (it is
  `7.1e-306`, as base R's `pnbinom()` gives); the variance does not overflow
  through `mean_contacts^2` (`mean_contacts = size = 1e200` gives `2e200`), and
  is `Inf` only when it exceeds the largest double. A default table whose
  0.999 quantile exceeds one million levels is refused with an explanation,
  without the coercion warning to integer.
* `calc_csd()` and `calc_cbd()` (MP-05): the canonical correlation terms are
  formed from the mean over the standard deviation of each vehicle, so that two
  audiences of `1e-200` (a product of variances of `1e-400`, below the smallest double)
  are no longer rejected as `NaN`, and a pair of zero correlation contributes
  nothing. Cells of the CBD grid whose product of audiences is below the
  smallest double are evaluated on the log scale.
* `evaluate_exposure_model()` (MP-06): a table with an exposure level beyond the
  integer range, or with no rows, is rejected with a message that names the
  table, the schedule and the level, without a coercion warning and without the
  generic `'to' must be a finite number`. Incomplete supports and duplicated
  cells are still rejected, as before.
* `calibrate_bbd()` (MP-08): the binomial and polarized limits of the family are
  evaluated as candidates for every number of insertions of at least two, and
  are returned only if they improve the error by more than 1e-12, so the results
  that the interval already resolved do not change. A target equal to the
  binomial limit (`R1 = 0.3`, `target_reach = 0.51`, two insertions) is now
  reached exactly. The result has the new element `fit_type`, the candidates the
  new column `fit_type`, and the help page states the interval of log
  concentrations searched.
* `calc_canex()` (MP-09): `R2` may be `NA` (or any value, which is ignored) for a
  vehicle with a single insertion, as in the sequential models; it is still
  required and validated for vehicles with two or more insertions. The
  distribution of a vehicle with one insertion is a Bernoulli of probability
  `R1`, as before.
* `calc_csd()`, `calc_msad()`, `calc_cbd()` and `calc_mbd()` (MP-10): the size of
  the largest joint table (CSD and MSAD) or the work (CBD and MBD) is estimated
  from the insertions before anything is allocated, and a schedule beyond the
  budget is refused with an explanation. The budgets are the options
  `mediaPlanR.max_table_cells` (default `1e7`) and `mediaPlanR.max_work`
  (default `1e9`); see "Computational limits" in `?mediaPlanR`.
* PDF manual (MP-11): a line may now break after an underscore of a function
  or argument name, and the correlation formula of `calc_canex()` is a display
  equation. The final pass of the regenerated manual has no overfull box (the
  earlier one had lines that ran into the right margin on pages 3, 5, 9, 10, 34,
  41, 45 and 46).
* `tools/build-manual.R` (MP-12) stops if `pdflatex` or `makeindex` return a
  non-zero code, no longer accepts a PDF left by an earlier pass, keeps a log of
  the build and lists the overfull boxes.

## Corrections from the definitive audit of 2026-10-12

Each correction is checked by `tests/testthat/test-extremes-audit-2026-10-12.R`.
No result that was correct changes.

* `nbd_exposure_distribution()`: for a very large `size` the quotient
  `mean_contacts / size` could underflow to zero, and the reach was zero (and the
  average frequency infinite) while the table kept the positive cells
  (`mean_contacts = 1e-17`, `size = 1e308`). The cumulant is now computed from
  its series when the quotient is below 1e-8 (error of order 1e-24).
* `nbd_exposure_distribution()`: `opportunities` above the integer range
  (for instance `3e9`) was turned into `NA`, with its diagnostics; it is now kept
  as a number. Values within the integer range are still integers.

## Corrections from the final audit of 2026-10-09

Each correction is checked by `tests/testthat/test-final-audit-2026-10-09.R`,
whose witnesses are computed without the code under test.

* `nbd_exposure_distribution()`: the reach is computed as `-expm1(-size *
  log1p(mean / size))` (`-expm1(-mean)` for the Poisson limit) instead of
  `1 - P(0)`. For a mean of 1e-17 the reach was zero and the average frequency
  infinite, although the table kept the positive probabilities. `calc_cbd()` and
  `calc_mbd()` obtain their reach as the sum of the positive cells for the same
  reason.
* Vignette: truncating negative probabilities in `calc_canex()` *may* change the
  mean number of exposures (it did not in a compatible symmetric example); the
  table of reactions to warnings no longer says that it did.

## Corrections from the theoretical audit of the manual (2026-10-09)

Documentation only: no calculation changes. Each statement added to the help
pages is checked by `tests/testthat/test-theory-audit-2026-10-09.R`.

* The two Hofmans models are described as two applications, to different plan
  shapes and with different data, of the same basis (Agostini's formula; Aldás
  Manzano, 1998, p. 118), not as unrelated procedures. For equal vehicles and
  equal duplications, `calc_hofmans_duplication()` and the constant-coefficient
  formula of `calc_hofmans_accumulation()` give the same reach (new Details and
  example).
* `calc_mbd()` has a new section, "What the result conserves". The bounded
  conditional allocation of the published procedure (Cheong, 2007, pp. 65-66)
  can change the marginals and the mean number of exposures with no negative
  probability, so `diagnostics$mean_error` must be read whether or not the safety
  net was engaged, and the average frequency of the result cannot be combined
  with the gross rating points of the inputs. The README no longer says that MBD
  conserves the mean, and `?mediaPlanR` states which models conserve it (CSD
  and MSAD) and which can alter it (CANEX, CBD and MBD).
* Unit of exposure: contacts are opportunities of exposure defined by the data
  and can be read as advertising impressions only if the audiences and
  duplications measure that opportunity (`?mediaPlanR` and the README).
* A duplication below the random one is described as a caution of application
  of the evaluated models (Hong, 1998), not as invalid data or as a condition of
  the formulas; the warning of class `mediaPlanR_low_duplication` says so, and
  `?calc_canex` gives an example of valid data below the random duplication that
  `calc_canex()` reproduces.
* `calc_cbd()`: in its general formulation CBD takes the zero probability of the
  schedule from any existing model (Kim, 1994, p. 124; Kim, 2005, p. 65), and this
  implementation takes it from the canonical expansion; the pages of Kim (2005)
  are 59-65; the earlier range quoted for the departure of the mean could not be
  reproduced and is replaced by figures that can (`csd_example` and the
  maintainer's local runs). CBD is called a multivariate model of conditional
  expansion, not a sequential-aggregation model (see also the title of
  `?print_sequential` and the `seealso` of CSD and MSAD).
* "Second-order" canonical expansion is defined once, in the new section "Order
  of the expansion" of `?calc_canex` (pairwise terms, linear in the correlations),
  and the messages of CANEX, CSD and CBD no longer use the phrase.
* The bound `R2 <= 2 * R1 - R1^2` is documented as the bound of the
  Beta-Binomial family, not as the logical limit of two insertions
  (`R2 <= min(1, 2 * R1)`), with an example of possible data outside the family.
* `evaluate_exposure_model()`: the denominator of the APE is the observed reach;
  it equals the sum of the observed positive cells used by Kim (2005, p. 118)
  when the observed mass is one.
* Vignette: Deming (1943) is a Wiley book (reprinted by Dover in 1964) and the
  iterative proportional fitting is due to Deming and Stephan (1940); the status
  of HBBD says that its two steps are available separately.
* `tools/build-manual.R` (not part of the archive) builds the PDF manual from the
  help pages with a layout setting that keeps long function names inside the
  right margin; the unmodified `R CMD Rd2pdf` output has about 75 lines that run
  into it.

## Corrections from the re-audit of 2026-10-09

* `evaluate_exposure_model()` accepts any observed reach above the
  double-precision epsilon (it used to reject reaches below about 1.5e-8 as
  zero); the limit is now stated in the help page and in the error message.
* The installed-package load test now checks a component that exists.
* `inst/DATA-PROVENANCE.md`: the statement on redistribution distinguishes the
  datasets and tables (not distributed) from the individual printed values
  quoted next to the computed differences.
* Documentation of the models checked against the sources and recomputed:
  the range of the mean departure quoted for CBD, the equation numbers cited
  for the Beta-Binomial (3.25-3.28 of Aldás Manzano), the timing quoted for
  5,000 insertions and the spelling of Burkart (Lee and Burkart, 1960).

## Corrections from the follow-up audit of the corrected manual (2026-10-08)

Validation:

* Exact structural constraints are checked with the rounding error of double
  arithmetic only, never with a fraction of the universe: the duplication
  matrices of Agostini, Hofmans and Metheringham reject a duplication one
  person beyond its bound in universes of any size.
* `fit_bbd_to_reach()`: `precision` measures the fit to the target and no longer
  widens the bounds of the schedule. A reach below the largest audience is
  rejected whatever the precision; a limit (polarized or binomial) is selected
  only if it lies inside the bounds of the schedule; the fitted reach is checked
  against those bounds, and the function stops when no Beta-Binomial with the
  mean of the schedule is compatible with it. The reach is computed with a
  numerically stable formula, and `parameters$schedule_bounds` reports the
  bounds next to `feasible_reach` (the interval of the family).

Documentation:

* The largest discrepancy of `calc_mbd()` with the printed three-vehicle example
  of Cheong is 0.0020348568 in probability, not 0.002 or 0.0019; the test
  threshold (0.0025) is distinguished from the real maximum, the printed
  intermediate tables are described as a plausible explanation and not as an
  established cause, and the maximum of MSAD with Lee (1988) is 0.001077973.
  The cell-by-cell comparisons are in `inst/DATA-PROVENANCE.md`.
* `sum_tolerance` of `evaluate_exposure_model()` does not guarantee that a
  table is complete: it rejects total-mass deficits above the threshold and
  cannot distinguish rounding from a small omitted tail.
* `DESCRIPTION` states what the three doctoral dissertations are and that they
  have no DOI; the Sainsbury help page marks Caffyn and Sagovsky (1963) as a
  secondary citation.
* A long line of the value of `evaluate_exposure_model()` that overflowed the
  right margin of the PDF manual was split.

Data:

* The datasets `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` were removed,
  because the basis for redistributing the inputs of published examples was not
  established. The examples and the vignette use `csd_example`, `msad_example`
  and `mbd_example`; the comparisons with the published examples are local-only
  tests of the maintainer.

## Corrections from the academic audit of the manual (2026-10-08)

Validation:

* `fit_bbd_to_reach()` separates two cases: a reach that is logically impossible
  for the schedule (below the largest audience or above its contacts) and a
  reach that is possible but that the Beta-Binomial family cannot represent.
  Its `precision` default is now `NULL`, meaning `1e-4 * population` (the former
  100 people for a universe of one million, and a scale-appropriate tolerance
  for small universes); the printed fit shows the reaches with enough digits.
* `evaluate_exposure_model()` rejects any probability above one (or percentage
  above 100) before applying the tolerance of the total, and has a new argument
  `censored_last_level` for tables whose last level is a collapsed tail: the
  exact mean and `mean_contact_bias` are then `NA`, and the means of the
  censored codes are reported separately.
* The average frequency of a result with no reach is `NA` in `calc_canex()`,
  instead of zero.

Documentation:

* The safety net of `calc_mbd()` is described as a package correction of the
  final distribution, not as Cheong's MBD-ADJ, which acts on the joint-exposure
  table before it is expanded; the study's accuracy figures are attributed to
  its own variants (MBD, MBD-ADJ, MBD-ADJ2).
* The help pages state the unit and scope of every quantity that the audit found
  ambiguous: the exceptions to the percentage rule, contacts versus people versus
  pairs, the limits of the Beta-Binomial family (`alpha`, `beta` of `0` and
  `Inf` are indicators of a limit), the homogenized Binomial, calibration as a
  search that can fail, the Negative-Binomial result and evaluation result
  structures, the open-tail mean, the sample required by `fit_nbd_exposure()`,
  normalization and the positive observed reach of the evaluator, and the
  contract differences of `calc_canex()` (`R2` for every vehicle).
* The general statement about errors and warnings distinguishes violated data
  constraints from approximations that fail on feasible data.
* The replication of published tables is separated from validation against
  independent observations, the datasets state what is and is not distributed,
  and a suggested study path is added to `?mediaPlanR`.
* The DOI of Cheong, Leckenby and Eakin (2011) in `calc_mbd()` is written as
  plain text (`doi:10.2753/...`) instead of a
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
  for large finite audience counts. Unsupported Beta-Binomial insertion counts
  fail explicitly.
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
  informative errors.
* The ad hoc formulas of Agostini and Hofmans (duplication) check the
  duplication matrix against the Fréchet bounds implied by the audiences and
  the population, and warn when the reach they compute leaves its logical
  range.
* `calc_csd()` reproduces the worked example of Kim (2005) within the rounding of
  its published tables; `calc_msad()` the numerical example of Lee (1988,
  pp. 81-93); `calc_cbd()` the worked example of Kim (1994, p. 139) except for
  its zero cell (about 0.4 percentage points) and the CBD columns of Kim (2005,
  Appendix B) and Hong (1998, Appendix E); and `calc_mbd()` the worked example of
  Cheong (2007, p. 75; also Cheong, Leckenby and Eakin, 2011), with a largest
  absolute difference of 0.0020348568 in probability. `calc_canex()`, `calc_csd()` and `calc_msad()` also reproduce the 40
  plans of Kim (2005, Appendix B) within 0.02 percentage points per cell
  (MSAD plan 19, 0.099 with the common reconstructed duplication; its printed
  row is reproduced with random duplication). The three remaining differences
  with published values (Kim 1994 zero cell, Cheong MBD, Kim 2005 MSAD plan 19)
  are documented, with the evidence that bears on each one and explicit
  regression bounds, in `inst/DATA-PROVENANCE.md`; the cause of none of them is
  established beyond that evidence. The test suite checks all of these.
* The example datasets are original illustrative data, ready for `do.call()`.
  The inputs of published worked examples are not distributed.

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
