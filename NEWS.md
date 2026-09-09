# mediaPlanR 2.0.0

## New planning core

- Added `media_plan()` as a validated, unit-explicit cross-media data contract.
- Added `plan_metrics()`, `estimate_reach()` and `compare_reach_models()`.
- Added `optimize_media_plan()` with exact and explicitly labelled heuristic
  modes. Exact results report a verifiable global optimum.
- Added `audience_metrics()` to separate target composition, rating, affinity
  and selectivity.
- Added `calc_cbd()`, implementing the Conditional Beta Distribution
  (Leckenby & Kim, reported in Kim, 1994; reviewed in Kim, 2005, pp. 59-64):
  between-vehicle duplication via Danaher's (1991) second-order canonical
  expansion for the full (0,1) joint exposure grid, then the same
  conditional Beta-Binomial peeling `calc_mbd()` uses to expand each vehicle
  into its own insertion-level distribution. Kim (2005) and Cheong (2007)
  each independently rank CBD among the two or three most accurate models
  they tested; it was the one such model this package did not yet have. No
  published numerical example was available to validate against (unlike
  `calc_csd()`); validated instead by construction (the raw joint grid
  recovers each vehicle's own R1 as its exact marginal) and by an exact
  reduction to independent convolution at zero correlation.
- Added `calc_hofmans_duplication()`, Hofmans' (1966) ad hoc correction of
  `calc_agostini_duplication()` for several vehicles with one insertion each: a
  pairwise duplication coefficient computed directly from each vehicle pair's
  own audiences, rather than one coefficient fitted from external calibration
  data. Reach only, like `calc_agostini_duplication()`; unrelated to
  `calc_hofmans_accumulation()` (same author, different model: one vehicle
  with several insertions). Aldas Manzano (1998, sec. 3.2.1.2) and Kim (2005,
  pp. 44-45) independently report the identical formula. Renamed
  `calc_agostini()` to `calc_agostini_duplication()` and `calc_hofmans()` to
  `calc_hofmans_accumulation()` so both Hofmans models, and their shared-domain
  Agostini counterpart, carry their domain in the function name; their example
  datasets follow suit (`hofmans_accumulation_example`, new
  `hofmans_duplication_example`).
- Added `calc_mbd()`, implementing Cheong's (2007) Multivariate Beta Binomial
  Distribution model: vehicle co-exposure via Waring's (1792) inclusion-
  exclusion, imputed from a Beta-Binomial fit to each subset's own mean
  audience and duplication for three or more vehicles, and a row-specific
  conditional Beta-Binomial expansion as vehicles are peeled off in reverse
  aggregation order. `mbd_cheong2007` reproduces Cheong's complete
  three-vehicle worked example; the first-order consistency check and the final negative-
  probability safety net ("MBD-ADJ") are unit-tested against Cheong's own
  intermediate numbers for four- and five-vehicle schedules. `calc_mbd()`
  warns for four or more vehicles and stops above 12, matching the scope
  Cheong's own dissertation verified; see `vignette("mediaPlanR-intro")` for
  what is, and is not, guaranteed beyond the three-vehicle case.

## Correctness

- Fixed `calc_beta_binomial()` silently returning an all-`NaN` distribution
  (with no error or warning) when `A1`/`A2` place `R2` exactly at the
  binomial (independence) limit: its inline alpha/beta formula produced
  `alpha = beta = Inf`, which `extraDistr::dbbinom()` turns into `NaN` rather
  than an error. `calc_beta_binomial()` now delegates to the same
  `calculate_bbd_params()` helper already used by `calc_canex()`,
  `calc_csd()`, `calc_msad()` and `calc_mbd()`, which handles the binomial
  limit (`alpha = beta = Inf`, falls back to `stats::dbinom()`) and the
  polarized limit (`alpha = beta = 0`, `R2 = R1`) explicitly. The polarized
  limit previously stopped with an unhelpful "Could not compute valid
  parameters" error even though it is a mathematically valid degenerate case
  handled correctly everywhere else in the package.
- Fixed the same underlying issue in `calc_mbd()`/`calc_cbd()`'s shared
  vehicle-peeling step (`mbd_peel_vehicle()`/`mbd_conditional_allocate()`):
  when the vehicle being peeled has its own `R1`/`R2` at the binomial or
  polarized limit, the conditional Beta-Binomial split previously called
  `extraDistr::dbbinom()` with non-finite or degenerate alpha/beta and then
  divided by `Inf` in `mbd_conditional_allocate()`, raising "missing value
  where TRUE/FALSE is needed". Both limits are now computed directly from
  their closed-form limiting distributions (a shared `Binomial(n, p)` at the
  binomial limit; point masses at 0 and at the vehicle's insertion count at
  the polarized limit).
- Fixed `print.reach_canex()` reporting an incorrect "average contacts per
  person reached": `print_reach_report()` (shared by the Sainsbury, Binomial,
  Beta-Binomial, Metheringham and CANEX print methods) divided by
  `sum(distribution$people)`, which for the other four models already
  excludes the zero-contact row but for CANEX includes it, so the printed
  average was silently computed over the whole population rather than over
  those actually reached. `calc_canex()`'s own `$stats$avg_contacts` field
  was never affected -- only the printed text was wrong. Fixed by dividing
  by the `reach_people` value the function already receives.
- Replaced exponential Sainsbury enumeration with exact dynamic convolution.
- Corrected the Binomial and polarized limits of CANEX.
- Added CANEX feasibility checks and diagnostics for truncated probability
  mass and correlation-matrix compatibility.
- Renamed the historical MBBD calculation to `fit_bbd_to_reach()` because it
  fits one BBD to external reach rather than implementing sequential
  Morgensztern aggregation.
- Added `calc_msad()`, implementing Kim's MSAD structure with per-vehicle BBD
  marginals, Morgensztern reach, sequential non-random convolution, explicit
  aggregation order, and probability diagnostics.
- Added `calc_csd()`; `csd_kim2005` reproduces Kim's complete three-vehicle
  Canonical Sequential Aggregation example within the rounding precision of
  its published intermediate tables. Invalid canonical targets are reported
  rather than silently truncated or renormalized.
- Added `evaluate_exposure_model()` for model-independent validation against
  analyst-supplied observations. It reproduces Kim's AER/APE example and also
  reports total variation, cell MAE, reach error, and contact-frequency bias.
  Scales and supports are explicit; incompatible open tails are rejected.
- Clarified that Kim specifies MSAD but locates its detailed worked example in
  Lee (1988); `msad_kim2005` is now explicitly a derived calculation using
  Kim's published CSD inputs, not a published MSAD output.
- Added `fit_nbd_exposure()` for maximum-likelihood estimation from observed
  counts and `nbd_exposure_distribution()` for explicitly unbounded
  Poisson-Gamma scenarios. NBD outputs now quantify mass above finite
  opportunities and are labelled experimental outside continuous processes.
- Added the documented Hofmans parameters to its return value.
- Renamed the example datasets inherited from 0.2.0 (`sainsbury`, `beta_binomial`,
  `metheringham`, `hofmans`, `agostini`) to `sainsbury_example`,
  `beta_binomial_example`, `metheringham_example`, `hofmans_example`, and
  `agostini_example`. Each bare name was indistinguishable at a glance from
  its model function (e.g. `metheringham` next to `calc_metheringham()`); the
  `_example` suffix matches the convention already used for the datasets v2
  introduced (`canex_example`, `csd_example`, `msad_example`) and the
  reasoning already applied to `binomial_example` (named to avoid masking
  `stats::binomial`).
- Renamed the `binomial_plan` dataset to `binomial_example` to follow the
  same `_example` convention as every other dataset.
- `calc_R1_R2()` is no longer exported. It had no internal callers and no
  documented public use case; it remains available internally for the
  Beta-Binomial alpha/beta <-> R1/R2 conversions.
- Consolidated `sainsbury_example`, `binomial_example`, and `agostini_example`
  into a single `ratings_example` dataset. `calc_sainsbury()`, `calc_binomial()`,
  and `calc_agostini_duplication()` share the same random-duplication starting
  hypothesis and the same minimal input (audiences and population), so one shared
  dataset replaces three near-identical ones.
- Fixed `calc_binomial()` to compute its contact distribution via
  `stats::dbinom()`, instead of a separately hand-written Binomial formula.
- Added an `insertions` argument to `calc_sainsbury()` and `calc_binomial()`
  (default: one per vehicle, reproducing the exact previous behaviour and
  citation). Aldas Manzano (1998) reviews these as two models restricted to
  one insertion per vehicle (Sainsbury: sec. 3.2.2.2; Binomial: sec.
  3.2.2.1, Chandon, 1985) alongside a *general* pair for several insertions
  per vehicle (Sainsbury: sec. 3.3.1.2; Binomial: sec. 3.3.1.1, Lee &
  Burkart, 1960) -- but the general formulas reduce exactly to the
  one-insertion ones at `insertions = 1`, so they are the same two models at
  different levels of generality, not four models. `estimate_reach()` (and
  `compare_reach_models()`, `optimize_media_plan()`) now call
  `calc_sainsbury()`/`calc_binomial()` directly instead of separately
  reimplementing this math, and their `model` values are `sainsbury`/
  `binomial` accordingly (an earlier `binom_heterogeneous`/`binom_homogeneous`
  rename, meant to avoid a name collision with `calc_binomial()` while the
  two implementations were still separate, is superseded now that
  consolidation removes the collision at its root).
- Removed the experimental NBD approximation from `estimate_reach()`'s and
  `compare_reach_models()`'s `model` options (`optimize_media_plan()` never
  offered it). It has no literature-grounded generalization to several
  vehicles with several insertions each -- unlike Sainsbury/Binomial above --
  so offering it as a third, equally-weighted option overstated its scope.
  Call `nbd_exposure_distribution()` directly for that approximation.
- Completed `calc_metheringham()`. It previously stopped at the model's
  calibration inputs (A1, mean audience; D, mean duplication; A2 = 2*A1 - D)
  without ever estimating alpha/beta or evaluating the Beta-Binomial contact
  distribution the model is defined by (Aldas Manzano, 1998, sec. 3.2.2.9) --
  so, unlike every other model in this family, it never returned reach or a
  distribution. A1 and A2 are exactly the R1/R2 `calc_beta_binomial()` takes
  for one vehicle with several insertions (R2 = 2*R1 - E_2^2 is an identity
  of the Beta-Binomial distribution), so `calc_metheringham()` now calls it
  directly for the plan's actual number of vehicles. This adds a required
  `population` argument; `metheringham_example` gained a `population` field
  to match.
- Gave the "classical reach models" (Sainsbury, Binomial, Beta-Binomial,
  Metheringham, CANEX) a single shared print format (`print_reach_report()`
  in `R/print_helpers.R`): the same section headers, rounding, and layout for
  reach, the contact distribution, the cumulative distribution, and model
  parameters, so results read the same way regardless of which model
  produced them. Agostini keeps its own, simpler layout (it has no
  per-contact distribution to show); the sequential-aggregation models
  (CSD, MSAD, MBD) keep their existing compact format, which already reads
  consistently across the three of them and suits distributions that can run
  into the hundreds of cells.
- `optimize_media_plan()` was briefly removed in early v2.0.0 development on
  the mistaken assumption that its reach evaluation (several insertions per
  channel, every step of its search) had no literature basis. A closer
  reading of Aldas Manzano (1998) found the model family above, which is
  exactly what it needed; the function was restored with the corrected
  citations and the `binom_heterogeneous`/`binom_homogeneous` naming.
- Split `csd_example`, `msad_example`, and `mbd_example` from the dissertation
  numbers they originally reproduced. `csd_example`, `msad_example`, and
  `mbd_example` are now original illustrative data, not derived from any
  published source, so the package's MIT licence applies to them
  unambiguously. `csd_kim2005`, `msad_kim2005`, and `mbd_cheong2007` carry the
  same numbers as before (from Kim 2005 and Cheong 2007) under an explicit
  provenance note: bare factual inputs reused for validation, not a creative
  or substantial reproduction of either dissertation. `evaluate_exposure_model()`
  documentation and its unit test that reproduce Kim's AER/APE example now
  reference `csd_kim2005` accordingly.

## Scope, before the first CRAN submission

`mediaPlanR` has never been released, on CRAN or otherwise, so this release
carries no compatibility obligation to past users. The package is scoped to
cross-media reach/frequency models and the v2 planning core; nothing here
should be read as a deprecation of functionality that used to exist under a
different name.

- Removed the historical `calc_MBBD()`/`print.MBBD` wrapper, the `MBBD` S3
  class and the `MBBD` and `canex`/`nbd` legacy-named datasets. `canex` and
  `nbd` were unlabelled duplicates of `canex_example` and `nbd_example`;
  `fit_bbd_to_reach()` is the only entry point for that calculation now, and
  its worked example is `bbd_reach_example` (renamed from `mbbd_example`).
- Removed the univariate NBD plan wrapper `calc_nbd()` and its `nbd`/
  `nbd_example` datasets. `fit_nbd_exposure()` (observed counts) and
  `nbd_exposure_distribution()` (explicit scenario parameters) are the NBD
  API; see `vignette("mediaPlanR-intro")`.
- Removed the Spanish-named compatibility layer around the v2 engine
  (`optimizar_d()`, `optimizar_dc()`, `optimize_media_sb()`,
  `calcular_metricas_medios()`) in favour of `calibrate_bbd()`,
  `optimize_media_plan()`, and `plan_metrics()` directly.
- Moved the budget/KPI functions (`calc_grps()`, `calc_cpm()`,
  `calcular_roas()`, `plot_grp_metricas()`) and the four Shiny explorers
  (`run_canex_explorer()`, `run_beta_binomial_explorer()`,
  `run_reach_converg_explorer()`, `run_aud_util_explorer()`) out of the
  package. They depended on `shiny`, `bslib`, `ggrepel` and `scales`, none of
  which the remaining reach/frequency core needs; those four dependencies
  were dropped accordingly. This is not a functionality judgement — they are
  kept, unmodified, as the seed of a separate budget/KPI/interactive package.
- Unexported `calculate_bbd_params()`, `calculate_mean_variance()`,
  `calculate_marginal_prob()`, `calculate_duplication()`,
  `transform_duplications()`, `calculate_metrics()`, `matriz_a_vector()` and
  `crear_matriz_oportunidades()`: internal computational steps shared by
  `calc_canex()`, `calc_csd()`, `calc_mbd()`, `calc_msad()` and
  `calc_metheringham()`, not independent entry points.
- Unified the two duplicate local implementations of the Beta-Binomial point
  probability (in the CANEX explorer, now moved out, and in
  `calc_beta_binomial()`) on the single `extraDistr::dbbinom()` already used
  everywhere else in the package.
- Dropped the unused `readr` `Suggests` declaration.

The `canex_example`, `csd_example`, `msad_example`, and `bbd_reach_example`
datasets are original illustrative inputs, ready for `do.call()`.
`csd_kim2005`, `msad_kim2005`, and `mbd_cheong2007` separately reproduce the
published Kim (2005) and Cheong (2007) worked examples for literature
validation, each with an explicit provenance qualification.

## References

The package itself does not yet have a DOI; it remains explicitly pending.
Model references include verified DOIs where available, and no package DOI has
been guessed or fabricated.
