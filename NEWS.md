# mediaPlanR 2.0.0

## New planning core

- Added `media_plan()` as a validated, unit-explicit cross-media data contract.
- Added `plan_metrics()`, `estimate_reach()` and `compare_reach_models()`.
- Added `optimize_media_plan()` with exact and explicitly labelled heuristic
  modes. Exact results report a verifiable global optimum.
- Added `audience_metrics()` to separate target composition, rating, affinity
  and selectivity.
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
  reasoning already applied to `binomial_plan` (named to avoid masking
  `stats::binomial`).
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
