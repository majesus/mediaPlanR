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
  Morgensztern aggregation. `calc_MBBD()` remains as a compatibility wrapper.
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
- Corrected cost-per-rating-point and useful-audience semantics in the legacy
  KPI table.
- Replaced the historical batch optimizer with a compatibility wrapper around
  the v2 optimization engine.
- Removed the dead legacy bodies of `optimize_media_sb()`, `calcular_metricas_medios()`,
  `optimizar_d()` and `optimizar_dc()` that remained in `R/calc_plan_modelos.R`
  and `R/model_optimization.R` after those functions were re-implemented as
  compatibility wrappers in `R/zz_compat_v2.R`. Those bodies were unreachable
  at runtime (R's default alphabetical file-loading order meant the
  `zz_compat_v2.R` definitions always won), but they still carried the
  `@export` tag that `roxygen2` used to build each function's help page, so
  the shipped documentation described the old, unreachable behaviour (e.g. an
  unvalidated affinity-index multiplication and a mislabelled
  `Audiencia_miles` column that was not actually expressed in thousands)
  instead of the corrected v2-backed implementation actually exported by the
  package. Documentation for all four functions has been rewritten to match
  their real behaviour.
- Renamed the example datasets inherited from 0.2.0 (`sainsbury`, `beta_binomial`,
  `metheringham`, `hofmans`, `agostini`, `grps`, `cpm`, `roas`) to
  `sainsbury_example`, `beta_binomial_example`, `metheringham_example`,
  `hofmans_example`, `agostini_example`, `grps_example`, `cpm_example`, and
  `roas_example`. Each bare name was indistinguishable at a glance from its
  model function (e.g. `metheringham` next to `calc_metheringham()`); the
  `_example` suffix matches the convention already used for the datasets v2
  introduced (`canex_example`, `csd_example`, `msad_example`, `nbd_example`,
  `mbbd_example`) and the reasoning already applied to `binomial_plan` (named
  to avoid masking `stats::binomial`). `binomial_plan`, and the three
  intentionally dual-named legacy/`_example` pairs (`canex`/`canex_example`,
  `nbd`/`nbd_example`, `MBBD`/`mbbd_example`), are unaffected.
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

## Compatibility

The established `calc_*`, `optimizar_*`, `optimize_media_sb()` and Spanish KPI
functions remain available. New projects should prefer the v2 API.

The new `canex_example`, `csd_example`, `nbd_example`, `mbbd_example`, and
`msad_example` datasets replace ambiguous historical names or provide
ready-to-use illustrative inputs. `csd_kim2005`, `msad_kim2005`, and
`mbd_cheong2007` separately reproduce the published Kim (2005) and Cheong
(2007) worked examples for literature validation, each with an explicit
provenance qualification. Historical datasets remain available.

## References

The package itself does not yet have a DOI; it remains explicitly pending.
Model references include verified DOIs where available, and no package DOI has
been guessed or fabricated.
