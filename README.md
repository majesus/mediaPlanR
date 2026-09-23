# mediaPlanR

[![R-CMD-check](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml)

`mediaPlanR` provides reproducible cross-media reach, frequency and budget
allocation in R. It combines a validated planning object, complete exposure
distributions, explicit metric units, and optimization results that state
whether a global optimum was actually verified, with the classical reach and
exposure-distribution models of the media-planning literature (Sainsbury,
Binomial, Beta-Binomial, Metheringham, Agostini, Hofmans, CANEX, CSD, MSAD, CBD
and MBD).

## Installation

```r
# install.packages("pak")
pak::pak("majesus/mediaPlanR")
```

## A complete workflow

```r
library(mediaPlanR)

plan <- media_plan(
  data.frame(
    channel = c("TV", "Radio", "Digital"),
    audience = c(300000, 180000, 120000),
    insertions = c(4, 6, 10),
    cost_per_insertion = c(18000, 3500, 1200),
    target_audience = c(180000, 90000, 84000)
  ),
  population = 1000000,
  target_audience = "target_audience"
)

metrics <- plan_metrics(plan)
reach <- estimate_reach(plan, model = "sainsbury")
comparison <- compare_reach_models(plan, c("sainsbury", "binomial"))
```

Every plan-based reach result contains:

- zero-to-N exposure probabilities;
- cumulative N+ reach;
- reach in probability, percent and people;
- average frequency among the people reached;
- model parameters.

## Budget allocation

```r
optimized <- optimize_media_plan(
  plan,
  budget = 60000,
  objective = "min_cost",
  target_reach = 0.55,
  effective_frequency = 2,
  max_insertions = c(4, 8, 12),
  method = "auto"
)

optimized$global_optimum
optimized$allocation
optimized$effective_reach
```

For manageable search spaces, `method = "exact"` evaluates every feasible
integer allocation and certifies the global optimum. For larger spaces,
`method = "greedy"` is explicitly reported as a heuristic and is never
presented as an exact optimum. The greedy search adds up to
`effective_frequency` insertions per step, so it also works when effective
reach needs several exposures.

## Target-audience metrics

```r
audience_metrics(
  gross_audience = c(300000, 180000),
  target_audience = c(180000, 90000),
  gross_universe = 1000000,
  target_universe = 400000
)
```

Target composition and affinity are different quantities. The package never
multiplies a gross audience by an affinity index, which could create a target
audience larger than the gross audience.

## Reach and exposure-distribution models

The models are organized by the shape of plan they were derived for:

- random duplication, several vehicles with any number of insertions:
  `calc_sainsbury()`, `calc_binomial()`;
- one vehicle, several insertions: `calc_beta_binomial()`,
  `calc_hofmans_accumulation()`;
- several vehicles, one insertion each, with observed duplication:
  `calc_agostini_duplication()`, `calc_hofmans_duplication()`;
- several vehicles and insertions with observed duplication:
  `calc_metheringham()`, `calc_canex()`, `calc_cbd()`, `calc_csd()`,
  `calc_msad()`, `calc_mbd()`;
- `fit_bbd_to_reach()` and `calibrate_bbd()` to fit a Beta-Binomial to an
  external reach or a target effective reach;
- `fit_nbd_exposure()` and `nbd_exposure_distribution()` for unbounded
  exposure-count processes.

```r
data(csd_kim2005)
csd <- do.call(calc_csd, csd_kim2005)

data(metheringham_example)
metheringham <- do.call(calc_metheringham, metheringham_example)

counts <- c(rep(0, 40), rep(1, 25), rep(2, 15), rep(3, 8), 5, 7)
nbd_fit <- fit_nbd_exposure(counts)
```

## Validation against observed data

Observed distributions are supplied by the analyst; they are never inferred
from model inputs. The table must contain `contacts` and `observed`, including
the zero-exposure cell, and its scale must be declared:

```r
observed <- data.frame(
  contacts = 0:6,
  observed = c(38.41, 17.89, 39.66, 2.67, 1.36, 0, 0)
)

data(csd_kim2005)
csd <- do.call(calc_csd, csd_kim2005)

evaluation <- evaluate_exposure_model(
  observed = observed,
  predicted = csd,
  observed_scale = "percent"
)

evaluation$summary
```

For a predicted data frame, columns must be named `contacts` and `predicted`
and `predicted_scale` must also be declared. Exact support equality is
required; open-tail Negative-Binomial cells are rejected unless observed and
predicted tails have first been collapsed identically.

## Example datasets

Datasets ending in `_example` are original illustrative inputs, ready for
`do.call()`. `csd_kim2005`, `msad_kim2005` and `mbd_cheong2007` instead
reproduce the minimal factual inputs (reach and duplication figures) published
by Kim (2005) and Cheong (2007), so that users can verify that `calc_csd()`,
`calc_msad()` and `calc_mbd()` reproduce the published worked examples.
`msad_kim2005` reuses Kim's CSD inputs and does not claim that its MSAD output
was published by Kim.

## Reproducibility guarantees

- Inputs are checked for units, bounds and logical compatibility, and invalid
  values (`NA`, `NaN`, `Inf`, wrong types or lengths) produce informative
  errors.
- Exposure distributions are normalized and include zero exposures.
- Exact optimization never exceeds the declared budget.
- Model boundary cases have regression tests.
- Observed-versus-predicted evaluations require declared scales and identical
  exposure support.

## References

Each function's help page lists the primary sources of its model, and
`vignette("mediaPlanR-intro")` collects them, with the DOIs that could be
verified.

## Author and license

Manuel J. Sánchez-Franco, Universidad de Sevilla

[ORCID 0000-0002-8042-3550](https://orcid.org/0000-0002-8042-3550)

MIT License.
