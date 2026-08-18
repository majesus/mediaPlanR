# mediaPlanR 2.0.0

[![R-CMD-check](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml)

`mediaPlanR` provides reproducible cross-media reach, frequency and budget
allocation in R. Version 2 introduces a validated planning object, complete
contact distributions, explicit metric units and optimization results that say
whether a global optimum was actually verified.

## Installation

```r
# install.packages("pak")
pak::pak("majesus/mediaPlanR")
```

## A complete v2 workflow

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
reach <- estimate_reach(plan, model = "independent")
comparison <- compare_reach_models(plan, c("independent", "binomial"))
```

All v2 reach results contain:

- zero-to-N contact probabilities;
- cumulative N+ reach;
- reach in probability, percent and people;
- average frequency among reached people;
- model parameters and diagnostics.

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
`method = "greedy"` is explicitly reported as a heuristic; it is never
presented as an exact optimum.

## Target-audience metrics

```r
audience_metrics(
  gross_audience = c(300000, 180000),
  target_audience = c(180000, 90000),
  gross_universe = 1000000,
  target_universe = 400000
)
```

Target composition and affinity are different quantities. Version 2 never
multiplies gross audience by an affinity index, which could otherwise create a
target audience larger than the gross audience.

## Classical models

The package distinguishes historical finite-opportunity models from continuous
exposure-count approximations:

- `calc_sainsbury()`, `calc_binomial()`, `calc_beta_binomial()`;
- `calc_metheringham()`, `calc_hofmans()`, `calc_agostini()`;
- `calc_canex()`, Kim's `calc_csd()`, and the Leckenby-Rice `calc_msad()`;
- `fit_bbd_to_reach()` for fitting one BBD to an external reach estimate;
- `fit_nbd_exposure()` and `nbd_exposure_distribution()` for unbounded
  exposure-count processes.

The historical `calc_MBBD()` and `calc_nbd()` entry points remain supported,
but their documentation now states their actual scope. `calc_MBBD()` is not a
full MSAD implementation, and a univariate NBD is not a finite-insertion or
cross-vehicle dependence model.

```r
data(csd_example)
csd <- do.call(calc_csd, csd_example)

data(msad_example)
msad <- do.call(calc_msad, msad_example)

counts <- c(rep(0, 40), rep(1, 25), rep(2, 15), rep(3, 8), 5, 7)
nbd_fit <- fit_nbd_exposure(counts)
```

## Validation against observed data

Observed distributions are supplied by the analyst; they are never inferred
from model inputs. The table must contain `contacts` and `observed`, including
the zero-contact cell, and its scale must be declared explicitly:

```r
observed <- data.frame(
  contacts = 0:6,
  observed = c(38.41, 17.89, 39.66, 2.67, 1.36, 0, 0)
)

data(csd_example)
csd <- do.call(calc_csd, csd_example)

evaluation <- evaluate_exposure_model(
  observed = observed,
  predicted = csd,
  observed_scale = "percent"
)

evaluation$summary
```

For a predicted data frame, columns must be named `contacts` and `predicted`
and `predicted_scale` must also be declared. Exact support equality is required;
open-tail NBD cells are rejected unless observed and predicted tails have first
been collapsed identically.

Descriptive example objects are available as `canex_example`, `csd_example`,
`nbd_example`, `mbbd_example`, and `msad_example`. `csd_example` reproduces
Kim's complete three-vehicle worked example; `msad_example` reuses those inputs
but does not claim that its MSAD output was published by Kim. Historical shorter
names remain available for compatibility.

## Reproducibility guarantees

- Inputs are checked for units, bounds and logical compatibility.
- Contact distributions are normalized and include zero contacts.
- Exact optimization never exceeds the declared budget.
- Model boundary cases have regression tests.
- Observed-versus-predicted evaluations require declared scales and identical
  contact support.
- Compatibility outputs are tested against the v2 core.

## References

The package documentation lists the original publications for each classical
model. Verified identifiers are included for MSAD, CANEX, and the NBD exposure
literature; remaining identifiers will be added only after verification.

## Author and license

Manuel J. Sanchez-Franco, Universidad de Sevilla

[ORCID 0000-0002-8042-3550](https://orcid.org/0000-0002-8042-3550)

MIT License.
