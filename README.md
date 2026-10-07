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

### Quick start

Every model ships with an example dataset that is already a list of
arguments, so `do.call()` runs it directly:

```r
data(csd_kim2005)                 # inputs of a worked example in Kim (2005)
csd <- do.call(calc_csd, csd_kim2005)

csd                               # compact summary
print(csd, full = TRUE)           # full report
```

```
Canonical Sequential Aggregation Distribution (CSD)
Reach: 61.81% | Average frequency: 1.791
Aggregation order: 1 -> 2 -> 3 (custom)
Probability sum: 1.000000000000 | Mean error: -2.22e-16
```

The full report lists the headline metrics, the model parameters, the
vehicles and the aggregation steps, the exposure distribution and the
cumulative N+ distribution:

```
CANONICAL SEQUENTIAL AGGREGATION DISTRIBUTION (CSD)
===================================================
Description: Beta-Binomial vehicles aggregated sequentially; the reach of every step comes from the second-order canonical expansion

HEADLINE METRICS:
-----------------
Total reach: 61.81%
Average exposures per person reached: 1.79

MODEL PARAMETERS:
-----------------
Probability of 0 exposures (%): 38.19
Total insertions (N): 6
Aggregation order: 1 -> 2 -> 3
Aggregation rule: custom
...
EXPOSURE DISTRIBUTION:
----------------------
(Percentage of the population receiving exactly N exposures)
1 exposure: 18.58%
2 exposures: 39.18%
3 exposures: 2.49%
...
```

`print(x, full = TRUE)` works the same way for `calc_csd()`, `calc_msad()`,
`calc_cbd()` and `calc_mbd()`. Their distributions have one row per insertion,
so `max_rows` (default 30) cuts long ones with a note; use `max_rows = Inf` to
list every level. The classical models (Sainsbury to CANEX) always print the
full report.

`str(csd_kim2005)` shows exactly what a function expects, and `?calc_csd`
documents each argument.

### Using your own data

CSD, MSAD, CBD and MBD take the same three kinds of input, all as
**proportions of the population** (between 0 and 1):

- `vehicles_data`: one row per vehicle with `insertions` (planned insertions,
  a whole number), `R1` (reach after one insertion, that is, the vehicle's
  audience) and `R2` (cumulative reach after two insertions; it must satisfy
  `R1 <= R2 <= 2 * R1 - R1^2`, the upper limit being random duplication; use
  `NA` for vehicles with a single insertion). Extra columns, such as a vehicle
  name, must be left out.
- `duplications`: a symmetric square matrix with the proportion of the
  population reached by both vehicles with one insertion each. The diagonal is
  ignored. The row and column order must match `vehicles_data`.
- `population`: the number of people in the target population, so the results
  are also expressed in people. The default, 1, leaves them as proportions.

```r
vehicles <- data.frame(
  vehicle    = c("TV", "Radio", "Digital"),
  insertions = c(3, 2, 4),
  R1         = c(0.35, 0.18, 0.10),
  R2         = c(0.44, 0.24, 0.15)
)

duplications <- matrix(
  c(NA,   0.07, 0.04,
    0.07, NA,   0.02,
    0.04, 0.02, NA),
  nrow = 3, byrow = TRUE,
  dimnames = list(vehicles$vehicle, vehicles$vehicle)
)

csd <- calc_csd(
  vehicles_data = vehicles[, c("insertions", "R1", "R2")],
  duplications = duplications,
  aggregation_order = "audience_desc",
  population = 8000000
)

print(csd, full = TRUE)
csd$reach$percent    # reach, in %
csd$distribution     # exposure distribution: probability, percent and people
```

`aggregation_order` can be `"audience_desc"` (largest audience first),
`"given"` (the row order of `vehicles_data`) or a permutation such as
`c(2, 1, 3)`. The order can change the exposure distribution, so report the one
you used. The other three models take the same arguments: replace `calc_csd`
with `calc_msad`, `calc_cbd` or `calc_mbd`. The models do not accept every
input: when the duplications are inconsistent with the model's assumptions the
function stops with a message that explains why, rather than returning a
silently altered result (`calc_mbd()` with the plan above is one such case).

### Inputs of each model

| Model | Function | Inputs | Units | Example dataset |
|---|---|---|---|---|
| Sainsbury, Binomial | `calc_sainsbury()`, `calc_binomial()` | `audiences`, `population`, `insertions` | people | `ratings_example` |
| Beta-Binomial | `calc_beta_binomial()` | `A1`, `A2`, `P`, `n` | people | `beta_binomial_example` |
| Hofmans (accumulation) | `calc_hofmans_accumulation()` | `R1`, `R2`, `N` | proportions | `hofmans_accumulation_example` |
| Agostini, Hofmans (duplication) | `calc_agostini_duplication()`, `calc_hofmans_duplication()` | `audiences`, `population`, `duplication_matrix` | people | `duplication_example` |
| Metheringham | `calc_metheringham()` | `audiences`, `insertions`, `duplication_matrix`, `population` | people | `metheringham_example` |
| CANEX | `calc_canex()` | `vehicles_data` (`k`, `R1`, `R2`), `duplications`, `population` | proportions | `canex_example` |
| CSD, MSAD, CBD, MBD | `calc_csd()`, `calc_msad()`, `calc_cbd()`, `calc_mbd()` | `vehicles_data` (`insertions`, `R1`, `R2`), `duplications`, `population` | proportions | `csd_example`, `msad_example`, `mbd_example` |

Examples with your own numbers, one per family:

```r
# Random duplication: three vehicles with 2, 1 and 3 insertions
calc_sainsbury(audiences = c(300000, 400000, 200000),
               population = 1000000, insertions = c(2, 1, 3))

# One vehicle: audience after one and after two insertions, six insertions
calc_beta_binomial(A1 = 400000, A2 = 620000, P = 1000000, n = 6)

# Several vehicles with one insertion each: duplication matrix in people
duplication <- matrix(c(NA,     140000, 70000,
                        140000, NA,     90000,
                        70000,  90000,  NA), nrow = 3, byrow = TRUE)
calc_hofmans_duplication(audiences = c(300000, 400000, 200000),
                         population = 1000000,
                         duplication_matrix = duplication)

# Several vehicles and insertions, with the duplication observed in people.
# Here the diagonal is the duplication between two insertions of the same
# vehicle, so it is not NA.
calc_metheringham(
  audiences = c(1500000, 800000, 1200000),
  insertions = c(4, 3, 5),
  duplication_matrix = matrix(c(150000, 200000, 180000,
                                200000, 120000, 140000,
                                180000, 140000, 170000), nrow = 3),
  population = 10000000
)
```

For the remaining models, copy the structure of their example dataset
(`str(canex_example)`, `str(mbd_example)`) and replace the numbers.

Exposure counts observed in a sample can instead be fitted with a
Negative-Binomial distribution:

```r
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
