<!-- README.md is generated from README.Rmd. Edit README.Rmd, then run
     knitr::knit("README.Rmd") with the installed package. -->



# mediaPlanR

[![R-CMD-check](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majesus/mediaPlanR/actions/workflows/R-CMD-check.yaml)

**mediaPlanR estimates how many people a media plan reaches, and how many
times, from the audience figures a planner already has.**

It brings together, in one package, the classical reach and
exposure-distribution models of the media-planning literature (Sainsbury,
Binomial, Beta-Binomial, Metheringham, Agostini, Hofmans, CANEX, CBD, CSD, MSAD
and MBD) and the tools around them: the fitting of a Beta-Binomial model to an
observed reach, a Negative-Binomial approximation for count processes, and the
comparison of a model with observed data.

The package is written for students and practitioners of media planning. This
document is meant to be read in order: first the ideas, then a plan worked from
start to finish, then how to choose a model and how to use your own data. The outputs shown below were produced by the code next to them.

## Installation

```r
# install.packages("pak")
pak::pak("majesus/mediaPlanR")
```

mediaPlanR requires R 4.0 or later. It contains no compiled code, but on
Windows `pak` may stop with *Could not find tools necessary to compile a
package* when it builds from source. You can either install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) (the version that
matches your R) or install the package without building it:

```r
install.packages("remotes")
remotes::install_github("majesus/mediaPlanR", build = FALSE)
```

This second route skips the vignette. Then load the package:


```r
library(mediaPlanR)
#> mediaPlanR 2.0.0 | Reach and exposure-distribution models for media planning. See ?mediaPlanR
```

## The ideas in five minutes

A media plan buys *insertions* (an advertisement in an issue, a spot, a post) in
*vehicles* (a newspaper, a radio station, a website). The planner knows how many
people each insertion exposes, the **audience**, and wants to know how many
different people the plan exposes and how often. These terms are used throughout:

| Term | Meaning |
|---|---|
| **Impressions** | Total exposures: audience times insertions, added over the plan. A person exposed twice counts twice. |
| **GRP** (gross rating points) | Impressions as a percentage of the population. |
| **Reach** | The share of the population exposed at least once. Each person counts once. |
| **Exposure distribution** | The share of the population exposed exactly 0, 1, 2, ... times. |
| **Average frequency** | Average number of exposures among the people reached: impressions divided by people reached. |
| **Effective reach** | The share exposed at least *k* times, where *k* (the effective frequency) is chosen by the planner. |
| **Duplication** | The people who belong to the audience of two vehicles. It is the reason reach is smaller than the sum of the audiences. |

Take a population of 1,000,000 people and two vehicles with one insertion each,
reaching 300,000 and 400,000 people:


```r
population <- 1000000
audiences  <- c(A = 300000, B = 400000)

sum(audiences)                       # impressions
#> [1] 700000
sum(audiences) / population * 100    # GRP
#> [1] 70
```

The two audiences add up to 700,000 impressions, but fewer than 700,000 *different*
people are reached, because some are in both audiences. If the two audiences
overlap only by chance (**random duplication**), the share of the population in
both is the product of the two shares, and the reach follows directly:


```r
duplicated_people <- prod(audiences / population) * population
duplicated_people                           # people expected in both audiences
#> [1] 120000
sum(audiences) - duplicated_people          # reach, in people
#> [1] 580000
```

`calc_sainsbury()` does this calculation and adds the full exposure
distribution:


```r
calc_sainsbury(audiences, population)
#> SAINSBURY MODEL
#> ===============
#> Description: random duplication and accumulation, with heterogeneous vehicles
#>
#> HEADLINE METRICS:
#> -----------------
#> Total reach: 58.00% (580000 people)
#>
#> EXPOSURE DISTRIBUTION:
#> ----------------------
#> (Percentage of the population receiving exactly N exposures)
#> 1 exposure: 46.00% (460000 people)
#> 2 exposures: 12.00% (120000 people)
#>
#> CUMULATIVE DISTRIBUTION:
#> -------------------------
#> (Percentage of the population receiving N or more exposures)
#> >= 1 exposure: 58.00% (580000 people)
#> >= 2 exposures: 12.00% (120000 people)
#>
#> SUMMARY STATISTICS:
#> --------------------
#> Average exposures per person reached: 1.21
```

The result can be read by hand: 42% of the population (0.7 x 0.6) is in
neither audience, 46% is in exactly one and 12% (0.3 x 0.4) is in both. Reach
is therefore 58%, and the average frequency among the people reached is
700,000 / 580,000 = 1.21.

In practice duplication is *observed*, for example in an audience survey. If
150,000 people are in both audiences, reach is 300,000 + 400,000 - 150,000 =
550,000 people (55%), lower than under random duplication. Most models in the
package exist to use such observed duplication when the plan has several
vehicles and several insertions, where no simple formula is available.

## A first plan, from start to finish

A plan with several insertions in each of several vehicles needs no duplication
data to be evaluated under random duplication. Audience is in people per
insertion and the population is in people; the functions reject inconsistent
inputs (for example an audience larger than the population) instead of
calculating with them. Take three vehicles with 3, 4 and 6 insertions:


```r
audiences  <- c(TV = 250000, Radio = 150000, Digital = 100000)
insertions <- c(3, 4, 6)
population <- 1000000

sainsbury <- calc_sainsbury(audiences, population, insertions)
sainsbury
#> SAINSBURY MODEL
#> ===============
#> Description: random duplication and accumulation, with heterogeneous vehicles
#>
#> HEADLINE METRICS:
#> -----------------
#> Total reach: 88.30% (882965 people)
#>
#> EXPOSURE DISTRIBUTION:
#> ----------------------
#> (Percentage of the population receiving exactly N exposures)
#> 1 exposure: 27.77% (277671 people)
#> 2 exposures: 29.83% (298264 people)
#> 3 exposures: 19.22% (192157 people)
#> 4 exposures: 8.29% (82895 people)
#> 5 exposures: 2.53% (25305 people)
#> 6 exposures: 0.56% (5628 people)
#> 7 exposures: 0.09% (924 people)
#> 8 exposures: 0.01% (112 people)
#> 9 exposures: 0.00% (10 people)
#> 10 exposures: 0.00% (1 people)
#> 11 exposures: 0.00% (0 people)
#> 12 exposures: 0.00% (0 people)
#> 13 exposures: 0.00% (0 people)
#>
#> CUMULATIVE DISTRIBUTION:
#> -------------------------
#> (Percentage of the population receiving N or more exposures)
#> >= 1 exposure: 88.30% (882965 people)
#> >= 2 exposures: 60.53% (605295 people)
#> >= 3 exposures: 30.70% (307031 people)
#> >= 4 exposures: 11.49% (114874 people)
#> >= 5 exposures: 3.20% (31979 people)
#> >= 6 exposures: 0.67% (6675 people)
#> >= 7 exposures: 0.10% (1047 people)
#> >= 8 exposures: 0.01% (123 people)
#> >= 9 exposures: 0.00% (11 people)
#> >= 10 exposures: 0.00% (1 people)
#> >= 11 exposures: 0.00% (0 people)
#> >= 12 exposures: 0.00% (0 people)
#> >= 13 exposures: 0.00% (0 people)
#>
#> SUMMARY STATISTICS:
#> --------------------
#> Average exposures per person reached: 2.21
```

The result is a list. Reach is available in percent and in people, and the
exposure distribution has one value for each number of exposures from one to
the total number of insertions. The share of the population that is never
exposed is 100 minus the reach, so the whole distribution always sums to 100:


```r
sainsbury$reach$percent
#> [1] 88.29653
round(data.frame(contacts = seq_along(sainsbury$distribution$percent),
                 percent = sainsbury$distribution$percent)[1:5, ], 2)
#>   contacts percent
#> 1        1   27.77
#> 2        2   29.83
#> 3        3   19.22
#> 4        4    8.29
#> 5        5    2.53
100 - sainsbury$reach$percent
#> [1] 11.70347
```

The `cumulative` element gives the share exposed *at least* N times. The value
for the effective frequency you choose is the effective reach: with an
effective frequency of 2, it is the 60.5% shown for two exposures.


```r
round(data.frame(min_contacts = 1:4,
                 percent = sainsbury$cumulative$percent[1:4],
                 people = sainsbury$cumulative$people[1:4]), 1)
#>   min_contacts percent   people
#> 1            1    88.3 882965.3
#> 2            2    60.5 605294.8
#> 3            3    30.7 307031.1
#> 4            4    11.5 114874.0
```

The impressions of the plan are the audience times the insertions, added over
the vehicles, and the average frequency is those impressions divided by the
people reached:


```r
impressions <- sum(audiences * insertions)
impressions
#> [1] 1950000
impressions / sainsbury$reach$people
#> [1] 2.208467
```

Sainsbury lets each vehicle keep its own audience; Binomial gives every
insertion the insertion-weighted average probability. The two models need no
duplication data, and side by side they show how much the heterogeneity of the
vehicles matters for this plan:


```r
binomial <- calc_binomial(audiences, population, insertions)
c(sainsbury = sainsbury$reach$percent, binomial = binomial$reach$percent)
#> sainsbury  binomial
#>  88.29653  87.90945
```

## Why the choice of model matters

Sainsbury and Binomial assume that every insertion is an independent
opportunity, even when two insertions are in the same vehicle. For different
vehicles that is the random-duplication hypothesis of the previous section. For
repeated insertions in one vehicle it is a strong assumption, because the people
who see the first insertion tend to be the same people who see the next one.

Consider one vehicle with a 500,000-person audience in a population of
1,000,000, and five insertions. Under random accumulation each insertion is an
independent 50% chance, so almost everybody is reached:


```r
calc_binomial(audiences = 500000, population = 1000000, insertions = 5)$reach$percent
#> [1] 96.875
```

Now suppose that audience research shows that the cumulative audience after two
insertions is only 550,000 (a reach of 55% against 50% after one insertion): the
second insertion adds just 50,000 new people.
That is evidence of a loyal audience, and `calc_beta_binomial()` uses it. The
**Beta-Binomial** model lets every person have a different, personal probability
of exposure and estimates how those probabilities are spread from the two
audience figures, written as reach proportions of the population (`R1` after one
insertion and `R2` after two):


```r
beta_binomial <- calc_beta_binomial(R1 = 0.50, R2 = 0.55, insertions = 5,
                                    population = 1000000)
beta_binomial$reach$percent
#> [1] 60.33654
beta_binomial$distribution$percent
#> [1]  6.009615  4.326923  4.326923  6.009615 39.663462
beta_binomial$parameters$zero_contact_probability
#> [1] 39.66346
```

Reach falls from 96.9% to 60.3%, and the distribution is polarized: 39.7% of the
population is exposed all five times (the last element of the distribution) and
39.7% is never exposed (the zero-exposure percentage shown last). These numbers are illustrative, but the lesson is general: **the same
vehicles and insertions can give very different reach depending on how
audiences repeat and overlap**, and the model is how that behavior enters the
estimate. The next section explains how to choose.

## Choosing a model

The models are organized by the shape of plan they were derived for (Aldás
Manzano, 1998) and by the data they need.

| Plan | Data you need | Models |
|---|---|---|
| Several vehicles, any number of insertions, no duplication data | Audience of each vehicle, population | `calc_sainsbury()`, `calc_binomial()` |
| One vehicle, several insertions | Reach after one and after two insertions | `calc_beta_binomial()`, `calc_hofmans_accumulation()` |
| Several vehicles, one insertion each, with observed duplication | Audiences and the duplication of each pair | `calc_agostini_duplication()`, `calc_hofmans_duplication()` (reach only) |
| Several vehicles and insertions, with observed duplication | Reach after one and two insertions of each vehicle, and the duplication of each pair | `calc_metheringham()`, `calc_canex()`, `calc_cbd()`, `calc_csd()`, `calc_msad()`, `calc_mbd()` |
| One Beta-Binomial for a whole schedule, matched to a reach figure you already have | Insertions, audiences and that reach | `fit_bbd_to_reach()`; `calibrate_bbd()` matches a target effective reach instead |
| Counts that are not limited by a number of insertions (page views, ad-server exposures) | Observed counts, or a mean and a dispersion | `fit_nbd_exposure()`, `nbd_exposure_distribution()` |

Some guidance for the common situations:

- **You only have audience figures.** Use Sainsbury or Binomial. They are exact
  under their assumptions, but those assumptions are strong, as the previous
  section showed: when audiences repeat or overlap more than chance would, they
  overstate reach.
- **You have a vehicle's audience after one and two insertions.** Use the
  Beta-Binomial model for that vehicle.
- **You have observed duplication between vehicles.** For one insertion per
  vehicle, the Agostini and Hofmans formulas return reach only. For several
  insertions, Metheringham's model is the simplest: it replaces the plan by an
  "average vehicle". CANEX, CBD, CSD, MSAD and MBD model each vehicle separately
  and combine them in different ways; they are more demanding and their
  differences are described in `vignette("mediaPlanR-intro")`.
- **Your exposures are unbounded counts.** The Negative-Binomial functions are
  scoped to those processes. They are deliberately not meant for finite
  insertion schedules, where several insertions fall in the same vehicle.

## Using your own data

Every model that needs more than audiences takes a set of arguments that mirror
a small table. The quickest way to see what a function expects is its example
dataset, which is already a list of arguments:


```r
data(csd_example)     # reach and duplication figures of an illustrative scenario
str(csd_example)
#> List of 3
#>  $ vehicles_data    :'data.frame':	3 obs. of  3 variables:
#>   ..$ insertions: num [1:3] 3 2 4
#>   ..$ R1        : num [1:3] 0.35 0.18 0.1
#>   ..$ R2        : num [1:3] 0.44 0.24 0.15
#>  $ duplications     : num [1:3, 1:3] NA 0.07 0.04 0.07 NA 0.02 0.04 0.02 NA
#>  $ aggregation_order: int [1:3] 1 2 3
```

so that `do.call()` runs the model on it. Printing a result gives the full report
of the model: headline metrics, parameters, the exposure distribution, the
cumulative distribution and diagnostics.


```r
csd <- do.call(calc_csd, csd_example)
csd
#> CANONICAL SEQUENTIAL AGGREGATION DISTRIBUTION (CSD)
#> ===================================================
#> Description: Beta-Binomial vehicles aggregated sequentially; the reach of every step comes from the pairwise canonical expansion
#>
#> HEADLINE METRICS:
#> -----------------
#> Total reach: 68.10%
#> Average exposures per person reached: 2.66
#>
#> MODEL PARAMETERS:
#> -----------------
#> Probability of 0 exposures (%): 31.90
#> Total insertions (N): 9
#> Aggregation order: 1 -> 2 -> 3
#> Aggregation rule: custom
#>
#> VEHICLES:
#> ---------
#>  vehicle insertions own_reach_percent aggregation_position
#>        1          3             48.83                    1
#>        2          2             24.00                    2
#>        3          4             20.67                    3
#>
#> AGGREGATION STEPS:
#> ------------------
#>  step added_vehicle target_reach_percent zero_probability
#>     1             2                60.45           0.3955
#>     2             3                68.10           0.3190
#>  random_zero_probability expansion_adjustment duplicated_reach
#>                   0.3889              0.01715           0.1239
#>                   0.3085              0.03416           0.1302
#>
#> CANONICAL CORRELATIONS BETWEEN VEHICLES:
#> ----------------------------------------
#>        V1     V2     V3
#> V1 1.0000 0.0382 0.0349
#> V2 0.0382 1.0000 0.0174
#> V3 0.0349 0.0174 1.0000
#>
#> EXPOSURE DISTRIBUTION:
#> ----------------------
#> (Percentage of the population receiving exactly N exposures)
#> 1 exposure: 16.60%
#> 2 exposures: 16.91%
#> 3 exposures: 19.35%
#> 4 exposures: 7.80%
#> 5 exposures: 4.70%
#> 6 exposures: 1.65%
#> 7 exposures: 0.80%
#> 8 exposures: 0.21%
#> 9 exposures: 0.07%
#>
#> CUMULATIVE DISTRIBUTION:
#> -------------------------
#> (Percentage of the population receiving N or more exposures)
#> >= 1 exposure: 68.10%
#> >= 2 exposures: 51.49%
#> >= 3 exposures: 34.59%
#> >= 4 exposures: 15.23%
#> >= 5 exposures: 7.43%
#> >= 6 exposures: 2.73%
#> >= 7 exposures: 1.08%
#> >= 8 exposures: 0.28%
#> >= 9 exposures: 0.07%
#>
#> DIAGNOSTICS:
#> ------------
#> Probability sum: 1.000000000000 | Smallest probability: 0.000694 | Mean error: 4.44e-16
#> Largest margin error across aggregation steps: 1.11e-16
#> Smallest eigenvalue of the correlation matrix: 0.9561
```

Example datasets exist for the main models (`ratings_example`,
`beta_binomial_example`, `hofmans_accumulation_example`, `duplication_example`,
`metheringham_example`, `canex_example`, `csd_example`, `msad_example`,
`mbd_example` and `bbd_reach_example`). All of them are original illustrative
scenarios: the package does not distribute the figures of published worked
examples, whose redistribution basis is not established.

### CSD, MSAD, CBD and MBD

These four models take the same three kinds of input. The reach and duplication
figures are **proportions of the population** (between 0 and 1); only
`population` is a count of people:

- `vehicles_data`: one row per vehicle with `insertions` (planned insertions, a
  whole number), `R1` (reach after one insertion, that is, the vehicle's
  audience) and `R2` (cumulative reach after two insertions). `R2` must satisfy
  `R1 <= R2 <= 2 * R1 - R1^2`, where the upper limit is random accumulation. Use
  `NA` for vehicles with a single insertion (`calc_canex()` accepts the same data
  frame and the same `NA`; for it, too, `R2` is needed only for vehicles with two
  or more insertions). Extra columns, such as a vehicle name, must be left out.
- `duplications`: a symmetric square matrix with the proportion of the
  population reached by both vehicles with one insertion each. The diagonal is
  ignored. Rows and columns follow the order of `vehicles_data`. A duplication
  cannot exceed the smaller of the two audiences, and cannot be smaller than
  the sum of the two audiences minus one.
- `population`: the number of people in the population, so that results are also
  expressed in people. The default, 1, leaves them as proportions.


```r
vehicles <- data.frame(
  vehicle    = c("TV", "Radio", "Digital"),
  insertions = c(3, 2, 4),
  R1         = c(0.35, 0.18, 0.10),
  R2         = c(0.44, 0.24, 0.15)
)

duplications <- matrix(
  c(NA,   0.08, 0.05,
    0.08, NA,   0.03,
    0.05, 0.03, NA),
  nrow = 3, byrow = TRUE,
  dimnames = list(vehicles$vehicle, vehicles$vehicle)
)

csd <- calc_csd(
  vehicles_data = vehicles[, c("insertions", "R1", "R2")],
  duplications = duplications,
  aggregation_order = "audience_desc",
  population = 8000000
)
csd
#> CANONICAL SEQUENTIAL AGGREGATION DISTRIBUTION (CSD)
#> ===================================================
#> Description: Beta-Binomial vehicles aggregated sequentially; the reach of every step comes from the pairwise canonical expansion
#>
#> HEADLINE METRICS:
#> -----------------
#> Total reach: 65.92% (5273987 people)
#> Average exposures per person reached: 2.75
#>
#> MODEL PARAMETERS:
#> -----------------
#> Probability of 0 exposures (%): 34.08
#> Total insertions (N): 9
#> Aggregation order: 1 -> 2 -> 3
#> Aggregation rule: audience_desc
#>
#> VEHICLES:
#> ---------
#>  vehicle insertions own_reach_percent aggregation_position
#>        1          3             48.83                    1
#>        2          2             24.00                    2
#>        3          4             20.67                    3
#>
#> AGGREGATION STEPS:
#> ------------------
#>  step added_vehicle target_reach_percent zero_probability
#>     1             2                59.49           0.4051
#>     2             3                65.92           0.3408
#>  random_zero_probability expansion_adjustment duplicated_reach
#>                   0.3889              0.04164           0.1334
#>                   0.3085              0.10462           0.1424
#>
#> CANONICAL CORRELATIONS BETWEEN VEHICLES:
#> ----------------------------------------
#>        V1     V2     V3
#> V1 1.0000 0.0928 0.1048
#> V2 0.0928 1.0000 0.1041
#> V3 0.1048 0.1041 1.0000
#>
#> EXPOSURE DISTRIBUTION:
#> ----------------------
#> (Percentage of the population receiving exactly N exposures)
#> 1 exposure: 15.06% (1204889 people)
#> 2 exposures: 15.85% (1268318 people)
#> 3 exposures: 18.73% (1498537 people)
#> 4 exposures: 8.11% (649123 people)
#> 5 exposures: 5.07% (405202 people)
#> 6 exposures: 1.85% (148342 people)
#> 7 exposures: 0.91% (72930 people)
#> 8 exposures: 0.25% (20005 people)
#> 9 exposures: 0.08% (6640 people)
#>
#> CUMULATIVE DISTRIBUTION:
#> -------------------------
#> (Percentage of the population receiving N or more exposures)
#> >= 1 exposure: 65.92% (5273987 people)
#> >= 2 exposures: 50.86% (4069098 people)
#> >= 3 exposures: 35.01% (2800779 people)
#> >= 4 exposures: 16.28% (1302242 people)
#> >= 5 exposures: 8.16% (653119 people)
#> >= 6 exposures: 3.10% (247916 people)
#> >= 7 exposures: 1.24% (99575 people)
#> >= 8 exposures: 0.33% (26645 people)
#> >= 9 exposures: 0.08% (6640 people)
#>
#> DIAGNOSTICS:
#> ------------
#> Probability sum: 1.000000000000 | Smallest probability: 0.00083 | Mean error: 4.44e-16
#> Largest margin error across aggregation steps: 1.11e-16
#> Smallest eigenvalue of the correlation matrix: 0.8915
```

The report of these four models also lists the vehicles and the aggregation
steps. Their distributions have one row per insertion of the plan, so a long
one is cut after `max_rows` exposure levels (30 by default; use
`print(csd, max_rows = Inf)` to list them all). For a four-line summary, use
`print(csd, full = FALSE)`.

`aggregation_order` can be `"audience_desc"` (largest audience first), `"given"`
(the row order of `vehicles_data`) or a permutation such as `c(2, 1, 3)`. The
order can change the exposure distribution of CSD, MSAD and MBD, so report the one
you used (CBD does not depend on it). The other three models take the same
arguments; they use different formulas and therefore give different results
from the same inputs (CSD and CBD obtain reach from the same canonical
expansion, which is why their reach coincides here):


```r
v <- vehicles[, c("insertions", "R1", "R2")]
models <- list(CSD = calc_csd, MSAD = calc_msad, CBD = calc_cbd, MBD = calc_mbd)
data.frame(
  model = names(models),
  reach_percent = sapply(models, function(f) f(v, duplications)$reach$percent),
  average_frequency = sapply(models, function(f) f(v, duplications)$average_frequency),
  row.names = NULL
)
#>   model reach_percent average_frequency
#> 1   CSD      65.92483          2.745551
#> 2  MSAD      64.04448          2.826161
#> 3   CBD      65.92483          2.724670
#> 4   MBD      64.69358          2.797805
```

The models do not accept every input. When the duplications are incompatible
with the model, the function stops with a message that explains why; it does
not silently alter your data. For example, `calc_mbd()` rejects the same plan
with duplications of 0.07, 0.04 and 0.02. Each pair overlaps slightly more than
chance would, but the Beta-Binomial distribution that the model imputes for
three vehicles uses the average audience (0.21) and the average duplication
(0.043), and that average is below the overlap expected by chance for the
average audience (0.21 x 0.21 = 0.044), which that distribution cannot
represent.

### The other models


```r
# Random duplication: three vehicles with 2, 1 and 3 insertions
calc_sainsbury(audiences = c(300000, 400000, 200000),
               population = 1000000, insertions = c(2, 1, 3))$reach$percent
#> [1] 84.9472

# Several vehicles, one insertion each: duplication matrix in people
duplication <- matrix(c(NA,     140000, 70000,
                        140000, NA,     90000,
                        70000,  90000,  NA), nrow = 3, byrow = TRUE)
calc_hofmans_duplication(audiences = c(300000, 400000, 200000),
                         population = 1000000,
                         duplication_matrix = duplication)$reach$percent
#> [1] 64.16971

# Several vehicles and insertions, duplication observed in people. The diagonal
# holds the duplication between two insertions of the same vehicle, so it is
# not NA.
calc_metheringham(
  audiences = c(1500000, 800000, 1200000),
  insertions = c(4, 3, 5),
  duplication_matrix = matrix(c(150000, 200000, 180000,
                                200000, 120000, 140000,
                                180000, 140000, 170000), nrow = 3),
  population = 10000000
)$reach$percent
#> [1] 74.34177
```

Units differ between families, and the functions check them:

| Model | Function | Main inputs | Units |
|---|---|---|---|
| Sainsbury, Binomial | `calc_sainsbury()`, `calc_binomial()` | `audiences`, `population`, `insertions` | people |
| Beta-Binomial | `calc_beta_binomial()` | `R1`, `R2`, `insertions`, `population` | proportions (`population` in people) |
| Hofmans (accumulation) | `calc_hofmans_accumulation()` | `R1`, `R2`, `insertions` | proportions |
| Agostini, Hofmans (duplication) | `calc_agostini_duplication()`, `calc_hofmans_duplication()` | `audiences`, `population`, `duplication_matrix` | people |
| Metheringham | `calc_metheringham()` | `audiences`, `insertions`, `duplication_matrix`, `population` | people |
| CANEX | `calc_canex()` | `vehicles_data` (`insertions`, `R1`, `R2`), `duplications`, `population` | proportions |
| CSD, MSAD, CBD, MBD | `calc_csd()`, `calc_msad()`, `calc_cbd()`, `calc_mbd()` | `vehicles_data` (`insertions`, `R1`, `R2`), `duplications`, `population` | proportions |

Each function's help page (`?calc_canex`, `?calc_cbd`, ...) documents its
arguments, its assumptions and the published sources.

## Reading the results

Results have a common core, but their shape depends on the family of models:

| Result | Reach | Exposure distribution |
|---|---|---|
| `calc_sainsbury()`, `calc_binomial()`, `calc_beta_binomial()`, `calc_metheringham()` | `$reach`: `percent`, `people` | `$distribution`: list of `percent` and `people`, from 1 exposure; the share with no exposure is 100 minus the reach |
| `calc_canex()`, `calc_csd()`, `calc_msad()`, `calc_cbd()`, `calc_mbd()` | `$reach`: `probability`, `percent`, `people`; also `$average_frequency` | `$distribution`: data frame, from 0 exposures |
| `calc_agostini_duplication()`, `calc_hofmans_duplication()` | `$reach` only | not estimated |

When you calculate with the objects yourself, two quick coherence checks are
that the probabilities add up to one and that the average frequency equals the
mean number of exposures per person divided by the reach.


```r
sum(csd$distribution$probability)
#> [1] 1
csd$average_frequency
#> [1] 2.745551
sum(vehicles$insertions * vehicles$R1) / csd$reach$probability
#> [1] 2.745551
```

The last two numbers coincide because CSD preserves the plan's mean number of
exposures per person (the sum of insertions times first-insertion reach). MSAD
does too. CBD, MBD and CANEX do not always: CBD changes it through the
adjustment of its zero cell, CANEX when it truncates negative probabilities, and
MBD can depart from it even when no probability is negative.
Each reports the difference in its diagnostics (`diagnostics$mean_error`, or
`mean_exposures_expected` and `mean_exposures_result` for CANEX), and the average
frequency of a result is always computed from the distribution that it returns.

## Comparing a model with observed data

When you have a measured exposure distribution for a schedule, for example from
a panel, `evaluate_exposure_model()` compares it with a model. The table must
contain `contacts` and `observed`, include the zero-exposure cell, and declare
its scale (`"count"`, `"probability"` or `"percent"`). The counts below are
hypothetical: a panel of 1,000 people classified by the number of exposures to
the three-vehicle schedule of the previous sections (0 to 9 exposures).


```r
observed <- data.frame(
  contacts = 0:9,
  observed = c(330, 150, 160, 190, 80, 50, 25, 10, 5, 0)
)

evaluation <- evaluate_exposure_model(
  observed = observed,
  predicted = csd,
  observed_scale = "count"
)
round(unlist(evaluation$summary[c("kim_aer", "kim_ape")]), 3)
#> kim_aer kim_ape
#>   0.016   0.026
```

`kim_aer` is Kim's average percentage error in reach (AER), |observed reach -
predicted reach| / observed reach, averaged over schedules. `kim_ape` is Kim's
average percentage error in the exposure distribution (APE): the sum, over the
exposure levels of one or more, of the absolute differences between observed and
predicted shares, divided by the observed reach (both are proposed by Kim,
2005). Both are shown as proportions. `evaluation$summary` also holds
the total variation distance and other diagnostics. These errors are descriptive
measures of fit, not statistical tests. The function
requires the observed and predicted distributions to cover exactly the same
exposure levels. Observed data are always supplied by the analyst; the package
never infers them from model inputs.

## Limits to keep in mind

- **Applying a model is not validating it.** The models can be applied to any
  mix of media, but the evidence on their accuracy comes mainly from magazine
  and Internet data (the sources cited in each help page). Their accuracy for a
  particular combination of media, such as television, radio and digital, and
  for a particular population has to be checked against observed data for that
  combination (see `evaluate_exposure_model()`).
- **Audience is an opportunity to see, not attention.** The models estimate how
  many people are exposed to a vehicle, not how many notice, remember or are
  persuaded by the message. Contacts are opportunities of exposure in the unit
  of the audience figures (an issue, a spot, the visits to a site in a week);
  they are advertising impressions in the technical sense (served, rendered or
  viewable) only if the figures measure that event.
- **Every model is an approximation.** Each one rests on stated assumptions about
  duplication and about how exposure probabilities vary between people. Where the
  assumption is wrong, so is the estimate, and the help pages state which
  assumption each model makes.
- **Observed inputs must be coherent.** The multivariate models stop when
  pairwise duplications are impossible given the audiences, or when some three
  vehicles have no compatible joint exposure probabilities. CANEX can produce small
  negative probabilities; it sets them to zero and reports in `diagnostics`
  how much probability was altered. MBD can do so too when it combines four or
  more vehicles, and warns when this is possible.
- **Size is limited.** The number of exposure combinations grows exponentially
  with vehicles and insertions. CANEX stops with an error on plans that are too
  large for it, and CBD and MBD are limited to 12 vehicles. CSD, MSAD, CBD and
  MBD also estimate the size of their tables, or their work, from the insertions
  before computing, and stop with an explanatory error beyond a budget that can
  be raised with options (section "Computational limits" of `?mediaPlanR`).
- **Published values are reproduced up to documented differences.** Where the
  package differs from a published table, `vignette("mediaPlanR-intro")` and the
  file `system.file("DATA-PROVENANCE.md", package = "mediaPlanR")` state by how
  much and why.

## Learn more

- `vignette("mediaPlanR-intro")` develops each model, its assumptions and its
  validation against the sources.
- Each function has a help page with its arguments, details, references and
  examples: start with `?mediaPlanR`, `?calc_sainsbury` and `?calc_csd`.
- `citation("mediaPlanR")` gives the reference for the package. When you use a
  specific model, please also cite the original source of that model.
- Questions, errors and suggestions are welcome in the
  [issue tracker](https://github.com/majesus/mediaPlanR/issues).

## References

- Agostini, J. M. (1961). How to estimate unduplicated audiences. *Journal of
  Advertising Research*, 1(3), 11-14. <https://doi.org/10.1080/00218499.1961.12519620>
- Aldás Manzano, J. (1998). *Modelos de determinación de la cobertura y la
  distribución de contactos en la planificación de medios publicitarios
  impresos*. Doctoral dissertation, Universidad de Valencia.
- Caffyn, J. M., & Sagovsky, M. (1963). Net audiences of British newspapers:
  A comparison of the Agostini and Sainsbury methods. *Journal of Advertising
  Research*, 3(1), 21-25. <https://doi.org/10.1080/00218499.1963.12527134>
- Chandon, J.-L. (1976). *A comparative study of media exposure models*.
  Unpublished doctoral dissertation, Northwestern University, Evanston, IL.
- Cheong, Y. (2007). *Multivariate Beta Binomial Distribution Model as a Web
  Media Exposure Model*. Doctoral dissertation, The University of Texas at
  Austin.
  Handle 2152/3215 (University of Texas at Austin repository).
- Cheong, Y., Leckenby, J. D., & Eakin, T. (2011). Evaluating the multivariate
  beta binomial distribution for estimating magazine and Internet exposure
  frequency distributions. *Journal of Advertising*, 40(1), 7-23.
  <https://doi.org/10.2753/JOA0091-3367400101>
- Danaher, P. J. (1991). A canonical expansion model for multivariate media
  exposure distributions: A generalization of the "duplication of viewing law".
  *Journal of Marketing Research*, 28(3), 361-367.
  <https://doi.org/10.1177/002224379102800311>
- Danaher, P. J. (1992). A Markov-chain model for multivariate
  magazine-exposure distributions. *Journal of Business & Economic Statistics*,
  10(4), 401-407. <https://doi.org/10.1080/07350015.1992.10509915>
- Danaher, P. J. (2007). Modeling page views across multiple websites with an
  application to Internet reach and frequency prediction. *Marketing Science*,
  26(3), 422-437. <https://doi.org/10.1287/mksc.1060.0226>
- Ehrenberg, A. S. C. (1959). The pattern of consumer purchases. *Applied
  Statistics*, 8(1), 26-41. <https://doi.org/10.2307/2985810>
- Hofmans, P. (1966). Measuring the cumulative net coverage of any combination of
  media. *Journal of Marketing Research*, 3(3), 269-278.
  <https://doi.org/10.1177/002224376600300307>
- Kim, H. (1994). *A conditional beta distribution model for advertising media
  reach/frequency estimation*. Doctoral dissertation, The University of Texas at
  Austin.
- Kim, H. G. (2005). *A Canonical Sequential Aggregation Media Model*. Doctoral
  dissertation, The University of Texas at Austin.
  Handle 2152/1590 (University of Texas at Austin repository).
- Leckenby, J. D., & Boyd, M. M. (1984). An improved beta binomial
  reach/frequency model for magazines. *Current Issues and Research in
  Advertising*, 7(1), 1-24.
- Leckenby, J. D., & Rice, M. D. (1986). The declining reach phenomenon in
  exposure distribution models. *Journal of Advertising*, 15(3), 13-20.
  <https://doi.org/10.1080/00913367.1986.10673014>
- Metheringham, R. A. (1964). Measuring the net cumulative coverage of a print
  campaign. *Journal of Advertising Research*, 4(4), 23-28.
  <https://doi.org/10.1080/00218499.1964.12519751>
- Morgensztern, A. (1970). Fréquentation de la presse, généralisation, effet
  publicitaire. Étude No. 19, Institut de Recherches et d'Études Publicitaires
  (IREP), Paris. Listed and attributed to the reach formula by Kim (2005); the
  original study was not consulted for this package.

## Author and license

Manuel J. Sánchez-Franco, Universidad de Sevilla.
[ORCID 0000-0002-8042-3550](https://orcid.org/0000-0002-8042-3550)

Released under the MIT license.
