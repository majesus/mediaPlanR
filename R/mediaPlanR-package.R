#' mediaPlanR: Reach and Exposure-Distribution Models for Media Planning
#'
#' Reproducible implementations of the classical reach and exposure-distribution
#' models of the media-planning literature, with tools to fit them to observed
#' reach and to evaluate their predictions against observed distributions.
#'
#' @section Scope of the validation:
#' The models can be applied to any mix of media, but the published evidence on
#' their accuracy comes mainly from magazine and Internet data. Their accuracy
#' for a particular combination of media and population should be checked
#' against observed data, for example with [evaluate_exposure_model()]. The
#' published evidence for Internet data (Cheong, Leckenby and Eakin, 2011) takes
#' a website as the vehicle and a week as the measurement interval, and counts
#' several visits within the week as one exposure: see "Unit of exposure" in the
#' section "Notation and units".
#'
#' Three kinds of evidence must not be confused. *Replication of a published
#' example or table*: in the maintainer's local tests, whose input tables are
#' not distributed, the package reproduces, within 0.01 percentage points, the
#' CBD columns of the 80 two-vehicle plans of Kim (2005) and Hong (1998), but
#' those sources do not print the duplications, which were reconstructed from
#' another model's published column; this is a consistency check between
#' columns, not an independent test. *Fidelity to the printed algorithm*: the
#' functions follow the procedures described in the sources, and where the
#' printed numbers cannot be reproduced the difference is recorded in the
#' tests. *Predictive validation*: agreement with independent observations of
#' reach and frequency. The package does not provide it and the replicated
#' tables do not supply it; use [evaluate_exposure_model()] with your own
#' data. The provenance of each dataset is recorded in the installed file
#' `DATA-PROVENANCE.md` (see `system.file("DATA-PROVENANCE.md", package =
#' "mediaPlanR")`).
#'
#' @section Notation and units:
#' The same quantity has the same meaning in every function. Names follow the
#' sources of each model only where the literature fixes them.
#' \itemize{
#'   \item *Population* (number of people in the planning universe): `population`
#'     everywhere.
#'   \item *Audience* (people exposed by one insertion of a vehicle):
#'     `audiences` (one value per vehicle) in the `calc_*()` functions, always
#'     in people. As a proportion of the population it is `R1`.
#'   \item *Reach after the first and second insertion of the same vehicle*:
#'     `R1` and `R2` (Kim, 2005, and the sources of each model) are proportions
#'     of the population in [calc_beta_binomial()], [calc_hofmans_accumulation()],
#'     [calc_canex()], [calc_csd()], [calc_msad()], [calc_cbd()] and [calc_mbd()].
#'     [calibrate_bbd()] uses `R1` but does not take `R2` as an input. `R2` is
#'     cumulative: the proportion exposed at least once in two insertions. If
#'     the audience is known in people, divide it by the population.
#'   \item *Number of insertions*: `insertions` (one value per vehicle) in all
#'     functions; the formulas use \eqn{n_i} for a vehicle and \eqn{N} for the
#'     total. The column of `vehicles_data` in [calc_canex()] may also be named
#'     `k`, Danaher's notation. The argument `k` of
#'     [calc_agostini_duplication()] is something else: Agostini's duplication
#'     coefficient.
#'   \item *Duplication between two vehicles*: the argument is
#'     `duplication_matrix`, in people, for [calc_agostini_duplication()],
#'     [calc_hofmans_duplication()] and [calc_metheringham()], and
#'     `duplications`, as proportions of the population, for [calc_canex()] and
#'     the sequential models. The different names signal the different units.
#'   \item *Reach* (also net audience) counts each person once. *Contacts*
#'     (also impressions, or gross audience in the duplication models) count a
#'     person once per opportunity of exposure, that is, once per insertion (see
#'     "Unit of exposure" below): with several insertions they are the sum of
#'     each audience times its insertions, \eqn{I = \sum n_i A_i}; with one
#'     insertion per vehicle they are the sum of the audiences, the *gross
#'     audience* of [calc_agostini_duplication()] and [calc_hofmans_duplication()].
#'     *Duplications* between vehicles are coincidences between pairs, so a
#'     person present in three vehicles contributes three pairs, not three
#'     people. Write the unit (people, contacts, pairs or proportion) before
#'     forming a ratio. With these symbols, the average frequency is
#'     \eqn{F = I / R} (contacts per person reached, defined only if \eqn{R > 0})
#'     and, per person of the universe, the mean number of contacts is
#'     \eqn{\mu = I / P}, so that \eqn{GRP = 100 \mu}.
#'   \item *Unit of exposure*: the models count opportunities of exposure, and
#'     the data define what an opportunity is. An audience is the number of
#'     people exposed by one insertion (an issue, a spot, a post, the visits to a
#'     site in a week), with at most one opportunity per person and insertion,
#'     and the audiences, `R1`, `R2` and the duplications must measure the same
#'     population, the same unit of exposure and the same period. Contacts are
#'     those opportunities, and they can be read as modeled advertising
#'     impressions only when the audiences and duplications measure that same
#'     opportunity. The audience of a vehicle does not show by itself that an
#'     advertisement was served, rendered, viewable or noticed; if the data count
#'     weekly visitors of a website, the reach is a reach of those visitors, not
#'     of viewable advertisements. An effective-frequency threshold is a delivery
#'     criterion of the analyst, not a measure of attention, conversion or
#'     causal effect.
#'   \item *Effective frequency*: `frequency` in [calibrate_bbd()]; effective
#'     reach is the proportion exposed that many times or more.
#'   \item *Units*: every argument documents its unit. A *proportion* is a
#'     number between 0 and 1 (0.35 for 35%, not 35); percentages appear in
#'     result columns named `percent` (and `reach_percent`); counts of people
#'     appear in arguments described as "in people" and in result columns named
#'     `people`. There is one exception, listed in the help page of its
#'     function: `zero_contact_probability` in [calc_beta_binomial()] is a
#'     percentage (0 to 100) despite its name.
#'   \item *Errors of evaluation*: the average percentage errors of
#'     [evaluate_exposure_model()] (`kim_aer`, `kim_ape`) are relative errors
#'     returned as proportions (multiply by 100 for percent); absolute errors of
#'     reach are differences between proportions (multiply by 100 for
#'     percentage points, so 0.002 of probability is 0.2 percentage points).
#' }
#'
#' @section Assumptions of the models:
#' Each model rests on assumptions about four things. They are the first thing
#' to check when choosing a model.
#' \itemize{
#'   \item *Individuals*: homogeneous (every person has the same probability of
#'     being exposed to an insertion of a vehicle) or heterogeneous
#'     (probabilities differ between people, as between loyal and occasional
#'     readers). Heterogeneity makes repeated exposures concentrate in the same
#'     people, so reach grows more slowly with the number of insertions.
#'     [calc_binomial()] and [calc_sainsbury()] assume homogeneous individuals;
#'     the Beta-Binomial family ([calc_beta_binomial()], [calc_metheringham()],
#'     [calc_canex()], [calc_csd()], [calc_msad()], [calc_cbd()],
#'     [calc_mbd()]) lets the probability vary between people.
#'   \item *Vehicles*: homogeneous (every vehicle has the plan's average
#'     audience) or heterogeneous (each has its own audience).
#'     [calc_binomial()] and [calc_metheringham()] treat vehicles as
#'     homogeneous; [calc_sainsbury()] and the multivariate models take the
#'     audience of each vehicle as an input. Whether the distribution that a
#'     multivariate model returns reproduces those audiences and the plan's mean
#'     number of contacts exactly depends on the model: [calc_csd()] and
#'     [calc_msad()] conform their joint tables to the marginal distributions
#'     that they combine and therefore conserve the mean, whereas [calc_canex()],
#'     [calc_cbd()] and [calc_mbd()] can alter both (by the truncation of
#'     negative cells, by the adjustment of the zero cell or by the bounded
#'     conditional allocation of MBD), in the last case even when no
#'     probability is negative. Read their `diagnostics` before interpreting the
#'     average frequency.
#'   \item *Duplication between vehicles*: random (people choose vehicles
#'     independently, so the audience shared by two vehicles is the product of
#'     their audiences) or observed (taken from data). [calc_binomial()] and
#'     [calc_sainsbury()] assume random duplication; the other models with
#'     several vehicles use observed duplication. The reference studies of the
#'     multivariate models mostly use observed duplications at or above the
#'     random one (Hong, 1998, p. 238); a lower duplication can be valid data,
#'     and its effect on each model is described in the section "Domain of
#'     validity" of [calc_canex()].
#'   \item *Accumulation in the same vehicle*: random (a repeat insertion in
#'     a vehicle is as independent as an insertion in another vehicle) or
#'     observed through the reach after two insertions (`R2`), which captures
#'     that those who saw the first insertion tend to see the next.
#'     [calc_binomial()] and [calc_sainsbury()] assume random accumulation; the
#'     models for several insertions in a vehicle use the observed
#'     accumulation: `R2` or, in [calc_metheringham()], the audience
#'     duplicated between two insertions of the same vehicle. The Beta-Binomial
#'     family requires `R2 <= 2 * R1 - R1^2`, that is, a duplication between two
#'     insertions of the vehicle not below the random one (it excludes negative
#'     dependence between insertions). That bound belongs to the family and is
#'     not the logical limit of two insertions, which is `R2 <= min(1, 2 * R1)`
#'     (see [calc_beta_binomial()]).
#' }
#' All the models also assume that exposure probabilities are stationary
#' (they do not change over the period of the plan). Each help page states the
#' assumptions of its model.
#'
#' @section Computational limits:
#' The schedule-level models build tables whose size grows with the number of
#' insertions, and their cost grows exponentially with the number of vehicles.
#' Before allocating anything, [calc_csd()], [calc_msad()], [calc_cbd()] and
#' [calc_mbd()] estimate the size or the work that a schedule needs from its
#' insertions, and stop with an explanatory error when it exceeds a budget.
#' [calc_canex()] has its own limit of two million combinations of exposure
#' levels, and [calc_cbd()] and [calc_mbd()] accept at most 12 vehicles.
#' The two budgets can be raised, if the memory or the computing time allow
#' it, with options:
#' \itemize{
#'   \item `mediaPlanR.max_table_cells` (default `1e7`): cells of the largest
#'     joint table of [calc_csd()] and [calc_msad()], which combine two
#'     distributions at each step; it is the number of exposure levels of the
#'     vehicles already combined times that of the vehicle added, and each matrix
#'     of that size takes 8 bytes per cell (80 MB at the default).
#'   \item `mediaPlanR.max_work` (default `1e9`): estimated elementary
#'     operations of [calc_cbd()] and [calc_mbd()], roughly one to two minutes
#'     of computation at the default. For [calc_cbd()] it is
#'     \eqn{2^m \sum_i (1 + n_1 + \cdots + n_{i-1})(n_i + 1)}, with \eqn{m}
#'     vehicles of \eqn{n_i} insertions; for [calc_mbd()], the work of peeling
#'     the vehicles one at a time, which does not include the inclusion-exclusion
#'     grid that depends only on the number of vehicles.
#' }
#' For example, `options(mediaPlanR.max_table_cells = 5e7)`. An option that is
#' not a single positive number is ignored.
#'
#' @section Suggested study path:
#' The help pages of a reference manual are in alphabetical order, which
#' assumes prior knowledge. For sequential study, read in this order: (1) the
#' sections "Notation and units" and "Assumptions of the models" above, to fix
#' what is counted (people, contacts, pairs) and what each model assumes; (2) the
#' models of independence, [calc_sainsbury()] and [calc_binomial()]; (3) the ad hoc duplication formulas,
#' [calc_agostini_duplication()] and [calc_hofmans_duplication()]; (4) the
#' accumulation of one vehicle, [calc_beta_binomial()] and
#' [calc_hofmans_accumulation()], with [fit_bbd_to_reach()] and
#' [calibrate_bbd()]; (5) the multivariate models, [calc_canex()], [calc_csd()],
#' [calc_msad()], [calc_cbd()] and [calc_mbd()], and the section "Domain of
#' validity" of [calc_canex()]; (6) the count approximation
#' [nbd_exposure_distribution()]; and (7) evaluation,
#' [evaluate_exposure_model()].
#'
#' The vignette `vignette("mediaPlanR-intro")` gives worked examples of the
#' models; it is the place for a longer teaching development, which the
#' reference pages do not attempt.
#'
#' @section Reach and exposure-distribution models:
#' Plans with one or more insertions in each of several vehicles, with random
#' duplication and accumulation: [calc_sainsbury()], [calc_binomial()].
#' One vehicle with several insertions (accumulation models):
#' [calc_beta_binomial()], [calc_hofmans_accumulation()]. Several vehicles with one insertion each:
#' [calc_agostini_duplication()], [calc_hofmans_duplication()]. Several
#' vehicles with several insertions: [calc_metheringham()], [calc_canex()],
#' [calc_csd()], [calc_msad()], [calc_cbd()] and [calc_mbd()]. Fitting and
#' validation: [fit_bbd_to_reach()], [calibrate_bbd()], [fit_nbd_exposure()],
#' [nbd_exposure_distribution()] and [evaluate_exposure_model()].
#'
#' @author Manuel J. Sánchez-Franco \email{majesus@us.es}
#' @references
#' References for individual models are listed in their help pages.
#' Development version and issue tracker: <https://github.com/majesus/mediaPlanR>.
#' @name mediaPlanR
#' @aliases mediaPlanR-package
"_PACKAGE"
