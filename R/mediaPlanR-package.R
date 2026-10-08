#' mediaPlanR: Reliable Cross-Media Reach and Frequency Planning
#'
#' Reproducible tools for cross-media reach, contact-frequency distributions,
#' target-audience metrics and budget-constrained reach optimization. The
#' package combines a validated media-plan data contract with the classical
#' reach and exposure-distribution models of the media-planning literature.
#'
#' @section Scope of the validation:
#' The models can be applied to any mix of media, but the published evidence on
#' their accuracy comes mainly from magazine and Internet data. Their accuracy
#' for a particular combination of media and population should be checked
#' against observed data, for example with [evaluate_exposure_model()].
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
#'     everywhere. In [audience_metrics()], `gross_universe` is the whole
#'     population and `target_universe` the people who belong to the target.
#'   \item *Audience* (people exposed by one insertion of a vehicle): `audience`
#'     in [media_plan()] and `audiences` (one value per vehicle) in the
#'     `calc_*()` functions, always in people. As a proportion of the
#'     population it is `R1`.
#'   \item *Reach after the first and second insertion of the same vehicle*:
#'     `R1` and `R2` (Kim, 2005, and the sources of each model), always as
#'     proportions of the population, in [calc_beta_binomial()],
#'     [calc_hofmans_accumulation()], [calibrate_bbd()], [calc_canex()] and the
#'     sequential models. `R2` is cumulative: the people exposed at least once
#'     in two insertions. If the audience is known in people, divide it by the
#'     population.
#'   \item *Number of insertions*: `insertions` (one value per vehicle) in all
#'     functions; the formulas use \eqn{n_i} for a vehicle and \eqn{N} for the
#'     total. The column of `vehicles_data` in [calc_canex()] may also be named
#'     `k`, Danaher's notation. The argument `k` of
#'     [calc_agostini_duplication()] is something else: Agostini's duplication
#'     coefficient.
#'   \item *Duplication between two vehicles*: `duplication_matrix`, in people
#'     ([calc_agostini_duplication()], [calc_hofmans_duplication()],
#'     [calc_metheringham()]); `duplications`, as proportions of the
#'     population ([calc_canex()] and the sequential models). The different
#'     names signal the different units.
#'   \item *Reach* (also net audience) counts each person once. *Contacts*
#'     (also impressions, or gross audience in the duplication models) count a
#'     person once per insertion: with several insertions they are the sum of
#'     each audience times its insertions, \eqn{I = \sum n_i A_i}; with one
#'     insertion per vehicle they are the sum of the audiences, the *gross
#'     audience* of [calc_agostini_duplication()] and [calc_hofmans_duplication()].
#'     The `gross_audience` argument of [audience_metrics()] is something else:
#'     the total audience of a channel, whatever the profile of its members.
#'     *Duplications* between vehicles are coincidences between pairs, so a
#'     person present in three vehicles contributes three pairs, not three
#'     people. Write the unit (people, contacts, pairs or proportion) before
#'     forming a ratio. With these symbols, the average frequency is
#'     \eqn{F = I / R} (contacts per person reached, defined only if \eqn{R > 0})
#'     and, per person of the universe, the mean number of contacts is
#'     \eqn{\mu = I / P}, so that \eqn{GRP = 100 \mu}.
#'   \item *Effective frequency*: `effective_frequency` in
#'     [optimize_media_plan()] and `frequency` in [calibrate_bbd()]; effective
#'     reach is the proportion exposed that many times or more.
#'   \item *Units*: every argument documents its unit. A *proportion* is a
#'     number between 0 and 1 (0.35 for 35%, not 35); percentages appear in
#'     result columns named `percent` (and `reach_percent`); counts of people
#'     appear in arguments described as "in people" and in result columns named
#'     `people`. There are three exceptions, and they are listed in the help
#'     page of each function: `zero_contact_probability` in
#'     [calc_beta_binomial()] is a percentage (0 to 100) despite its name;
#'     `affinity_index` in [audience_metrics()] has base 100 and can exceed 100;
#'     and the rating points of [plan_metrics()] (`grps`, `rating_points`) are
#'     points per 100 people of the universe, which can also exceed 100 when
#'     contacts exceed the population.
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
#'     homogeneous; [calc_sainsbury()] and the multivariate models keep each
#'     vehicle's audience.
#'   \item *Duplication between vehicles*: random (people choose vehicles
#'     independently, so the audience shared by two vehicles is the product of
#'     their audiences) or observed (taken from data). [calc_binomial()] and
#'     [calc_sainsbury()] assume random duplication; the other models with
#'     several vehicles use observed duplication.
#'   \item *Accumulation in the same vehicle*: random (a repeat insertion in
#'     a vehicle is as independent as an insertion in another vehicle) or
#'     observed through the reach after two insertions (`R2`), which captures
#'     that those who saw the first insertion tend to see the next.
#'     [calc_binomial()] and [calc_sainsbury()] assume random accumulation; the
#'     models for several insertions in a vehicle use the observed
#'     accumulation: `R2` or, in [calc_metheringham()], the audience
#'     duplicated between two insertions of the same vehicle.
#' }
#' All the models also assume that exposure probabilities are stationary
#' (they do not change over the period of the plan). Each help page states the
#' assumptions of its model.
#'
#' @section Planning workflow:
#' [media_plan()] creates a validated plan, [plan_metrics()] and
#' [audience_metrics()] compute its metrics, [estimate_reach()] and
#' [compare_reach_models()] estimate reach and the exposure distribution, and
#' [optimize_media_plan()] allocates insertions under a budget.
#'
#' @section Suggested study path:
#' The help pages of a reference manual are in alphabetical order, which
#' assumes prior knowledge. For sequential study, read in this order: (1) the
#' sections "Notation and units" and "Assumptions of the models" above, to fix
#' what is counted (people, contacts, pairs) and what each model assumes; (2) the
#' models of independence, [calc_sainsbury()] and [calc_binomial()], and
#' [estimate_reach()]; (3) the ad hoc duplication formulas,
#' [calc_agostini_duplication()] and [calc_hofmans_duplication()]; (4) the
#' accumulation of one vehicle, [calc_beta_binomial()] and
#' [calc_hofmans_accumulation()], with [fit_bbd_to_reach()] and
#' [calibrate_bbd()]; (5) the multivariate models, [calc_canex()], [calc_csd()],
#' [calc_msad()], [calc_cbd()] and [calc_mbd()], and the section "Domain of
#' validity" of [calc_canex()]; (6) the count approximation
#' [nbd_exposure_distribution()]; (7) evaluation, [evaluate_exposure_model()];
#' and (8) optimization, [optimize_media_plan()].
#'
#' The vignette `vignette("mediaPlanR-intro")` gives worked examples of the
#' planning workflow and of the models; it is the place for a longer teaching
#' development, which the reference pages do not attempt.
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
#' The models follow the primary sources cited in each function's help page.
#' Development version and issue tracker: <https://github.com/majesus/mediaPlanR>.
#' @name mediaPlanR
#' @aliases mediaPlanR-package
"_PACKAGE"
