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
#'   \item *Reach* (also net audience) counts each person once;
#'     *gross audience* (also duplicated audience) is the sum of the audiences
#'     and counts a person once per audience, as in impressions.
#'   \item *Effective frequency*: `effective_frequency` in
#'     [optimize_media_plan()] and `frequency` in [calibrate_bbd()]; effective
#'     reach is the proportion exposed that many times or more.
#'   \item *Units*: every argument documents its unit. A *proportion* is a
#'     number between 0 and 1 (0.35 for 35%, not 35); percentages appear only in
#'     result columns named `percent`; counts of people appear in arguments
#'     described as "in people" and in result columns named `people`.
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
