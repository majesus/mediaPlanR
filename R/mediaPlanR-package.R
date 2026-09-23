#' mediaPlanR: Reliable Cross-Media Reach and Frequency Planning
#'
#' Reproducible tools for cross-media reach, contact-frequency distributions,
#' target-audience metrics and budget-constrained reach optimization. The
#' package combines a validated media-plan data contract with the classical
#' reach and exposure-distribution models of the media-planning literature.
#'
#' @section Planning workflow:
#' [media_plan()] creates a validated plan, [plan_metrics()] and
#' [audience_metrics()] compute its metrics, [estimate_reach()] and
#' [compare_reach_models()] estimate reach and the exposure distribution, and
#' [optimize_media_plan()] allocates insertions under a budget.
#'
#' @section Reach and exposure-distribution models:
#' Plans with random duplication: [calc_sainsbury()], [calc_binomial()].
#' One vehicle with several insertions: [calc_beta_binomial()],
#' [calc_hofmans_accumulation()]. Several vehicles with one insertion each:
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
