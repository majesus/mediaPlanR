#__________________________________________________________#
# Collection of example datasets, one per model function.
#
# Each dataset is a list whose elements match EXACTLY, by name, the formal
# arguments of its corresponding function, so they can be passed directly
# with do.call() without having to build the input data by hand:
#
#   do.call(calc_canex, canex_example)
#   do.call(calc_sainsbury, sainsbury_example)
#
# The dataset for calc_binomial() is called 'binomial_plan' rather than
# 'binomial' because 'binomial' already exists in stats (the family used by
# generalized linear models); using that name would mask it once mediaPlanR
# is loaded. The remaining datasets inherited from version 0.2.0
# (sainsbury, beta_binomial, metheringham, hofmans, agostini) were renamed
# with the '_example' suffix to follow the same convention as the newer v2
# datasets and to avoid being confused with their model function's own name
# (e.g. metheringham_example vs. calc_metheringham()).
#__________________________________________________________#

#' @encoding UTF-8
#' @title Example inputs for calc_sainsbury()
#' @description List of arguments for \code{\link{calc_sainsbury}}, ready for
#' \code{do.call()}.
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle}
#'   \item{population}{Population size}
#' }
#' @examples
#' data(sainsbury_example)
#' do.call(calc_sainsbury, sainsbury_example)
"sainsbury_example"

#' @encoding UTF-8
#' @title Example inputs for calc_binomial()
#' @description List of arguments for \code{\link{calc_binomial}}, ready for
#' \code{do.call()}. Called \code{binomial_plan} rather than \code{binomial}
#' to avoid masking \code{stats::binomial} once the package is loaded.
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle}
#'   \item{population}{Population size}
#' }
#' @examples
#' data(binomial_plan)
#' do.call(calc_binomial, binomial_plan)
"binomial_plan"

#' @encoding UTF-8
#' @title Example inputs for calc_beta_binomial()
#' @description List of arguments for \code{\link{calc_beta_binomial}}, ready
#' for \code{do.call()}.
#' @format A list with the components:
#' \describe{
#'   \item{A1}{Vehicle audience after the first insertion}
#'   \item{A2}{Vehicle audience after the second insertion}
#'   \item{P}{Total population size}
#'   \item{n}{Total number of planned insertions}
#' }
#' @examples
#' data(beta_binomial_example)
#' do.call(calc_beta_binomial, beta_binomial_example)
"beta_binomial_example"

#' @encoding UTF-8
#' @title Example inputs for calc_metheringham()
#' @description List of arguments for \code{\link{calc_metheringham}}, ready
#' for \code{do.call()}.
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle}
#'   \item{insertions}{Numeric vector with the number of insertions per vehicle}
#'   \item{duplication_matrix}{Symmetric matrix with the duplication between vehicles}
#' }
#' @examples
#' data(metheringham_example)
#' do.call(calc_metheringham, metheringham_example)
"metheringham_example"

#' @encoding UTF-8
#' @title Example inputs for calc_hofmans()
#' @description List of arguments for \code{\link{calc_hofmans}}, ready for
#' \code{do.call()}.
#' @format A list with the components:
#' \describe{
#'   \item{R1}{Reach after the first insertion (0-1)}
#'   \item{R2}{Reach after the second insertion (0-1)}
#'   \item{N}{Number of insertions for which to calculate cumulative audience}
#' }
#' @examples
#' data(hofmans_example)
#' do.call(calc_hofmans, hofmans_example)
"hofmans_example"

#' @encoding UTF-8
#' @title Example inputs for calc_agostini()
#' @description List of arguments for \code{\link{calc_agostini}}, ready for
#' \code{do.call()}.
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle}
#'   \item{population}{Population size}
#'   \item{k}{Agostini's empirical duplication coefficient}
#' }
#' @examples
#' data(agostini_example)
#' do.call(calc_agostini, agostini_example)
"agostini_example"

#' @encoding UTF-8
#' @title Example inputs for calc_canex()
#' @description List of arguments for \code{\link{calc_canex}}, ready for
#' \code{do.call()}.
#' @format A list with \code{vehicles_data}, \code{duplications}, and
#' \code{population}.
#' @examples
#' data(canex_example)
#' do.call(calc_canex, canex_example)
"canex_example"

#' @encoding UTF-8
#' @title Example inputs for fitting a BBD to an external reach estimate
#' @description List of arguments for \code{\link{fit_bbd_to_reach}}, ready
#' for \code{do.call()}. Fits one Beta-Binomial distribution to external
#' reach; it is not the Morgensztern MSAD sequential model.
#' @format A list with \code{insertions}, \code{audiences}, \code{RM},
#' \code{universe}, and \code{A0}.
#' @examples
#' data(bbd_reach_example)
#' do.call(fit_bbd_to_reach, bbd_reach_example)
"bbd_reach_example"

#' @encoding UTF-8
#' @title Illustrative example inputs for the Morgensztern MSAD model
#' @description A small, self-contained two-vehicle scenario. This is original
#' illustrative data (not derived from any published source), ready for
#' `do.call(calc_msad, msad_example)`. For a dataset that instead reproduces a
#' published worked example for literature validation, see [msad_kim2005].
#' @format A list with \code{vehicles_data}, \code{duplications}, and
#' \code{aggregation_order}.
#' @examples
#' data(msad_example)
#' do.call(calc_msad, msad_example)
#' @seealso [calc_msad()], [msad_kim2005]
"msad_example"

#' @encoding UTF-8
#' @title Kim's (2005) worked inputs for the Morgensztern MSAD model
#' @description Minimal factual inputs (insertion counts, reach, and pairwise
#' duplication figures) reproduced from Kim (2005), included solely so users
#' can verify that \code{calc_msad()} reproduces the published worked example.
#' These are bare numeric values reused for validation, not a creative or
#' substantial reproduction of the dissertation. Kim's dissertation publishes
#' these as the CSD example inputs and does not itself publish a resulting
#' MSAD distribution; this dataset is therefore an input benchmark, not a
#' claim that an MSAD output appears in the thesis.
#' @format A list with \code{vehicles_data}, \code{duplications}, and
#' \code{aggregation_order}.
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 65-71
#' and 80-97.
#' @examples
#' data(msad_kim2005)
#' do.call(calc_msad, msad_kim2005)
#' @seealso [calc_msad()], [msad_example], [csd_kim2005]
"msad_kim2005"

#' @encoding UTF-8
#' @title Illustrative example inputs for the CSD model
#' @description A small, self-contained three-vehicle scenario. This is
#' original illustrative data (not derived from any published source), ready
#' for `do.call(calc_csd, csd_example)`. For a dataset that instead reproduces
#' a published worked example for literature validation, see [csd_kim2005].
#' @format A list ready for `do.call(calc_csd, csd_example)` with components:
#' \describe{
#'   \item{vehicles_data}{Three rows containing `insertions`, `R1`, and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion pair duplication.}
#'   \item{aggregation_order}{The illustrative order, `1:3`.}
#' }
#' @examples
#' data(csd_example)
#' result <- do.call(calc_csd, csd_example)
#' result$distribution
#' @seealso [calc_csd()], [csd_kim2005], [msad_example]
"csd_example"

#' @encoding UTF-8
#' @title Kim's (2005) complete worked example for the CSD model
#' @description Minimal factual inputs (insertion counts, reach, and pairwise
#' duplication figures) reproduced from Kim (2005), Tables 4.2.2.1-4.2.2.10,
#' for the three-vehicle Canonical Sequential Aggregation example, using the
#' published TD forward aggregation order. Included solely so users can verify
#' that \code{calc_csd()} reproduces the published result; exact calculations
#' retain more precision than the intermediate values rounded in the thesis.
#' These are bare numeric values reused for validation, not a creative or
#' substantial reproduction of the dissertation.
#' @format A list ready for `do.call(calc_csd, csd_kim2005)` with components:
#' \describe{
#'   \item{vehicles_data}{Three rows containing `insertions`, `R1`, and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion pair duplication.}
#'   \item{aggregation_order}{The published forward order, `1:3`.}
#' }
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 80-97.
#' @examples
#' data(csd_kim2005)
#' result <- do.call(calc_csd, csd_kim2005)
#' result$distribution
#' @seealso [calc_csd()], [csd_example], [msad_kim2005]
"csd_kim2005"

#' @encoding UTF-8
#' @title Illustrative example inputs for the MBD model
#' @description A small, self-contained two-vehicle scenario. This is original
#' illustrative data (not derived from any published source), ready for
#' `do.call(calc_mbd, mbd_example)`. Two vehicles avoid the Beta-Binomial
#' co-exposure imputation that three or more vehicles require. For a fully
#' worked three-vehicle literature-validation benchmark instead, see
#' [mbd_cheong2007].
#' @format A list ready for `do.call(calc_mbd, mbd_example)` with components:
#' \describe{
#'   \item{vehicles_data}{Two rows containing `insertions`, `R1`, and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion pair duplication.}
#'   \item{aggregation_order}{The illustrative order, `1:2`.}
#' }
#' @examples
#' data(mbd_example)
#' result <- do.call(calc_mbd, mbd_example)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_cheong2007]
"mbd_example"

#' @encoding UTF-8
#' @title Cheong's (2007) complete worked example for the MBD model
#' @description Minimal factual inputs (insertion counts, reach, and pairwise
#' duplication figures) reproduced from Cheong (2007), Chapter 4.2, for the
#' three-vehicle conceptual example: vehicle A (2 insertions), vehicle B (1
#' insertion), vehicle C (3 insertions). Included solely so users can verify
#' that \code{calc_mbd()} reproduces the published result; this is the only
#' example in Cheong's dissertation that is fully specified and internally
#' consistent without relying on the negative-probability safety net. These
#' are bare numeric values reused for validation, not a creative or
#' substantial reproduction of the dissertation.
#' @format A list ready for `do.call(calc_mbd, mbd_cheong2007)` with components:
#' \describe{
#'   \item{vehicles_data}{Three rows containing `insertions`, `R1`, and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion pair duplication.}
#'   \item{aggregation_order}{`1:3`, matching Cheong's own worked order.}
#' }
#' @references Cheong, Y. (2007). Multivariate Beta Binomial Distribution
#' Model as a Web Media Exposure Model. Doctoral dissertation, The University
#' of Texas at Austin, Ch. 4.2.
#' @examples
#' data(mbd_cheong2007)
#' result <- do.call(calc_mbd, mbd_cheong2007)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_example]
"mbd_cheong2007"
