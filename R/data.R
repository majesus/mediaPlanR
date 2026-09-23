# Example datasets, built by data-raw/datasets.R.
#
# Each dataset is a list whose elements match, by name, the formal arguments of
# the function it feeds, so it can be passed directly with do.call():
#
#   do.call(calc_canex, canex_example)
#
# Functions that share the same input shape share one dataset: ratings_example
# feeds calc_sainsbury() and calc_binomial(), and duplication_example feeds
# calc_agostini_duplication() and calc_hofmans_duplication(). No dataset is
# called "binomial": that name would mask stats::binomial once the package is
# attached.

#' Example inputs for the Sainsbury and Binomial models
#'
#' List of arguments ready for `do.call()` with [calc_sainsbury()] and
#' [calc_binomial()], the two models that assume random duplication and random
#' accumulation. Original illustrative data, not derived from any published
#' source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each of three
#'     vehicles, in people per insertion.}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(ratings_example)
#' do.call(calc_sainsbury, ratings_example)
#' do.call(calc_binomial, ratings_example)
"ratings_example"

#' Example inputs for the Beta-Binomial model
#'
#' List of arguments for [calc_beta_binomial()], ready for `do.call()`.
#' Original illustrative data, not derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{A1}{Vehicle audience after the first insertion, in people.}
#'   \item{A2}{Cumulative vehicle audience after the second insertion, in
#'     people.}
#'   \item{P}{Population size, in people.}
#'   \item{n}{Total number of planned insertions.}
#' }
#' @examples
#' data(beta_binomial_example)
#' do.call(calc_beta_binomial, beta_binomial_example)
"beta_binomial_example"

#' Example inputs for the Metheringham model
#'
#' List of arguments for [calc_metheringham()], ready for `do.call()`: three
#' vehicles with four, three and five insertions. Original illustrative data,
#' not derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle, in
#'     people per insertion.}
#'   \item{insertions}{Numeric vector with the number of insertions in each
#'     vehicle.}
#'   \item{duplication_matrix}{Symmetric matrix, in people, with the audience
#'     duplicated between vehicles (off-diagonal) and between two insertions
#'     in the same vehicle (diagonal).}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(metheringham_example)
#' do.call(calc_metheringham, metheringham_example)
"metheringham_example"

#' Example inputs for the Hofmans accumulation model
#'
#' List of arguments for [calc_hofmans_accumulation()], ready for `do.call()`.
#' One vehicle with several insertions: the "accumulation" domain. For the
#' other Hofmans model (several vehicles, one insertion each), see
#' [duplication_example]. Original illustrative data, not derived from any
#' published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{R1}{Reach after the first insertion, as a proportion.}
#'   \item{R2}{Cumulative reach after the second insertion, as a proportion.}
#'   \item{N}{Number of insertions up to which the cumulative reach is
#'     calculated.}
#' }
#' @examples
#' data(hofmans_accumulation_example)
#' do.call(calc_hofmans_accumulation, hofmans_accumulation_example)
#' @seealso [calc_hofmans_accumulation()], [duplication_example]
"hofmans_accumulation_example"

#' Example inputs for the Agostini and Hofmans duplication models
#'
#' List of arguments ready for `do.call()` with [calc_agostini_duplication()]
#' and [calc_hofmans_duplication()], the two ad hoc duplication models for
#' several vehicles with one insertion each. Original illustrative data, not
#' derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each of three
#'     vehicles, in people per insertion.}
#'   \item{population}{Population size, in people.}
#'   \item{duplication_matrix}{Symmetric matrix with the audience duplicated
#'     between every pair of vehicles, in people. The diagonal is not used.}
#' }
#' @examples
#' data(duplication_example)
#' do.call(calc_agostini_duplication, duplication_example)
#' do.call(calc_hofmans_duplication, duplication_example)
#' @seealso [calc_agostini_duplication()], [calc_hofmans_duplication()],
#'   [hofmans_accumulation_example]
"duplication_example"

#' Example inputs for the CANEX model
#'
#' List of arguments for [calc_canex()], ready for `do.call()`: two vehicles
#' with three and two insertions. Original illustrative data, not derived from
#' any published source. For inputs taken from a published worked example, see
#' [csd_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `k` (insertions), `R1` and
#'     `R2` (reach after one and two insertions, as proportions).}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications, as
#'     proportions of the population.}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(canex_example)
#' do.call(calc_canex, canex_example)
"canex_example"

#' Example inputs for fitting a Beta-Binomial to an external reach
#'
#' List of arguments for [fit_bbd_to_reach()], ready for `do.call()`. Original
#' illustrative data, not derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{insertions}{Number of insertions in each of three vehicles.}
#'   \item{audiences}{Audience of each vehicle, in people per insertion.}
#'   \item{reach}{External schedule reach, in people.}
#'   \item{universe}{Universe size, in people.}
#' }
#' @examples
#' data(bbd_reach_example)
#' do.call(fit_bbd_to_reach, bbd_reach_example)
"bbd_reach_example"

#' Illustrative example inputs for the MSAD model
#'
#' A small, self-contained two-vehicle scenario ready for
#' `do.call(calc_msad, msad_example)`. Original illustrative data, not derived
#' from any published source. For a dataset that reproduces published inputs
#' for literature validation, see [msad_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `insertions`, `R1` and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{The order of aggregation, `1:2`.}
#' }
#' @examples
#' data(msad_example)
#' do.call(calc_msad, msad_example)
#' @seealso [calc_msad()], [msad_kim2005]
"msad_example"

#' Kim's (2005) inputs for the MSAD model
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Kim (2005), included solely so that users can
#' verify calculations against the source. They are bare numeric values reused
#' for validation, not a reproduction of the dissertation. Kim publishes these
#' as the inputs of the CSD example and does not publish an MSAD distribution
#' for them; this dataset is therefore an input benchmark, and the MSAD output
#' it produces is a derived calculation, not a published result.
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `insertions`, `R1` and `R2`
#'     for three vehicles.}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{The order of aggregation, `1:3`.}
#' }
#' @references
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 65-71 and
#' 80-97.
#' @examples
#' data(msad_kim2005)
#' do.call(calc_msad, msad_kim2005)
#' @seealso [calc_msad()], [msad_example], [csd_kim2005]
"msad_kim2005"

#' Illustrative example inputs for the CSD model
#'
#' A small, self-contained three-vehicle scenario ready for
#' `do.call(calc_csd, csd_example)`. Original illustrative data, not derived
#' from any published source. For a dataset that reproduces published inputs
#' for literature validation, see [csd_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and
#'     `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{The order of aggregation, `1:3`.}
#' }
#' @examples
#' data(csd_example)
#' result <- do.call(calc_csd, csd_example)
#' result$distribution
#' @seealso [calc_csd()], [csd_kim2005], [msad_example]
"csd_example"

#' Kim's (2005) worked example for the CSD model
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Kim (2005), Tables 4.2.2.1-4.2.2.10, for the
#' three-vehicle Canonical Sequential Aggregation example with the published TD
#' forward aggregation order. They are included solely so that users can verify
#' that [calc_csd()] reproduces the published result; exact calculations keep
#' more precision than the intermediate values rounded in the dissertation.
#' They are bare numeric values reused for validation, not a reproduction of
#' the dissertation.
#'
#' @format A list ready for `do.call(calc_csd, csd_kim2005)` with the
#' components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and
#'     `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{The published forward order, `1:3`.}
#' }
#' @references
#' Kim, H. G. (2005). A Canonical Sequential Aggregation Media Model.
#' Doctoral dissertation, The University of Texas at Austin, pp. 80-97.
#' @examples
#' data(csd_kim2005)
#' result <- do.call(calc_csd, csd_kim2005)
#' result$distribution
#' @seealso [calc_csd()], [csd_example], [msad_kim2005]
"csd_kim2005"

#' Illustrative example inputs for the MBD model
#'
#' A small, self-contained two-vehicle scenario ready for
#' `do.call(calc_mbd, mbd_example)`. Original illustrative data, not derived
#' from any published source. Two vehicles avoid the Beta-Binomial imputation
#' of co-exposure that three or more vehicles require. For a three-vehicle
#' literature-validation benchmark, see [mbd_cheong2007].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Two rows with columns `insertions`, `R1` and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{The order of aggregation, `1:2`.}
#' }
#' @examples
#' data(mbd_example)
#' result <- do.call(calc_mbd, mbd_example)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_cheong2007]
"mbd_example"

#' Cheong's (2007) worked example for the MBD model
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Cheong (2007), Chapter 4.2, for the three-vehicle
#' conceptual example: vehicle A (2 insertions), vehicle B (1 insertion) and
#' vehicle C (3 insertions). They are included solely so that users can verify
#' that [calc_mbd()] reproduces the published result. This is the example of
#' Cheong's dissertation that is fully specified and internally consistent
#' without the negative-probability safety net. They are bare numeric values
#' reused for validation, not a reproduction of the dissertation.
#'
#' @format A list ready for `do.call(calc_mbd, mbd_cheong2007)` with the
#' components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and `R2`
#'     (`R2` is `NA` for the single-insertion vehicle).}
#'   \item{duplications}{Symmetric matrix of one-insertion duplications.}
#'   \item{aggregation_order}{`1:3`, Cheong's own worked order.}
#' }
#' @references
#' Cheong, Y. (2007). Multivariate Beta Binomial Distribution Model as a Web
#' Media Exposure Model. Doctoral dissertation, The University of Texas at
#' Austin, Ch. 4.2.
#' @examples
#' data(mbd_cheong2007)
#' result <- do.call(calc_mbd, mbd_cheong2007)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_example]
"mbd_cheong2007"
