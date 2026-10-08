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

#' Illustrative inputs for the Sainsbury and Binomial models
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

#' Illustrative inputs for the Beta-Binomial model
#'
#' List of arguments ready for `do.call()` with [calc_beta_binomial()]. Original
#' illustrative data, not derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{R1}{Reach after the first insertion, as a proportion.}
#'   \item{R2}{Cumulative reach after the second insertion, as a proportion.}
#'   \item{insertions}{Total number of planned insertions.}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(beta_binomial_example)
#' do.call(calc_beta_binomial, beta_binomial_example)
"beta_binomial_example"

#' Illustrative inputs for the Metheringham model
#'
#' List of arguments ready for `do.call()` with [calc_metheringham()]: three
#' vehicles with four, three and five insertions. Original illustrative data,
#' not derived from any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{audiences}{Numeric vector with the audience of each vehicle, in
#'     people per insertion.}
#'   \item{insertions}{Numeric vector with the number of insertions in each
#'     vehicle.}
#'   \item{duplication_matrix}{Symmetric matrix, in people: the people exposed
#'     to both one insertion in vehicle `i` and one in vehicle `j`
#'     (off-diagonal), and the people exposed to two insertions of the same
#'     vehicle (diagonal, as defined in [calc_metheringham()]).}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(metheringham_example)
#' do.call(calc_metheringham, metheringham_example)
"metheringham_example"

#' Illustrative inputs for the Hofmans accumulation model
#'
#' List of arguments ready for `do.call()` with [calc_hofmans_accumulation()].
#' One vehicle with several insertions: the "accumulation" domain. For the
#' other Hofmans model (several vehicles, one insertion each), see
#' [duplication_example]. Original illustrative data, not derived from any
#' published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{R1}{Reach after the first insertion, as a proportion.}
#'   \item{R2}{Cumulative reach after the second insertion, as a proportion.}
#'   \item{insertions}{Number of insertions up to which the cumulative reach
#'     is calculated.}
#' }
#' @examples
#' data(hofmans_accumulation_example)
#' do.call(calc_hofmans_accumulation, hofmans_accumulation_example)
#' @seealso [calc_hofmans_accumulation()], [duplication_example]
"hofmans_accumulation_example"

#' Illustrative inputs for the Agostini and Hofmans duplication models
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
#'   \item{duplication_matrix}{Symmetric matrix whose element `[i, j]` is the
#'     number of people in the audience of both vehicle `i` and vehicle `j`, in
#'     people. The diagonal is not used.}
#' }
#' @examples
#' data(duplication_example)
#' do.call(calc_agostini_duplication, duplication_example)
#' do.call(calc_hofmans_duplication, duplication_example)
#' @seealso [calc_agostini_duplication()], [calc_hofmans_duplication()],
#'   [hofmans_accumulation_example]
"duplication_example"

#' Illustrative inputs for the CANEX model
#'
#' List of arguments ready for `do.call()` with [calc_canex()]: two vehicles
#' with three and two insertions. Original illustrative data, not derived from
#' any published source. For inputs taken from a published worked example, see
#' [csd_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `insertions`, `R1` and
#'     `R2` (reach after one and two insertions, as proportions between 0
#'     and 1).}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(canex_example)
#' do.call(calc_canex, canex_example)
"canex_example"

#' Illustrative inputs for fitting a Beta-Binomial to an external reach
#'
#' List of arguments ready for `do.call()` with [fit_bbd_to_reach()]: three
#' vehicles and an external reach. Original illustrative data, not derived from
#' any published source.
#'
#' @format A list with the components:
#' \describe{
#'   \item{insertions}{Number of insertions in each vehicle (three vehicles in
#'     this example; [fit_bbd_to_reach()] accepts any number).}
#'   \item{audiences}{Audience of each vehicle, in people per insertion (one
#'     value per vehicle).}
#'   \item{reach}{External schedule reach, in people.}
#'   \item{population}{Population size, in people.}
#' }
#' @examples
#' data(bbd_reach_example)
#' do.call(fit_bbd_to_reach, bbd_reach_example)
"bbd_reach_example"

#' Illustrative inputs for the MSAD model
#'
#' List of arguments ready for `do.call(calc_msad, msad_example)`: a small
#' two-vehicle scenario. Original illustrative data, not derived from any
#' published source. For a dataset that reproduces published inputs
#' for literature validation, see [msad_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `insertions`, `R1` and `R2`.}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
#'   \item{aggregation_order}{The order of aggregation, `1:2`.}
#' }
#' @examples
#' data(msad_example)
#' do.call(calc_msad, msad_example)
#' @seealso [calc_msad()], [msad_kim2005]
"msad_example"

#' Published inputs for the MSAD model (Kim, 2005)
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Kim (2005), included solely so that users can
#' verify calculations against the source. They are bare numeric values reused
#' for validation, not a reproduction of the dissertation. Kim publishes these
#' as the inputs of the CSD example and does not publish an MSAD distribution
#' for them; this dataset is therefore an input benchmark, and the MSAD output
#' it produces is a derived calculation, not a published result. The audiences
#' and reaches originate from comScore Media Metrix data of September 2003,
#' which Kim (2005) uses and Cheong, Leckenby and Eakin (2011, Table 5) also
#' publish.
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Data frame with columns `insertions`, `R1` and `R2`
#'     for three vehicles.}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
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

#' Illustrative inputs for the CSD model
#'
#' List of arguments ready for `do.call(calc_csd, csd_example)`: a small
#' three-vehicle scenario. Original illustrative data, not derived from any
#' published source. For a dataset that reproduces published inputs
#' for literature validation, see [csd_kim2005].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and
#'     `R2`.}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
#'   \item{aggregation_order}{The order of aggregation, `1:3`.}
#' }
#' @examples
#' data(csd_example)
#' result <- do.call(calc_csd, csd_example)
#' result$distribution
#' @seealso [calc_csd()], [csd_kim2005], [msad_example]
"csd_example"

#' Published inputs for the CSD model (Kim, 2005)
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Kim (2005), Tables 4.2.2.1-4.2.2.10, for the
#' three-vehicle Canonical Sequential Aggregation example with the aggregation
#' order of the worked example (labeled "TD forward" by Kim). They are included solely so that users can verify
#' that [calc_csd()] reproduces the published result; exact calculations keep
#' more precision than the intermediate values rounded in the dissertation.
#' They are bare numeric values reused for validation, not a reproduction of
#' the dissertation. The audiences and reaches originate from comScore Media
#' Metrix data of September 2003, which Kim (2005) uses and Cheong, Leckenby
#' and Eakin (2011, Table 5) also publish.
#'
#' @format A list ready for `do.call(calc_csd, csd_kim2005)` with the
#' components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and
#'     `R2`.}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
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

#' Illustrative inputs for the MBD model
#'
#' List of arguments ready for `do.call(calc_mbd, mbd_example)`: a small
#' two-vehicle scenario. Original illustrative data, not derived from any
#' published source. Two vehicles avoid the Beta-Binomial imputation
#' of co-exposure that three or more vehicles require. For a three-vehicle
#' literature-validation benchmark, see [mbd_cheong2007].
#'
#' @format A list with the components:
#' \describe{
#'   \item{vehicles_data}{Two rows with columns `insertions`, `R1` and `R2`.}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
#'   \item{aggregation_order}{The order of aggregation, `1:2`.}
#' }
#' @examples
#' data(mbd_example)
#' result <- do.call(calc_mbd, mbd_example)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_cheong2007]
"mbd_example"

#' Published inputs for the MBD model (Cheong, 2007)
#'
#' Minimal factual inputs (insertion counts, reach and pairwise duplication
#' figures) reproduced from Cheong (2007), Chapter 4.2, for the three-vehicle
#' conceptual example: vehicle A (2 insertions), vehicle B (1 insertion) and
#' vehicle C (3 insertions). They are included solely so that users can verify
#' that [calc_mbd()] reproduces the published result up to the differences
#' described in [calc_mbd()] (the printed intermediate tables contain
#' arithmetic inconsistencies, so the published distribution is matched only
#' within 0.002 per cell). This is the example of Cheong's dissertation that is
#' fully specified and that the implementation computes without the
#' negative-probability safety net. They are bare numeric values
#' reused for validation, not a reproduction of the dissertation. The figures
#' originate from Simmons Market Research Bureau (SMRB) magazine audience data
#' of 1979; the same example appears in Kim (1994, pp. 125-139) and in Cheong,
#' Leckenby and Eakin (2011, Tables 2 to 4).
#'
#' @format A list ready for `do.call(calc_mbd, mbd_cheong2007)` with the
#' components:
#' \describe{
#'   \item{vehicles_data}{Three rows with columns `insertions`, `R1` and `R2`
#'     (`R2` is `NA` for the single-insertion vehicle).}
#'   \item{duplications}{Symmetric matrix of proportions of the population
#'     exposed to both vehicles of each pair (one insertion in each).}
#'   \item{aggregation_order}{`1:3`, Cheong's own worked order.}
#' }
#' @references
#' Cheong, Y. (2007). Multivariate Beta Binomial Distribution Model as a Web
#' Media Exposure Model. Doctoral dissertation, The University of Texas at
#' Austin, Ch. 4.2.
#'
#' Cheong, Y., Leckenby, J. D., & Eakin, T. (2011). Evaluating the
#' multivariate beta binomial distribution for estimating magazine and
#' Internet exposure frequency distributions. Journal of Advertising, 40(1),
#' 7-23. <https://doi.org/10.2753/JOA0091-3367400101>
#' @examples
#' data(mbd_cheong2007)
#' result <- do.call(calc_mbd, mbd_cheong2007)
#' result$distribution
#' @seealso [calc_mbd()], [mbd_example]
"mbd_cheong2007"
