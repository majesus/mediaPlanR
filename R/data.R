#__________________________________________________________#
# Coleccion de datasets de ejemplo, uno por funcion modelo.
#
# Cada dataset es una lista cuyos elementos coinciden EXACTAMENTE, en
# nombre, con los argumentos formales de su funcion correspondiente, de
# modo que se pueden pasar directamente con do.call() sin tener que
# construir a mano los datos de entrada:
#
#   do.call(calc_canex, canex_example)
#   do.call(calc_sainsbury, sainsbury_example)
#
# El dataset de calc_binomial() se llama 'binomial_plan' y no 'binomial'
# porque 'binomial' ya existe en stats (la familia de calc_binomial()
# para modelos lineales generalizados); usar ese nombre lo enmascararia
# tras cargar mediaPlanR. El resto de datasets heredados de la version 0.2.0
# (sainsbury, beta_binomial, metheringham, hofmans, agostini) se renombraron
# con el sufijo '_example' para seguir la misma convencion que los datasets
# nuevos de v2 y no confundirse con el nombre de su funcion modelo (p. ej.
# metheringham_example vs. calc_metheringham()).
#__________________________________________________________#

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_sainsbury()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_sainsbury}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamano de la poblacion}
#' }
#' @examples
#' data(sainsbury_example)
#' do.call(calc_sainsbury, sainsbury_example)
"sainsbury_example"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_binomial()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_binomial}}, lista para usar con \code{do.call()}.
#' Se llama \code{binomial_plan} y no \code{binomial} para no enmascarar
#' \code{stats::binomial} tras cargar el paquete.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamano de la poblacion}
#' }
#' @examples
#' data(binomial_plan)
#' do.call(calc_binomial, binomial_plan)
"binomial_plan"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_beta_binomial()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_beta_binomial}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{A1}{Audiencia del soporte tras la primera insercion}
#'   \item{A2}{Audiencia del soporte tras la segunda insercion}
#'   \item{P}{Tamano total de la poblacion}
#'   \item{n}{Numero total de inserciones planificadas}
#' }
#' @examples
#' data(beta_binomial_example)
#' do.call(calc_beta_binomial, beta_binomial_example)
"beta_binomial_example"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_metheringham()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_metheringham}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numerico con el numero de inserciones por soporte}
#'   \item{matriz_duplicacion}{Matriz simetrica con la duplicacion entre soportes}
#' }
#' @examples
#' data(metheringham_example)
#' do.call(calc_metheringham, metheringham_example)
"metheringham_example"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_hofmans()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_hofmans}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{R1}{Cobertura tras la primera insercion (0-1)}
#'   \item{R2}{Cobertura tras la segunda insercion (0-1)}
#'   \item{N}{Numero de inserciones para las que calcular la audiencia acumulada}
#' }
#' @examples
#' data(hofmans_example)
#' do.call(calc_hofmans, hofmans_example)
"hofmans_example"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_agostini()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_agostini}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamano de la poblacion}
#'   \item{k}{Coeficiente empirico de duplicacion de Agostini}
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
#' \code{poblacion}.
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
