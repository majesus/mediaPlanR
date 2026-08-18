#' @encoding UTF-8
#' @title Datos ilustrativos de soportes de prensa
#' @description Conjunto de datos ficticio con audiencias, tarifas e indices
#' de utilidad de 60 soportes de prensa espanola, empleado en los ejemplos
#' de \code{\link{calc_sainsbury}}, \code{\link{calc_binomial}},
#' \code{\link{optimize_media_sb}} y \code{\link{calcular_metricas_medios}}.
#'
#' @format Un data frame con 60 filas y 7 variables:
#' \describe{
#'   \item{soportes}{Caracter. Nombre del soporte}
#'   \item{soportes_.}{Numerico. Coeficiente de soporte (uso interno docente)}
#'   \item{audiencias}{Entero. Audiencia del soporte (personas)}
#'   \item{tarifas}{Numerico. Tarifa de una insercion en el soporte}
#'   \item{indices_utilidad}{Numerico. Indice de utilidad de la audiencia (0-1 aprox.)}
#'   \item{inserciones}{Entero. Numero de inserciones consideradas}
#'   \item{duplicacion}{Entero. Audiencia duplicada estimada frente al resto de soportes (personas)}
#' }
#'
#' @note Los datos son ficticios y se emplean unicamente con fines docentes e
#' ilustrativos; no representan cifras reales de audiencia o tarifas.
#'
#' @source Elaboracion propia con fines docentes.
#'
#' @examples
#' data(datos_medios)
#' head(datos_medios)
#'
"datos_medios"

#__________________________________________________________#
# Coleccion de datasets de ejemplo, uno por funcion modelo.
#
# Cada dataset es una lista cuyos elementos coinciden EXACTAMENTE, en
# nombre, con los argumentos formales de su funcion correspondiente, de
# modo que se pueden pasar directamente con do.call() sin tener que
# construir a mano los datos de entrada:
#
#   do.call(calc_canex, canex)
#   do.call(calc_sainsbury, sainsbury)
#
# El dataset de calc_binomial() se llama 'binomial_plan' y no 'binomial'
# porque 'binomial' ya existe en stats (la familia de calc_binomial()
# para modelos lineales generalizados); usar ese nombre lo enmascararia
# tras cargar mediaPlanR.
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
#' data(sainsbury)
#' do.call(calc_sainsbury, sainsbury)
"sainsbury"

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
#' data(beta_binomial)
#' do.call(calc_beta_binomial, beta_binomial)
"beta_binomial"

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
#' data(metheringham)
#' do.call(calc_metheringham, metheringham)
"metheringham"

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
#' data(hofmans)
#' do.call(calc_hofmans, hofmans)
"hofmans"

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
#' data(agostini)
#' do.call(calc_agostini, agostini)
"agostini"

#' @encoding UTF-8
#' @title Historical MBBD-named example data
#' @description Legacy name for the example now available as
#' \code{mbbd_example}. It fits one BBD to external reach and is not an MSAD
#' example. New code should use \code{mbbd_example} with
#' \code{\link{fit_bbd_to_reach}}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{insertions}{Vector numerico. Numero de inserciones para cada soporte}
#'   \item{audiences}{Vector numerico. Audiencia de cada soporte en personas}
#'   \item{RM}{Entero. Estimacion de cobertura segun Morgensztern en personas}
#'   \item{universe}{Entero. Tamano del universo objetivo en personas}
#'   \item{A0}{Numerico. Valor inicial del parametro A}
#' }
#' @examples
#' data(MBBD)
#' do.call(calc_MBBD, MBBD)
"MBBD"

#' @encoding UTF-8
#' @title Historical CANEX example data name
#' @description Legacy name for \code{canex_example}. New code should prefer
#' the descriptive name.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{vehicles_data}{Data frame con columnas k, R1 y R2 por vehiculo}
#'   \item{duplications}{Matriz cuadrada de duplicaciones brutas entre vehiculos}
#'   \item{poblacion}{Tamano de la poblacion objetivo}
#' }
#' @examples
#' data(canex)
#' do.call(calc_canex, canex)
"canex"

#' @encoding UTF-8
#' @title Historical NBD example data name
#' @description Legacy name for \code{nbd_example}. New statistical work
#' should normally use \code{\link{fit_nbd_exposure}} with observed counts.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numerico con el numero de inserciones por soporte}
#'   \item{pob_total}{Tamano de la poblacion}
#'   \item{k}{Parametro de heterogeneidad (forma de la distribucion Gamma)}
#' }
#' @examples
#' data(nbd)
#' do.call(calc_nbd, nbd)
"nbd"

#' @encoding UTF-8
#' @title Example inputs for calc_canex()
#' @description Descriptively named replacement for the historical
#' \code{canex} example object. The list is ready for \code{do.call()}.
#' @format A list with \code{vehicles_data}, \code{duplications}, and
#' \code{poblacion}.
#' @examples
#' data(canex_example)
#' do.call(calc_canex, canex_example)
"canex_example"

#' @encoding UTF-8
#' @title Example inputs for the experimental NBD plan wrapper
#' @description Descriptively named replacement for the historical \code{nbd}
#' example object. For new analyses prefer \code{fit_nbd_exposure()} with
#' observed counts or \code{nbd_exposure_distribution()} with explicit
#' count-process parameters.
#' @format A list with \code{audiencias}, \code{inserciones},
#' \code{pob_total}, and \code{k}.
#' @examples
#' data(nbd_example)
#' do.call(calc_nbd, nbd_example)
"nbd_example"

#' @encoding UTF-8
#' @title Legacy example for fitting a BBD to external reach
#' @description Descriptively named replacement for the historical
#' \code{MBBD} object. Despite its legacy name, this example exercises
#' \code{fit_bbd_to_reach()}, not the MSAD sequential model.
#' @format A list with \code{insertions}, \code{audiences}, \code{RM},
#' \code{universe}, and \code{A0}.
#' @examples
#' data(mbbd_example)
#' do.call(fit_bbd_to_reach, mbbd_example)
"mbbd_example"

#' @encoding UTF-8
#' @title Derived example inputs for the Morgensztern MSAD model
#' @description The three-vehicle inputs published in Kim's worked CSD example,
#' reused to illustrate MSAD with the same TD forward order. Kim does not
#' publish the resulting MSAD distribution; this dataset is therefore an input
#' benchmark, not a claim that the MSAD output appears in the thesis.
#' @format A list with \code{vehicles_data}, \code{duplications}, and
#' \code{aggregation_order}.
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 65-71
#' and 80-97.
#' @examples
#' data(msad_example)
#' do.call(calc_msad, msad_example)
"msad_example"

#' @title Kim's complete worked example for the CSD model
#' @description Inputs from Kim (2005), Tables 4.2.2.1-4.2.2.10, for the
#' three-vehicle Canonical Sequential Aggregation example using the published
#' TD forward aggregation order. Exact calculations retain more precision than
#' the intermediate values rounded in the thesis.
#' @format A list ready for `do.call(calc_csd, csd_example)` with components:
#' \describe{
#'   \item{vehicles_data}{Three rows containing `insertions`, `R1`, and `R2`.}
#'   \item{duplications}{Symmetric matrix of one-insertion pair duplication.}
#'   \item{aggregation_order}{The published forward order, `1:3`.}
#' }
#' @references Kim, H. G. (2005). A Canonical Sequential Aggregation Media
#' Model. Doctoral dissertation, The University of Texas at Austin, pp. 80-97.
#' @examples
#' data(csd_example)
#' result <- do.call(calc_csd, csd_example)
#' result$distribution
#' @seealso [calc_csd()], [msad_example]
"csd_example"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_grps()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_grps}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numerico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numerico del numero de inserciones por soporte}
#'   \item{pob_total}{Tamano de la poblacion}
#' }
#' @examples
#' data(grps)
#' do.call(calc_grps, grps)
"grps"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_cpm()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_cpm}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{precios}{Vector numerico con precios de cada insercion}
#'   \item{audiencias}{Vector numerico con audiencias de cada soporte}
#' }
#' @examples
#' data(cpm)
#' do.call(calc_cpm, cpm)
"cpm"

#' @encoding UTF-8
#' @title Datos de ejemplo para calcular_roas()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calcular_roas}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencia_efectiva}{Numero total de personas alcanzadas por la campana}
#'   \item{precio_unidad}{Precio de venta por unidad}
#'   \item{margen_unidad}{Beneficio neto por unidad vendida}
#'   \item{inversion}{Inversion total en publicidad}
#' }
#' @examples
#' data(roas)
#' do.call(calcular_roas, roas)
"roas"
