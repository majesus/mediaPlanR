#' @encoding UTF-8
#' @title Datos ilustrativos de soportes de prensa
#' @description Conjunto de datos ficticio con audiencias, tarifas e índices
#' de utilidad de 60 soportes de prensa española, empleado en los ejemplos
#' de \code{\link{calc_sainsbury}}, \code{\link{calc_binomial}},
#' \code{\link{optimize_media_sb}} y \code{\link{calcular_metricas_medios}}.
#'
#' @format Un data frame con 60 filas y 7 variables:
#' \describe{
#'   \item{soportes}{Carácter. Nombre del soporte}
#'   \item{soportes_.}{Numérico. Coeficiente de soporte (uso interno docente)}
#'   \item{audiencias}{Entero. Audiencia del soporte (personas)}
#'   \item{tarifas}{Numérico. Tarifa de una inserción en el soporte}
#'   \item{indices_utilidad}{Numérico. Índice de utilidad de la audiencia (0-1 aprox.)}
#'   \item{inserciones}{Entero. Número de inserciones consideradas}
#'   \item{duplicacion}{Entero. Audiencia duplicada estimada frente al resto de soportes (personas)}
#' }
#'
#' @note Los datos son ficticios y se emplean únicamente con fines docentes e
#' ilustrativos; no representan cifras reales de audiencia o tarifas.
#'
#' @source Elaboración propia con fines docentes.
#'
#' @examples
#' data(datos_medios)
#' head(datos_medios)
#'
"datos_medios"

#__________________________________________________________#
# Colección de datasets de ejemplo, uno por función modelo.
#
# Cada dataset es una lista cuyos elementos coinciden EXACTAMENTE, en
# nombre, con los argumentos formales de su función correspondiente, de
# modo que se pueden pasar directamente con do.call() sin tener que
# construir a mano los datos de entrada:
#
#   do.call(calc_canex, canex)
#   do.call(calc_sainsbury, sainsbury)
#
# El dataset de calc_binomial() se llama 'binomial_plan' y no 'binomial'
# porque 'binomial' ya existe en stats (la familia de calc_binomial()
# para modelos lineales generalizados); usar ese nombre lo enmascararía
# tras cargar mediaPlanR.
#__________________________________________________________#

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_sainsbury()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_sainsbury}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamaño de la población}
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
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamaño de la población}
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
#'   \item{A1}{Audiencia del soporte tras la primera inserción}
#'   \item{A2}{Audiencia del soporte tras la segunda inserción}
#'   \item{P}{Tamaño total de la población}
#'   \item{n}{Número total de inserciones planificadas}
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
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numérico con el número de inserciones por soporte}
#'   \item{matriz_duplicacion}{Matriz simétrica con la duplicación entre soportes}
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
#'   \item{R1}{Cobertura tras la primera inserción (0-1)}
#'   \item{R2}{Cobertura tras la segunda inserción (0-1)}
#'   \item{N}{Número de inserciones para las que calcular la audiencia acumulada}
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
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{pob_total}{Tamaño de la población}
#'   \item{k}{Coeficiente empírico de duplicación de Agostini}
#' }
#' @examples
#' data(agostini)
#' do.call(calc_agostini, agostini)
"agostini"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_MBBD()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_MBBD}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{insertions}{Vector numérico. Número de inserciones para cada soporte}
#'   \item{audiences}{Vector numérico. Audiencia de cada soporte en personas}
#'   \item{RM}{Entero. Estimación de cobertura según Morgensztern en personas}
#'   \item{universe}{Entero. Tamaño del universo objetivo en personas}
#'   \item{A0}{Numérico. Valor inicial del parámetro A}
#' }
#' @examples
#' data(MBBD)
#' do.call(calc_MBBD, MBBD)
"MBBD"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_canex()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_canex}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{vehicles_data}{Data frame con columnas k, R1 y R2 por vehículo}
#'   \item{duplications}{Matriz cuadrada de duplicaciones brutas entre vehículos}
#'   \item{poblacion}{Tamaño de la población objetivo}
#' }
#' @examples
#' data(canex)
#' do.call(calc_canex, canex)
"canex"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_nbd()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_nbd}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numérico con el número de inserciones por soporte}
#'   \item{pob_total}{Tamaño de la población}
#'   \item{k}{Parámetro de heterogeneidad (forma de la distribución Gamma)}
#' }
#' @examples
#' data(nbd)
#' do.call(calc_nbd, nbd)
"nbd"

#' @encoding UTF-8
#' @title Datos de ejemplo para calc_grps()
#' @description Lista con los argumentos de ejemplo para
#' \code{\link{calc_grps}}, lista para usar con \code{do.call()}.
#' @format Una lista con los componentes:
#' \describe{
#'   \item{audiencias}{Vector numérico con las audiencias de cada soporte}
#'   \item{inserciones}{Vector numérico del número de inserciones por soporte}
#'   \item{pob_total}{Tamaño de la población}
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
#'   \item{precios}{Vector numérico con precios de cada inserción}
#'   \item{audiencias}{Vector numérico con audiencias de cada soporte}
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
#'   \item{audiencia_efectiva}{Número total de personas alcanzadas por la campaña}
#'   \item{precio_unidad}{Precio de venta por unidad}
#'   \item{margen_unidad}{Beneficio neto por unidad vendida}
#'   \item{inversion}{Inversión total en publicidad}
#' }
#' @examples
#' data(roas)
#' do.call(calcular_roas, roas)
"roas"
