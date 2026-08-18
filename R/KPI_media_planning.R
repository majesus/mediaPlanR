#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de GRPs mediante la cobertura y frecuencia media, o el calculo de las impresiones totales
#' @description Calcula los Gross Rating Points (GRPs) de un plan de medios
#' utilizando dos metodos diferentes: mediante impresiones totales o mediante
#' cobertura y frecuencia media. Los GRP (Gross Rating Points) son una metrica
#' publicitaria que indica el impacto total de una campana sobre una audiencia determinada,
#' expresando la suma del alcance por la frecuencia de exposicion.
#' Se calculan dividiendo el numero total de impresiones (contactos o veces
#' que el anuncio fue visto) por la poblacion relevante, multiplicado por 100,
#' lo cual permite expresar la exposicion acumulativa de la campana como un porcentaje.
#'
#' @param audiencias Vector numerico con las audiencias de cada soporte
#' @param inserciones Vector numerico del numero de inserciones por soporte
#' @param pob_total Tamano de la poblacion
#' @param cobertura Opcional. Cobertura en porcentaje (si se conoce)
#' @param metodo Character. Metodo de calculo: "impresiones" o "cobertura" (default: "impresiones")
#'
#' @details
#' El calculo se puede realizar mediante dos metodos:
#' \enumerate{
#'   \item Metodo por impresiones:
#'     \itemize{
#'       \item Calcula impresiones totales: SUMATORIO(Audiencia_i ? Inserciones_i)
#'       \item GRPs = (Impresiones / Poblacion) ? 100
#'     }
#'   \item Metodo por cobertura:
#'     \itemize{
#'       \item Frecuencia media = Impresiones totales / (Cobertura ? Poblacion)
#'       \item GRPs = Cobertura ? Frecuencia media
#'     }
#' }
#'
#' @return Una lista conteniendo:
#' \itemize{
#'   \item grps: Valor de GRPs calculado
#'   \item impresiones_totales: Suma total de impresiones
#'   \item frecuencia_media: Frecuencia media (si aplica)
#'   \item metodo: Metodo utilizado para el calculo
#' }
#'
#' @note
#' Los GRPs son una medida de presion publicitaria que:
#' \itemize{
#'   \item Pueden superar el 100%
#'   \item Indican el numero de impactos por cada 100 personas de la poblacion
#'   \item Son utiles para comparar campanas de publicidad
#'   \item Su debilidad reside en que campanas con diferentes valores de cobertura % y frecuencia media pueden arrojar un mismo nivel de GRPs
#' }
#'
#' @examples
#' # Calculo por metodo de impresiones
#' grps1 <- calc_grps(
#'   audiencias = c(300000, 400000, 200000),
#'   inserciones = c(3, 2, 4),
#'   pob_total = 1000000
#' )
#'
#' # Calculo por metodo de cobertura
#' grps2 <- calc_grps(
#'   audiencias = c(300000, 400000, 200000),
#'   inserciones = c(3, 2, 4),
#'   pob_total = 1000000,
#'   cobertura = 65.5,
#'   metodo = "cobertura"
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_cpm}} para calculo de costes por mil (CPM)
calc_grps <- function(audiencias, inserciones, pob_total,
                          cobertura = NULL, metodo = "impresiones") {
  # Validacion de inputs
  if (!all(is.numeric(c(audiencias, inserciones, pob_total)))) {
    stop("Los argumentos audiencias, inserciones y poblacion deben ser numericos")
  }
  if (any(audiencias < 0) || any(inserciones < 0) || pob_total <= 0) {
    stop("Todos los valores deben ser positivos")
  }
  if (length(audiencias) != length(inserciones)) {
    stop("Los vectores de audiencias e inserciones deben tener la misma longitud")
  }

  # Calculo de impresiones totales
  impresiones_totales <- sum(audiencias * inserciones)

  # Calculo segun metodo
  if (metodo == "impresiones") {
    grps <- (impresiones_totales / pob_total) * 100
    frecuencia_media <- NULL
  } else if (metodo == "cobertura") {
    if (is.null(cobertura)) {
      stop("Para el metodo de cobertura, debe proporcionar el valor de cobertura")
    }
    if (cobertura <= 0 || cobertura > 100) {
      stop("La cobertura debe estar entre 0 y 100")
    }
    frecuencia_media <- impresiones_totales / (cobertura/100 * pob_total)
    grps <- cobertura * frecuencia_media
  } else {
    stop("Metodo no valido. Use 'impresiones' o 'cobertura'")
  }

  return(list(
    grps = grps,
    impresiones_totales = impresiones_totales,
    frecuencia_media = frecuencia_media,
    metodo = metodo
  ))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de CPM para plan de medios o soportes individuales
#' @description Calcula el Coste Por Mil (CPM) ya sea para un plan de medios
#' completo o para soportes individuales, permitiendo evaluar la eficiencia
#' en terminos de coste por cada mil personas alcanzadas, es decir, permite
#' comparar la rentabilidad de diferentes estrategias y medios dentro del mismo plan de campana.
#'
#' @param precios Vector numerico con precios de cada insercion o precio total (presupuesto) del plan
#' @param audiencias Vector numerico con audiencias de cada soporte
#' @param cobertura Opcional. Cobertura del plan en personas
#' @param tipo Character. Tipo de calculo: "soporte" o "plan" (default: "soporte")
#'
#' @details
#' El CPM se puede calcular de dos formas:
#' \enumerate{
#'   \item Para soportes individuales:
#'     \itemize{
#'       \item CPM = (Precio insercion / Audiencia) ? 1000
#'     }
#'   \item Para plan completo:
#'     \itemize{
#'       \item CPM = (Precio total / Cobertura) ? 1000
#'     }
#' }
#'
#' @return Una lista conteniendo:
#' \itemize{
#'   \item cpm: Vector de CPMs calculados o CPM del plan
#'   \item tipo: Tipo de calculo realizado
#'   \item total: Suma total de precios (si aplica)
#' }
#'
#' @note
#' El CPM es util para:
#' \itemize{
#'   \item Comparar eficiencia entre soportes
#'   \item Evaluar rentabilidad de planes de medios
#'   \item Optimizar presupuestos publicitarios
#' }
#'
#' @examples
#' # CPM por soportes
#' cpm1 <- calc_cpm(
#'   precios = c(1000, 1500, 800),
#'   audiencias = c(300000, 400000, 200000)
#' )
#'
#' # CPM del plan completo
#' cpm2 <- calc_cpm(
#'   precios = 25000,
#'   cobertura = 750000,
#'   tipo = "plan"
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_grps}} para calculo de GRPs
calc_cpm <- function(precios, audiencias = NULL, cobertura = NULL,
                         tipo = "soporte") {
  # Validacion de inputs
  if (!is.numeric(precios) || any(precios < 0)) {
    stop("Los precios deben ser numericos y positivos")
  }

  if (tipo == "soporte") {
    if (is.null(audiencias)) {
      stop("Para calculo por soporte, debe proporcionar audiencias")
    }
    if (length(precios) != length(audiencias)) {
      stop("Los vectores de precios y audiencias deben tener la misma longitud")
    }
    if (any(audiencias <= 0)) {
      stop("Las audiencias deben ser positivas")
    }

    # Calculo CPM por soporte
    cpm <- (precios / audiencias) * 1000
    total <- sum(precios)

  } else if (tipo == "plan") {
    if (is.null(cobertura)) {
      stop("Para calculo del plan, debe proporcionar la cobertura")
    }
    if (length(precios) > 1) {
      warning("Se usara la suma total de precios para el calculo del plan")
    }
    if (cobertura <= 0) {
      stop("La cobertura debe ser positiva")
    }

    # Calculo CPM del plan
    total <- sum(precios)
    cpm <- (total / cobertura) * 1000

  } else {
    stop("Tipo no valido. Use 'soporte' o 'plan'")
  }

  return(list(
    cpm = cpm,
    tipo = tipo,
    total = total
  ))
}

#__________________________________________________________#
