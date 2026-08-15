#' @encoding UTF-8
#' @title Cálculo de la cobertura y distribución de contactos según el modelo de Distribución Binomial Negativa (NBD)
#' @description Implementa el modelo NBD (Poisson-Gamma) como alternativa al
#' modelo Beta-Binomial para estimar la distribución de contactos de un plan
#' de medios. En lugar de modelar un número fijo de n inserciones discretas
#' (como el Beta-Binomial), el modelo NBD asume que cada individuo tiene una
#' tasa personal de exposición (Poisson) que varía entre la población según
#' una distribución Gamma, dando lugar a una distribución Binomial Negativa
#' para el número total de contactos.
#'
#' @references
#' Ehrenberg, A. S. C. (1959). The pattern of consumer purchases. Journal of
#' the Royal Statistical Society: Series C (Applied Statistics), 8(1), 26-41.
#' (formulación original de la mezcla Poisson-Gamma / Binomial Negativa)
#'
#' Leckenby, J. D., & Kishi, S. (1982). Performance of exposure distribution
#' models. Journal of Advertising Research, 22(2), 35-44. (aplicación y
#' comparación de los modelos Beta-Binomial y Binomial Negativa como
#' modelos de exposición a medios)
#'
#' @param audiencias Vector numérico con las audiencias de cada soporte
#' @param inserciones Vector numérico con el número de inserciones por soporte
#' @param pob_total Tamaño de la población
#' @param k Numérico. Parámetro de heterogeneidad (forma de la distribución
#' Gamma) de la NBD. Valores pequeños de k indican alta heterogeneidad
#' (la exposición se concentra en pocos individuos); valores grandes de k
#' aproximan el caso homogéneo (modelo de Poisson puro). Debe indicarse
#' \code{k} o \code{reach_conocida}, pero no ambos
#' @param reach_conocida Numérico (0-1). Cobertura del plan conocida o
#' estimada por otra vía (p.ej. panel de audiencias, u otro modelo aplicado
#' al mismo plan). Si se indica, \code{k} se calibra numéricamente para
#' reproducir exactamente esta cobertura dada la frecuencia media del plan.
#' Debe indicarse \code{k} o \code{reach_conocida}, pero no ambos
#' @param max_contactos Entero. Número máximo de contactos a reportar en la
#' distribución. Por defecto, el total de inserciones del plan
#' (\code{sum(inserciones)}), ya que ningún individuo puede resultar
#' expuesto más veces que el número de inserciones planificadas
#'
#' @details
#' El modelo se apoya en dos cantidades:
#' \enumerate{
#'   \item La frecuencia media de exposición del plan,
#'   \eqn{m = \sum(Audiencia_i \times Inserciones_i) / Poblacion}, esto es,
#'   el mismo cálculo que \code{\link{calc_grps}} expresa como GRPs/100
#'   \item El parámetro de heterogeneidad k de la distribución Gamma que
#'   mezcla con la Poisson
#' }
#' Dados m y k, el número de contactos X sigue una distribución Binomial
#' Negativa, \eqn{P(X = x) = dnbinom(x, size = k, mu = m)}, y la cobertura
#' es \eqn{P(X \geq 1) = 1 - P(X = 0)}. Como la distribución Binomial
#' Negativa tiene soporte teórico ilimitado, mientras que en la práctica
#' nadie puede recibir más contactos que el total de inserciones del plan,
#' toda la masa de probabilidad correspondiente a \code{max_contactos} o
#' más contactos se acumula en el último tramo reportado (en vez de
#' truncarse y renormalizarse, lo que distorsionaría la cobertura ya
#' calibrada): así, \code{P(X = 0)} y, por tanto, la cobertura total,
#' se reproducen de forma exacta con independencia de \code{max_contactos}.
#'
#' Para un mismo valor de m, la cobertura es una función creciente de k,
#' acotada superiormente por el caso homogéneo (Poisson puro),
#' \eqn{1 - e^{-m}}. Por ello, si se indica \code{reach_conocida}, su valor
#' debe ser estrictamente menor que \eqn{1 - e^{-m}}; de lo contrario no
#' existe ningún k que la reproduzca y la función se detiene con un error.
#'
#' @return Una lista "reach_nbd" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en número de personas
#'     }
#'   \item distribucion: Lista con la distribución de contactos (0 a max_contactos):
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada número de contactos
#'       \item personas: Vector con número de personas para cada número de contactos
#'     }
#'   \item acumulada: Lista con la distribución acumulada (1 o más contactos, 2 o más, ...):
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con número de personas acumuladas al menos i veces
#'     }
#'   \item parametros: Lista con m (frecuencia media), k (heterogeneidad) y
#'   max_contactos empleados
#' }
#'
#' @examples
#' # Indicando k directamente
#' resultado <- calc_nbd(
#'   audiencias = c(300000, 400000, 200000),
#'   inserciones = c(3, 2, 4),
#'   pob_total = 1000000,
#'   k = 1.7
#' )
#' print(resultado)
#'
#' # Calibrando k a partir de una cobertura conocida (p.ej. estimada con
#' # calc_sainsbury sobre el mismo plan, para comparar ambos modelos)
#' resultado_calibrado <- calc_nbd(
#'   audiencias = c(300000, 400000, 200000),
#'   inserciones = c(3, 2, 4),
#'   pob_total = 1000000,
#'   reach_conocida = 0.60
#' )
#' resultado_calibrado$parametros$k
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para el modelo alternativo de heterogeneidad Beta-Binomial
#' \code{\link{calc_sainsbury}} para el supuesto de duplicación aleatoria
#' \code{\link{calc_grps}} para el cálculo de la frecuencia media (GRPs)
#'
#' @importFrom stats dnbinom uniroot
calc_nbd <- function(audiencias, inserciones, pob_total,
                      k = NULL, reach_conocida = NULL,
                      max_contactos = NULL) {

  if (!is.numeric(audiencias) || !is.numeric(inserciones) || !is.numeric(pob_total)) {
    stop("audiencias, inserciones y pob_total deben ser numéricos")
  }
  if (length(audiencias) != length(inserciones)) {
    stop("audiencias e inserciones deben tener la misma longitud")
  }
  if (any(audiencias < 0) || any(audiencias > pob_total) || any(inserciones < 0)) {
    stop("Las audiencias deben ser no negativas y como máximo la población; las inserciones no negativas")
  }
  if (length(pob_total) != 1 || pob_total <= 0) {
    stop("pob_total debe ser un único número positivo")
  }
  if (is.null(k) && is.null(reach_conocida)) {
    stop("Debe indicarse 'k' o 'reach_conocida' (pero no ambos)")
  }
  if (!is.null(k) && !is.null(reach_conocida)) {
    stop("Indique solo uno de 'k' o 'reach_conocida', no ambos")
  }

  n_total <- sum(inserciones)
  if (n_total <= 0) {
    stop("El total de inserciones debe ser mayor que 0")
  }
  if (is.null(max_contactos)) {
    max_contactos <- n_total
  }
  if (max_contactos <= 0 || max_contactos != round(max_contactos)) {
    stop("max_contactos debe ser un entero positivo")
  }

  # Frecuencia media de exposición del plan (m = GRPs/100, ver calc_grps())
  m <- sum(audiencias * inserciones) / pob_total
  if (m <= 0) {
    stop("La frecuencia media del plan (impresiones totales / población) debe ser mayor que 0")
  }

  reach_dado_k <- function(k_val) 1 - stats::dnbinom(0, size = k_val, mu = m)

  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1 || k <= 0) {
      stop("k debe ser un único número positivo")
    }
  } else {
    if (!is.numeric(reach_conocida) || length(reach_conocida) != 1 ||
        reach_conocida <= 0 || reach_conocida >= 1) {
      stop("reach_conocida debe ser un único número entre 0 y 1")
    }
    reach_maxima <- 1 - exp(-m)
    if (reach_conocida >= reach_maxima) {
      stop(sprintf(
        paste(
          "reach_conocida (%.4f) no es alcanzable para una frecuencia media",
          "de %.4f: incluso sin heterogeneidad (modelo de Poisson puro) la",
          "cobertura máxima posible es %.4f. Reduzca reach_conocida o revise",
          "audiencias/inserciones/pob_total."
        ),
        reach_conocida, m, reach_maxima
      ))
    }
    sol <- stats::uniroot(
      function(k_val) reach_dado_k(k_val) - reach_conocida,
      interval = c(1e-8, 1e8), tol = .Machine$double.eps^0.5
    )
    k <- sol$root
  }

  # P(X = 0), ..., P(X = max_contactos - 1) de forma exacta, y toda la masa
  # restante (X >= max_contactos) se acumula en el último tramo (ver
  # @details): así P(X=0) -y por tanto la cobertura- no se ve alterado por
  # dónde se trunque la tabla, a diferencia de una renormalización proporcional.
  probs_exactas <- stats::dnbinom(0:(max_contactos - 1), size = k, mu = m)
  p_cola <- max(0, 1 - sum(probs_exactas))
  probs <- c(probs_exactas, p_cola)
  if (any(!is.finite(probs)) || sum(probs) <= 0) {
    stop("No se pudo calcular una distribución válida con los parámetros proporcionados")
  }

  reach <- 1 - probs[1]

  # Distribución acumulada: P(X >= i) para i = 1..max_contactos
  acumulada <- rev(cumsum(rev(probs)))[-1]

  structure(list(
    reach = list(
      porcentaje = reach * 100,
      personas = reach * pob_total
    ),
    distribucion = list(
      porcentaje = probs * 100,
      personas = probs * pob_total
    ),
    acumulada = list(
      porcentaje = acumulada * 100,
      personas = acumulada * pob_total
    ),
    parametros = list(
      m = m,
      k = k,
      max_contactos = max_contactos
    )
  ), class = "reach_nbd")
}

#' @encoding UTF-8
#' @title Imprimir un objeto reach_nbd
#' @description Genera un informe formateado con las métricas del modelo NBD.
#'
#' @param x Objeto de clase \code{"reach_nbd"}, resultado de \code{\link{calc_nbd}}
#' @param ... Argumentos adicionales (no usados)
#'
#' @return Invisible \code{x}. La función se invoca por su efecto de impresión.
#'
#' @examples
#' resultado <- calc_nbd(c(300000, 400000, 200000), c(3, 2, 4), 1000000, k = 1.7)
#' print(resultado)
#'
#' @export
print.reach_nbd <- function(x, ...) {
  cat("MODELO NBD (Binomial Negativa / Poisson-Gamma)\n")
  cat("===============================================\n")
  cat("Descripción: Heterogeneidad de exposición modelada como mezcla Poisson-Gamma\n\n")

  cat("PARÁMETROS DEL MODELO:\n")
  cat("----------------------\n")
  cat(sprintf("Frecuencia media (m = GRPs/100): %.4f\n", x$parametros$m))
  cat(sprintf("k (heterogeneidad): %.4f\n", x$parametros$k))

  cat("\nMÉTRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  cat("\nDISTRIBUCIÓN DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de población; el último tramo agrupa 'max_contactos o más')\n")
  n_filas <- length(x$distribucion$porcentaje)
  for (i in seq_len(n_filas)) {
    n <- i - 1
    etiqueta <- if (i == n_filas) sprintf("%d o más contactos", n) else
      sprintf("%d contacto%s", n, ifelse(n == 1, "", "s"))
    cat(sprintf("%s: %.2f%% (%.0f personas)\n",
                etiqueta, x$distribucion$porcentaje[i], x$distribucion$personas[i]))
  }

  cat("\nDISTRIBUCIÓN ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de población que recibe N o más contactos)\n")
  for (i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("≥ %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }

  invisible(x)
}
