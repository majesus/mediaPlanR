#' @encoding UTF-8
#' @title Legacy plan wrapper for an experimental Negative-Binomial exposure approximation
#' @description Applies a univariate NBD (Poisson-Gamma) approximation to the
#' aggregate contact pressure of a media plan. Unlike the Beta-Binomial, it
#' does not model a fixed number of discrete insertion opportunities and does
#' not preserve vehicle-level dependence. It is retained for compatibility;
#' use \code{\link{nbd_exposure_distribution}} for explicit scenario work or
#' \code{\link{fit_nbd_exposure}} with observed person-level counts.
#' The model assumes that each individual has a
#' tasa personal de exposicion (Poisson) que varia entre la poblacion segun
#' una distribucion Gamma, dando lugar a una distribucion Binomial Negativa
#' para el numero total de contactos.
#'
#' @references
#' Ehrenberg, A. S. C. (1959). The pattern of consumer purchases. Applied
#' Statistics, 8(1), 26-41. \doi{10.2307/2985810}
#'
#' Danaher, P. J. (2007). Modeling Page Views Across Multiple Websites with an
#' Application to Internet Reach and Frequency Prediction. Marketing Science,
#' 26(3), 422-437. \doi{10.1287/mksc.1060.0226}
#'
#' @param audiencias Vector numerico con las audiencias de cada soporte
#' @param inserciones Vector numerico con el numero de inserciones por soporte
#' @param pob_total Tamano de la poblacion
#' @param k Numerico. Parametro de heterogeneidad (forma de la distribucion
#' Gamma) de la NBD. Valores pequenos de k indican alta heterogeneidad
#' (la exposicion se concentra en pocos individuos); valores grandes de k
#' aproximan el caso homogeneo (modelo de Poisson puro). Debe indicarse
#' \code{k} o \code{reach_conocida}, pero no ambos
#' @param reach_conocida Numerico (0-1). Cobertura del plan conocida o
#' estimada por otra via (p.ej. panel de audiencias, u otro modelo aplicado
#' al mismo plan). Si se indica, \code{k} se calibra numericamente para
#' reproducir exactamente esta cobertura dada la frecuencia media del plan.
#' Debe indicarse \code{k} o \code{reach_conocida}, pero no ambos
#' @param max_contactos Entero. Numero maximo de contactos a reportar en la
#' distribucion. Por defecto, el total de inserciones del plan
#' (\code{sum(inserciones)}), ya que ningun individuo puede resultar
#' expuesto mas veces que el numero de inserciones planificadas
#'
#' @details
#' El modelo se apoya en dos cantidades:
#' \enumerate{
#'   \item La frecuencia media de exposicion del plan,
#'   \eqn{m = \sum(Audiencia_i \times Inserciones_i) / Poblacion}, esto es,
#'   el mismo calculo que \code{\link{calc_grps}} expresa como GRPs/100
#'   \item El parametro de heterogeneidad k de la distribucion Gamma que
#'   mezcla con la Poisson
#' }
#' Dados m y k, el numero de contactos X sigue una distribucion Binomial
#' Negativa, \eqn{P(X = x) = dnbinom(x, size = k, mu = m)}, y la cobertura
#' es \eqn{P(X \geq 1) = 1 - P(X = 0)}. Como la distribucion Binomial
#' Negativa tiene soporte teorico ilimitado, mientras que en la practica
#' nadie puede recibir mas contactos que el total de inserciones del plan,
#' toda la masa de probabilidad correspondiente a \code{max_contactos} o
#' mas contactos se informa como una cola abierta en el ultimo tramo (en vez de
#' truncarse y renormalizarse, lo que distorsionaria la cobertura ya
#' calibrada): asi, \code{P(X = 0)} y, por tanto, la cobertura total,
#' se reproducen de forma exacta con independencia de \code{max_contactos}.
#'
#' Para un mismo valor de m, la cobertura es una funcion creciente de k,
#' acotada superiormente por el caso homogeneo (Poisson puro),
#' \eqn{1 - e^{-m}}. Por ello, si se indica \code{reach_conocida}, su valor
#' debe ser estrictamente menor que \eqn{1 - e^{-m}}; de lo contrario no
#' existe ningun k que la reproduzca y la funcion se detiene con un error.
#'
#' @return Una lista "reach_nbd" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en numero de personas
#'     }
#'   \item distribucion: Lista con la distribucion de contactos (0 a max_contactos):
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada numero de contactos
#'       \item personas: Vector con numero de personas para cada numero de contactos
#'     }
#'   \item acumulada: Lista con la distribucion acumulada (1 o mas contactos, 2 o mas, ...):
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con numero de personas acumuladas al menos i veces
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
#' \code{\link{calc_sainsbury}} para el supuesto de duplicacion aleatoria
#' \code{\link{calc_grps}} para el calculo de la frecuencia media (GRPs)
#'
#' @importFrom stats dnbinom uniroot
calc_nbd <- function(audiencias, inserciones, pob_total,
                      k = NULL, reach_conocida = NULL,
                      max_contactos = NULL) {

  if (!is.numeric(pob_total) || length(pob_total) != 1L ||
      !is.finite(pob_total) || pob_total <= 0) {
    stop("pob_total debe ser un unico numero positivo y finito", call. = FALSE)
  }
  if (!is.numeric(audiencias) || !length(audiencias) || anyNA(audiencias) ||
      any(!is.finite(audiencias)) || any(audiencias < 0) ||
      any(audiencias > pob_total)) {
    stop("audiencias debe contener valores finitos entre cero y pob_total",
         call. = FALSE)
  }
  if (!is.numeric(inserciones) || length(inserciones) != length(audiencias) ||
      anyNA(inserciones) || any(!is.finite(inserciones)) ||
      any(inserciones < 0 | inserciones != round(inserciones))) {
    stop("inserciones debe contener un entero no negativo por audiencia",
         call. = FALSE)
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
  if (!is.numeric(max_contactos) || length(max_contactos) != 1L ||
      !is.finite(max_contactos) || max_contactos <= 0 ||
      max_contactos != round(max_contactos)) {
    stop("max_contactos debe ser un entero positivo")
  }

  # Frecuencia media de exposicion del plan (m = GRPs/100, ver calc_grps())
  m <- sum(audiencias * inserciones) / pob_total
  if (m <= 0) {
    stop("La frecuencia media del plan (impresiones totales / poblacion) debe ser mayor que 0")
  }

  reach_dado_k <- function(k_val) 1 - stats::dnbinom(0, size = k_val, mu = m)

  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
      stop("k debe ser un unico numero positivo")
    }
    k_source <- "supplied"
  } else {
    if (!is.numeric(reach_conocida) || length(reach_conocida) != 1 ||
        !is.finite(reach_conocida) ||
        reach_conocida <= 0 || reach_conocida >= 1) {
      stop("reach_conocida debe ser un unico numero entre 0 y 1")
    }
    reach_maxima <- 1 - exp(-m)
    if (reach_conocida >= reach_maxima) {
      stop(sprintf(
        paste(
          "reach_conocida (%.4f) no es alcanzable para una frecuencia media",
          "de %.4f: incluso sin heterogeneidad (modelo de Poisson puro) la",
          "cobertura maxima posible es %.4f. Reduzca reach_conocida o revise",
          "audiencias/inserciones/pob_total."
        ),
        reach_conocida, m, reach_maxima
      ))
    }
    log_interval <- c(-30, 30)
    endpoint_reach <- vapply(log_interval, function(log_k) {
      reach_dado_k(exp(log_k))
    }, numeric(1))
    if (reach_conocida <= endpoint_reach[1L]) {
      stop("reach_conocida is too close to zero for stable NBD calibration.",
           call. = FALSE)
    }
    sol <- stats::uniroot(
      function(log_k) reach_dado_k(exp(log_k)) - reach_conocida,
      interval = log_interval, tol = .Machine$double.eps^0.5
    )
    k <- exp(sol$root)
    k_source <- "calibrated_from_external_reach"
  }

  model <- nbd_exposure_distribution(
    mean_contacts = m,
    size = k,
    report_max = max_contactos,
    opportunities = n_total
  )
  probs <- model$distribution$probability
  reach <- model$reach$probability

  # Distribucion acumulada: P(X >= i) para i = 1..max_contactos
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
      k_source = k_source,
      max_contactos = max_contactos
    ),
    diagnostics = model$diagnostics
  ), class = "reach_nbd")
}

#' @encoding UTF-8
#' @title Imprimir un objeto reach_nbd
#' @description Genera un informe formateado con las metricas del modelo NBD.
#'
#' @param x Objeto de clase \code{"reach_nbd"}, resultado de \code{\link{calc_nbd}}
#' @param ... Argumentos adicionales (no usados)
#'
#' @return Invisible \code{x}. La funcion se invoca por su efecto de impresion.
#'
#' @examples
#' resultado <- calc_nbd(c(300000, 400000, 200000), c(3, 2, 4), 1000000, k = 1.7)
#' print(resultado)
#'
#' @export
print.reach_nbd <- function(x, ...) {
  cat("MODELO NBD (Binomial Negativa / Poisson-Gamma)\n")
  cat("===============================================\n")
  cat("Scope: experimental unbounded count approximation; not a finite-insertion cross-media model\n\n")

  cat("PARAMETROS DEL MODELO:\n")
  cat("----------------------\n")
  cat(sprintf("Frecuencia media (m = GRPs/100): %.4f\n", x$parametros$m))
  cat(sprintf("k (heterogeneidad): %.4f\n", x$parametros$k))
  if (!is.null(x$parametros$k_source)) {
    cat(sprintf("k source: %s\n", x$parametros$k_source))
  }
  if (!is.null(x$diagnostics$probability_above_opportunities)) {
    cat(sprintf("Probability above the plan's finite opportunities: %.6g\n",
                x$diagnostics$probability_above_opportunities))
  }

  cat("\nMETRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  cat("\nDISTRIBUCION DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de poblacion; el ultimo tramo agrupa 'max_contactos o m\u00e1s')\n")
  n_filas <- length(x$distribucion$porcentaje)
  for (i in seq_len(n_filas)) {
    n <- i - 1
    etiqueta <- if (i == n_filas) sprintf("%d o m\u00e1s contactos", n) else
      sprintf("%d contacto%s", n, ifelse(n == 1, "", "s"))
    cat(sprintf("%s: %.2f%% (%.0f personas)\n",
                etiqueta, x$distribucion$porcentaje[i], x$distribucion$personas[i]))
  }

  cat("\nDISTRIBUCION ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de poblacion que recibe N o mas contactos)\n")
  for (i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf(">= %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }

  invisible(x)
}
