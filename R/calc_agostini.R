#' @encoding UTF-8
#' @title Calculo de la cobertura acumulada segun el modelo de Agostini
#' @description Implementa el modelo de Agostini (1961) para estimar la
#' cobertura acumulada (neta) de un plan de medios con varios soportes,
#' corrigiendo el supuesto de duplicacion aleatoria (independencia) mediante
#' un coeficiente empirico k que ajusta la duplicacion predicha entre cada
#' nuevo soporte incorporado y la cobertura acumulada hasta ese momento.
#'
#' @references
#' Agostini, J. M. (1961). How to estimate unduplicated audiences.
#' Journal of Advertising Research, 1(3), 11-14.
#'
#' @param audiencias Vector numerico con las audiencias individuales de cada
#' soporte, en el orden en que se incorporan al plan
#' @param pob_total Tamano de la poblacion
#' @param k Numerico. Coeficiente empirico de duplicacion de Agostini (por
#' defecto 0.9). Valores por debajo de 1 implican menor duplicacion que la
#' esperada bajo independencia (audiencias mas complementarias entre si);
#' valores por encima de 1 implican mayor duplicacion (audiencias mas
#' solapadas). Idealmente k debe calibrarse con datos de duplicacion
#' observada para el tipo de medio analizado; en su ausencia, la literatura
#' recomienda valores orientativos entre 0.85 y 1.15.
#'
#' @details
#' Partiendo de la cobertura acumulada tras incorporar los primeros i-1
#' soportes, R(i-1), el modelo anade el soporte i-esimo mediante:
#' \deqn{R(i) = R(i-1) + Audiencia_i - k \times \frac{R(i-1) \times Audiencia_i}{Poblacion}}
#' con R(1) = Audiencia_1. Cuando k = 1 el modelo coincide exactamente con el
#' supuesto de duplicacion aleatoria (independencia) aplicado de forma
#' iterativa, esto es, la misma hipotesis de partida de los modelos
#' Sainsbury y Binomial, pero sin necesitar conocer de antemano todas las
#' audiencias simultaneamente. A diferencia de \code{\link{calc_metheringham}}
#' o \code{\link{calc_canex}}, el modelo de Agostini no requiere una matriz
#' de duplicaciones observadas entre cada par de soportes, sino un unico
#' coeficiente empirico agregado, lo que lo hace especialmente practico
#' cuando solo se dispone de una duplicacion media estimada para el tipo de
#' medio.
#'
#' @return Una lista "reach_agostini" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura acumulada final del plan:
#'     \itemize{
#'       \item porcentaje: Cobertura final en porcentaje
#'       \item personas: Cobertura final en numero de personas
#'     }
#'   \item acumulada: Lista con la evolucion de la cobertura acumulada tras
#'   incorporar cada soporte (porcentaje y personas)
#'   \item k: Coeficiente de duplicacion empleado
#'   \item n_soportes: Numero de soportes incorporados
#' }
#'
#' @examples
#' audiencias <- c(300000, 400000, 200000)
#' resultado <- calc_agostini(audiencias, pob_total = 1000000, k = 0.9)
#' print(resultado)
#'
#' # k = 1 equivale al supuesto de duplicacion aleatoria (independencia)
#' resultado_independencia <- calc_agostini(audiencias, pob_total = 1000000, k = 1)
#' resultado_independencia$reach$porcentaje
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para el supuesto de duplicacion aleatoria con heterogeneidad de soportes
#' \code{\link{calc_binomial}} para el supuesto de duplicacion aleatoria con homogeneidad de soportes
#' \code{\link{calc_metheringham}} para el ajuste mediante duplicacion media observada
calc_agostini <- function(audiencias, pob_total, k = 0.9) {
  if (!is.numeric(audiencias) || !is.numeric(pob_total) || !is.numeric(k)) {
    stop("audiencias, pob_total y k deben ser numericos")
  }
  if (length(audiencias) < 1) {
    stop("audiencias debe contener al menos un soporte")
  }
  if (any(audiencias <= 0) || any(audiencias > pob_total)) {
    stop("Las audiencias deben ser positivas y no superiores a la poblacion")
  }
  if (length(pob_total) != 1 || pob_total <= 0) {
    stop("pob_total debe ser un unico numero positivo")
  }
  if (length(k) != 1 || k < 0) {
    stop("k debe ser un unico numero no negativo")
  }

  n <- length(audiencias)
  acumulada_personas <- numeric(n)
  acumulada_personas[1] <- audiencias[1]

  if (n > 1) {
    for (i in 2:n) {
      acumulada_personas[i] <- acumulada_personas[i - 1] + audiencias[i] -
        k * (acumulada_personas[i - 1] * audiencias[i] / pob_total)
    }
  }
  # La cobertura acumulada nunca puede superar la poblacion total, incluso si
  # se emplea un coeficiente k mal calibrado.
  acumulada_personas <- pmin(pmax(acumulada_personas, 0), pob_total)

  reach_final <- acumulada_personas[n]

  structure(list(
    reach = list(
      porcentaje = reach_final / pob_total * 100,
      personas = reach_final
    ),
    acumulada = list(
      porcentaje = acumulada_personas / pob_total * 100,
      personas = acumulada_personas
    ),
    k = k,
    n_soportes = n
  ), class = "reach_agostini")
}

#' @encoding UTF-8
#' @title Imprimir un objeto reach_agostini
#' @description Genera un informe formateado con las metricas del modelo de Agostini.
#'
#' @param x Objeto de clase \code{"reach_agostini"}, resultado de \code{\link{calc_agostini}}
#' @param ... Argumentos adicionales (no usados)
#'
#' @return Invisible \code{x}. La funcion se invoca por su efecto de impresion.
#'
#' @examples
#' resultado <- calc_agostini(c(300000, 400000, 200000), pob_total = 1000000)
#' print(resultado)
#'
#' @export
print.reach_agostini <- function(x, ...) {
  cat("MODELO DE AGOSTINI\n")
  cat("==================\n")
  cat(sprintf("Descripcion: Duplicacion aleatoria corregida mediante coeficiente empirico k = %.3f\n\n", x$k))

  cat("COBERTURA ACUMULADA TRAS INCORPORAR CADA SOPORTE:\n")
  cat("--------------------------------------------------\n")
  for (i in seq_len(x$n_soportes)) {
    cat(sprintf("Tras soporte %d: %.2f%% (%.0f personas)\n",
                i, x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }

  cat("\nCOBERTURA TOTAL DEL PLAN:\n")
  cat("-------------------------\n")
  cat(sprintf("%.2f%% (%.0f personas)\n", x$reach$porcentaje, x$reach$personas))

  invisible(x)
}
