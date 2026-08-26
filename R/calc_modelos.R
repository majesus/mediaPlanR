#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de cobertura y distribucion de contactos (y acumulada) segun modelo de Sainsbury
#' @description Implementa el modelo de Sainsbury, desarrollado por E. J. Sansbury en la London Press Exchange,
#' para calcular la cobertura y la distribucion de contactos para un conjunto de soportes publicitarios y una unica insercion por soporte.
#' El modelo considera la duplicacion aleatoria, las probabilidades individuales de exposicion homogeneas, y las probabilidades de
#' exposicion del soporte heterogeneas para una estimacion mas precisa de la cobertura y la distribucion de contactos (y acumulada).
#' De las dos ultimas hipotesis se deriva que la probabilidad de que un individuo resulte expuesto al soporte i vendra dado por
#' el cociente entre la audiencia del soporte i (casos favorables) y la poblacion (casos totales). Por su parte, de la asuncion de la duplicacion aleatoria se deriva que
#' la probabilidad de exposicion continuara siendo una variable Bernouilli con diferentes probabilidadades de exposicion en cada soporte.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiencias Vector numerico con las audiencias individuales de cada soporte
#' @param pob_total Tamano de la poblacion
#'
#' @details
#' El modelo de Sainsbury simplificado calcula:
#' \enumerate{
#'   \item Cobertura considerando la duplicacion entre soportes como el producto de las probabilidades individuales
#'   \item Distribucion de contactos para cada nivel de exposicion i
#'   \item Distribucion de contactos acumulada (expuestos al menos i veces)
#' }
#'
#' El proceso incluye:
#' \itemize{
#'   \item Conversion de audiencias a probabilidades
#'   \item Calculo de las posibles combinaciones de soportes
#'   \item Estimacion de probabilidades conjuntas
#'   \item Agregacion de resultados: distribucion de contactos (y acumulada)
#' }
#'
#' @return Una lista "reach_sainsbury" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en numero de personas
#'     }
#'   \item distribucion: Lista con la distribucion de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada numero de exposiciones
#'       \item personas: Vector con numero de personas para cada numero de exposiciones
#'     }
#'   \item acumulada: Lista con la distribucion acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con numero de personas acumuladas al menos i veces
#'     }
#' }
#'
#' @examples
#' # Ejemplo basico con tres soportes
#' audiencias <- c(300000, 400000, 200000)
#' pob_total <- 1000000
#' resultado <- calc_sainsbury(audiencias, pob_total)
#'
#' # Examinar los resultados
#' print(resultado$reach$porcentaje)  # Cobertura en porcentaje
#' print(resultado$distribucion$personas)  # Personas por numero de contactos
#'
#' # Ejemplo con validacion de datos
#' \dontrun{
#' audiencias_invalidas <- c(300000, -400000, 200000)
#' resultado <- calc_sainsbury(audiencias_invalidas, pob_total)
#' # Generara un error por audiencia negativa
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_binomial}} para estimaciones con la distribucion Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribucion de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribucion de Hofmans
#' @importFrom utils combn
calc_sainsbury <- function(audiencias, pob_total) {
  # Validacion de inputs
  if (!is.numeric(audiencias) || !is.numeric(pob_total)) {
    stop("Los argumentos deben ser numericos")
  }
  if (length(audiencias) < 1L || anyNA(audiencias) || any(!is.finite(audiencias))) {
    stop("audiencias must contain at least one finite value")
  }
  if (length(pob_total) != 1L || !is.finite(pob_total) ||
      any(audiencias < 0) || any(audiencias > pob_total)) {
    stop("Las audiencias deben ser positivas y menores que la poblacion")
  }
  if (pob_total <= 0) {
    stop("La poblacion debe ser positiva")
  }

  # Convertir audiencias a probabilidades
  probs <- audiencias / pob_total
  n <- length(probs)

  # Exact Poisson-binomial distribution using dynamic convolution. This is
  # O(n^2), whereas enumerating every combination is O(2^n).
  full_distribution <- poisson_binomial_distribution(probs)
  P <- full_distribution[-1L]
  R <- rev(cumsum(rev(P)))

  # Calculo del reach total
  reach <- 1 - prod(1 - probs)

  return(structure(list(
    reach = list(
      porcentaje = reach * 100,
      personas = reach * pob_total
    ),
    distribucion = list(
      porcentaje = P * 100,
      personas = P * pob_total
    ),
    acumulada = list(
      porcentaje = R * 100,
      personas = R * pob_total
    )
  ), class = "reach_sainsbury"))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de cobertura y distribucion de contactos (y acumulada) segun modelo Binomial
#' @description Implementa el modelo Binomial, desarrollado por Chandon (1985), para calcular la cobertura y
#' distribucion de contactos (y acumulada) de plan de medios de n soportes y una unica insercion por soporte.
#' El modelo Binomial asume la duplicacion aleatoria (i.e.,la exposicion a un soporte no modifica
#' la probabilidad de resultar expuesto a otro), y la homogeneidad de las probabilidades de exposicion del soporte y
#' las probabilidades individuales de exposicion. Uniendo estas dos hipotesis ultimas, la probabilidad de exposicion de
#' cualquier individuo a un soporte determinado se calcula como la media de las audiencias de cada soporte.
#' Las probabilidades de exposicion son estacionarias respecto al tiempo.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiencias Vector numerico con las audiencias individuales de cada soporte
#' @param pob_total Tamano de la poblacion
#'
#' @details
#' El modelo Bnomial calcula:
#' \enumerate{
#'   \item Cobertura considerando un soporte hipotetico "promedio" cuya audiencia es la media simple de las audiencias de cada soporte
#'   \item Distribucion de contactos para cada nivel de exposicion
#'   \item Distribucion de contactos acumulada (expuestos al menos i veces)
#' }
#'
#' La metodologia incluye:
#' \itemize{
#'   \item Conversion de audiencias a probabilidades individuales
#'   \item Calculo de probabilidad media de exposicion
#'   \item Aplicacion del modelo Binomial para n inserciones
#'   \item Calculo de distribuciones de contactos (y acumulada)
#' }
#'
#' @return Una lista "reach_binomial" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en numero de personas
#'     }
#'   \item distribucion: Lista con la distribucion de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada numero de exposiciones
#'       \item personas: Vector con numero de personas para cada numero de exposiciones
#'     }
#'   \item acumulada: Lista con la distribucion acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con numero de personas acumuladas al menos i veces
#'     }
#' }
#'
#' @examples
#' # Ejemplo basico con tres soportes
#' audiencias <- c(300000, 400000, 200000)
#' pob_total <- 1000000
#' resultado <- calc_binomial(audiencias, pob_total)
#'
#' # Examinar los resultados
#' print(paste("Cobertura total:", resultado$reach$porcentaje, "%"))
#' print(paste("Probabilidad media:", resultado$probabilidad_media))
#'
#' # Verificar que las distribuciones suman 1 (100%)
#' \dontrun{
#' sum_dist <- sum(resultado$distribucion$porcentaje)/100
#' print(paste("Suma distribucion:", round(sum_dist, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con la distribucion Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribucion de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribucion de Hofmans
calc_binomial <- function(audiencias, pob_total) {
  # Validacion de inputs
  if (!is.numeric(audiencias) || !is.numeric(pob_total)) {
    stop("Los argumentos deben ser numericos")
  }
  if (length(audiencias) < 1L || anyNA(audiencias) || any(!is.finite(audiencias))) {
    stop("audiencias must contain at least one finite value")
  }
  if (length(pob_total) != 1L || !is.finite(pob_total) ||
      any(audiencias < 0) || any(audiencias > pob_total)) {
    stop("Las audiencias deben ser positivas y menores que la poblacion total")
  }
  if (pob_total <= 0) {
    stop("La poblacion total debe ser positiva")
  }

  # Convertir audiencias a probabilidad media
  probs <- audiencias / pob_total
  p <- mean(probs)
  n <- length(audiencias)

  P <- numeric(n) # Distribucion de contactos
  R <- numeric(n) # Distribucion acumulada

  # Calculo de la distribucion de contactos (P)
  for(i in 1:n) {
    P[i] <- choose(n, i) * p^i * (1-p)^(n-i)
  }

  # Calculo de la distribucion acumulada (R)
  for(i in 1:n) {
    R[i] <- sum(P[i:n])
  }

  # Calculo del reach total
  reach <- 1 - (1-p)^n

  return(structure(list(
    reach = list(
      porcentaje = reach * 100,
      personas = reach * pob_total
    ),
    distribucion = list(
      porcentaje = P * 100,
      personas = P * pob_total
    ),
    acumulada = list(
      porcentaje = R * 100,
      personas = R * pob_total
    ),
    probabilidad_media = p
  ), class = "reach_binomial"))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de la cobertura y distribucion de contactos (y acumulada) usando modelo Beta-Binomial
#' @description Implementa el modelo Beta-Binomial para calcular la audiencia neta acumulada
#' y la distribucion de contactos (y acumulada). El modelo Beta-Binomial considera la
#' heterogeneidad en la probabilidad de exposicion de los individuos. Combina dos pasos:
#' modela la probabilidad de exito aplicando la distribucion Beta de parametros alpha y beta -lo cual reduce a dos
#' los datos necesarios para su estimacion; y emplea la probabilidad en la distribucion Binomial (combinada con la distribucion Beta)
#' para valorar la distribucion de contactos (y acumulada). Es util cuando la probabilidad de
#' exito no es conocida a priori, y puede variar entre los individuos. Los parametros alpha y beta precisamente permiten
#' ajustar la forma de la distribucion para que refleje la incertidumbre en relacion con la probabilidad de exito.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param A1 Audiencia del soporte tras la primera insercion
#' @param A2 Audiencia del soporte tras la segunda insercion
#' @param P Tamano total de la poblacion
#' @param n Numero total de inserciones planificadas (debe ser entero positivo)
#'
#' @details
#' El modelo Beta-Binomial:
#' \enumerate{
#'   \item Calcula los parametros alpha y beta a partir de A1 y A2
#'   \item Modela la heterogeneidad en la exposicion mediante la distribucion Beta
#'   \item Combina la distribucion Beta con la Binomial para la distribucion de contactos
#'   \item Calcula probabilidades exactas para cada nivel de exposicion
#' }
#'
#' El proceso incluye:
#' \itemize{
#'   \item Estimacion de coeficientes de duplicacion R1 y R2
#'   \item Calculo de parametros alpha y beta del modelo
#'   \item Generacion de distribucion de contactos
#'   \item Calculo de la distribucion de contactos (y acumuladas)
#' }
#'
#' @return Una lista "reach_beta_binomial" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en numero de personas
#'     }
#'   \item distribucion: Lista con la distribucion de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada numero de exposiciones
#'       \item personas: Vector con numero de personas para cada numero de exposiciones
#'     }
#'   \item acumulada: Lista con la distribucion acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con numero de personas acumuladas al menos i veces
#'     }
#'   \item parametros: Lista con parametros del modelo:
#'     \itemize{
#'       \item alpha: Parametro alpha estimado
#'       \item beta: Parametro beta estimado
#'       \item prob_cero_contactos: Probabilidad de no exposicion
#'     }
#' }
#'
#' @note
#' El modelo Beta-Binomial es especialmente adecuado cuando:
#' \itemize{
#'   \item Existe heterogeneidad significativa en la poblacion
#'   \item Se dispone de datos de audiencias acumuladas (A1 y A2)
#' }
#'
#' @examples
#' # Ejemplo basico
#' resultado <- calc_beta_binomial(
#'   A1 = 500000,    # Primera audiencia
#'   A2 = 550000,    # Segunda audiencia
#'   P = 1000000,    # Poblacion total
#'   n = 5           # Numero de inserciones
#' )
#'
#' # Examinar resultados
#' print(paste("Cobertura:", round(resultado$reach$porcentaje, 2), "%"))
#' print(paste("Alpha:", round(resultado$parametros$alpha, 4)))
#' print(paste("Beta:", round(resultado$parametros$beta, 4)))
#'
#' # Verificar consistencia de las distribuciones
#' \dontrun{
#' sum_dist <- sum(resultado$distribucion$porcentaje)/100
#' print(paste("Suma distribucion:", round(sum_dist +
#'             resultado$parametros$prob_cero_contactos/100, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con la distribucion Binomial
#' \code{\link{calc_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribucion de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribucion de Hofmans
#' \code{\link{nbd_exposure_distribution}} para la aproximacion experimental
#' de conteos de exposicion mediante Binomial Negativa (NBD)
calc_beta_binomial <- function(A1, A2, P, n) {
  # Validacion de inputs
  if (!all(is.numeric(c(A1, A2, P, n)))) {
    stop("Todos los argumentos deben ser numericos")
  }
  if (A1 <= 0 || A2 <= 0 || P <= 0) {
    stop("Las audiencias y poblacion deben ser positivas")
  }
  if (A1 > P || A2 > P) {
    stop("Las audiencias no pueden ser mayores que la poblacion total")
  }
  if (n <= 0 || n != round(n)) {
    stop("El numero de inserciones debe ser un entero positivo")
  }

  # Asegurar que n sea entero
  n <- as.integer(n)

  # Calculo de R1 y R2
  R1 <- A1 / P
  R2 <- A2 / P

  # Calculo de alpha y beta
  alpha <- (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2)
  beta <- (alpha * (1 - R1)) / R1

  # Validar que alpha y beta sean validos
  if (is.na(alpha) || is.na(beta) || alpha <= 0 || beta <= 0) {
    stop("No se pudieron calcular parametros validos con los datos proporcionados")
  }

  # Calculo de la distribucion de contactos (P)
  P_dist <- extraDistr::dbbinom(0:n, size = n, alpha = alpha, beta = beta)

  # Calculo de la distribucion acumulada (R)
  R_dist <- sapply(0:n, function(k) sum(P_dist[(k+1):length(P_dist)]))

  # El reach total es 1 menos la probabilidad de 0 contactos
  reach <- 1 - P_dist[1]

  # Eliminar el 0 de las distribuciones finales
  P_sin_cero <- P_dist[-1]
  R_sin_cero <- R_dist[-1]

  return(structure(list(
    reach = list(
      porcentaje = reach * 100,
      personas = reach * P
    ),
    distribucion = list(
      porcentaje = P_sin_cero * 100,
      personas = P_sin_cero * P
    ),
    acumulada = list(
      porcentaje = R_sin_cero * 100,
      personas = R_sin_cero * P
    ),
    parametros = list(
      alpha = alpha,
      beta = beta,
      prob_cero_contactos = P_dist[1] * 100
    )
  ), class = "reach_beta_binomial"))
}

#__________________________________________________________#

#' @export
print.reach_sainsbury <- function(x, ...) {
  cat("MODELO SAINSBURY\n")
  cat("================\n")
  cat("Descripcion: Modelo que considera independencia entre soportes y heterogeneidad de soportes\n\n")

  # Metricas principales
  cat("METRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  # Distribucion de contactos
  cat("\nDISTRIBUCION DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de poblacion que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribucion acumulada
  cat("\nDISTRIBUCION ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de poblacion que recibe N o mas contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf(">= %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadistico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADISTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
}

#' @export
print.reach_binomial <- function(x, ...) {
  cat("MODELO BINOMIAL\n")
  cat("===============\n")
  cat("Descripcion: Modelo que asume independencia entre soportes y homogeneidad\n\n")

  # Metricas principales
  cat("METRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))
  cat(sprintf("Probabilidad media de exposicion: %.3f\n", x$probabilidad_media))

  # Distribucion de contactos
  cat("\nDISTRIBUCION DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de poblacion que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribucion acumulada
  cat("\nDISTRIBUCION ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de poblacion que recibe N o mas contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf(">= %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadistico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADISTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
}

#' @export
print.reach_beta_binomial <- function(x, ...) {
  cat("MODELO BETA-BINOMIAL\n")
  cat("===================\n")
  cat("Descripcion: Modelo que considera heterogeneidad en la poblacion\n\n")

  # Metricas principales
  cat("METRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  # Parametros del modelo
  cat("\nPARAMETROS DEL MODELO:\n")
  cat("---------------------\n")
  cat(sprintf("Alpha: %.3f (forma de la distribucion beta)\n", x$parametros$alpha))
  cat(sprintf("Beta: %.3f (forma de la distribucion beta)\n", x$parametros$beta))
  cat(sprintf("Probabilidad de 0 contactos: %.2f%%\n",
              x$parametros$prob_cero_contactos))

  # Distribucion de contactos
  cat("\nDISTRIBUCION DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de poblacion que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribucion acumulada
  cat("\nDISTRIBUCION ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de poblacion que recibe N o mas contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf(">= %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadistico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADISTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
  cat(sprintf("Media teorica de la distribucion beta: %.3f\n",
              x$parametros$alpha / (x$parametros$alpha + x$parametros$beta)))
}

#__________________________________________________________#

# Linealiza una matriz simetrica (p.ej. de duplicaciones o de oportunidades
# de contacto) recorriendo su triangulo superior, incluyendo la diagonal, en
# el orden (1,1), (1,2), ..., (1,n), (2,2), (2,3), .... Uso interno de
# calc_metheringham().
matriz_a_vector <- function(matriz) {
  n <- nrow(matriz)
  vector <- numeric()
  for(i in 1:n) {
    for(j in i:n) {
      vector <- c(vector, matriz[i,j])
    }
  }
  return(vector)
}

# A partir del numero de inserciones de cada soporte, calcula el numero de
# pares de oportunidades de contacto entre cada par de soportes (fuera de la
# diagonal) y dentro de un mismo soporte (en la diagonal, como combinaciones
# de 2 entre sus propias inserciones). Uso interno de calc_metheringham().
crear_matriz_oportunidades <- function(inserciones) {
  n <- length(inserciones)
  matriz <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n) {
    for(j in i:n) {
      if(i == j) {
        matriz[i,j] <- choose(inserciones[i], 2)
      } else {
        matriz[i,j] <- inserciones[i] * inserciones[j]
        matriz[j,i] <- matriz[i,j]  # Simetria
      }
    }
  }
  return(matriz)
}

#' @encoding UTF-8
#' @title Calculo de metricas segun el modelo de Metheringham
#' @description Calcula metricas fundamentales para la aplicacion del modelo de Metheringham,
#' incluyendo la audiencia media (A1), duplicacion media (D) y audiencia tras la segunda exposicion en
#' el hipotetico soporte promedio (A2). El modelo de Metheringham (1964) se basa en que
#' los individuos tienen probabilidades heterogeneas que se distribuyen como una distribucion Beta para el conjunto.
#' Los soportes son homogeneos (a saber, todos los soportes acaban con la misma distribucion Beta de probabilidades de exposicion).
#' La acumulacion y duplicacion de audiencias se promedian entre los soportes para disenar un soporte hipotetico promedio.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param audiencias Vector numerico con las audiencias de cada soporte
#' @param inserciones Vector numerico con el numero de inserciones por soporte
#' @param matriz_duplicacion Matriz simetrica con los valores de duplicacion entre soportes
#'
#' @details
#' La funcion realiza los siguientes calculos principales:
#' \enumerate{
#'   \item Audiencia media tras la primera insercion (A1):
#'     \itemize{
#'       \item Calcula la media ponderada de audiencias por numero de inserciones
#'       \item Formula: A1 = SUMATORIO(Audiencia_i ? Inserciones_i) / SUMATORIO(Inserciones_i)
#'     }
#'   \item Duplicacion media (D):
#'     \itemize{
#'       \item Calcula la media ponderada de duplicaciones por oportunidades de contacto
#'       \item Considera todas las combinaciones posibles entre soportes ii, ij
#'     }
#'   \item Audiencia tras la segunda insercion (A2):
#'     \itemize{
#'       \item Calcula la audiencia que se expone al menos una vez tras la segunda insercion
#'       \item Formula: A2 = 2 ? A1 - D
#'     }
#' }
#'
#' @return Un objeto de clase 'reach_metheringham' conteniendo:
#' \itemize{
#'   \item audiencia_media: Media ponderada de audiencias (A1)
#'   \item duplicacion_media: Media ponderada de duplicaciones (D)
#'   \item audiencia_segunda: Audiencia tras la segunda insercion (A2)
#'   \item matriz_oportunidades: Matriz que contiene el numero de oportunidades de contacto
#'         entre pares de inserciones
#'   \item vector_oportunidades: Version linealizada de la matriz de oportunidades
#'   \item vector_duplicacion: Version linealizada de la matriz de duplicacion
#' }
#'
#' @note
#' La matriz de duplicacion debe ser simetrica donde:
#' \itemize{
#'   \item La diagonal contiene la duplicacion de cada soporte consigo mismo
#'   \item El elemento `[i,j]` contiene la duplicacion entre los soportes i y j
#'   \item Se debe cumplir que `matriz[i,j] = matriz[j,i]`
#'   \item Para n soportes, la matriz debe ser de dimensiones n x n
#' }
#'
#' @examples
#' # Ejemplo basico con tres soportes
#' matriz_dup <- matrix(c(
#'   150000, 200000, 180000,
#'   200000, 120000, 140000,
#'   180000, 140000, 170000
#' ), nrow = 3, byrow = TRUE)
#'
#' metricas <- calc_metheringham(
#'   audiencias = c(1500000, 800000, 1200000),
#'   inserciones = c(4, 3, 5),
#'   matriz_duplicacion = matriz_dup
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con la distribucion Binomial
#' \code{\link{calc_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribucion de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribucion de Hofmans
# Funcion principal de Metheringham
calc_metheringham <- function(audiencias, inserciones, matriz_duplicacion) {
  if (length(audiencias) != length(inserciones)) {
    stop("Los vectores de audiencias e inserciones deben tener la misma longitud")
  }
  if (any(inserciones < 0) || any(audiencias < 0)) {
    stop("Las audiencias y las inserciones deben ser no negativas")
  }
  if (sum(inserciones) <= 0) {
    stop("El total de inserciones debe ser mayor que 0")
  }

  n_soportes <- length(audiencias)

  if (!is.matrix(matriz_duplicacion)) {
    stop("matriz_duplicacion debe ser una matriz")
  }

  if (nrow(matriz_duplicacion) != n_soportes || ncol(matriz_duplicacion) != n_soportes) {
    stop("Las dimensiones de la matriz de duplicacion no coinciden con el numero de soportes")
  }

  if (!all(matriz_duplicacion == t(matriz_duplicacion))) {
    warning("La matriz de duplicacion no es simetrica. Se utilizara la parte triangular superior.")
    matriz_duplicacion[lower.tri(matriz_duplicacion)] <- t(matriz_duplicacion)[lower.tri(matriz_duplicacion)]
  }

  matriz_oportunidades <- crear_matriz_oportunidades(inserciones)

  vec_duplicacion <- matriz_a_vector(matriz_duplicacion)
  vector_oportunidades <- matriz_a_vector(matriz_oportunidades)

  if (sum(vector_oportunidades) <= 0) {
    stop("No hay oportunidades de contacto entre soportes (revisa las inserciones)")
  }

  A1 <- sum(audiencias * inserciones) / sum(inserciones)
  D <- sum(vec_duplicacion * vector_oportunidades) / sum(vector_oportunidades)
  A2 <- 2 * A1 - D

  resultado <- list(
    audiencia_media = A1,
    duplicacion_media = D,
    audiencia_segunda = A2,
    matriz_oportunidades = matriz_oportunidades,
    vector_oportunidades = vector_oportunidades,
    vector_duplicacion = vec_duplicacion,
    total_inserciones = sum(inserciones)
  )

  class(resultado) <- "reach_metheringham"
  return(resultado)
}


#' @export
print.reach_metheringham <- function(x, ...) {
  cat("Modelo de Metheringham\n")
  cat("---------------------\n")

  cat("\nAUDIENCIA MEDIA (A1):\n")
  cat(sprintf("%.0f personas\n", x$audiencia_media))
  cat("Interpretacion: Audiencia del soporte\n")

  cat("\nDUPLICACION MEDIA (D):\n")
  cat(sprintf("%.0f personas\n", x$duplicacion_media))
  cat("Interpretacion: Numero medio de personas que ven dos inserciones cualesquiera\n")

  cat("\nAUDIENCIA SEGUNDA INSERCION (A2):\n")
  cat(sprintf("%.0f personas\n", x$audiencia_segunda))
  cat("Interpretacion: Audiencia acumulada tras dos inserciones (personas expuestas al menos una vez)\n")

  cat("\nMATRIZ DE OPORTUNIDADES DE CONTACTO:\n")
  print(x$matriz_oportunidades)
  cat("Interpretacion: Numero de pares de inserciones posibles entre soportes\n")
  cat("- Diagonal: Oportunidades de contacto dentro del mismo soporte\n")
  cat("- Fuera diagonal: Oportunidades de contacto entre diferentes soportes\n")

  cat("\nVECTOR DE OPORTUNIDADES:\n")
  print(x$vector_oportunidades)
  cat("Interpretacion: Version linealizada de la matriz de oportunidades\n")
  cat("Orden: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3), ...\n")

  cat("\nHALLAZGOS CLAVE:\n")
  cat(sprintf("- Total de inserciones: %d\n", x$total_inserciones))
  cat(sprintf("- Audiencia promedio por insercion: %.0f personas\n", x$audiencia_media))
  cat(sprintf("- Duplicacion promedio: %.1f%%\n",
              (x$duplicacion_media / x$audiencia_media) * 100))
  cat(sprintf("- Incremento en segunda insercion: %.1f%%\n",
              ((x$audiencia_segunda - x$audiencia_media) / x$audiencia_media) * 100))
}
