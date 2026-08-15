#__________________________________________________________#

#' @encoding UTF-8
#' @title Cálculo de cobertura y distribución de contactos (y acumulada) según modelo de Sainsbury
#' @description Implementa el modelo de Sainsbury, desarrollado por E. J. Sansbury en la London Press Exchange,
#' para calcular la cobertura y la distribución de contactos para un conjunto de soportes publicitarios y una única inserción por soporte.
#' El modelo considera la duplicación aleatoria, las probabilidades individuales de exposición homogéneas, y las probabilidades de
#' exposición del soporte heterogéneas para una estimación más precisa de la cobertura y la distribución de contactos (y acumulada).
#' De las dos últimas hipótesis se deriva que la probabilidad de que un individuo resulte expuesto al soporte i vendrá dado por
#' el cociente entre la audiencia del soporte i (casos favorables) y la población (casos totales). Por su parte, de la asunción de la duplicación aleatoria se deriva que
#' la probabilidad de exposición continuará siendo una variable Bernouilli con diferentes probabilidadades de exposición en cada soporte.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param audiencias Vector numérico con las audiencias individuales de cada soporte
#' @param pob_total Tamaño de la población
#'
#' @details
#' El modelo de Sainsbury simplificado calcula:
#' \enumerate{
#'   \item Cobertura considerando la duplicación entre soportes como el producto de las probabilidades individuales
#'   \item Distribución de contactos para cada nivel de exposición i
#'   \item Distribución de contactos acumulada (expuestos al menos i veces)
#' }
#'
#' El proceso incluye:
#' \itemize{
#'   \item Conversión de audiencias a probabilidades
#'   \item Cálculo de las posibles combinaciones de soportes
#'   \item Estimación de probabilidades conjuntas
#'   \item Agregación de resultados: distribución de contactos (y acumulada)
#' }
#'
#' @return Una lista "reach_sainsbury" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en número de personas
#'     }
#'   \item distribucion: Lista con la distribución de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada número de exposiciones
#'       \item personas: Vector con número de personas para cada número de exposiciones
#'     }
#'   \item acumulada: Lista con la distribución acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con número de personas acumuladas al menos i veces
#'     }
#' }
#'
#' @examples
#' # Ejemplo básico con tres soportes
#' audiencias <- c(300000, 400000, 200000)
#' pob_total <- 1000000
#' resultado <- calc_sainsbury(audiencias, pob_total)
#'
#' # Examinar los resultados
#' print(resultado$reach$porcentaje)  # Cobertura en porcentaje
#' print(resultado$distribucion$personas)  # Personas por número de contactos
#'
#' # Ejemplo con validación de datos
#' \dontrun{
#' audiencias_invalidas <- c(300000, -400000, 200000)
#' resultado <- calc_sainsbury(audiencias_invalidas, pob_total)
#' # Generará un error por audiencia negativa
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_binomial}} para estimaciones con la distribución Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribución de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribución de Hofmans
#' @importFrom utils combn
calc_sainsbury <- function(audiencias, pob_total) {
  # Validación de inputs
  if (!is.numeric(audiencias) || !is.numeric(pob_total)) {
    stop("Los argumentos deben ser numéricos")
  }
  if (any(audiencias < 0) || any(audiencias > pob_total)) {
    stop("Las audiencias deben ser positivas y menores que la población")
  }
  if (pob_total <= 0) {
    stop("La población debe ser positiva")
  }

  # Convertir audiencias a probabilidades
  probs <- audiencias / pob_total
  n <- length(probs)

  P <- numeric(n) # Distribución de contactos
  R <- numeric(n) # Distribución acumulada

  # Cálculo de la distribución de contactos (P)
  for(i in 1:n) {
    combs <- combn(n, i)
    P[i] <- sum(apply(combs, 2, function(idx) {
      prob <- 1
      for(j in 1:n) {
        if(j %in% idx) {
          prob <- prob * probs[j]
        } else {
          prob <- prob * (1 - probs[j])
        }
      }
      prob
    }))
  }

  # Cálculo de la distribución acumulada (R)
  for(i in 1:n) {
    R[i] <- sum(P[i:n])
  }

  # Cálculo del reach total
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
#' @title Cálculo de cobertura y distribución de contactos (y acumulada) según modelo Binomial
#' @description Implementa el modelo Binomial, desarrollado por Chandon (1985), para calcular la cobertura y
#' distribución de contactos (y acumulada) de plan de medios de n soportes y una única inserción por soporte.
#' El modelo Binomial asume la duplicación aleatoria (i.e.,la exposición a un soporte no modifica
#' la probabilidad de resultar expuesto a otro), y la homogeneidad de las probabilidades de exposición del soporte y
#' las probabilidades individuales de exposición. Uniendo estas dos hipótesis últimas, la probabilidad de exposición de
#' cualquier individuo a un soporte determinado se calcula como la media de las audiencias de cada soporte.
#' Las probabilidades de exposición son estacionarias respecto al tiempo.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param audiencias Vector numérico con las audiencias individuales de cada soporte
#' @param pob_total Tamaño de la población
#'
#' @details
#' El modelo Bnomial calcula:
#' \enumerate{
#'   \item Cobertura considerando un soporte hipotético “promedio” cuya audiencia es la media simple de las audiencias de cada soporte
#'   \item Distribución de contactos para cada nivel de exposición
#'   \item Distribución de contactos acumulada (expuestos al menos i veces)
#' }
#'
#' La metodología incluye:
#' \itemize{
#'   \item Conversión de audiencias a probabilidades individuales
#'   \item Cálculo de probabilidad media de exposición
#'   \item Aplicación del modelo Binomial para n inserciones
#'   \item Cálculo de distribuciones de contactos (y acumulada)
#' }
#'
#' @return Una lista "reach_binomial" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en número de personas
#'     }
#'   \item distribucion: Lista con la distribución de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada número de exposiciones
#'       \item personas: Vector con número de personas para cada número de exposiciones
#'     }
#'   \item acumulada: Lista con la distribución acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con número de personas acumuladas al menos i veces
#'     }
#' }
#'
#' @examples
#' # Ejemplo básico con tres soportes
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
#' print(paste("Suma distribución:", round(sum_dist, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con la distribución Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribución de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribución de Hofmans
calc_binomial <- function(audiencias, pob_total) {
  # Validación de inputs
  if (!is.numeric(audiencias) || !is.numeric(pob_total)) {
    stop("Los argumentos deben ser numéricos")
  }
  if (any(audiencias < 0) || any(audiencias > pob_total)) {
    stop("Las audiencias deben ser positivas y menores que la población total")
  }
  if (pob_total <= 0) {
    stop("La población total debe ser positiva")
  }

  # Convertir audiencias a probabilidad media
  probs <- audiencias / pob_total
  p <- mean(probs)
  n <- length(audiencias)

  P <- numeric(n) # Distribución de contactos
  R <- numeric(n) # Distribución acumulada

  # Cálculo de la distribución de contactos (P)
  for(i in 1:n) {
    P[i] <- choose(n, i) * p^i * (1-p)^(n-i)
  }

  # Cálculo de la distribución acumulada (R)
  for(i in 1:n) {
    R[i] <- sum(P[i:n])
  }

  # Cálculo del reach total
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
#' @title Cálculo de la cobertura y distribución de contactos (y acumulada) usando modelo Beta-Binomial
#' @description Implementa el modelo Beta-Binomial para calcular la audiencia neta acumulada
#' y la distribución de contactos (y acumulada). El modelo Beta-Binomial considera la
#' heterogeneidad en la probabilidad de exposición de los individuos. Combina dos pasos:
#' modela la probabilidad de éxito aplicando la distribución Beta de parámetros alpha y beta -lo cual reduce a dos
#' los datos necesarios para su estimación; y emplea la probabilidad en la distribución Binomial (combinada con la distribución Beta)
#' para valorar la distribución de contactos (y acumulada). Es útil cuando la probabilidad de
#' éxito no es conocida a priori, y puede variar entre los individuos. Los parámetros alpha y beta precisamente permiten
#' ajustar la forma de la distribución para que refleje la incertidumbre en relación con la probabilidad de éxito.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param A1 Audiencia del soporte tras la primera inserción
#' @param A2 Audiencia del soporte tras la segunda inserción
#' @param P Tamaño total de la población
#' @param n Número total de inserciones planificadas (debe ser entero positivo)
#'
#' @details
#' El modelo Beta-Binomial:
#' \enumerate{
#'   \item Calcula los parámetros alpha y beta a partir de A1 y A2
#'   \item Modela la heterogeneidad en la exposición mediante la distribución Beta
#'   \item Combina la distribución Beta con la Binomial para la distribución de contactos
#'   \item Calcula probabilidades exactas para cada nivel de exposición
#' }
#'
#' El proceso incluye:
#' \itemize{
#'   \item Estimación de coeficientes de duplicación R1 y R2
#'   \item Cálculo de parámetros alpha y beta del modelo
#'   \item Generación de distribución de contactos
#'   \item Cálculo de la distribución de contactos (y acumuladas)
#' }
#'
#' @return Una lista "reach_beta_binomial" conteniendo:
#' \itemize{
#'   \item reach: Lista con la cobertura:
#'     \itemize{
#'       \item porcentaje: Cobertura en porcentaje
#'       \item personas: Cobertura en número de personas
#'     }
#'   \item distribucion: Lista con la distribución de contactos:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidad de cada número de exposiciones
#'       \item personas: Vector con número de personas para cada número de exposiciones
#'     }
#'   \item acumulada: Lista con la distribución acumulada:
#'     \itemize{
#'       \item porcentaje: Vector con probabilidades acumuladas
#'       \item personas: Vector con número de personas acumuladas al menos i veces
#'     }
#'   \item parametros: Lista con parámetros del modelo:
#'     \itemize{
#'       \item alpha: Parámetro alpha estimado
#'       \item beta: Parámetro beta estimado
#'       \item prob_cero_contactos: Probabilidad de no exposición
#'     }
#' }
#'
#' @note
#' El modelo Beta-Binomial es especialmente adecuado cuando:
#' \itemize{
#'   \item Existe heterogeneidad significativa en la población
#'   \item Se dispone de datos de audiencias acumuladas (A1 y A2)
#' }
#'
#' @examples
#' # Ejemplo básico
#' resultado <- calc_beta_binomial(
#'   A1 = 500000,    # Primera audiencia
#'   A2 = 550000,    # Segunda audiencia
#'   P = 1000000,    # Población total
#'   n = 5           # Número de inserciones
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
#' print(paste("Suma distribución:", round(sum_dist +
#'             resultado$parametros$prob_cero_contactos/100, 4)))
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con la distribución Binomial
#' \code{\link{calc_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_metheringham}} para estimaciones con la distribución de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribución de Hofmans
#' \code{\link{calc_nbd}} para el modelo alternativo de heterogeneidad Binomial Negativa (NBD)
calc_beta_binomial <- function(A1, A2, P, n) {
  # Validación de inputs
  if (!all(is.numeric(c(A1, A2, P, n)))) {
    stop("Todos los argumentos deben ser numéricos")
  }
  if (A1 <= 0 || A2 <= 0 || P <= 0) {
    stop("Las audiencias y población deben ser positivas")
  }
  if (A1 > P || A2 > P) {
    stop("Las audiencias no pueden ser mayores que la población total")
  }
  if (n <= 0 || n != round(n)) {
    stop("El número de inserciones debe ser un entero positivo")
  }

  # Asegurar que n sea entero
  n <- as.integer(n)

  # Cálculo de R1 y R2
  R1 <- A1 / P
  R2 <- A2 / P

  # Cálculo de alpha y beta
  alpha <- (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2)
  beta <- (alpha * (1 - R1)) / R1

  # Validar que alpha y beta sean válidos
  if (is.na(alpha) || is.na(beta) || alpha <= 0 || beta <= 0) {
    stop("No se pudieron calcular parámetros válidos con los datos proporcionados")
  }

  # Función auxiliar para calcular la probabilidad beta-binomial
  dbetabinom <- function(x, n, alpha, beta) {
    choose(n, x) * beta(x + alpha, n - x + beta) / beta(alpha, beta)
  }

  # Cálculo de la distribución de contactos (P)
  P_dist <- sapply(0:n, function(k) dbetabinom(x = k, n = n, alpha = alpha, beta = beta))

  # Cálculo de la distribución acumulada (R)
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
  cat("Descripción: Modelo que considera independencia entre soportes y heterogeneidad de soportes\n\n")

  # Métricas principales
  cat("MÉTRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  # Distribución de contactos
  cat("\nDISTRIBUCIÓN DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de población que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribución acumulada
  cat("\nDISTRIBUCIÓN ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de población que recibe N o más contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("≥ %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadístico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADÍSTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
}

#' @export
print.reach_binomial <- function(x, ...) {
  cat("MODELO BINOMIAL\n")
  cat("===============\n")
  cat("Descripción: Modelo que asume independencia entre soportes y homogeneidad\n\n")

  # Métricas principales
  cat("MÉTRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))
  cat(sprintf("Probabilidad media de exposición: %.3f\n", x$probabilidad_media))

  # Distribución de contactos
  cat("\nDISTRIBUCIÓN DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de población que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribución acumulada
  cat("\nDISTRIBUCIÓN ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de población que recibe N o más contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("≥ %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadístico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADÍSTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
}

#' @export
print.reach_beta_binomial <- function(x, ...) {
  cat("MODELO BETA-BINOMIAL\n")
  cat("===================\n")
  cat("Descripción: Modelo que considera heterogeneidad en la población\n\n")

  # Métricas principales
  cat("MÉTRICAS PRINCIPALES:\n")
  cat("--------------------\n")
  cat(sprintf("Cobertura total: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))

  # Parámetros del modelo
  cat("\nPARÁMETROS DEL MODELO:\n")
  cat("---------------------\n")
  cat(sprintf("Alpha: %.3f (forma de la distribución beta)\n", x$parametros$alpha))
  cat(sprintf("Beta: %.3f (forma de la distribución beta)\n", x$parametros$beta))
  cat(sprintf("Probabilidad de 0 contactos: %.2f%%\n",
              x$parametros$prob_cero_contactos))

  # Distribución de contactos
  cat("\nDISTRIBUCIÓN DE CONTACTOS:\n")
  cat("-------------------------\n")
  cat("(Porcentaje de población que recibe exactamente N contactos)\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$distribucion$porcentaje[i],
                x$distribucion$personas[i]))
  }

  # Distribución acumulada
  cat("\nDISTRIBUCIÓN ACUMULADA:\n")
  cat("----------------------\n")
  cat("(Porcentaje de población que recibe N o más contactos)\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("≥ %d contacto%s: %.2f%% (%.0f personas)\n",
                i, ifelse(i == 1, "", "s"),
                x$acumulada$porcentaje[i],
                x$acumulada$personas[i]))
  }

  # Resumen estadístico
  total_contactos <- sum(seq_along(x$distribucion$porcentaje) *
                           x$distribucion$personas)
  contactos_promedio <- total_contactos / sum(x$distribucion$personas)
  cat("\nRESUMEN ESTADÍSTICO:\n")
  cat("-------------------\n")
  cat(sprintf("Promedio de contactos por individuo alcanzado: %.2f\n",
              contactos_promedio))
  cat(sprintf("Media teórica de la distribución beta: %.3f\n",
              x$parametros$alpha / (x$parametros$alpha + x$parametros$beta)))
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Convertir una matriz simétrica en un vector (triángulo superior)
#' @description Linealiza una matriz simétrica (p.ej. de duplicaciones o de
#' oportunidades de contacto) recorriendo su triángulo superior, incluyendo
#' la diagonal, en el orden (1,1), (1,2), ..., (1,n), (2,2), (2,3), ....
#' Función auxiliar empleada por \code{\link{calc_metheringham}}.
#'
#' @param matriz Matriz cuadrada simétrica
#'
#' @return Vector numérico con los elementos del triángulo superior de \code{matriz}
#'
#' @examples
#' m <- matrix(c(1, 2, 2, 3), nrow = 2)
#' matriz_a_vector(m)
#'
#' @export
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

#' @encoding UTF-8
#' @title Crear la matriz de oportunidades de contacto entre soportes
#' @description A partir del número de inserciones de cada soporte, calcula
#' el número de pares de oportunidades de contacto entre cada par de
#' soportes (fuera de la diagonal) y dentro de un mismo soporte (en la
#' diagonal, como combinaciones de 2 entre sus propias inserciones). Función
#' auxiliar empleada por \code{\link{calc_metheringham}}.
#'
#' @param inserciones Vector numérico con el número de inserciones de cada soporte
#'
#' @return Matriz cuadrada simétrica de oportunidades de contacto
#'
#' @examples
#' crear_matriz_oportunidades(c(4, 3, 5))
#'
#' @export
crear_matriz_oportunidades <- function(inserciones) {
  n <- length(inserciones)
  matriz <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n) {
    for(j in i:n) {
      if(i == j) {
        matriz[i,j] <- choose(inserciones[i], 2)
      } else {
        matriz[i,j] <- inserciones[i] * inserciones[j]
        matriz[j,i] <- matriz[i,j]  # Simetría
      }
    }
  }
  return(matriz)
}

#' @encoding UTF-8
#' @title Cálculo de métricas según el modelo de Metheringham
#' @description Calcula métricas fundamentales para la aplicación del modelo de Metheringham,
#' incluyendo la audiencia media (A1), duplicación media (D) y audiencia tras la segunda exposición en
#' el hipotético soporte promedio (A2). El modelo de Metheringham (1964) se basa en que
#' los individuos tienen probabilidades heterogéneas que se distribuyen como una distribución Beta para el conjunto.
#' Los soportes son homogéneos (a saber, todos los soportes acaban con la misma distribución Beta de probabilidades de exposición).
#' La acumulación y duplicación de audiencias se promedian entre los soportes para diseñar un soporte hipotético promedio.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param audiencias Vector numérico con las audiencias de cada soporte
#' @param inserciones Vector numérico con el número de inserciones por soporte
#' @param matriz_duplicacion Matriz simétrica con los valores de duplicación entre soportes
#'
#' @details
#' La función realiza los siguientes cálculos principales:
#' \enumerate{
#'   \item Audiencia media tras la primera inserción (A1):
#'     \itemize{
#'       \item Calcula la media ponderada de audiencias por número de inserciones
#'       \item Fórmula: A1 = SUMATORIO(Audiencia_i × Inserciones_i) / SUMATORIO(Inserciones_i)
#'     }
#'   \item Duplicación media (D):
#'     \itemize{
#'       \item Calcula la media ponderada de duplicaciones por oportunidades de contacto
#'       \item Considera todas las combinaciones posibles entre soportes ii, ij
#'     }
#'   \item Audiencia tras la segunda inserción (A2):
#'     \itemize{
#'       \item Calcula la audiencia que se expone al menos una vez tras la segunda inserción
#'       \item Fórmula: A2 = 2 × A1 - D
#'     }
#' }
#'
#' @return Un objeto de clase 'reach_metheringham' conteniendo:
#' \itemize{
#'   \item audiencia_media: Media ponderada de audiencias (A1)
#'   \item duplicacion_media: Media ponderada de duplicaciones (D)
#'   \item audiencia_segunda: Audiencia tras la segunda inserción (A2)
#'   \item matriz_oportunidades: Matriz que contiene el número de oportunidades de contacto
#'         entre pares de inserciones
#'   \item vector_oportunidades: Versión linealizada de la matriz de oportunidades
#'   \item vector_duplicacion: Versión linealizada de la matriz de duplicación
#' }
#'
#' @note
#' La matriz de duplicación debe ser simétrica donde:
#' \itemize{
#'   \item La diagonal contiene la duplicación de cada soporte consigo mismo
#'   \item El elemento `[i,j]` contiene la duplicación entre los soportes i y j
#'   \item Se debe cumplir que `matriz[i,j] = matriz[j,i]`
#'   \item Para n soportes, la matriz debe ser de dimensiones n x n
#' }
#'
#' @examples
#' # Ejemplo básico con tres soportes
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
#' \code{\link{calc_sainsbury}} para estimaciones con la distribución Binomial
#' \code{\link{calc_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución de Metheringham
#' \code{\link{calc_hofmans}} para estimaciones con la distribución de Hofmans
# Función principal de Metheringham
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
    stop("Las dimensiones de la matriz de duplicación no coinciden con el número de soportes")
  }

  if (!all(matriz_duplicacion == t(matriz_duplicacion))) {
    warning("La matriz de duplicación no es simétrica. Se utilizará la parte triangular superior.")
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
  cat("Interpretación: Audiencia del soporte\n")

  cat("\nDUPLICACIÓN MEDIA (D):\n")
  cat(sprintf("%.0f personas\n", x$duplicacion_media))
  cat("Interpretación: Número medio de personas que ven dos inserciones cualesquiera\n")

  cat("\nAUDIENCIA SEGUNDA INSERCIÓN (A2):\n")
  cat(sprintf("%.0f personas\n", x$audiencia_segunda))
  cat("Interpretación: Audiencia acumulada tras dos inserciones (personas expuestas al menos una vez)\n")

  cat("\nMATRIZ DE OPORTUNIDADES DE CONTACTO:\n")
  print(x$matriz_oportunidades)
  cat("Interpretación: Número de pares de inserciones posibles entre soportes\n")
  cat("- Diagonal: Oportunidades de contacto dentro del mismo soporte\n")
  cat("- Fuera diagonal: Oportunidades de contacto entre diferentes soportes\n")

  cat("\nVECTOR DE OPORTUNIDADES:\n")
  print(x$vector_oportunidades)
  cat("Interpretación: Versión linealizada de la matriz de oportunidades\n")
  cat("Orden: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3), ...\n")

  cat("\nHALLAZGOS CLAVE:\n")
  cat(sprintf("- Total de inserciones: %d\n", x$total_inserciones))
  cat(sprintf("- Audiencia promedio por inserción: %.0f personas\n", x$audiencia_media))
  cat(sprintf("- Duplicación promedio: %.1f%%\n",
              (x$duplicacion_media / x$audiencia_media) * 100))
  cat(sprintf("- Incremento en segunda inserción: %.1f%%\n",
              ((x$audiencia_segunda - x$audiencia_media) / x$audiencia_media) * 100))
}
