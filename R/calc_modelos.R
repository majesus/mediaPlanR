#__________________________________________________________#

#' @encoding UTF-8
#' @title Cálculo de cobertura y distribución de contactos (y acumulada) según modelo de Sainsbury
#' @description Implementa el modelo de Sainsbury simplificado para calcular la cobertura
#' y la distribución de contactos para un conjunto de soportes publicitarios.
#' Este modelo considera la independencia de los soportes (duplicación aleatoria), la homogeneidad de los individuos y
#' la heterogeneidad de los soportes para una estimación más precisa de la cobertura total y la frecuencia de contactos.
#' La probabilidad de que un individuo resulte expuesto al soporte i, vendrá dado por la audiencia del soportei dividido por la población.
#' Mientras que por la hipótesis de duplicación aleatoria, la probabilidad de exposición continuará siendo una variable bemouilli,
#' pero con diferentes probabilidadades de exposición en cada soporte.
#'
#' @param audiencias Vector numérico con las audiencias individuales de cada soporte
#' @param pob_total Tamaño de la población
#'
#' @details
#' El modelo de Sainsbury simplificado calcula:
#' \enumerate{
#'   \item Cobertura considerando la duplicación entre soportes como el producto de las probabilidades individuales
#'   \item Distribución de contactos para cada nivel de exposición
#'   \item Distribución de contactos acumulada (expuestos al menos i veces)
#' }
#'
#' El proceso incluye:
#' \itemize{
#'   \item Conversión de audiencias a probabilidades
#'   \item Cálculo de todas las posibles combinaciones de soportes
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
#' @title Cálculo de cobertura y distribución de contactos (y acumulada) usando modelo Binomial
#' @description Implementa un modelo Binomial para calcular la cobertura y
#' distribución de contactos (y acumulada) en un plan de medios. Este modelo asume independencia
#' entre soportes (duplicación aleatoria), y homogeneidad de los soportes e individuos, y
#' utiliza una probabilidad media de exposición (p). La acumulación de las audiencias es
#' un suceso aleatorio. Finalmente, las probabilidades de exposición son estacionarias respecto al tiempo.
#' Las hipótesis aquí expuestas, llevan pues a que la probabilidad de exposición a distintas inserciones en diferentes soportes,
#' sea equivalente a la de distintas inserciones en un soporte hipotético “promedio” cuya audiencia sea
#' la media simple de las audiencias de cada soporte.
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
#' \code{\link{calc_sainsbury}} para estimaciones con el modelo de Sainsbury
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución Beta-Binomial
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
#' @description Implementa el modelo Beta-Binomial para calcular la cobertura
#' y distribución de contactos (y acumulada) en planes de medios. Este modelo considera la
#' heterogeneidad en la probabilidad de exposición entre individuos. la Beta-Binomial mezcla estos dos pasos:
#' primero modela la probabilidad de éxito usando la distribución beta de parámetros alpha y beta, y luego utiliza esa probabilidad en
#' una distribución binomial para contar cuántos éxitos se obtienen. Esto es útil cuando la probabilidad de
#' éxito no es conocida de antemano y puede variar entre los individuos. Los parámetros alpha y beta nos permiten
#' ajustar la forma de la distribución para que refleje mejor la incertidumbre que tenemos sobre la probabilidad de éxito.
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
#' \code{\link{calc_sainsbury}} para estimaciones con el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con la distribución Binomial
#' \code{\link{calc_R1_R2}} para el cálculo de coeficientes de duplicación
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
  cat("Modelo Sainsbury\n")
  cat("---------------\n")
  cat(sprintf("Reach: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))
  cat("\nDistribución de contactos:\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contactos: %.2f%% (%.0f personas)\n",
                i, x$distribucion$porcentaje[i], x$distribucion$personas[i]))
  }
  cat("\nDistribución acumulada:\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("%d o más contactos: %.2f%% (%.0f personas)\n",
                i, x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }
}

#' @export
print.reach_binomial <- function(x, ...) {
  cat("Modelo Binomial\n")
  cat("--------------\n")
  cat(sprintf("Reach: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))
  cat(sprintf("Probabilidad media: %.3f\n", x$probabilidad_media))
  cat("\nDistribución de contactos:\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contactos: %.2f%% (%.0f personas)\n",
                i, x$distribucion$porcentaje[i], x$distribucion$personas[i]))
  }
  cat("\nDistribución acumulada:\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("%d o más contactos: %.2f%% (%.0f personas)\n",
                i, x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }
}

#' @export
print.reach_beta_binomial <- function(x, ...) {
  cat("Modelo Beta-Binomial\n")
  cat("-------------------\n")
  cat(sprintf("Reach: %.2f%% (%.0f personas)\n",
              x$reach$porcentaje, x$reach$personas))
  cat(sprintf("Parámetros: alpha=%.3f, beta=%.3f\n",
              x$parametros$alpha, x$parametros$beta))
  cat(sprintf("Probabilidad de 0 contactos: %.2f%%\n",
              x$parametros$prob_cero_contactos))
  cat("\nDistribución de contactos:\n")
  for(i in seq_along(x$distribucion$porcentaje)) {
    cat(sprintf("%d contactos: %.2f%% (%.0f personas)\n",
                i, x$distribucion$porcentaje[i], x$distribucion$personas[i]))
  }
  cat("\nDistribución acumulada:\n")
  for(i in seq_along(x$acumulada$porcentaje)) {
    cat(sprintf("%d o más contactos: %.2f%% (%.0f personas)\n",
                i, x$acumulada$porcentaje[i], x$acumulada$personas[i]))
  }
}

#__________________________________________________________#

#' @encoding UTF-8
#' @title Cálculo de métricas según el modelo de Metheringham
#' @description Calcula métricas fundamentales para la aplicación del modelo de Metheringham,
#' incluyendo la audiencia media (A1), duplicación media (D) y audiencia tras la segunda exposición (A2)
#' del hipotético soporte medio. El modelo de Metheringham (1964) se basa en que
#' los individuos tienen probabilidades heterogéneas e independientes de exposición a los soportes,
#' siguiendo una distribución beta; esto convierte la exposición en un proceso de Bernoulli.
#' Además, asume que los soportes son homogéneos, por lo que usa la media de las audiencias para
#' modelar la exposición. La duplicación de audiencias no se considera aleatoria,
#' y al tratar los soportes como homogéneos, la acumulación de m inserciones se modela como
#' una distribución beta-binomial, simplificando el problema de duplicación entre soportes.
#'
#' @param audiencias Vector numérico con las audiencias de cada soporte
#' @param inserciones Vector numérico con el número de inserciones por soporte
#' @param vector_duplicacion Vector numérico con valores de duplicación entre soportes
#' @param ayuda Lógico. Si TRUE, muestra una guía de uso detallada (default: TRUE)
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
#' @return Una lista conteniendo:
#' \itemize{
#'   \item audiencia_media: Media ponderada de audiencias (A1)
#'   \item duplicacion_media: Media ponderada de duplicaciones (D)
#'   \item audiencia_segunda: Audiencia tras la segunda inserción (A2)
#'   \item matriz_oportunidades: Matriz de oportunidades de contacto
#'   \item vector_oportunidades: Vector de oportunidades de contacto
#' }
#'
#' @note
#' El vector de duplicación debe seguir un orden específico:
#' \itemize{
#'   \item Para n soportes, se requieren n*(n+1)/2 valores
#'   \item Los valores se ordenan por filas de la matriz triangular superior
#'   \item Incluye la duplicación de cada soporte consigo mismo
#'   \item El orden sigue el patrón: [1,1], [1,2], [1,3], [2,2], [2,3], [3,3]
#' }
#'
#' @examples
#' # Ejemplo básico con tres soportes
#' metricas <- calc_metheringham(
#'   audiencias = c(1500000, 800000, 1200000),
#'   inserciones = c(4, 3, 5),
#'   vector_duplicacion = c(150000, 200000, 180000,
#'                         120000, 140000,
#'                         170000),
#'   ayuda = FALSE
#' )
#'
#' # Mostrar solo la guía de uso
#' calc_metheringham(
#'   audiencias = NULL,
#'   inserciones = NULL,
#'   vector_duplicacion = NULL,
#'   ayuda = TRUE
#' )
#'
#' @export
#' @seealso
#' \code{\link{calc_sainsbury}} para estimaciones con el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_beta_binomial}} para estimaciones con el modelo Beta-Binomial
calc_metheringham <- function(audiencias, inserciones, vector_duplicacion, ayuda = TRUE) {
  if(ayuda) {
    cat("
    GUÍA PARA INTRODUCIR DATOS DE AUDIENCIA Y DUPLICACIÓN

    1. FORMATO DE ENTRADA:
       - audiencias: Vector numérico con la audiencia de cada soporte
       - inserciones: Vector numérico con número de inserciones por soporte
       - vector_duplicacion: Vector con valores de duplicación entre soportes

    2. TAMAÑO DEL VECTOR DE DUPLICACIÓN:
       Para n soportes, necesitas n*(n+1)/2 valores de duplicación.
       Ejemplo:
       - 2 soportes → 3 valores
       - 3 soportes → 6 valores
       - 4 soportes → 10 valores
       - 5 soportes → 15 valores

    3. ORDEN DEL VECTOR DE DUPLICACIÓN:
       Para 3 soportes, el orden sería:
       [1,1] [1,2] [1,3]
       [2,2] [2,3]
       [3,3]

    4. EJEMPLO DE USO:
       audiencias <- c(1500000, 800000, 1200000)
       inserciones <- c(4, 3, 5)
       vector_duplicacion <- c(150000, 200000, 180000,
                              120000, 140000,
                              170000)
    ")
    return(invisible(NULL))
  }

  # Validación de inputs
  if (length(audiencias) != length(inserciones)) {
    stop("Los vectores de audiencias e inserciones deben tener la misma longitud")
  }

  n_elementos_duplicacion <- length(audiencias) * (length(audiencias) + 1) / 2
  if (length(vector_duplicacion) != n_elementos_duplicacion) {
    stop("La longitud del vector de duplicación no coincide con el número esperado de elementos")
  }

  # Funciones auxiliares internas
  matriz_a_vector <- function(matriz) {
    matriz[upper.tri(matriz, diag = TRUE)]
  }

  calcular_matriz_oportunidades <- function(ins) {
    n <- length(ins)
    matriz <- matrix(0, n, n)
    matriz[upper.tri(matriz, diag = TRUE)] <- c(
      choose(ins, 2),
      outer(ins, ins, "*")[upper.tri(matriz)]
    )
    matriz
  }

  # Cálculos principales
  matriz_oportunidades <- calcular_matriz_oportunidades(inserciones)
  vector_oportunidades <- matriz_a_vector(matriz_oportunidades)

  A1 <- sum(audiencias * inserciones) / sum(inserciones)
  D <- sum(vector_duplicacion * vector_oportunidades) / sum(vector_oportunidades)
  A2 <- 2 * A1 - D

  # Resultados
  return(list(
    audiencia_media = A1,
    duplicacion_media = D,
    audiencia_segunda = A2,
    matriz_oportunidades = matriz_oportunidades,
    vector_oportunidades = vector_oportunidades
  ))
}

# Datos de ejemplo
audiencias <- c(1500000, 800000, 1200000, 900000)  # Audiencias por soporte
inserciones <- c(4, 3, 5, 2)  # Número de inserciones por soporte

# Vector de duplicación (valores para matriz triangular superior)
# Para 4 soportes necesitamos 10 valores de duplicación:
# [1,1] [1,2] [1,3] [1,4]
# [2,2] [2,3] [2,4]
# [3,3] [3,4]
# [4,4]
vector_duplicacion <- c(
  150000,  # [1,1]
  200000,  # [1,2]
  180000,  # [1,3]
  160000,  # [1,4]
  120000,  # [2,2]
  140000,  # [2,3]
  130000,  # [2,4]
  170000,  # [3,3]
  150000,  # [3,4]
  110000   # [4,4]
)
