#' @encoding UTF-8
#' @title Calcular parametros de la Distribucion Beta-Binomial (BBD)
#' @description Calcula alpha y beta para la distribucion Beta-Binomial mediante
#' el metodo de los momentos a partir de las coberturas tras la primera y segunda
#' insercion (R1, R2).
#'
#' @param R1 Numerico. Cobertura tras la primera insercion (0-1)
#' @param R2 Numerico. Cobertura tras la segunda insercion (0-1)
#'
#' @details
#' La implementacion representa explicitamente dos limites. Si R2 coincide con
#' el alcance bajo independencia, devuelve el limite binomial
#' (`alpha = beta = Inf`). Si R2 coincide con R1, devuelve el limite polarizado
#' (`alpha = beta = 0`), donde cada persona tiene propension cero o uno.
#'
#' @return Lista con los componentes:
#' \itemize{
#'   \item alpha: Parametro alpha de la BBD
#'   \item beta: Parametro beta de la BBD
#'   \item p: Probabilidad media de exposicion
#'   \item type: `beta_binomial`, `binomial_limit` o `polarized_limit`
#' }
#'
#' @examples
#' params <- calculate_bbd_params(0.4902, 0.5805)
#'
#' @export
calculate_bbd_params <- function(R1, R2) {
  if (!is.numeric(R1) || !is.numeric(R2) || length(R1) != 1L ||
      length(R2) != 1L || !is.finite(R1) || !is.finite(R2) ||
      R1 <= 0 || R1 > 1 || R2 <= 0 || R2 > 1) {
    stop("R1 y R2 deben ser numericos y estar en el intervalo (0, 1]")
  }
  if (R2 < R1) {
    stop("R2 no puede ser menor que R1 (la cobertura debe ser no decreciente)")
  }

  independence_limit <- 2 * R1 - R1^2
  if (R2 > independence_limit + 1e-10) {
    stop("R2 is incompatible with a Beta-Binomial exposure model: it exceeds the independence limit")
  }

  denom <- 2 * R1 - R2 - R1^2
  if (abs(denom) < 1e-9) {
    return(list(alpha = Inf, beta = Inf, p = R1, type = "binomial_limit"))
  }
  if (abs(R2 - R1) < 1e-10) {
    return(list(alpha = 0, beta = 0, p = R1, type = "polarized_limit"))
  }

  alpha <- R1 * (R2 - R1) / denom
  beta <- alpha * (1 - R1) / R1
  if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
    stop("R1 and R2 do not imply valid Beta-Binomial parameters")
  }
  list(alpha = alpha, beta = beta, p = R1, type = "beta_binomial")
}

#' @encoding UTF-8
#' @title Calcular media y varianza de la Distribucion Beta-Binomial
#' @description Calcula la media y la varianza de una Beta-Binomial dados
#' el numero de ensayos y sus parametros de forma.
#'
#' @param k Entero. Numero de inserciones (ensayos)
#' @param alpha Numerico. Parametro alpha de la BBD
#' @param beta Numerico. Parametro beta de la BBD
#'
#' @return Lista con los componentes:
#' \itemize{
#'   \item mean: Media de la distribucion
#'   \item variance: Varianza de la distribucion
#' }
#'
#' @examples
#' mv <- calculate_mean_variance(2, 0.5, 1.2)
#'
#' @export
calculate_mean_variance <- function(k, alpha, beta) {
  mean_val <- k * alpha / (alpha + beta)
  variance <- k * alpha * beta * (alpha + beta + k) /
    ((alpha + beta)^2 * (alpha + beta + 1))
  list(mean = mean_val, variance = variance)
}

#' @encoding UTF-8
#' @title Calcular la probabilidad marginal Beta-Binomial
#' @description Calcula, en espacio logaritmico por estabilidad numerica, la
#' probabilidad de obtener x exitos en k ensayos bajo una distribucion
#' Beta-Binomial de parametros alpha y beta.
#'
#' @param x Entero. Numero de exitos (contactos)
#' @param k Entero. Numero de ensayos (inserciones)
#' @param alpha Numerico. Parametro alpha de la BBD
#' @param beta Numerico. Parametro beta de la BBD
#'
#' @return Numerico. Probabilidad P(X = x)
#'
#' @examples
#' prob <- calculate_marginal_prob(2, 5, 0.5, 1.2)
#'
#' @export
calculate_marginal_prob <- function(x, k, alpha, beta) {
  if (x < 0 || x > k) return(0)

  log_num <- lgamma(k + 1) + lgamma(alpha + beta) +
    lgamma(alpha + x) + lgamma(beta + k - x)
  log_den <- lgamma(x + 1) + lgamma(k - x + 1) + lgamma(alpha) +
    lgamma(beta) + lgamma(alpha + beta + k)

  exp(log_num - log_den)
}

#' @encoding UTF-8
#' @title Calcular la duplicacion (correlacion) entre dos vehiculos
#' @description Calcula el coeficiente de correlacion entre las exposiciones
#' a dos vehiculos a partir de su probabilidad de exposicion conjunta y sus
#' probabilidades marginales de exposicion.
#'
#' @param pij Numerico. Probabilidad de exposicion conjunta a ambos vehiculos
#' @param pi Numerico. Probabilidad marginal de exposicion al primer vehiculo
#' @param pj Numerico. Probabilidad marginal de exposicion al segundo vehiculo
#'
#' @return Numerico. Coeficiente de correlacion (entre -1 y 1); 0 si la
#' varianza de alguno de los dos vehiculos es nula.
#'
#' @examples
#' corr <- calculate_duplication(0.15, 0.3, 0.4)
#'
#' @export
calculate_duplication <- function(pij, pi, pj) {
  eps <- 1e-10
  if (pi < eps || pi > 1 - eps || pj < eps || pj > 1 - eps) return(0)

  denominator <- sqrt(pi * (1 - pi) * pj * (1 - pj))
  if (denominator < eps) return(0)

  (pij - pi * pj) / denominator
}

#' @encoding UTF-8
#' @title Transformar la matriz de duplicaciones en matriz de correlaciones
#' @description Convierte la matriz de duplicaciones brutas (proporcion de
#' poblacion expuesta simultaneamente a cada par de vehiculos) en la matriz
#' de coeficientes de correlacion que el modelo CANEX usa como termino de
#' ajuste de la probabilidad conjunta.
#'
#' @param duplications Matriz cuadrada. Duplicaciones brutas entre vehiculos (0-1)
#' @param vehicles_data Data frame con, al menos, la columna R1 de cada vehiculo
#'
#' @return Matriz de coeficientes de correlacion (diagonal = 1)
#'
#' @examples
#' dup_matrix <- matrix(c(1, 0.0157, 0.0157, 1), nrow = 2)
#' vehicles <- data.frame(R1 = c(0.4902, 0.033))
#' correlations <- transform_duplications(dup_matrix, vehicles)
#'
#' @export
transform_duplications <- function(duplications, vehicles_data) {
  m <- nrow(duplications)
  correlations <- diag(1, m)

  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (i != j) {
        correlations[i, j] <- calculate_duplication(
          duplications[i, j], vehicles_data$R1[i], vehicles_data$R1[j]
        )
      }
    }
  }
  correlations
}

#' @encoding UTF-8
#' @title Validar los datos de entrada del modelo CANEX
#' @description Comprueba que los datos de vehiculos, la matriz de
#' duplicaciones y la poblacion cumplen los requisitos minimos del modelo
#' CANEX, y estima el tamano de la rejilla de combinaciones de exposicion
#' que sera necesario evaluar.
#' @param vehicles_data Data frame con columnas k, R1, R2
#' @param duplications Matriz cuadrada de duplicaciones brutas
#' @param poblacion Entero positivo. Tamano de la poblacion objetivo
#' @return Invisible \code{NULL}. La funcion se invoca por sus efectos
#' (lanza un error si algun requisito no se cumple).
#' @keywords internal
#' @noRd
validate_canex_inputs <- function(vehicles_data, duplications, poblacion) {
  required_cols <- c("k", "R1", "R2")
  if (!is.data.frame(vehicles_data) || !all(required_cols %in% names(vehicles_data))) {
    stop("vehicles_data debe ser un data.frame con las columnas k, R1 y R2")
  }
  m <- nrow(vehicles_data)
  if (m < 1) {
    stop("vehicles_data debe contener al menos un vehiculo")
  }
  if (any(vehicles_data$k < 1) || any(vehicles_data$k != round(vehicles_data$k))) {
    stop("k debe ser un numero entero positivo para cada vehiculo")
  }
  if (any(vehicles_data$R1 <= 0) || any(vehicles_data$R1 > 1) ||
      any(vehicles_data$R2 <= 0) || any(vehicles_data$R2 > 1)) {
    stop("R1 y R2 deben estar en el intervalo (0, 1] para cada vehiculo")
  }
  if (any(vehicles_data$R2 < vehicles_data$R1)) {
    stop("R2 no puede ser menor que R1 para ningun vehiculo")
  }
  if (!is.matrix(duplications) || nrow(duplications) != m || ncol(duplications) != m) {
    stop("duplications debe ser una matriz cuadrada de dimension igual al numero de vehiculos (", m, ")")
  }
  if (any(duplications < 0 | duplications > 1)) {
    stop("Todos los valores de la matriz de duplicaciones deben estar entre 0 y 1")
  }
  if (!isTRUE(all.equal(duplications, t(duplications), check.attributes = FALSE))) {
    warning("La matriz de duplicaciones no es simetrica; se usara su parte triangular superior")
  }
  if (!is.numeric(poblacion) || length(poblacion) != 1 || poblacion <= 0) {
    stop("poblacion debe ser un unico numero positivo")
  }

  if (m >= 2L) {
    for (i in seq_len(m - 1L)) {
      for (j in (i + 1L):m) {
        pij <- duplications[i, j]
        lower <- max(0, vehicles_data$R1[i] + vehicles_data$R1[j] - 1)
        upper <- min(vehicles_data$R1[i], vehicles_data$R1[j])
        if (pij < lower - 1e-10 || pij > upper + 1e-10) {
          stop(sprintf("Duplication [%d,%d] is outside its feasible Frechet bounds [%.6f, %.6f]",
                       i, j, lower, upper), call. = FALSE)
        }
      }
    }
    corr <- transform_duplications(duplications, vehicles_data)
    corr[lower.tri(corr)] <- t(corr)[lower.tri(corr)]
    min_eigenvalue <- min(eigen(corr, symmetric = TRUE, only.values = TRUE)$values)
    if (min_eigenvalue < -1e-8) {
      stop(sprintf("Pairwise duplications imply a non-positive-semidefinite correlation matrix (minimum eigenvalue %.6g)",
                   min_eigenvalue), call. = FALSE)
    }
  }

  total_combinations <- prod(vehicles_data$k + 1)
  if (total_combinations > 2e6) {
    stop(sprintf(
      paste(
        "El numero de combinaciones de exposicion (%.0f) excede el limite",
        "practico de calculo (2.000.000). Reduce el numero de vehiculos o",
        "el numero de inserciones (k) por vehiculo; este es un limite",
        "computacional conocido del modelo CANEX para pautas extensas."
      ),
      total_combinations
    ))
  }

  invisible(NULL)
}

#' @encoding UTF-8
#' @title Calcular el modelo CANEX (Canonical Expansion Model)
#' @description Implementa el modelo de expansion canonica de Danaher (1991)
#' para calcular la distribucion de alcance y frecuencia de una pauta
#' multivehiculo, considerando explicitamente la heterogeneidad de exposicion
#' de cada vehiculo (via Beta-Binomial) y las duplicaciones observadas entre
#' vehiculos (via un termino de correlacion de segundo orden).
#'
#' @param vehicles_data Data frame con los datos de cada vehiculo de medios:
#' \itemize{
#'   \item k: Numero de inserciones del vehiculo (entero positivo)
#'   \item R1: Cobertura tras la primera insercion (0-1)
#'   \item R2: Cobertura tras la segunda insercion (0-1)
#' }
#' @param duplications Matriz cuadrada donde el elemento \verb{[i,j]} es la
#' proporcion de poblacion expuesta simultaneamente a los vehiculos i y j
#' @param poblacion Entero. Tamano de la poblacion objetivo (por defecto 1.000.000)
#'
#' @details
#' El modelo sigue estos pasos:
#' \enumerate{
#'   \item Calcula los parametros Beta-Binomial (alpha, beta) de cada vehiculo
#'   \item Transforma la matriz de duplicaciones brutas en correlaciones
#'   \item Genera la distribucion de probabilidad conjunta de exposicion,
#'   truncando a cero las probabilidades negativas que puede producir la
#'   expansion canonica truncada, y renormalizando el resultado para que la
#'   masa de probabilidad total vuelva a sumar 1
#'   \item Agrega la distribucion conjunta por numero total de contactos y
#'   calcula las metricas de alcance y frecuencia
#' }
#'
#' El numero de combinaciones de exposicion a evaluar crece de forma
#' exponencial con el numero de vehiculos y de inserciones
#' (\code{prod(k_i + 1)}), por lo que el modelo esta pensado para pautas de
#' tamano pequeno o mediano; para pautas extensas la funcion se detiene con
#' un error informativo en lugar de agotar la memoria disponible.
#'
#' @return Un objeto de clase \code{"reach_canex"}: una lista con los
#' componentes:
#' \itemize{
#'   \item total_reach: Proporcion de poblacion alcanzada (0-1)
#'   \item total_reach_people: Numero de personas alcanzadas
#'   \item distribution: Data frame con columnas contacts, percentage, people
#'   \item cumulative: Data frame con columnas min_contacts, percentage, people
#'   \item stats: Lista con avg_contacts (contactos medios entre alcanzados)
#'   y zero_contacts_prob (probabilidad de cero contactos)
#'   \item diagnostics: Masa negativa truncada, masa previa a la
#'   renormalizacion y menor autovalor de la matriz de correlaciones. Estos
#'   valores permiten evaluar cuanto corrigio la aproximacion de segundo orden.
#' }
#'
#' @examples
#' # Dos vehiculos (valores tomados de un caso de referencia del modelo)
#' vehicles <- data.frame(
#'   k = c(2, 2),
#'   R1 = c(0.4902, 0.033),
#'   R2 = c(0.5805, 0.0502)
#' )
#' duplications <- matrix(
#'   c(1.000, 0.0157,
#'     0.0157, 1.000),
#'   nrow = 2, byrow = TRUE
#' )
#' results <- calc_canex(vehicles, duplications)
#' print(results)
#'
#' # Tres vehiculos con poblacion personalizada
#' vehicles2 <- data.frame(
#'   k = c(2, 2, 2),
#'   R1 = c(0.4902, 0.033, 0.0300),
#'   R2 = c(0.5805, 0.0502, 0.0371)
#' )
#' duplications2 <- matrix(
#'   c(1.000, 0.0157, 0.0139,
#'     0.0157, 1.000, 0.0003,
#'     0.0139, 0.0003, 1.000),
#'   nrow = 3, byrow = TRUE
#' )
#' results2 <- calc_canex(vehicles2, duplications2, poblacion = 500000)
#' total_reach <- results2$total_reach
#' avg_contacts <- results2$stats$avg_contacts
#'
#' @seealso
#' \code{\link{calculate_bbd_params}} para el calculo de parametros BBD
#' \code{\link{transform_duplications}} para la transformacion de la matriz de duplicacion
#' \code{\link{calc_beta_binomial}} para el modelo univariante (un solo vehiculo)
#'
#' @references
#' Danaher, P. J. (1991). A canonical expansion model for multivariate media
#' exposure distributions: A generalization of the "duplication of viewing
#' law". Journal of Marketing Research, 28(3), 361-367.
#'
#' @importFrom stats aggregate
#' @export
calc_canex <- function(vehicles_data, duplications, poblacion = 1000000) {
  validate_canex_inputs(vehicles_data, duplications, poblacion)

  m <- nrow(vehicles_data)
  correlations <- transform_duplications(duplications, vehicles_data)
  correlation_matrix <- correlations
  correlation_matrix[lower.tri(correlation_matrix)] <-
    t(correlation_matrix)[lower.tri(correlation_matrix)]
  min_eigenvalue <- min(eigen(correlation_matrix, symmetric = TRUE,
                              only.values = TRUE)$values)

  bbd_params <- lapply(seq_len(m), function(i) {
    calculate_bbd_params(vehicles_data$R1[i], vehicles_data$R2[i])
  })
  mv_params <- lapply(seq_len(m), function(i) {
    params <- bbd_params[[i]]
    if (params$type == "binomial_limit") {
      list(mean = vehicles_data$k[i] * params$p,
           variance = vehicles_data$k[i] * params$p * (1 - params$p))
    } else if (params$type == "polarized_limit") {
      list(mean = vehicles_data$k[i] * params$p,
           variance = vehicles_data$k[i]^2 * params$p * (1 - params$p))
    } else {
      calculate_mean_variance(vehicles_data$k[i], params$alpha, params$beta)
    }
  })

  precalculated_marginals <- lapply(seq_len(m), function(i) {
    params <- bbd_params[[i]]
    vapply(0:vehicles_data$k[i], function(x) {
      if (params$type == "binomial_limit") {
        stats::dbinom(x, vehicles_data$k[i], params$p)
      } else if (params$type == "polarized_limit") {
        if (x == 0) 1 - params$p else if (x == vehicles_data$k[i]) params$p else 0
      } else {
        calculate_marginal_prob(x, vehicles_data$k[i], params$alpha, params$beta)
      }
    }, numeric(1))
  })

  var_sqrt_inv <- vapply(mv_params, function(p) {
    if (p$variance > 1e-9) 1 / sqrt(p$variance) else 0
  }, numeric(1))
  means <- vapply(mv_params, function(p) p$mean, numeric(1))

  exposure_grid <- as.matrix(expand.grid(lapply(vehicles_data$k, function(k) 0:k)))

  # Probabilidades marginales de toda la rejilla (vectorizado por vehiculo)
  marginals_matrix <- vapply(seq_len(m), function(i) {
    precalculated_marginals[[i]][exposure_grid[, i] + 1]
  }, numeric(nrow(exposure_grid)))

  base_prob <- rep(1, nrow(exposure_grid))
  for (i in seq_len(m)) base_prob <- base_prob * marginals_matrix[, i]

  # Termino de ajuste por duplicaciones (interacciones canonicas de 2o orden)
  z_scores <- matrix(0, nrow = nrow(exposure_grid), ncol = m)
  for (i in seq_len(m)) {
    z_scores[, i] <- (exposure_grid[, i] - means[i]) * var_sqrt_inv[i]
  }

  dup_term <- rep(0, nrow(exposure_grid))
  if (m >= 2) {
    for (i in 1:(m - 1)) {
      for (j in (i + 1):m) {
        if (var_sqrt_inv[i] != 0 && var_sqrt_inv[j] != 0) {
          dup_term <- dup_term + correlations[i, j] * z_scores[, i] * z_scores[, j]
        }
      }
    }
  }

  base_prob[!is.finite(base_prob)] <- 0
  dup_term[!is.finite(dup_term)] <- 0

  # Probabilidad conjunta ajustada, truncada a 0 (ver @details). Diagnostics
  # expose how much the second-order approximation had to be corrected.
  raw_joint_prob <- base_prob * (1 + dup_term)
  negative_mass <- -sum(pmin(raw_joint_prob, 0))
  joint_prob <- pmax(0, raw_joint_prob)

  total_exposures <- rowSums(exposure_grid)
  agg <- stats::aggregate(joint_prob, by = list(exposures = total_exposures), FUN = sum)
  names(agg) <- c("exposures", "probability")

  report <- calculate_metrics(agg, poblacion)
  report$diagnostics <- list(
    negative_mass_truncated = negative_mass,
    mass_before_renormalization = sum(joint_prob),
    correlation_min_eigenvalue = if (m >= 2L) min_eigenvalue else 1
  )
  report
}

#' @encoding UTF-8
#' @title Calcular metricas de alcance y frecuencia CANEX
#' @description A partir de una distribucion de probabilidad por numero total
#' de contactos, calcula el alcance, la distribucion de contactos (y
#' acumulada) y las metricas resumen del modelo CANEX.
#'
#' @param distribution Data frame con columnas \code{exposures} (numero de
#' contactos) y \code{probability} (probabilidad asociada, no necesariamente
#' normalizada a 1)
#' @param poblacion Entero. Tamano de la poblacion objetivo (por defecto 1.000.000)
#'
#' @return Un objeto de clase \code{"reach_canex"} (ver \code{\link{calc_canex}}).
#'
#' @examples
#' dist <- data.frame(exposures = 0:2, probability = c(0.3, 0.5, 0.2))
#' metrics <- calculate_metrics(dist, 1000000)
#'
#' @export
calculate_metrics <- function(distribution, poblacion = 1000000) {
  if (!is.data.frame(distribution) ||
      !all(c("exposures", "probability") %in% names(distribution)) ||
      anyNA(distribution[c("exposures", "probability")]) ||
      any(distribution$probability < 0) || !0 %in% distribution$exposures) {
    stop("distribution must contain non-negative probabilities and an explicit zero-contact row",
         call. = FALSE)
  }
  if (!is.numeric(poblacion) || length(poblacion) != 1L ||
      !is.finite(poblacion) || poblacion <= 0) {
    stop("poblacion must be one positive finite number", call. = FALSE)
  }
  distribution <- distribution[order(distribution$exposures), ]

  # La expansion canonica truncada puede dejar la masa de probabilidad total
  # por debajo de 1 tras truncar a 0 las probabilidades negativas; se
  # renormaliza para que la distribucion vuelva a sumar 1.
  total_prob <- sum(distribution$probability)
  if (total_prob > 0 && abs(total_prob - 1) > 1e-6) {
    distribution$probability <- distribution$probability / total_prob
  }

  distribution$percentage <- distribution$probability * 100
  distribution$people <- round(distribution$probability * poblacion)

  cumulative_people <- vapply(distribution$exposures, function(n) {
    sum(distribution$people[distribution$exposures >= n])
  }, numeric(1))

  cumulative_dist <- data.frame(
    min_contacts = distribution$exposures,
    people = cumulative_people,
    percentage = (cumulative_people / poblacion) * 100
  )

  reach_prob <- 1 - distribution$probability[1]
  reach_people <- round(reach_prob * poblacion)

  avg_contacts <- if (reach_prob > 1e-9) {
    sum(distribution$exposures * distribution$probability) / reach_prob
  } else {
    0
  }

  report <- list(
    total_reach = reach_prob,
    total_reach_people = reach_people,
    distribution = data.frame(
      contacts = distribution$exposures,
      percentage = distribution$percentage,
      people = distribution$people
    ),
    cumulative = data.frame(
      min_contacts = cumulative_dist$min_contacts,
      percentage = cumulative_dist$percentage,
      people = cumulative_dist$people
    ),
    stats = list(
      avg_contacts = avg_contacts,
      zero_contacts_prob = distribution$probability[1]
    )
  )

  class(report) <- "reach_canex"
  report
}

#' @encoding UTF-8
#' @title Imprimir un objeto reach_canex
#' @description Genera un informe formateado con las metricas del modelo CANEX.
#'
#' @param x Objeto de clase \code{"reach_canex"}, resultado de \code{\link{calc_canex}}
#' @param ... Argumentos adicionales (no usados)
#'
#' @return Invisible \code{x}. La funcion se invoca por su efecto de impresion.
#'
#' @examples
#' dist <- data.frame(exposures = 0:2, probability = c(0.3, 0.5, 0.2))
#' metrics <- calculate_metrics(dist)
#' print(metrics)
#'
#' @export
print.reach_canex <- function(x, ...) {
  cat("\nMODELO CANEX (Canonical Expansion)")
  cat("\n===================================")
  cat("\nDescripcion: Modelo que considera heterogeneidad y duplicaciones entre vehiculos\n")

  cat("\nMETRICAS PRINCIPALES:")
  cat("\n--------------------")
  cat(sprintf("\nCobertura total: %.2f%% (%.0f personas)\n",
              x$total_reach * 100, x$total_reach_people))

  cat("\nDISTRIBUCION DE CONTACTOS:")
  cat("\n-------------------------")
  cat("\n(Porcentaje de poblacion que recibe exactamente N contactos)")
  for (i in seq_len(nrow(x$distribution))) {
    contacts <- x$distribution$contacts[i]
    cat(sprintf("\n%d contacto%s: %.2f%% (%.0f personas)",
                contacts, ifelse(contacts == 1, "", "s"),
                x$distribution$percentage[i], x$distribution$people[i]))
  }

  cat("\n\nDISTRIBUCION ACUMULADA:")
  cat("\n-----------------------")
  cat("\n(Porcentaje de poblacion que recibe al menos N contactos)")
  for (i in seq_len(nrow(x$cumulative))) {
    min_contacts <- x$cumulative$min_contacts[i]
    cat(sprintf("\n>= %d contacto%s: %.2f%% (%.0f personas)",
                min_contacts, ifelse(min_contacts == 1, "", "s"),
                x$cumulative$percentage[i], x$cumulative$people[i]))
  }

  cat("\n\nRESUMEN ESTADISTICO:")
  cat("\n-------------------")
  cat(sprintf("\nPromedio de contactos por individuo alcanzado: %.2f",
              x$stats$avg_contacts))
  cat(sprintf("\nProbabilidad de 0 contactos: %.2f%%",
              x$stats$zero_contacts_prob * 100))
  cat("\n")

  invisible(x)
}
