
#' @encoding UTF-8
#' @title Cálculo de audiencia acumulada según el modelo de audiencia acumulada de Hofmans
#' @description Implementa el modelo de Hofmans (1966) para calcular la audiencia acumulada
#' de un plan de medios con múltiples inserciones en un soporte. El modelo considera
#' la duplicación entre inserciones, y utiliza un parámetro de ajuste (alpha) para mejorar
#' la estimación de las audiencias acumuladas.
#'
#' @references
#' Aldás Manzano, J. (1998). Modelos de determinación de la cobertura y la distribución de
#' contactos en la planificación de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, España.
#'
#' @param R1 Numérico. Cobertura tras la primera inserción (como proporción entre 0 y 1)
#' @param R2 Numérico. Cobertura tras la segunda inserción (como proporción entre 0 y 1)
#' @param N Entero. Número de inserciones para las que calcular la audiencia acumulada
#' @param show_steps Lógico. Si TRUE muestra los pasos intermedios del cálculo
#'
#' @details
#' El modelo de Hofmans calcula la cobertura acumulada en dos etapas:
#' \enumerate{
#'   \item Utiliza una primera formulación para calcular R3:
#'     \itemize{
#'       \item R3 = (3R1)^2 / (3R1 + k(2R1-R2)(3 choose 2))
#'       \item donde k = 2R1/R2
#'     }
#'   \item Para N>3 aplica una formulación mejorada que incorpora un parámetro alpha:
#'     \itemize{
#'       \item RN = (NR1)^2 / (NR1 + k*(N-1)^a*(N/2)*d)
#'       \item donde alpha se calcula usando R3
#'       \item y d = 2R1-R2 es la duplicación entre inserciones
#'     }
#' }
#'
#' El modelo asume:
#' \itemize{
#'   \item Audiencia constante para todas las inserciones
#'   \item Duplicación constante entre pares de inserciones
#'   \item Comportamiento no lineal de la acumulación para N > 3
#' }
#'
#' @return Una lista "hofmans_reach" conteniendo:
#' \itemize{
#'   \item resultados: Data frame con:
#'     \itemize{
#'       \item N: Número de inserción
#'       \item RN: Cobertura acumulada (proporción)
#'     }
#'   \item parametros: Lista con los parámetros calculados:
#'     \itemize{
#'       \item k: Factor k calculado
#'       \item d: Duplicación entre inserciones
#'       \item alpha: Parámetro de ajuste para N>3
#'     }
#'   \item plot: Gráfico de la evolución de la cobertura
#' }
#'
#' @examples
#' # Ejemplo básico con 5 inserciones
#' R1 <- 0.06    # 6% de cobertura primera inserción
#' R2 <- 0.103   # 10.3% de cobertura segunda inserción
#' resultado <- calc_hofmans(R1, R2, N = 5)
#'
#' # Examinar los resultados
#' print(resultado$resultados)
#' print(resultado$parametros)
#'
#' # Ejemplo con validación de datos
#' \dontrun{
#' R1_invalido <- 1.2  # >100% cobertura
#' resultado <- calc_hofmans(R1_invalido, R2, N = 5)
#' # Generará un error por cobertura inválida
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribución Beta-Binomial
#' \code{\link{calc_sainsbury}} para estimaciones el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_metheringham}} para estimaciones con el modelo de Metheringham
#' @importFrom ggplot2 .data
calc_hofmans <- function(R1, R2, N, show_steps=TRUE) {
  # Validación de inputs
  if(any(c(R1, R2) > 1) || R1 <= 0 || R2 <= 0) {
    stop("R1 y R2 deben ser mayores que 0 y como maximo 1")
  }
  if(N < 3 || N != round(N)) {
    stop("N debe ser un entero mayor o igual que 3")
  }
  if(R2 <= R1) {
    stop("La cobertura debe ser creciente: R1 < R2")
  }
  if(abs(2 * R1 - R2) < 1e-9) {
    stop("2*R1 - R2 es practicamente 0: el modelo de Hofmans no esta definido para estos valores de R1 y R2 (division por cero)")
  }

  # Cálculos iniciales
  k <- 2 * R1 / R2
  d <- 2 * R1 - R2

  if(show_steps) {
    cat("\nPASO 1: Cálculos iniciales")
    cat("\n- k = 2R1/R2 =", round(k,4))
    cat("\n- d = 2R1-R2 =", round(d,4))
  }

  # Calcular R3 usando la fórmula [3.11, Aldás-Manzano, 1998]
  n3 <- 3
  numerator3 <- (n3 * R1)^2
  denominator3 <- n3 * R1 + k * (2*R1-R2) * choose(n3,2)
  R3 <- numerator3/denominator3

  if(show_steps) {
    cat("\n\nPASO 2: Cálculo de R3 usando fórmula [3.11]")
    cat("\n- R3 =", round(R3,4))
  }

  # Calcular alpha usando R3
  alpha <- log((3*R1-R3)*R2/((2*R1-R2)*R3))/log(2)

  if(show_steps) {
    cat("\n\nPASO 3: Cálculo de alpha")
    cat("\n- alpha =", round(alpha,4))
  }

  # Calcular cobertura para cada inserción
  results <- data.frame(
    N = 1:N,
    RN = numeric(N)
  )

  for(n in 1:N) {
    if(n == 1) {
      results$RN[n] <- R1
    } else if(n == 2) {
      results$RN[n] <- R2
    } else if(n == 3) {
      results$RN[n] <- R3
    } else {
      # Calcular RN usando la fórmula final de Hofmans
      numerator <- (n * R1)^2
      denominator <- n * R1 + k * (n-1)^alpha * (n/2) * d
      results$RN[n] <- numerator/denominator
    }
  }

  # Crear gráfico (objeto ggplot2 reutilizable, no un efecto secundario de graficado base)
  plot_hofmans <- ggplot2::ggplot(results, ggplot2::aes(x = .data$N, y = .data$RN * 100)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(size = 2, color = "steelblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(.data$RN * 100, 1), "%")),
      vjust = -0.8, size = 3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max(results$RN * 100) * 1.15)) +
    ggplot2::labs(
      x = "Número de Inserciones (N)",
      y = "Cobertura (%)",
      title = "Evolución de la Audiencia Acumulada",
      subtitle = "Modelo de Hofmans"
    ) +
    ggplot2::theme_minimal()

  if(show_steps) {
    cat("\n\nRESULTADOS:\n")
    print(data.frame(
      N = results$N,
      Cobertura = paste0(round(results$RN * 100, 2), "%")
    ))

    cat("\nVALIDACIONES:")
    cat("\n- Cobertura siempre creciente:", all(diff(results$RN) >= 0))
    cat("\n- Coberturas entre 0 y 1:", all(results$RN >= 0 & results$RN <= 1))
    cat("\n- R1, R2 coinciden con inputs:",
        all.equal(c(results$RN[1:2]), c(R1, R2)))
  }

  # Devolver resultados y gráfico
  invisible(list(
    results = results,
    plot = plot_hofmans
  ))
}
