
#' @encoding UTF-8
#' @title Calculo de audiencia acumulada segun el modelo de audiencia acumulada de Hofmans
#' @description Implementa el modelo de Hofmans (1966) para calcular la audiencia acumulada
#' de un plan de medios con multiples inserciones en un soporte. El modelo considera
#' la duplicacion entre inserciones, y utiliza un parametro de ajuste (alpha) para mejorar
#' la estimacion de las audiencias acumuladas.
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param R1 Numerico. Cobertura tras la primera insercion (como proporcion entre 0 y 1)
#' @param R2 Numerico. Cobertura tras la segunda insercion (como proporcion entre 0 y 1)
#' @param N Entero. Numero de inserciones para las que calcular la audiencia acumulada
#' @param show_steps Logico. Si TRUE muestra los pasos intermedios del calculo
#'
#' @details
#' El modelo de Hofmans calcula la cobertura acumulada en dos etapas:
#' \enumerate{
#'   \item Utiliza una primera formulacion para calcular R3:
#'     \itemize{
#'       \item R3 = (3R1)^2 / (3R1 + k(2R1-R2)(3 choose 2))
#'       \item donde k = 2R1/R2
#'     }
#'   \item Para N>3 aplica una formulacion mejorada que incorpora un parametro alpha:
#'     \itemize{
#'       \item RN = (NR1)^2 / (NR1 + k*(N-1)^a*(N/2)*d)
#'       \item donde alpha se calcula usando R3
#'       \item y d = 2R1-R2 es la duplicacion entre inserciones
#'     }
#' }
#'
#' El modelo asume:
#' \itemize{
#'   \item Audiencia constante para todas las inserciones
#'   \item Duplicacion constante entre pares de inserciones
#'   \item Comportamiento no lineal de la acumulacion para N > 3
#' }
#'
#' @return Una lista "hofmans_reach" conteniendo:
#' \itemize{
#'   \item resultados: Data frame con:
#'     \itemize{
#'       \item N: Numero de insercion
#'       \item RN: Cobertura acumulada (proporcion)
#'     }
#'   \item parametros: Lista con los parametros calculados:
#'     \itemize{
#'       \item k: Factor k calculado
#'       \item d: Duplicacion entre inserciones
#'       \item alpha: Parametro de ajuste para N>3
#'     }
#'   \item plot: Grafico de la evolucion de la cobertura
#' }
#'
#' @examples
#' # Ejemplo basico con 5 inserciones
#' R1 <- 0.06    # 6% de cobertura primera insercion
#' R2 <- 0.103   # 10.3% de cobertura segunda insercion
#' resultado <- calc_hofmans(R1, R2, N = 5)
#'
#' # Examinar los resultados
#' print(resultado$resultados)
#' print(resultado$parametros)
#'
#' # Ejemplo con validacion de datos
#' \dontrun{
#' R1_invalido <- 1.2  # >100% cobertura
#' resultado <- calc_hofmans(R1_invalido, R2, N = 5)
#' # Generara un error por cobertura invalida
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para estimaciones con la distribucion Beta-Binomial
#' \code{\link{calc_sainsbury}} para estimaciones el modelo de Sainsbury
#' \code{\link{calc_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_metheringham}} para estimaciones con el modelo de Metheringham
#' @importFrom ggplot2 .data
calc_hofmans <- function(R1, R2, N, show_steps=TRUE) {
  # Validacion de inputs
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

  # Calculos iniciales
  k <- 2 * R1 / R2
  d <- 2 * R1 - R2

  if(show_steps) {
    cat("\nPASO 1: Calculos iniciales")
    cat("\n- k = 2R1/R2 =", round(k,4))
    cat("\n- d = 2R1-R2 =", round(d,4))
  }

  # Calcular R3 usando la formula [3.11, Aldas-Manzano, 1998]
  n3 <- 3
  numerator3 <- (n3 * R1)^2
  denominator3 <- n3 * R1 + k * (2*R1-R2) * choose(n3,2)
  R3 <- numerator3/denominator3

  if(show_steps) {
    cat("\n\nPASO 2: Calculo de R3 usando formula [3.11]")
    cat("\n- R3 =", round(R3,4))
  }

  # Calcular alpha usando R3
  alpha <- log((3*R1-R3)*R2/((2*R1-R2)*R3))/log(2)

  if(show_steps) {
    cat("\n\nPASO 3: Calculo de alpha")
    cat("\n- alpha =", round(alpha,4))
  }

  # Calcular cobertura para cada insercion
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
      # Calcular RN usando la formula final de Hofmans
      numerator <- (n * R1)^2
      denominator <- n * R1 + k * (n-1)^alpha * (n/2) * d
      results$RN[n] <- numerator/denominator
    }
  }

  # Crear grafico (objeto ggplot2 reutilizable, no un efecto secundario de graficado base)
  plot_hofmans <- ggplot2::ggplot(results, ggplot2::aes(x = .data$N, y = .data$RN * 100)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(size = 2, color = "steelblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(.data$RN * 100, 1), "%")),
      vjust = -0.8, size = 3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max(results$RN * 100) * 1.15)) +
    ggplot2::labs(
      x = "Numero de Inserciones (N)",
      y = "Cobertura (%)",
      title = "Evolucion de la Audiencia Acumulada",
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

  # Devolver resultados y grafico
  invisible(structure(list(
    results = results,
    parametros = list(k = k, d = d, alpha = alpha),
    plot = plot_hofmans
  ), class = "reach_hofmans"))
}
