
#__________________________________________________________#

#' @encoding UTF-8
#' @title Calculo de los valores R1 y R2 (modelo: Beta-Binomial)
#' @name calc_R1_R2
#'
#' @description Calcula los valores R1 y R2 a partir de los
#' parametros de forma alpha y beta del modelo de audiencia neta acumulada Beta-Binomial.
#' Los valores son clave para evaluar la audiencia neta y la distribucion de contactos (y acumulada).
#' Si la probabilidad de exito se distribuye segun una distribucion beta de parametros alpha y beta,
#' la distribucion de contactos, es una distribucion compuesta: la distribucion beta binomial.
#'
#' @param A Parametro de forma alpha, debe ser numerico y positivo
#' @param B Parametro de forma beta, debe ser numerico y positivo
#'
#' @details
#' Los coeficientes R1 y R2 son medidas de la duplicacion de audiencias:
#' \itemize{
#'   \item R1 mide el tanto por uno de personas alcanzadas tras la primera insercion en el soporte elegido
#'   \item R2 mide el tanto por uno de personas alcanzadas tras la segunda insercion en el soporte elegido
#' }
#'
#' El proceso de calculo:
#' \enumerate{
#'   \item Calcula R1 directamente como A/(A+B)
#'   \item Optimiza R2 mediante un proceso iterativo
#'   \item Verifica que los valores R1 y R2 esten en el rango (0,1)
#' }
#'
#' @return Una lista con dos componentes:
#' \itemize{
#'   \item R1: Coeficiente (tanto por uno) de audiencia acumulada tras la primera insercion
#'   \item R2: Coeficiente (tanto por uno) de audiencia acumulada tras la segunda insercion
#' }
#'
#' @examples
#' # Calcular R1 y R2 para alpha=0.5 y beta=0.3
#' resultados <- calc_R1_R2(0.5, 0.3)
#'
#' # Ver resultados
#' print(paste("R1:", round(resultados$R1, 4)))
#' print(paste("R2:", round(resultados$R2, 4)))
#'
#' # Verificar que los valores estan en el rango esperado
#' stopifnot(resultados$R1 >= 0, resultados$R1 <= 1)
#' stopifnot(resultados$R2 >= 0, resultados$R2 <= 1)
#'
#' @export
#' @seealso
#' \code{\link{calc_beta_binomial}} para estimaciones con el modelo Binomial
#' \code{\link{calc_sainsbury}} para estimaciones con el modelo de Sainsbury
calc_R1_R2 <- function(A, B) {
  if (!is.numeric(A) || !is.numeric(B) || A <= 0 || B <= 0) {
    stop("A y B deben ser numericos y positivos.")
  }

  R1 <- A / (A + B)

  objetivo_R2 <- function(R2) {
    (A - (R1 * (R2 - R1)) / (2 * R1 - R1^2 - R2))^2
  }

  resultado <- stats::optimize(objetivo_R2, c(0, 1))
  R2 <- resultado$minimum

  return(list(R1 = R1, R2 = R2))
}
