
#__________________________________________________________#

#' @encoding UTF-8
#' @title Impresion editada de resultados del analisis de medios
#' @description Imprime en consola un resumen estructurado de los resultados del analisis
#' de medios, incluyendo combinaciones de soportes, distribucion de contactos y
#' los parametros alpha y beta utilizados.
#'
#' @param data_ls Una lista que debe contener los siguientes elementos:
#' \itemize{
#'   \item resultados: Data frame con las combinaciones mas relevantes de soportes
#'   \item distribucion: Data frame con la distribucion de contactos, incluyendo:
#'     \itemize{
#'       \item cont: Numero de contactos
#'       \item prob: Probabilidad asociada
#'     }
#'   \item alpha: Valor del parametro alpha utilizado en el analisis
#'   \item beta: Valor del parametro beta utilizado en el analisis
#' }
#'
#' @return No retorna valor. Imprime en consola una visualizacion estructurada de:
#' \itemize{
#'   \item Combinaciones mas relevantes de soportes
#'   \item Distribucion de contactos y sus probabilidades
#'   \item Valores de los parametros alpha y beta utilizados
#' }
#'
#' @examples
#' # Crear datos de ejemplo
#' data_ls <- list(
#'   resultados = data.frame(
#'     soporte = c("TV", "Radio", "Digital"),
#'     audiencia = c(1000, 800, 600)
#'   ),
#'   distribucion = data.frame(
#'     cont = 0:3,
#'     prob = c(0.2, 0.3, 0.3, 0.2)
#'   ),
#'   alpha = 0.5,
#'   beta = 0.3
#' )
#'
#' # Imprimir resultados
#' \dontrun{
#' imprimir_resultados(data_ls)
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_R1_R2}} para calculos de coeficientes de duplicacion
imprimir_resultados <- function(data_ls) {
  nombres_resultados <- c(
    "Combinaciones mas relevantes",
    "Distribucion de contactos",
    "Valor Alpha seleccionado",
    "Valor Beta seleccionado"
  )

  cat("\n=== RESULTADOS DEL ANALISIS ===\n")

  for (i in 2:5) {
    cat(sprintf("\n%s:\n", nombres_resultados[i-1]))

    if (is.data.frame(data_ls[[i]])) {
      print(data_ls[[i]], row.names = FALSE)
    } else if (is.numeric(data_ls[[i]])) {
      cat(sprintf("Valor: %.4f\n", data_ls[[i]]))
    } else {
      print(data_ls[[i]])
    }

    cat("\n", paste(rep("-", 50), collapse = ""), "\n")
  }
}


