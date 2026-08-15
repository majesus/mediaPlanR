#' @encoding UTF-8
#' @title Cálculo de métricas de soportes para un plan publicitario
#' @description Calcula métricas de un plan de medios aceptando tanto datos
#' desde CSV como vectores directamente, permitiendo sobrescribir valores específicos
#' del CSV con vectores. Las métricas calculadas incluyen:
#' \enumerate{
#'   \item RP: Alcance ponderado por inserciones y población
#'   \item SOV: Share of Voice como % del total de GRP
#'   \item CPM: Coste por mil contactos
#'   \item C/RP: Coste por punto de RP
#'   \item Audiencia Útil: Audiencia ajustada por índice de utilidad
#'   \item Coste por Contacto Útil: Tarifa dividida entre la audiencia útil
#' }
#'
#' @param soportes Vector de nombres o nombre de la columna en CSV
#' @param audiencias Vector numérico o nombre de la columna en CSV
#' @param tarifas Vector numérico o nombre de la columna en CSV
#' @param ind_utilidad Vector numérico o nombre de la columna en CSV
#' @param inserciones Vector numérico o nombre de la columna en CSV (opcional)
#' @param pob_total Tamaño de la población objetivo
#' @param file Ruta al archivo CSV (opcional)
#' @param sep Separador usado en el CSV (default: ",")
#'
#' @return Un data.frame con las siguientes columnas:
#' \itemize{
#'   \item Soporte: Nombre del medio
#'   \item Audiencia_miles: Audiencia en miles de lectores
#'   \item Numero_Inserciones: Número de inserciones
#'   \item RP: Rating Points
#'   \item SOV: Share of Voice
#'   \item Tarifa_Pag_Color: Tarifa página color
#'   \item CPM: Coste por mil
#'   \item C_RP: Coste por Rating Point
#'   \item Ind_Utilidad: Índice de utilidad
#'   \item Audiencia_Util: Audiencia útil en miles
#'   \item Coste_Contacto_Util: Coste por contacto útil
#' }
#'
#' @examples
#' # Ejemplo 1: Usando solo vectores
#' resultado <- calcular_metricas_medios(
#'   soportes = c("El País", "El Mundo"),
#'   audiencias = c(1520000, 780000),
#'   tarifas = c(39800, 35600),
#'   ind_utilidad = c(1.2, 1.1),
#'   pob_total = 39500000
#' )
#'
#' \dontrun{
#' # Ejemplo 2: Usando CSV con nombres de columnas por defecto
#' resultado <- calcular_metricas_medios(
#'   file = "data.csv",
#'   soportes = "soportes",
#'   audiencias = "audiencias",
#'   tarifas = "tarifas",
#'   ind_utilidad = "ind_utilidad",
#'   pob_total = 39500000
#' )
#'
#' # Ejemplo 3: Combinando CSV con vector de inserciones personalizado
#' resultado <- calcular_metricas_medios(
#'   file = "data.csv",
#'   soportes = "soportes",
#'   audiencias = "audiencias",
#'   tarifas = "tarifas",
#'   ind_utilidad = "ind_utilidad",
#'   inserciones = c(2, 3, 1, 2),  # Sobrescribe las inserciones del CSV
#'   pob_total = 39500000
#' )
#' }
#'
#' @export
#' @seealso
#' \code{\link{calc_cpm}} para cálculo de costes por mil (CPM)
#' \code{\link{calc_grps}} para cálculo de GRPs
#' @importFrom utils read.csv
calcular_metricas_medios <- function(soportes = NULL,
                                     audiencias = NULL,
                                     tarifas = NULL,
                                     ind_utilidad = NULL,
                                     inserciones = NULL,
                                     pob_total,
                                     file = NULL,
                                     sep = ",") {
  # Variables para almacenar los datos finales
  datos_finales <- list()
  # Si se proporciona un archivo, leer los datos base
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("El archivo ", file, " no existe")
    }
    datos <- read.csv(file, sep = sep, stringsAsFactors = FALSE)
    # Procesar cada columna del CSV
    columnas <- list(
      soportes = soportes,
      audiencias = audiencias,
      tarifas = tarifas,
      ind_utilidad = ind_utilidad,
      inserciones = inserciones
    )
    # Para cada campo, usar el vector si se proporciona, si no, intentar leer del CSV
    for (nombre in names(columnas)) {
      valor <- get(nombre)
      # Un vector se usa directamente salvo que sea un unico caracter, en
      # cuyo caso se interpreta como el nombre de una columna del CSV (esto
      # incluye el caso de un unico soporte, donde el vector de datos
      # tambien tiene longitud 1)
      if (!is.null(valor) && !(is.character(valor) && length(valor) == 1)) {
        datos_finales[[nombre]] <- valor
      } else if (is.character(valor) && length(valor) == 1) {
        # Si es nombre de columna, leer del CSV
        if (!valor %in% names(datos)) {
          if (nombre == "inserciones") {
            # Para inserciones, usar 1 por defecto si no existe
            datos_finales[[nombre]] <- rep(1, nrow(datos))
          } else {
            stop("La columna '", valor, "' no existe en el CSV")
          }
        } else {
          datos_finales[[nombre]] <- if(nombre %in% c("audiencias", "tarifas", "ind_utilidad", "inserciones")) {
            as.numeric(datos[[valor]])
          } else {
            datos[[valor]]
          }
        }
      } else if (is.null(valor) && nombre == "inserciones") {
        # Caso especial para inserciones
        if ("inserciones" %in% names(datos)) {
          datos_finales[[nombre]] <- as.numeric(datos[["inserciones"]])
        } else {
          datos_finales[[nombre]] <- rep(1, nrow(datos))
        }
      }
    }

    # Guardar la columna de duplicación solo para los soportes especificados
    if ("duplicacion" %in% names(datos)) {
      # Crear un vector de duplicación del mismo tamaño que los soportes especificados
      duplicacion_filtrada <- numeric(length(datos_finales$soportes))
      # Para cada soporte especificado, buscar su valor de duplicación en el CSV
      for (i in seq_along(datos_finales$soportes)) {
        soporte_actual <- datos_finales$soportes[i]
        indice <- which(datos$soportes == soporte_actual)  # Cambiado de datos$Soporte a datos$soportes
        if (length(indice) > 0) {
          duplicacion_filtrada[i] <- datos$duplicacion[indice[1]]
        }
      }
      datos_finales$duplicacion <- duplicacion_filtrada
    }
  } else {
    # Si no hay archivo, usar los vectores directamente
    datos_finales <- list(
      soportes = soportes,
      audiencias = audiencias,
      tarifas = tarifas,
      ind_utilidad = ind_utilidad,
      inserciones = if(is.null(inserciones)) rep(1, length(soportes)) else inserciones
    )
  }
  # Validar que tenemos todos los datos necesarios
  if (any(sapply(datos_finales[c("soportes", "audiencias", "tarifas", "ind_utilidad")], is.null))) {
    stop("Faltan datos requeridos. Proporciona todos los vectores o nombres de columnas válidos")
  }
  # Validar longitudes iguales
  n <- length(datos_finales$soportes)
  if (!all(sapply(datos_finales, length) == n)) {
    stop("Todos los vectores deben tener la misma longitud")
  }
  # Calcular métricas
  RP <- (datos_finales$audiencias * datos_finales$inserciones / pob_total) * 100
  GRP_total <- sum(RP)
  SOV <- (RP / GRP_total) * 100
  CPM <- (datos_finales$tarifas / datos_finales$audiencias) * 1000
  CRP <- datos_finales$tarifas / RP
  audiencia_util <- datos_finales$ind_utilidad * datos_finales$audiencias
  coste_contacto_util <- datos_finales$tarifas / audiencia_util
  # Crear el data.frame de salida
  tabla_medios <- data.frame(
    Soporte = datos_finales$soportes,
    Audiencia_miles = datos_finales$audiencias,
    Numero_Inserciones = datos_finales$inserciones,
    RP = round(RP, 2),
    SOV = round(SOV, 2),
    Tarifa_Pag_Color = datos_finales$tarifas,
    CPM = round(CPM, 2),
    C_RP = round(CRP, 2),
    Indice_Utilidad = datos_finales$ind_utilidad,
    Audiencia_Util = round(audiencia_util, 0),
    Coste_Contacto_Util = round(coste_contacto_util, 2)
  )

  # Añadir la columna de duplicación si existe
  if (!is.null(datos_finales$duplicacion)) {
    tabla_medios$Duplicacion <- round(datos_finales$duplicacion, 2)  # Redondeado a 2 decimales
  }

  return(tabla_medios)
}


