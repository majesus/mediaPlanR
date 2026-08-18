#' @encoding UTF-8
#' @title Optimizacion de planes de medios con restricciones presupuestarias y de cobertura
#' @description Encuentra la combinacion de soportes que alcanza un objetivo de
#' cobertura para una Frecuencia Efectiva Minima (FEM) determinada, respetando
#' un presupuesto maximo. Mantiene la firma y el formato de salida historicos,
#' pero delega el calculo en el motor v2 (\code{\link{media_plan}} y
#' \code{\link{optimize_media_plan}}): si el espacio de combinaciones es
#' manejable se evalua de forma exacta y se certifica el optimo global; en
#' espacios grandes se usa una heuristica voraz, marcada explicitamente como
#' tal en el resultado. Permite excluir soportes concretos y trabajar con
#' audiencia bruta o con audiencia util (composicion de target, no indice de
#' afinidad).
#'
#' @references
#' Aldas Manzano, J. (1998). Modelos de determinacion de la cobertura y la distribucion de
#' contactos en la planificacion de medios publicitarios impresos. Tesis doctoral, Universidad de Valencia, Espana.
#'
#' @param soportes_df Data frame con columnas: soportes (character), audiencias (numeric), tarifas (numeric).
#'        Si se usa audiencia util, debe incluir tambien indices_utilidad (numeric, proporcion 0-1)
#' @param FEM Frecuencia Efectiva Minima requerida (numero entero positivo)
#' @param objetivo_cobertura Cobertura objetivo a alcanzar (porcentaje o personas, segun tolerancia_cobertura_tipo)
#' @param presupuesto_max Presupuesto maximo disponible
#' @param poblacion_total Tamano de la poblacion objetivo (por defecto 47000000)
#' @param tam_batch Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param soportes_vetados Vector de caracteres con nombres de soportes a excluir (opcional)
#' @param modelo Modelo a utilizar para el calculo de cobertura: "sainsbury" (independencia exacta,
#'        via convolucion de Poisson-binomial) o "binomial" (por defecto "sainsbury")
#' @param usar_audiencia_util Logical indicando si usar audiencia util (TRUE) o bruta (FALSE, por defecto).
#'        Con TRUE, \code{indices_utilidad} debe ser una proporcion de composicion de target entre 0 y 1
#' @param tol Tolerancia para comparacion de numeros flotantes al verificar el presupuesto
#' @param tolerancia_presupuesto_tipo Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param tolerancia_cobertura_tipo "porcentaje" (por defecto) o "personas": unidad de \code{objetivo_cobertura}
#' @param tolerancia_presupuesto Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param tolerancia_cobertura Argumento heredado por compatibilidad; no afecta al calculo en v2
#'
#' @return Una lista conteniendo:
#' \itemize{
#'   \item exito: Logical indicando si se encontro una solucion factible
#'   \item cobertura_alcanzada: Porcentaje de cobertura logrado
#'   \item coste_total: Coste total del plan
#'   \item soportes_seleccionados: Data frame con los soportes del plan optimo e inserciones asignadas
#'   \item plan_completo: Vector con las inserciones asignadas a cada soporte de \code{soportes_df}
#'   \item objetivo_alcanzado: Logical indicando si se alcanzo el objetivo de cobertura
#'   \item evaluacion: Lista con el detalle de presupuesto, cobertura, FEM y si el optimo es global
#'   \item distribucion: Lista con distribucion de contactos y acumulada del plan resultante
#'   \item optimization: Objeto \code{optimized_media_plan} completo devuelto por \code{\link{optimize_media_plan}}
#'   \item soportes_vetados, soportes_no_encontrados: Igual que en la version historica
#' }
#'
#' @examples
#' datos_util <- data.frame(
#'   soportes = c("Medio1", "Medio2", "Medio3"),
#'   audiencias = c(1000000, 800000, 600000),
#'   tarifas = c(50000, 40000, 30000),
#'   indices_utilidad = c(0.60, 0.55, 0.45)
#' )
#'
#' resultado_util <- optimize_media_sb(
#'   soportes_df = datos_util,
#'   FEM = 2,
#'   objetivo_cobertura = 10,
#'   presupuesto_max = 120000,
#'   poblacion_total = 2000000,
#'   modelo = "binomial",
#'   usar_audiencia_util = TRUE
#' )
#' resultado_util$cobertura_alcanzada
#'
#' @export
#' @seealso
#' \code{\link{optimize_media_plan}} para el motor v2 subyacente
#' \code{\link{calc_sainsbury}} para el modelo de Sainsbury
#' \code{\link{calc_binomial}} para el modelo Binomial
optimize_media_sb <- function(soportes_df,
                              FEM,
                              objetivo_cobertura,
                              presupuesto_max,
                              poblacion_total = 47000000,
                              tam_batch = 5,
                              soportes_vetados = NULL,
                              modelo = c("sainsbury", "binomial"),
                              usar_audiencia_util = FALSE,
                              tol = 1e-10,
                              tolerancia_presupuesto_tipo = "porcentaje",
                              tolerancia_cobertura_tipo = "porcentaje",
                              tolerancia_presupuesto = 0.10,
                              tolerancia_cobertura = 0.05) {
  modelo <- match.arg(modelo)
  required <- c("soportes", "audiencias", "tarifas")
  if (!is.data.frame(soportes_df) || !all(required %in% names(soportes_df))) {
    stop("soportes_df must contain soportes, audiencias and tarifas", call. = FALSE)
  }
  original_names <- as.character(soportes_df$soportes)
  not_found <- character()
  if (!is.null(soportes_vetados)) {
    not_found <- setdiff(soportes_vetados, original_names)
    soportes_df <- soportes_df[!soportes_df$soportes %in% soportes_vetados, , drop = FALSE]
  }
  if (!nrow(soportes_df)) stop("No channels remain after exclusions", call. = FALSE)

  audiences <- soportes_df$audiencias
  if (usar_audiencia_util) {
    if (!"indices_utilidad" %in% names(soportes_df)) {
      stop("usar_audiencia_util requires indices_utilidad", call. = FALSE)
    }
    if (any(soportes_df$indices_utilidad < 0 | soportes_df$indices_utilidad > 1)) {
      stop("In v2, indices_utilidad must be target composition proportions between 0 and 1; affinity indices cannot be multiplied by audience", call. = FALSE)
    }
    audiences <- audiences * soportes_df$indices_utilidad
  }
  target <- if (tolerancia_cobertura_tipo == "personas") {
    objetivo_cobertura / poblacion_total
  } else {
    objetivo_cobertura / 100
  }
  if (target < 0 || target > 1) stop("Invalid coverage target", call. = FALSE)

  p <- media_plan(data.frame(
    channel = as.character(soportes_df$soportes),
    audience = audiences,
    insertions = rep(1L, nrow(soportes_df)),
    cost_per_insertion = soportes_df$tarifas
  ), population = poblacion_total)

  fit <- tryCatch(
    optimize_media_plan(
      p, budget = presupuesto_max, objective = "min_cost",
      target_reach = target, effective_frequency = FEM,
      max_insertions = rep(1L, nrow(soportes_df)),
      model = if (modelo == "sainsbury") "independent" else "binomial",
      method = "auto"
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    best <- optimize_media_plan(
      p, budget = presupuesto_max, objective = "max_reach",
      effective_frequency = FEM,
      max_insertions = rep(1L, nrow(soportes_df)),
      model = if (modelo == "sainsbury") "independent" else "binomial",
      method = "auto"
    )
    return(list(
      exito = FALSE,
      mensaje = conditionMessage(fit),
      mejor_plan_factible = best,
      cobertura_alcanzada = 100 * best$effective_reach,
      coste_total = best$spend,
      presupuesto_max = presupuesto_max,
      objetivo_cobertura = objetivo_cobertura,
      soportes_vetados = soportes_vetados,
      soportes_no_encontrados = not_found
    ))
  }

  selected <- fit$allocation > 0
  table <- soportes_df[selected, , drop = FALSE]
  table$inserciones <- unname(fit$allocation[selected])
  legacy_distribution <- list(
    reach = list(
      porcentaje = fit$reach$reach$percent,
      personas = fit$reach$reach$people
    ),
    distribucion = list(
      porcentaje = fit$reach$distribution$percent[-1L],
      personas = fit$reach$distribution$people[-1L]
    ),
    acumulada = list(
      porcentaje = fit$reach$cumulative$percent[-1L],
      personas = fit$reach$cumulative$people[-1L]
    )
  )
  list(
    exito = TRUE,
    cobertura_alcanzada = 100 * fit$effective_reach,
    coste_total = fit$spend,
    soportes_seleccionados = table,
    plan_completo = unname(fit$allocation),
    objetivo_alcanzado = fit$target_met,
    presupuesto_cumplido = fit$spend <= presupuesto_max + tol,
    evaluacion = list(
      presupuesto = list(disponible = presupuesto_max, utilizado = fit$spend,
                         cumplido = fit$spend <= presupuesto_max + tol),
      cobertura = list(objetivo = objetivo_cobertura,
                       alcanzada = 100 * fit$effective_reach,
                       cumplido = fit$target_met),
      FEM = list(requerida = FEM, poblacion_alcanzada = 100 * fit$effective_reach),
      global_optimum = fit$global_optimum
    ),
    distribucion = legacy_distribution,
    optimization = fit,
    soportes_vetados = soportes_vetados,
    soportes_no_encontrados = not_found,
    tipo_audiencia = if (usar_audiencia_util) "util" else "bruta",
    modelo_usado = modelo
  )
}

#' @encoding UTF-8
#' @title Calculo de metricas de soportes para un plan publicitario
#' @description Calcula metricas de eficiencia de un plan de medios aceptando
#' datos desde CSV o directamente como vectores. Mantiene la firma historica,
#' pero delega el calculo en \code{\link{media_plan}} y
#' \code{\link{plan_metrics}}. \code{ind_utilidad} es la composicion de
#' target del soporte (proporcion entre 0 y 1 de la audiencia bruta que
#' pertenece al target), nunca un indice de afinidad: multiplicar la
#' audiencia bruta por un indice de afinidad (que puede superar 100) produce
#' una audiencia util mayor que la propia audiencia bruta, que es el error
#' que esta funcion evita exigiendo dicho rango.
#'
#' @param soportes Vector de nombres o nombre de la columna en CSV
#' @param audiencias Vector numerico o nombre de la columna en CSV
#' @param tarifas Vector numerico o nombre de la columna en CSV
#' @param ind_utilidad Composicion de target de cada soporte, como proporcion
#'   entre 0 y 1 (no un indice de afinidad), o nombre de la columna en CSV
#' @param inserciones Vector numerico o nombre de la columna en CSV (opcional; 1 por defecto)
#' @param pob_total Tamano de la poblacion objetivo
#' @param file Ruta al archivo CSV (opcional)
#' @param sep Separador usado en el CSV (default: ",")
#'
#' @return Un data.frame con columnas Soporte, Audiencia, Audiencia_miles,
#' Numero_Inserciones, RP (rating points), GRP_Share, SOV (alias historico de
#' GRP_Share; no representa share of voice competitivo), Tarifa_Insercion,
#' Coste_Total, CPM, C_RP (coste por rating point), Composicion_Target,
#' Audiencia_Util (personas del target en la audiencia bruta) y CPM_Util
#' (coste por mil impactos en el target).
#'
#' @examples
#' resultado <- calcular_metricas_medios(
#'   soportes = c("El Pais", "El Mundo"),
#'   audiencias = c(1520000, 780000),
#'   tarifas = c(39800, 35600),
#'   ind_utilidad = c(0.60, 0.50),
#'   pob_total = 39500000
#' )
#'
#' @export
#' @seealso
#' \code{\link{plan_metrics}} para el motor v2 subyacente
#' \code{\link{calc_cpm}} para calculo de costes por mil (CPM)
#' \code{\link{calc_grps}} para calculo de GRPs
calcular_metricas_medios <- function(soportes = NULL,
                                     audiencias = NULL,
                                     tarifas = NULL,
                                     ind_utilidad = NULL,
                                     inserciones = NULL,
                                     pob_total,
                                     file = NULL,
                                     sep = ",") {
  if (!is.null(file)) {
    if (!file.exists(file)) stop("File does not exist: ", file, call. = FALSE)
    raw <- utils::read.csv(file, sep = sep, stringsAsFactors = FALSE)
    resolve <- function(value, default_name, numeric = FALSE) {
      if (is.null(value)) value <- default_name
      out <- if (is.character(value) && length(value) == 1L && value %in% names(raw)) {
        raw[[value]]
      } else value
      if (numeric) as.numeric(out) else out
    }
    soportes <- resolve(soportes, "soportes")
    audiencias <- resolve(audiencias, "audiencias", TRUE)
    tarifas <- resolve(tarifas, "tarifas", TRUE)
    ind_utilidad <- resolve(ind_utilidad, "ind_utilidad", TRUE)
    inserciones <- if (is.null(inserciones) && !"inserciones" %in% names(raw)) {
      rep(1L, nrow(raw))
    } else resolve(inserciones, "inserciones", TRUE)
  }
  if (is.null(inserciones)) inserciones <- rep(1L, length(soportes))
  values <- list(soportes, audiencias, tarifas, ind_utilidad, inserciones)
  if (any(vapply(values, is.null, logical(1)))) stop("Missing required inputs", call. = FALSE)
  if (length(unique(vapply(values, length, integer(1)))) != 1L) {
    stop("All input vectors must have the same length", call. = FALSE)
  }
  if (any(ind_utilidad < 0 | ind_utilidad > 1)) {
    stop("ind_utilidad is target composition and must be between 0 and 1", call. = FALSE)
  }
  p <- media_plan(data.frame(
    channel = soportes, audience = audiencias, insertions = inserciones,
    cost_per_insertion = tarifas, target_audience = audiencias * ind_utilidad
  ), population = pob_total, target_audience = "target_audience")
  m <- plan_metrics(p)$by_channel
  out <- data.frame(
    Soporte = m$channel,
    Audiencia = m$audience,
    Audiencia_miles = m$audience / 1000,
    Numero_Inserciones = m$insertions,
    RP = round(m$rating_points, 2),
    GRP_Share = round(100 * m$grp_share, 2),
    SOV = round(100 * m$grp_share, 2),
    Tarifa_Insercion = m$cost_per_insertion,
    Coste_Total = m$spend,
    CPM = round(m$cpm_impressions, 2),
    C_RP = round(m$cost_per_rating_point, 2),
    Composicion_Target = ind_utilidad,
    Audiencia_Util = round(m$target_audience, 0),
    CPM_Util = ifelse(m$target_impressions > 0,
                      round(m$spend / m$target_impressions * 1000, 2), NA_real_)
  )
  attr(out, "SOV_definition") <- "Deprecated alias of GRP_Share; not competitive share of voice"
  out
}

legacy_bbd_calibration <- function(Pob, threshold, cob_efectiva, A1,
                                   max_inserciones, type, tolerancia) {
  if (!is.numeric(Pob) || length(Pob) != 1L || Pob <= 0 ||
      !is.numeric(cob_efectiva) || length(cob_efectiva) != 1L ||
      cob_efectiva <= 0 || cob_efectiva > Pob ||
      !is.numeric(A1) || length(A1) != 1L || A1 <= 0 || A1 >= Pob) {
    stop("Pob, cob_efectiva and A1 are not compatible", call. = FALSE)
  }
  fit <- calibrate_bbd(
    first_reach = A1 / Pob,
    target_reach = cob_efectiva / Pob,
    frequency = threshold,
    max_insertions = max_inserciones,
    type = type,
    tolerance = tolerancia * cob_efectiva / Pob
  )
  positive <- fit$distribution[-1L, , drop = FALSE]
  combinations <- fit$candidates
  combinations$alpha <- fit$first_reach * exp(combinations$log_concentration)
  combinations$beta <- (1 - fit$first_reach) * exp(combinations$log_concentration)
  combinations$prob <- combinations$predicted * Pob
  combinations$distancia_objetivo <- combinations$error * Pob
  list(
    cob_efectiva = cob_efectiva,
    Pob = Pob,
    mejores_combinaciones = combinations[order(combinations$error), ],
    mejores_combinaciones_top_10 = utils::head(combinations[order(combinations$error), ], 10),
    data = data.frame(
      inserciones = positive$contacts,
      d_probabilidad = positive$probability,
      dc_acumulada = positive$cumulative_probability
    ),
    alpha = fit$alpha,
    beta = fit$beta,
    n_optimo = fit$insertions,
    calibration = fit
  )
}

#' @encoding UTF-8
#' @title Calibracion Beta-Binomial para una frecuencia efectiva exacta
#' @description Busca el numero de inserciones y los parametros
#' Beta-Binomial (alpha, beta) que reproducen una cobertura objetivo para
#' exactamente \code{FE} contactos, fijando la audiencia tras la primera
#' insercion. Mantiene la firma historica, pero delega el ajuste en
#' \code{\link{calibrate_bbd}}, que calibra unicamente la concentracion de la
#' Beta (la media queda fijada por \code{A1/Pob}) en lugar de recorrer una
#' rejilla bidimensional de alpha y beta.
#'
#' @param Pob Tamano de la poblacion
#' @param FE Frecuencia efectiva (FE, numero objetivo exacto de impactos por persona)
#' @param cob_efectiva Numero objetivo de personas a alcanzar con exactamente FE contactos
#' @param A1 Audiencia tras la primera insercion
#' @param max_inserciones Numero de inserciones maximo a considerar
#' @param tolerancia Margen de error permitido, como proporcion de \code{cob_efectiva} (default: 0.05)
#' @param step_A Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param step_B Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param batch_size Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param min_soluciones Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param error_aceptable Argumento heredado por compatibilidad; no afecta al calculo en v2
#'
#' @return Una lista con los siguientes componentes:
#' \itemize{
#'   \item mejores_combinaciones: Data frame con las combinaciones (n, alpha,
#'         beta, prob, distancia_objetivo) evaluadas para cada numero de
#'         inserciones, ordenadas por error
#'   \item mejores_combinaciones_top_10: Las 10 mejores combinaciones segun ese criterio
#'   \item data: Data frame con la distribucion final, con columnas
#'         inserciones, d_probabilidad y dc_acumulada
#'   \item alpha, beta: Parametros Beta-Binomial de la solucion seleccionada
#'   \item n_optimo: Numero optimo de inserciones
#'   \item calibration: Objeto \code{bbd_calibration} completo devuelto por \code{\link{calibrate_bbd}}
#' }
#'
#' @examples
#' resultado <- optimizar_d(
#'   Pob = 100000, FE = 3, cob_efectiva = 20000, A1 = 50000, max_inserciones = 5
#' )
#' resultado$alpha
#' resultado$beta
#'
#' @seealso
#' \code{\link{calibrate_bbd}} para el motor v2 subyacente
#' \code{\link{optimizar_dc}} para calibracion con frecuencia efectiva minima (cobertura acumulada)
#'
#' @references
#' Leckenby, J. D., & Boyd, M. M. (1984). An improved beta binomial reach/frequency model for magazines.
#' Current Issues and Research in Advertising, 7(1), 1-24.
#'
#' @export
optimizar_d <- function(Pob, FE, cob_efectiva, A1, max_inserciones,
                        tolerancia = 0.05, step_A = 0.1, step_B = 0.1,
                        batch_size = 1000000, min_soluciones = 10,
                        error_aceptable = 0.01) {
  invisible(legacy_bbd_calibration(Pob, FE, cob_efectiva, A1,
                                   max_inserciones, "exact", tolerancia))
}

#' @encoding UTF-8
#' @title Calibracion Beta-Binomial para una frecuencia efectiva minima (cobertura acumulada)
#' @description Igual que \code{\link{optimizar_d}}, pero el objetivo es la
#' cobertura acumulada de personas expuestas a \code{FEM} o mas contactos, en
#' lugar de a exactamente \code{FE} contactos. Delega en
#' \code{\link{calibrate_bbd}} con \code{type = "at_least"}.
#'
#' @param Pob Tamano de la poblacion
#' @param FEM Frecuencia Efectiva Minima (numero objetivo de impactos por persona, o mas)
#' @param cob_efectiva Numero objetivo de personas a alcanzar con FEM o mas contactos
#' @param A1 Audiencia tras la primera insercion
#' @param max_inserciones Numero de inserciones maximo a considerar
#' @param tolerancia Margen de error permitido, como proporcion de \code{cob_efectiva} (default: 0.05)
#' @param step_A Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param step_B Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param batch_size Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param min_soluciones Argumento heredado por compatibilidad; no afecta al calculo en v2
#' @param error_aceptable Argumento heredado por compatibilidad; no afecta al calculo en v2
#'
#' @return Igual estructura que \code{\link{optimizar_d}}; en \code{data},
#' \code{dc_acumulada} es la probabilidad de FEM o mas contactos.
#'
#' @examples
#' resultado <- optimizar_dc(
#'   Pob = 1000000, FEM = 3, cob_efectiva = 600000, A1 = 450000, max_inserciones = 8
#' )
#' resultado$n_optimo
#'
#' @seealso
#' \code{\link{calibrate_bbd}} para el motor v2 subyacente
#' \code{\link{optimizar_d}} para optimizacion con frecuencia efectiva exacta
#'
#' @references
#' Leckenby, J. D., & Boyd, M. M. (1984). An improved beta binomial reach/frequency model for magazines.
#' Current Issues and Research in Advertising, 7(1), 1-24.
#'
#' @export
optimizar_dc <- function(Pob, FEM, cob_efectiva, A1, max_inserciones,
                         tolerancia = 0.05, step_A = 0.1, step_B = 0.1,
                         batch_size = 1000000, min_soluciones = 10,
                         error_aceptable = 0.01) {
  invisible(legacy_bbd_calibration(Pob, FEM, cob_efectiva, A1,
                                   max_inserciones, "at_least", tolerancia))
}

