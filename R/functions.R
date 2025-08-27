#' Obtener url API SIMEL Chile (data)
#'
#' @param dataflow Un único valor de clase "character" con el nombre del dataflow (id en tabla indicadores).
#'
#' @returns Un único valor de clase "character" con la url de la API para la data.
#' @export
#'
#' @examples
#' get_data_url("TP_SEXO")
get_data_url <- function(dataflow){

  servidor_data <- "https://sdmx.ine.gob.cl/rest/data/"
  cl <- "CL01"
  df <- dataflow
  version <- "1.0"
  extra_url <- "?&dimensionAtObservation=AllDimensions"

  out <-
    paste0(
      servidor_data,
      cl,
      ",DF_",
      df,
      ",",
      version,
      "/",
      extra_url
    )

  return(out)
}

#' Obtener url API SIMEL Chile (metadata)
#'
#' @param dataflow Un único valor de clase "character" con el nombre del dataflow (id en tabla indicadores).
#'
#' @returns Un único valor de clase "character" con la url de la API para la metadata.
#' @export
#'
#' @examples
#' get_dsd_url("TP_SEXO")
get_dsd_url <- function(dataflow){

  servidor_dsd <- "https://sdmx.ine.gob.cl/rest/dataflow/"
  cl <- "CL01"
  df <- dataflow
  version <- "1.0"
  extra_url <- "?references=all"

  out <-
    paste0(
      servidor_dsd,
      cl,
      "/DF_",
      df,
      "/",
      version,
      extra_url
    )

  return(out)
}

#' Obtener data SIMEL
#'
#' @description
#'  Descarga información de una tabla de datos publicada en SIMEL.
#'
#'
#' @param dataflow Un único valor de clase "character" con el nombre del dataflow (id en tabla indicadores).
#'
#' @returns Un objeto de clase "data.frame" con los datos de una tabla en SIMEL.
#' @export
#'
#' @examples
#' get_data("TP_SEXO")
get_data <- function(dataflow){

  sdmx.data <- rsdmx::readSDMX(get_data_url(dataflow))

  sdmx.dsd <- rsdmx::readSDMX(file = get_dsd_url(dataflow))

  sdmx.data <- rsdmx:::setDSD(sdmx.data, sdmx.dsd)

  out <- as.data.frame(sdmx.data, labels = TRUE)

  return(out)
}

#' Desagregaciones de SIMEL
#' @returns Un vector de clase "character" con los nombres de las columnas de desagregación.
desagregaciones <- function(){

  out <- c(
    "UNIDAD",
    "SEXO",
    "NACION",
    "GEN",
    "EDAD",
    "RAMA",
    "SLAB",
    "CLAB",
    "ZONA",
    "SINS",
    "CIUO",
    "CIUO88",
    "CISE",
    "QUINTIL",
    "NTRAB",
    "EDU",
    "EDU11",
    "TDCON",
    "RELPAR",
    "TRAMOHORA",
    "LUGAR",
    "TP",
    "SFOR",
    "ANTIG",
    "TEMPV",
    "CDIS",
    "CONUR",
    "CONTRIB"
  )

  return(out)
}

#' Simplificar tablas SUSESO
#'
#' @description
#' Simplifica las tablas de SUSESO seleccionando las  columnas más importantes. Además, redondea  los valores al número de decimales establecidos en cada tablas.
#'
#'
#' @param df Objeto de clase "data.frame" proveniente de SIMEL.
#' @param redondear Valor lógico.
#'
#' @returns Un objeto de clase "data.frame" simplificado.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_data("TP_SEXO")
#' df_v2 <- simplificar_tabla(df)
#' }
simplificar_tabla <- function(df, redondear = TRUE){

  decimales <- as.integer(unique(df$DECIMALS))

  # if(length(unique(df$INDICADOR))>1){stop("Hay más de un indicador")}

  out <-
    df |>
    dplyr::select(
      dplyr::any_of(
        c(
          "TIME_PERIOD",
          "AREA_REF",
          "AREA_REF_label.es",
          desagregaciones(),
          "obsValue"
          )
        )
      )
  if(redondear== TRUE){
    out <- out |> dplyr::mutate(obsValue = round(obsValue, decimales))
  }

  return(out)

}


