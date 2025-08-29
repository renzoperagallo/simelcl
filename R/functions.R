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
#' @param dataflow Un único valor de clase "character" con el nombre del
#' dataflow (id en tabla indicadores).
#'
#' @returns Un único valor de clase "character" con la url de la API para la
#' metadata.
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
#' @param dataflow Un único valor de clase "character" con el nombre del
#' dataflow (id en tabla indicadores).
#' @param ... Otros argumentos de readSMDX
#'
#' @returns Un objeto de clase "data.frame" con los datos de una tabla en SIMEL.
#' @export
#'
#' @examples
#' get_data("TP_SEXO")
get_data <- function(dataflow, ...){

  sdmx.data <- rsdmx::readSDMX(get_data_url(dataflow), ...)

  sdmx.dsd <- rsdmx::readSDMX(file = get_dsd_url(dataflow))

  sdmx.data <- rsdmx:::setDSD(sdmx.data, sdmx.dsd)

  out <- as.data.frame(sdmx.data, labels = TRUE)

  return(out)
}

#' Desagregaciones de SIMEL
#' @returns Un vector de clase "character" con los nombres de las columnas de
#' desagregación.
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
#' Simplifica las tablas de SUSESO seleccionando las  columnas más importantes.
#' Además, redondea  los valores al número de decimales establecidos en cada
#' tablas.
#'
#'
#' @param df Objeto de clase "data.frame" proveniente de SIMEL.
#' @param redondear Valor lógico.
#'
#' @note
#' En el caso de las tablas que utilizan la ENE, los trimestres móviles se
#' transforma la variable fecha utilizando como referencia el mes central.
#'
#' @returns Un objeto de clase "data.frame" simplificado.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_data("TP_SEXO")
#' df_v2 <- simplificar(df)
#' }
simplificar <- function(df, redondear = TRUE){

  decimales <- as.integer(unique(df$DECIMALS))

  # if(length(unique(df$INDICADOR))>1){stop("Hay más de un indicador")}

  df <- timeperiod_to_date(df)

  df <- labelizar_df(df)

  out <-
    df |>
    dplyr::select(
      dplyr::any_of(
        c(
          "TIME_PERIOD",
          "AREA_REF",
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

#' Transformar timeperiod a clase "date"
#'
#' @description
#' Transforma la variable TIME_PERIOD a clase date.
#'
#'
#' @param df Objeto de clase "data.frame" proveniente de SIMEL.
#'
#' @returns Un objeto de clase "data.frame" con la columna TIME_PERIOD como
#' clase "date".
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_data("TP_SEXO")
#' df_v2 <- timeperiod_to_date(df)
#' }
timeperiod_to_date <- function(df){

  if(unique(df$FREQ) == "M"){

    if(identificar_trimestre_movil(df)){
      out <-
        df |>
        dplyr::mutate(
          TIME_PERIOD = lubridate::as_date(
            stringr::str_sub(TIME_PERIOD, 1, 10)
          ),
          TIME_PERIOD = lubridate::add_with_rollback(
            TIME_PERIOD,
            months(1)
          )
        )
    }else{
      out <-
        df |>
        dplyr::mutate(
          TIME_PERIOD = lubridate::as_date(
            paste(
              stringr::str_sub(TIME_PERIOD, 1, 4),
              stringr::str_sub(TIME_PERIOD, 7, 8),
              "01",
              sep = "-"
            )
          )
        )
    }

  }else if(unique(df$FREQ) == "A"){
    out <-
      df |>
      dplyr::mutate(
        TIME_PERIOD = lubridate::as_date(
          paste(
            stringr::str_sub(TIME_PERIOD, 1, 4),
            "01",
            "01",
            sep = "-"
          )
        )
      )
  } else{
    stop("No se encuentra la columna FREQ con un valor valido.")
  }

  return(out)
}

#' Identifica trimestre movil
#'
#' @description
#' Identifica si la columna TIME_PERIOD  viene en formato de trimestre movil.
#'
#'
#' @param df Objeto de clase "data.frame" proveniente de SIMEL.
#'
#' @returns Objeto clase "Logical" con valor TRUE cuando el data.frame viene
#' con fechas en formato trimestre movil.
#'
#' @examples
#' tbl <- data.frame(TIME_PERIOD = "2016-05-01/P3M")
#' identificar_trimestre_movil(tbl)
identificar_trimestre_movil <- function(df){

  valor_prueba <- df$TIME_PERIOD[1] |> stringr::str_sub(-4, -1)

  if(valor_prueba == "/P3M"){
    return(TRUE)
  }else{
    return(FALSE)
  }

}

#' Obtener lista indicadores.
#'
#' @description
#' Retorna la lista de indicadores.
#'
#' @returns Objeto clase "data.frame" con la lista indicadores de SIMEL.
#' @export
#'
#' @examples
#' indicadores <- lista_indicadores()
lista_indicadores <- function(){
  return(tabla_indicadores)
}

#' Ver lista de indicadores
#'
#' @description
#' Imprime la lista de indicadores.
#'
#' @param n Número de filas a imprimir.
#'
#' @returns Print de la lista de indicadores.
#' @export
#'
#' @examples
#' ver_lista_indicadores()
ver_lista_indicadores <- function(n = 1000){
  tabla_indicadores |>
    tibble::as_tibble() |>
    print(n = n)
}

#' Crea un diccionario de  valores de una columna.
#'
#' @param df Un objeto de clase "data.frame" con los datos de una tabla en SIMEL.
#' @param col_codes Valor de clase "character"  con la columna con los códigos.
#' @param col_labels Valor de clase "character" con la columna con las labels.
#'
#' @returns Un vector de clase "character" tipo "named vector" (diccionario).
#'
#' @examples
#' tabla <- data.frame(levels = 1:3, labels = c("uno", "dos", "tres"))
#' create_labels_dictionary(tabla, "levels", "labels")
create_labels_dictionary <- function(df, col_codes, col_labels){

  tabla <-
    df |>
    dplyr::distinct(
      !!dplyr::sym(col_codes),
      !!dplyr::sym(col_labels)
    )

  out <-
    setNames(
      dplyr::pull(tabla, !!dplyr::sym(col_codes)),
      dplyr::pull(tabla, !!dplyr::sym(col_labels))
    )

  return(out)
}

#' Labeliza una columna
#'
#' @description
#' Permite transformar una columna a haven::labelled a partir de un diccionario.
#'
#' @param df Un objeto de clase "data.frame" con los datos de una tabla en SIMEL.
#' @param col_name Objeto "character" con el nombre de la columna a transformar.
#' @param labels_dictionary Objeto tipo named vector  utilizado como diccionario.
#'
#' @returns Objeto de clase "data.frame" con la columna  en formato
#' haven::labelled.
#' @export
#'
#' @examples
#' tabla = data.frame(valores = 1:3)
#' diccionario = c(1 = "uno", 2 = "dos", 3 = "tres")
#' to_labelled(tabla, "valores", diccionario)
to_labelled <- function(df, col_name, labels_dictionary){

  out <-
    df |>
    dplyr::mutate(
      !!dplyr::sym(col_name) := haven::labelled(
        !!dplyr::sym(col_name),
        labels = labels_dictionary
      )
    )

  return(out)

}

#' Labeliza una tabla de SIMEL
#'
#' @description
#' Transforma en variables haven::labelled() las columnas que tienen una
#' label asociada en las tablas SIMEL (excepto por columna DECIMAL).
#'
#'
#' @param df Un objeto de clase "data.frame" con los datos de una tabla en SIMEL.
#'
#' @returns Un objeto de clase "data.frame" con las variables categóricas
#' en formato haven::labelled.
#'
#' @export
#'
#' @examples
#' tabla <- data.frame(numeros = 1:3, numeros_label.es = c("uno", "dos", "tres"))
#' labelizar_df(tabla)
labelizar_df <- function(df){

  # Obtener columnas

  nombres_columnas <- names(df)

  nombres_columnas <- stringr::str_subset(nombres_columnas, "DECIMALS", negate = TRUE)

  columnas_labels <- stringr::str_subset(nombres_columnas, "_label.es$")

  columnas_codes <- stringr::str_extract(columnas_labels, "\\w+(?=_label.es)")

  # Obtiene los diccionarios

  diccionarios <-
    purrr::map2(
      columnas_codes,
      columnas_labels,
      ~create_labels_dictionary(df, .x, .y)
    )

  # Labeliza las columnas

  for (i in 1:length(columnas_codes)){

    df <- to_labelled(df, columnas_codes[[i]],  diccionarios[[i]])

  }

  return(df)
}
