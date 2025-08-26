get_data_url <- function(df_name){

  servidor_data <- "https://sdmx.ine.gob.cl/rest/data/"
  cl <- "CL01"
  df <- df_name
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

get_dsd_url <- function(df_name){

  servidor_dsd <- "https://sdmx.ine.gob.cl/rest/dataflow/"
  cl <- "CL01"
  df <- df_name
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

get_data <- function(df_name){

  sdmx.data <- rsdmx::readSDMX(get_data_url(df_name))

  sdmx.dsd <- rsdmx::readSDMX(file = get_dsd_url(df_name))

  sdmx.data <- rsdmx:::setDSD(sdmx.data, sdmx.dsd)

  out <- as.data.frame(sdmx.data, labels = TRUE)

  return(out)
}

simplificar_tabla <- function(df){

  decimales <- unique(df$DECIMALS) |> as.integer()

  if(length(unique(df$INDICADOR))>1){stop("Hay más de un indicador")}

  out <-
    df |>
    select(
      any_of(
        c(
          "TIME_PERIOD",
          "AREA_REF",
          "AREA_REF_label.es",
          "obsValue"
          )
        )
      ) |>
    mutate(obsValue = round(obsValue, decimales))

  return(out)

}
