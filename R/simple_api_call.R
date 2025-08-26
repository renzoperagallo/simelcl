# Paquete para hacer consultas a SIMEL
# install.packages("rsdmx")

library(dplyr)
library(rsdmx)

url_datos <- "https://sdmx.ine.gob.cl/rest/data/CL01,DF_ICL2023_RAMA,1.0/.M..IX.?"

url_dsd <- "https://sdmx.ine.gob.cl/rest/dataflow/CL01/DF_ICL2023_RAMA/1.0?references=all"

sdmx_object <- readSDMX(file = url_datos)

stats <- as.data.frame(sdmx_object)

# Datos del ICL, año base 2023 para el sector de "Transporte y almacenamiento"
stats |> filter(RAMA == "H")


# También se puede filtrar por año:

sdmx_2 <- readSDMX(file = url_datos, start = 2024, end = 2025)
stats_2 <- as.data.frame(sdmx_2)

unique(stats_2$obsTime)



# Agregar labels desde DSD ------------------------------------------------

url_datos <- "https://sdmx.ine.gob.cl/rest/data/CL01,DF_ICL2023_RAMA,1.0/.M..IX.?endPeriod=2024-12&lastNObservations=13"

sdmx.data <-
  readSDMX(
    file = url_datos,
    start = 2024,
    end = 2025
  )

sdmx.dsd <- readSDMX(file = url_dsd)

#associate data and dsd

sdmx.data <- rsdmx:::setDSD(sdmx.data, sdmx.dsd)

stats <- as.data.frame(sdmx.data, labels = TRUE)

head(stats)

sdmx.dsd



# Prueba funciones --------------------------------------------------------

df_name <- "TRL"

sdmx.data <- readSDMX(file = get_data_url(df_name))

sdmx.dsd <- readSDMX(file = get_dsd_url(df_name))

sdmx.data <- rsdmx:::setDSD(sdmx.data, sdmx.dsd)

stats <- as.data.frame(sdmx.data, labels = TRUE)


"https://sdmx.ine.gob.cl/rest/data/CL01,DF_TRL,1.0/?&dimensionAtObservation=AllDimensions" |>
  readSDMX() |>
  as.data.frame()

get_dsd_url(df_name)

url_2 <- "https://sdmx.ine.gob.cl/rest/data/CL01,DF_PARTFEM,1.0/.A.?startPeriod=2018&endPeriod=2024&dimensionAtObservation=AllDimensions"

url_3 <- "https://sdmx.ine.gob.cl/rest/data/CL01,DF_PARTFEM,1.0/.A.?&dimensionAtObservation=AllDimensions"

servidor <- "https://sdmx.ine.gob.cl/rest/data/"
cl <- "CL01"
df <- "DF_PARTFEM"
version <- "1.0"
extra_url <- ".A.?&dimensionAtObservation=AllDimensions"

url_4 <-
  paste0(
    servidor,
    cl,
    ",",
    df,
    ",",
    version,
    "/",
    extra_url
  )

prueba <- readSDMX(file = url_4)
prueba <- as.data.frame(prueba)

df <- get_data("TRL")

df_2 <- get_data("TDES_SEXO")

df_3 <- get_data("BGYMEDIOOCU")

get_dsd_url("TRL_SEXO")


df_3 |> select(any_of(c("FREQ", "INDICADOR")))
