
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simelcl

<!-- badges: start -->
<!-- badges: end -->

El paquete `simelcl` permite acceder a los datos del Sistema de
Información del Mercado Laboral de (Chile) directo en R. Se accede a los
datos directamente desde la API de SIMEL Chile por lo que se encuentran
actualizados constantemente.

Además, se provee una función de simplificación de los datos que permite
transformar en formato fecha los periodos, quitar columnas extras y
etiquetar los datos.

## Instalación

Se puede instalar la versión de desarrollo de simelcl desde
[GitHub](https://github.com/) con:

``` r
# install.packages("pak")
pak::pak("renzoperagallo/simelcl")
```

## Ejemplo

A continuación, se presenta un ejemplo básico para descargar datos desde
SIMEL y simplificar el dataset.

Para ver la lista completa de tablas disponibles, puede usar:

``` r
library(simelcl)

# Imprime en la terminal la lista de indicadores
ver_lista_indicadores(n = 5) # Solo muestra los primeros 5 registros de la tabla.
#> # A tibble: 259 × 5
#>    id_n id             Indicador Nombres                                  Fuente
#>   <int> <chr>          <chr>     <chr>                                    <chr> 
#> 1     1 TP_SEXO        TP        Tasa de participación nacional y region… Encue…
#> 2     2 TP_EDAD        TP        Tasa de participación nacional y region… Encue…
#> 3     3 TP_NACION_SEXO TP        Tasa de participación extranjeros a niv… Encue…
#> 4     4 TOCU_SEXO      TOCU      Tasa de ocupación nacional y regional, … Encue…
#> 5     5 TOCU_EDAD      TOCU      Tasa de ocupación nacional y regional, … Encue…
#> # ℹ 254 more rows
```

O asignarla a un objeto para verla directamente desde el visualizador de
Rstudio.

``` r

indicadores <- lista_indicadores()

View(indicadores)
```

La columna id indica el nombre en sistema de la tabla. Con este valor se
puede descargar la tabla correspondiente. Por ejemplo:

``` r

df <- get_data("TP_SEXO")

head(df, 5)
#>      TIME_PERIOD FREQ FREQ_label.es INDICADOR    INDICADOR_label.es AREA_REF
#> 1 2010-01-01/P3M    M       Mensual        TP Tasa de participación       13
#> 2 2010-02-01/P3M    M       Mensual        TP Tasa de participación       13
#> 3 2010-03-01/P3M    M       Mensual        TP Tasa de participación       13
#> 4 2010-04-01/P3M    M       Mensual        TP Tasa de participación       13
#> 5 2010-05-01/P3M    M       Mensual        TP Tasa de participación       13
#>           AREA_REF_label.es SEXO SEXO_label.es obsValue DECIMALS
#> 1 Metropolitana de Santiago    F         Mujer 49.54337        1
#> 2 Metropolitana de Santiago    F         Mujer 50.52659        1
#> 3 Metropolitana de Santiago    F         Mujer 50.70735        1
#> 4 Metropolitana de Santiago    F         Mujer 50.12191        1
#> 5 Metropolitana de Santiago    F         Mujer 50.93403        1
#>   DECIMALS_label.es
#> 1                 1
#> 2                 1
#> 3                 1
#> 4                 1
#> 5                 1
```

Lo anterior descarga el dato en el mismo formato que se descarga al
utilizar “Download unfiltered CSV” en la plataforma web.

Si se quiere dar un formato más amable a la tabla, se puede usar la
siguiente función:

``` r

df_simplificado <- simplificar(df)

head(df_simplificado, 5)
#>   TIME_PERIOD AREA_REF SEXO obsValue
#> 1  2010-02-01       13    F     49.5
#> 2  2010-03-01       13    F     50.5
#> 3  2010-04-01       13    F     50.7
#> 4  2010-05-01       13    F     50.1
#> 5  2010-06-01       13    F     50.9
```

La función anterior une las columnas con sus respectivas etiquetas y las
formatea como clase `haven::labelled()` que es un tipo de dato carácter
más su etiqueta. En la tabla de datos se guarda el valor de la variable,
por ejemplo, sexo “F”, “M” o “AS”, así como su diccionario. A este
último se puede acceder utilizando el paquete `haven`:

``` r

library(haven)

haven::print_labels(df_simplificado$SEXO)
#> 
#> Labels:
#>  value       label
#>      F       Mujer
#>      M      Hombre
#>     AS Ambos sexos
```

En algunos casos se puede requerir utilizar las etiquetas (labels) en
vez de los niveles de la variable. Para reemplazar los valores por sus
etiquetas se puede utilizar nuevamente el paquete `haven`:

``` r

df_con_etiquetas <- haven::as_factor(df_simplificado)

head(df_con_etiquetas, 5)  
#>   TIME_PERIOD                  AREA_REF  SEXO obsValue
#> 1  2010-02-01 Metropolitana de Santiago Mujer     49.5
#> 2  2010-03-01 Metropolitana de Santiago Mujer     50.5
#> 3  2010-04-01 Metropolitana de Santiago Mujer     50.7
#> 4  2010-05-01 Metropolitana de Santiago Mujer     50.1
#> 5  2010-06-01 Metropolitana de Santiago Mujer     50.9
```

Si solo se quiere transformar una de las variables, se puede aplicar la
función sobre dicha variable:

``` r
library(dplyr)

df_etiquetado_sexo <- mutate(df_simplificado, sexo_etiquetado = haven::as_factor(SEXO)) 

head(df_etiquetado_sexo, 5)  
#>   TIME_PERIOD AREA_REF SEXO obsValue sexo_etiquetado
#> 1  2010-02-01       13    F     49.5           Mujer
#> 2  2010-03-01       13    F     50.5           Mujer
#> 3  2010-04-01       13    F     50.7           Mujer
#> 4  2010-05-01       13    F     50.1           Mujer
#> 5  2010-06-01       13    F     50.9           Mujer
```
