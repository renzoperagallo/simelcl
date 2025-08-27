library(devtools)
install.packages("devtools")
install.packages("rsdmx") # CRAN

# Version actualizada
remotes::install_github("opensdmx/rsdmx")


create_package("../simelcl")

usethis::use_package(
  min_version = "0.6-6",
  remotes = "git::https://github.com/eblondel/rsdmx@main"
  )
1

usethis::use_package("rsdmx", type = "Imports")
