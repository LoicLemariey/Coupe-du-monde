#renvconfig
library(renv)

.libPaths()

renv_lib<-"C:/Users/loicl/OneDrive/Documents/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026/renv/library/R-4.2/x86_64-w64-mingw32"
lib2<-"C:/Users/loicl/AppData/Local/R/cache/R/renv/sandbox/R-4.2/x86_64-w64-mingw32/0cdf27ab"


project_runapp<-"~/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026/runapp"

.libPaths(renv_lib)
.libPaths(lib2)

.libPaths()
renv::init(bare = TRUE)



install.packages("shiny")
install.packages("bslib")
install.packages("tidyr")
install.packages("dplyr")
install.packages("readxl")
install.packages("DT")
install.packages("purrr")
install.packages("shinyjs")
install.packages("rhandsontable")
install.packages("stringr")
install.packages("scales")
install.packages("ggplot2")
install.packages("future.apply")
install.packages("furrr")

install.packages("writexl",lib = lib2)
install.packages("dplyr",lib = lib2)
install.packages("jsonlite",lib = lib2)
install.packages("httr",lib = lib2)
install.packages("rlang",lib = lib2)
install.packages("stringr",lib = lib2)
install.packages("readxl",lib = lib2)
install.packages("purrr",lib = lib2)
install.packages("readr",lib = lib2)
install.packages("xml2",lib = lib2)
install.packages("rvest",lib = lib2)
#renv
.libPaths()
renv::status()
renv::snapshot(library = renv_lib)
dep<-renv::dependencies()

library(rsconnect)
.libPaths()
#manifest

deps <- rsconnect::appDependencies()
deps[deps$Package == "data.table", ]

rsconnect::writeManifest(verbose = TRUE,)
