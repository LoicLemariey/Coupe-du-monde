#renvconfig
library(renv)

.libPaths()

renv_lib<-"C:/Users/loicl/OneDrive/Documents/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026/renv/library/R-4.2/x86_64-w64-mingw32"
.libPaths(renv_lib)

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




#renv
.libPaths()
renv::status()
renv::snapshot()
dep<-renv::dependencies()


#manifest
rsconnect::writeManifest(verbose = TRUE)
