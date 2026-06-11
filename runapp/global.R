#globals
library(shiny)
library(bslib)
library(DT)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(shinyjs)
library(rhandsontable)
library(stringr)
library(scales)
library(ggplot2)
library(future)
library(furrr)
library(future.apply)
#library(progressr)


#------------------parameters-------------

source("config.R")
source("tab_ui.R")
source("function.R")
options(warn = -1)

#plan(multisession, workers = min(availableCores() - 1))
# handlers("txtprogressbar")

#  load data--------------------------------------------------------------------

#recover proba
load(paste0("www/elo_and_proba/",date_update_proba,"Proba_list.Rdata"))
elo<-read_xlsx(paste0("www/elo_and_proba/",date_update_proba,"elo.xlsx"))

tournament<-read_xlsx("www/round32assignment.xlsx")
levels_r32<-paste0("n°",tournament$ID[1:16]," ",
                   tournament$rank_team_1[1:16],
                   tournament$group_team_1[1:16],
                   " vs ",
                   tournament$rank_team_2[1:16],
                   tournament$group_team_2[1:16])
levels_r32<-str_replace(levels_r32,"NA","")


assignment<-tournament %>% filter(Match_type=="Round of 32")
annexe <- read.csv(
    "www/third.txt",
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE
)[,-1]
annexe$combination <- apply(annexe, 1, function(x) {
    paste(sort(x), collapse = "/")
})

















