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
# library(future)
# library(furrr)
# library(future.apply)
library(data.table)
library(Rcpp)

#library(profvis)#a enlever apres diag
#library(progressr)


#------------------parameters-------------

source("config.R")
source("tab_ui.R")
source("function.R")
#source("compute_rank.R")
options(warn = -1)

#plan(multisession, workers = min(availableCores() - 1))
# handlers("txtprogressbar")

#  load data--------------------------------------------------------------------

#recover proba

load(paste0("www/elo_and_proba/",date_update_proba,"Proba_list.Rdata"))
elo<-read_xlsx(paste0("www/elo_and_proba/",date_update_proba,"elo.xlsx"))
Rcpp::sourceCpp("compute_rank.cpp")

team_index <- setNames(seq_len(nrow(elo)), elo$Team_En)
elo<-elo$Elo
for(i in seq_along(list)) {

    scores_num <- do.call(
        rbind,
        strsplit(list[[i]]$name, ":", fixed = TRUE)
    )

    list[[i]]$home_score <- as.integer(scores_num[,1])
    list[[i]]$away_score <- as.integer(scores_num[,2])
}

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

scores<-read_xlsx("www/scores.xlsx")
n_groups <- length(unique(scores$Group))
group_start <- seq(1, by=6, length.out=n_groups)
group_size <- rep(6, n_groups)



# 
# #
# res_time<-profvis({
#     one_simulation(scores,
#                    assignment,
#                    tournament,
#                    elo)
#     Sys.sleep(0.5)
# })
# res_time
# 
# 
# elo_df<-elo
# tournament<-read_xlsx("runapp/www/round32assignment.xlsx")
# assignment<-tournament %>% filter(Match_type=="Round of 32")
# scores<-read_xlsx("score_ties.xlsx")
# 
# 
# t1<-Sys.time()
# group<-simulate_groups_phase(scores,elo_df)
# t2<-Sys.time()
# rank_table<-compute_table(group$scores_out)
# t2bb<-Sys.time()
# third_qualified = paste(get_qualified_3_vec(rank_table),collapse="/")
# t2bis<-Sys.time()
# assignment<-compute_assignment(assignment,
#                                third_qualified = third_qualified,
#                                rank_table = rank_table)
# t3<-Sys.time()
# 
# # tournament$team_1[1:length(assignment$id)]<-assignment$team_1
# # tournament$team_2[1:length(assignment$id)]<-assignment$team_2
# # tournament$group_team_2[1:length(assignment$id)]<-assignment$group_team_2
# # 
# # simu<-simulate_all_round(tournament,group$elo_df)
# # t4<-Sys.time()
# # 
# # res<-list(rank_table=rank_table,tournament=simu$tournament,elo = simu$elo_df)
# # 
# # 
# # 
# # (time_simu_group<-difftime(t2,t1))
# # (time_assignment_global<-difftime(t3,t2))
# # (time_simu_round<-difftime(t4,t3))
# 
# (time_rank_table<-difftime(t2bb,t2))#c'est la function rank_table qui prend 20 fois plus de temps
# (time_rank_table<-difftime(t2bis,t2bb))
# (time_assignment<-difftime(t3,t2bis))
# 
# 
# 
# 
# 
# #--------------version fast----------------------------------------------------
# elo_df<-elo
# tournament<-read_xlsx("runapp/www/round32assignment.xlsx")
# assignment<-tournament %>% filter(Match_type=="Round of 32")
# scores<-read_xlsx("score_ties.xlsx")
# 
# 
# group<-simulate_groups_phase(scores,elo_df)
# t2<-Sys.time()
# rank_table<-compute_table_fast_rcpp(group$scores_out)
# t2bb<-Sys.time()
# 
# 
# (time_rank_table<-difftime(t2bb,t2))
# 
# 
# 
# 
# 
# # #--------------nouvelle version rcpp------------------------------------------
# elo_df<-elo
# tournament<-read_xlsx("runapp/www/round32assignment.xlsx")
# assignment<-tournament %>% filter(Match_type=="Round of 32")
# scores<-read_xlsx("score_ties.xlsx")
# 
# 
# group<-simulate_groups_phase(scores,elo_df)
# t2<-Sys.time()
# rank_table<-compute_table_fast_rcpp(group$scores_out)
# t2bb<-Sys.time()
# 
# 
# (time_rank_table<-difftime(t2bb,t2))


