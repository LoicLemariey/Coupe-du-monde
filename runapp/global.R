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
# ---------------------------------
# PARAMETRES
# ---------------------------------


rm(list=(ls()))
N_SIM <- 100
tab_names <- c("A","B","C","D","E","F","G","H","I","J","K","L")

#scores<-read_xlsx("runapp/www/scores.xlsx")





renderer_lock<- "function(instance, td) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = '#f0f0f0';
                }
            "

tournament<-read_xlsx("www/round32assignment.xlsx")
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


# ---------------------------------
# CLASSEMENT
# ---------------------------------



compute_assignment<-function(assignment,third_qualified,rank_table){
    index<-which(annexe$combination==third_qualified)
    third<-t(annexe[index,-dim(annexe)[2]])
    first<-names(annexe)[-dim(annexe)[2]]
    df_1_3<-data.frame(first=first,third=third)
    
    
    assignment_bis<-assignment[is.na(assignment$group_team_2),] %>%
        arrange(group_team_1) %>%
        mutate(group_team_2=df_1_3[,2])
    
    assignment<-rbind(assignment,assignment_bis) %>% filter(!is.na(group_team_2))
    
    for(i in 1:length(assignment$id)){
        index_1<-which(rank_table$Group==assignment$group_team_1[i]&
                           rank_table$rank==assignment$rank_team_1[i])
        assignment$team_1[i]<-rank_table$team[index_1]
        
        index_2<-which(rank_table$Group==assignment$group_team_2[i]&
                           rank_table$rank==assignment$rank_team_2[i])
        assignment$team_2[i]<-rank_table$team[index_2]
    }
    
    
    assignment<-assignment%>% arrange(id_2)
    return(assignment)
}





compute_table <- function(matches){
    

    all_teams <- bind_rows(
        matches %>% select(Group, team = home),
        matches %>% select(Group, team = away)
    ) %>%
        distinct()
    # ==================================================
    # MATCHES PLAYED
    # ==================================================
    
    played <- matches %>%
        filter(
            !is.na(Score_Home),
            !is.na(Score_Away)
        )
    
    if(nrow(played) == 0){
        return(data.frame())
    }
    
    # ==================================================
    # LONG FORMAT
    # ==================================================
    
    home <- played %>%
        transmute(
            Group = Group,
            team = home,
            opponent = away,
            GF = Score_Home,
            GA = Score_Away,
            Pts = case_when(
                Score_Home > Score_Away ~ 3,
                Score_Home == Score_Away ~ 1,
                TRUE ~ 0
            )
        )
    
    away <- played %>%
        transmute(
            Group = Group,
            team = away,
            opponent = home,
            GF = Score_Away,
            GA = Score_Home,
            Pts = case_when(
                Score_Away > Score_Home ~ 3,
                Score_Away == Score_Home ~ 1,
                TRUE ~ 0
            )
        )
    
    all_matches <- bind_rows(home, away)
    
    # ==================================================
    # GLOBAL TABLE
    # ==================================================
    
    table_global <- all_matches %>%
        group_by(Group, team) %>%
        summarise(
            Points = sum(Pts),
            Match = n(),
            GF = sum(GF),
            GA = sum(GA),
            GD = GF - GA,
            .groups = "drop"
        )
    table_global <- all_teams %>%
        left_join(table_global, by = c("Group", "team")) %>%
        mutate(
            Match = coalesce(Match, 0L),
            Points = coalesce(Points, 0),
            GF = coalesce(GF, 0),
            GA = coalesce(GA, 0),
            GD = coalesce(GD, 0)
        )

    
    
    
    # ==================================================
    # H2H RANKING
    # ==================================================
    
    h2h_rank <- function(block){
        
        # ----------------------------------------------
        # PAR DEFAUT
        # ----------------------------------------------
        
        block <- block %>%
            mutate(
                Points_h2h = NA_real_,
                GD_h2h = NA_real_,
                GF_h2h = NA_real_
            )
        
        # ----------------------------------------------
        # IDENTIFIER LES GROUPES D'EGALITE
        # ----------------------------------------------
        
        ties <- block %>%
            group_by(Points) %>%
            filter(n() > 1) %>%
            ungroup()
        
        # ----------------------------------------------
        # SI AUCUNE EGALITE
        # ----------------------------------------------
        
        if(nrow(ties) == 0){
            
            return(
                block %>%
                    arrange(
                        desc(Points),
                        desc(GD),
                        desc(GF)
                    ) %>%
                    mutate(rank = row_number())
            )
        }
        
        # ----------------------------------------------
        # CALCUL H2H POUR CHAQUE BLOC D'EGALITE
        # ----------------------------------------------
        
        tie_levels <- unique(ties$Points)
        
        for(p in tie_levels){
            
            tied_teams <- block %>%
                filter(Points == p) %>%
                pull(team)
            
            h2h <- all_matches %>%
                filter(
                    team %in% tied_teams,
                    opponent %in% tied_teams
                ) %>%
                group_by(team) %>%
                summarise(
                    Points_h2h = sum(Pts),
                    GD_h2h = sum(GF - GA),
                    GF_h2h = sum(GF),
                    .groups = "drop"
                )
            
            block <- block %>%
                left_join(
                    h2h,
                    by = "team",
                    suffix = c("", "_new")
                ) %>%
                mutate(
                    Points_h2h = coalesce(
                        Points_h2h_new,
                        Points_h2h
                    ),
                    GD_h2h = coalesce(
                        GD_h2h_new,
                        GD_h2h
                    ),
                    GF_h2h = coalesce(
                        GF_h2h_new,
                        GF_h2h
                    )
                ) %>%
                select(
                    -ends_with("_new")
                )
        }
        
        # ----------------------------------------------
        # FALLBACK
        # ----------------------------------------------
        
        block <- block %>%
            mutate(
                Points_h2h = coalesce(Points_h2h, 0),
                GD_h2h = coalesce(GD_h2h, 0),
                GF_h2h = coalesce(GF_h2h, 0)
            )
        
        # ----------------------------------------------
        # FINAL RANKING
        # ----------------------------------------------
        
        block %>%
            arrange(
                desc(Points),
                desc(Points_h2h),
                desc(GD_h2h),
                desc(GF_h2h),
                desc(GD),
                desc(GF)
            ) %>%
            mutate(rank = row_number())
    }
    
    # ==================================================
    # APPLY GROUP BY GROUP
    # ==================================================
    
    table_global <- table_global %>%
        group_by(Group) %>%
        group_modify(~ h2h_rank(.x)) %>%
        ungroup() %>%
        arrange(Group, rank)
    
    # ==================================================
    # BEST 3RD PLACES
    # ==================================================
    
    thirds <- table_global %>%
        filter(rank == 3) %>%
        arrange(
            desc(Points),
            desc(GD),
            desc(GF)
        ) %>%
        mutate(rank_third = row_number())
    
    # ==================================================
    # QUALIFICATION
    # ==================================================
    
    table_global <- table_global %>%
        left_join(
            thirds %>%
                select(team, rank_third),
            by = "team"
        ) %>%
        mutate(
            Qualified =
                (rank <= 2) |
                (rank == 3 & rank_third <= 8),
            
            Qualification = ifelse(
                Qualified,
                "🟢 Qualified",
                "🔴 Eliminated"
            )
        ) %>%
        mutate(
            across(where(is.numeric), as.integer)
        )
    
    return(table_global)
}


get_qualified_3_vec<-function(table_global){
    vec<-sort(table_global$Group[table_global$rank==3&table_global$Qualified])
    return(vec)
}


# -----------------------Simulation---------------------------------------------

load("www/Proba_list.Rdata")
elo<-read_xlsx("www/elo.xlsx")



simulate_scores<-function(id){
    p<-list[[id]]
    score<-sample(p$name,prob=p$probability,size=1)
    score_num<-as.numeric(str_split_1(score,":"))
    return(score_num)
}

simulate_elo_match<-function(elo,team1,team2){
    diff_elo<-elo$Elo[elo$Team_En==team1]-elo$Elo[elo$Team_En==team2]
    p<-1/(1+10**(-diff_elo/400))
    winner<-NA
    if(runif(1)<p){
        winner<-team1
    }else{
        winner<-team2
    }
    return(winner)#doit retourner le winner et un score 
}


simulate_elo_match_score <- function(elo, team1, team2,
                               avg_goals = 2.6) {
    
    elo1 <- elo$Elo[elo$Team_En == team1]
    elo2 <- elo$Elo[elo$Team_En == team2]
    
    diff_elo <- elo1 - elo2
    
    # Probabilité Elo de victoire
    p1 <- 1 / (1 + 10^(-diff_elo / 400))
    
    # Conversion Elo -> buts attendus
    lambda1 <- avg_goals * p1
    lambda2 <- avg_goals * (1 - p1)
    
    # Tirage des buts
    score1 <- rpois(1, lambda1)
    score2 <- rpois(1, lambda2)
    
    # Détermination du vainqueur
    if (score1 > score2) {
        winner <- team1
    } else if (score2 > score1) {
        winner <- team2
    } else {
        winner <- sample(c(team1, team2), 1)
    }
    loser<-team2
    if(winner==team2){
        loser=team1
    }
    
    return(list(
        winner = winner,
        score1 = score1,
        score2 = score2,
        loser  = loser
    ))
}


update_elo <- function(team1, team2, score1, score2,
                       elo_df, K = 60) {
    # Elo actuels
    elo1 <- elo_df$Elo[elo_df$Team_En == team1]
    elo2 <- elo_df$Elo[elo_df$Team_En == team2]
    
    if(length(elo1) == 0 || length(elo2) == 0){
        stop("Equipe non trouvée dans elo_df")
    }
    
    # Résultat
    if(score1 > score2){
        S1 <- 1
        S2 <- 0
    } else if(score1 < score2){
        S1 <- 0
        S2 <- 1
    } else {
        S1 <- 0.5
        S2 <- 0.5
    }
    
    # Probabilités attendues
    E1 <- 1 / (1 + 10^((elo2 - elo1)/400))
    E2 <- 1 - E1
    
    # Multiplicateur différence de buts (EloRatings)
    gd <- abs(score1 - score2)
    
    G <- if(gd <= 1){
        1
    } else if(gd == 2){
        1.5
    }else if(gd == 2){
        1.5
    }
    else if(gd == 3){
        1.75
    }
    else {
        1.75+(gd-3)/8
    }
    
    # Mise à jour Elo
    delta1 <- K * G * (S1 - E1)
    delta2 <- K * G * (S2 - E2)
    
    elo_df$Elo[elo_df$Team_En == team1] <- elo1 + delta1
    elo_df$Elo[elo_df$Team_En == team2] <- elo2 + delta2
    
    return(elo_df)
}


simulate_groups_phase<-function(scores,elo_df){
    scores_out<-scores
    remaining_match<-which(is.na(scores$Score_Home)|is.na(scores$Score_Away))
    
    for(i in remaining_match){
        res<-simulate_scores(scores$match_id[i])
        scores_out[i,c("Score_Home","Score_Away")]<-t(simulate_scores(scores$match_id[i]))
        elo_df<-update_elo(scores_out$home[i],
                        scores_out$away[i],
                        scores_out$Score_Home[i],
                        scores_out$Score_Away[i],elo_df)
    }
    
    out<-list(scores_out=scores_out,
              elo_df=elo_df)
    return(out)
}


simulate_round<-function(tournament,elo_df,round,next_round){
    
    # round<-"Round of 32"
    # next_round<-"Round of 16"
    idx <- which(tournament$Match_type == round)
    if(round=="Final"){
        idx<-which(tournament$Match_type %in%c("Final","Third place"))
    }
    
    for(i in idx){
        
        team1 <- tournament$team_1[i]
        team2 <- tournament$team_2[i]
        
        scores_out <- simulate_elo_match_score(
            elo   = elo,
            team1 = team1,
            team2 = team2
        )
        elo_df<-update_elo(team1,
                           team2,
                           scores_out$score1,
                           scores_out$score2,
                           elo_df)
        
        tournament$Winner[i] <- scores_out$winner
        
        tournament$Loser[i] <- scores_out$loser
        


    }
    if(round!="Final"){
        index<-which(tournament$Match_type==round)
        index1<-index[index%%2!=0]
        index2<-index[index%%2==0]
        tournament$team_1[tournament$Match_type==next_round]<-tournament$Winner[index1]
        tournament$team_2[tournament$Match_type==next_round]<-tournament$Winner[index2]
        
        if(next_round=="Final"){
            tournament$team_1[tournament$Match_type=="Third place"]<-tournament$Loser[index1]
            tournament$team_2[tournament$Match_type=="Third place"]<-tournament$Loser[index2]
        }
    }

    

    out<-list(tournament=tournament,
              elo_df=elo_df)
    return(out)
}



get_place<-function(team,finish_tournament){
    index<-which(finish_tournament$Loser==team)
    result<-character()
    if(length(index)==0){
        if(team==finish_tournament$Winner[finish_tournament$Match_type=="Final"]){
            result<-"🏆 Winner"
        }else{
            result<-"Group stage"
        }
    }
    else if(length(index)==2){
        result<-"🥉 Third"
    }else{
        result<-finish_tournament$Match_type[index]
        if(result=="Final"){
            result<-"🥈 Second"
        }
    }
    return(result)
    
}


simulate_all_round<-function(tournament,elo_df){
    trnt<-simulate_round(tournament,elo_df,"Round of 32","Round of 16")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Round of 16","Quarter")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Quarter","Semi")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Semi","Final")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Final","test")
    return(trnt)
}


get_path<-function(finish_tournament,team){

    index<-which((finish_tournament$team_1==team|finish_tournament$team_2==team)&finish_tournament$Match_type=="Round of 32")
    path<-character(5)
    if(length(index)==0){
        return(NA)
    }

    
    r32<-c(finish_tournament$team_1[index],finish_tournament$team_2[index])
    path[1]<-r32[which(r32!=team)]
    nxt_round_idx<-which(finish_tournament$Next_opponent==finish_tournament$Match[index])
    path[2]<-finish_tournament$Winner[nxt_round_idx]
    
    
    index<-which((finish_tournament$team_1==path[2]|finish_tournament$team_2==path[2])&finish_tournament$Match_type=="Round of 16")
    nxt_round_idx<-which(finish_tournament$Next_opponent==finish_tournament$Match[index])
    path[3]<-finish_tournament$Winner[nxt_round_idx]
    
    index<-which((finish_tournament$team_1==path[3]|finish_tournament$team_2==path[3])&finish_tournament$Match_type=="Quarter")
    nxt_round_idx<-which(finish_tournament$Next_opponent==finish_tournament$Match[index])
    path[4]<-finish_tournament$Winner[nxt_round_idx]
    
    index<-which((finish_tournament$team_1==path[4]|finish_tournament$team_2==path[4])&finish_tournament$Match_type=="Semi")
    nxt_round_idx<-which(finish_tournament$Next_opponent==finish_tournament$Match[index])
    path[5]<-finish_tournament$Winner[nxt_round_idx]

    return(path)
}


# get_path(finish_tournament,"fr")
# get_path(finish_tournament,"France")
# get_path(finish_tournament,"Argentina")
# 
# get_place(finish_tournament = finish_tournament,"Switzerland")
# get_place(finish_tournament = finish_tournament,"Argentina")
# get_place(finish_tournament = finish_tournament,"Germany")
# 



# test_phase<-simulate_groups_phase(scores,elo)
# (test_rank<-compute_table(test_phase$scores_out) %>% filter(Group=="I"))
# View(test_phase$elo_df)
# simulate_elo_matcsimulate_elo_matcsimulate_elo_match(df_elo_world_cup,"Spain","France")
# simulate_elo_match_score(df_elo_world_cup,"Spain","France")
# elo2<-update_elo("France","Argentina",3,0,elo)



one_simulation<-function(scores,assignment,tournament,elo_df){
    group<-simulate_groups_phase(scores,elo_df)
    rank_table<-compute_table(group$scores_out)
    third_qualified = paste(get_qualified_3_vec(rank_table),collapse="/")
    assignment<-compute_assignment(assignment,
                                   third_qualified = third_qualified,
                                   rank_table = rank_table)

    tournament$team_1[1:length(assignment$id)]<-assignment$team_1
    tournament$team_2[1:length(assignment$id)]<-assignment$team_2
    tournament$group_team_2[1:length(assignment$id)]<-assignment$group_team_2
    
    simu<-simulate_all_round(tournament,group$elo_df)
    
    res<-list(rank_table=rank_table,tournament=simu$tournament,elo = simu$elo_df)
    return(res)
}


test<-one_simulation(scores,assignment,tournament,elo)
print(test$rank_table$rank[test$rank_table$team=="France"])
get_path(test$tournament,"France")
test$elo$Elo[test$elo$Team_En=="France"]
View(test$tournament)

N_simu<-100
t1<-Sys.time()
results <- lapply(
    seq_len(N_simu),
    function(i) one_simulation(scores, assignment, tournament, elo)
)
t2<-Sys.time()
difftime(t2,t1)
#100=30s


get_all_rank<-function(rank_table,team){
    index<-rank_table$rank_table$team==team
    out<-paste(rank_table$rank_table$rank[index],rank_table$rank_table$Qualification[index],sep=": ")
    return(out)
}


get_all_results<-function(one_simu,team){
    out<-get_place(team,one_simu$tournament)
}

get_all_r32<-function(one_simu,team){
    
    index_t1<-which(one_simu$tournament$team_1[1:16]==team)
    index_t2<-which(one_simu$tournament$team_2[1:16]==team)
    bool_t1<-length(index_t1)!=0
    bool_t2<-length(index_t2)!=0
    
    index<-one_simu$rank_table$team==team
    rank<-paste(one_simu$rank_table$rank[index])
    group<-paste(one_simu$rank_table$Group[index])
    
    out<-"Not Qualified"
    if(bool_t1|bool_t2){
        if(bool_t1){
            out<-one_simu$tournament$Match[index_t1]
        }
        
        if(bool_t2){
            out<-one_simu$tournament$Match[index_t2]
        }
       
        out<-paste0(group,rank," ",out)
    }
    return(out) 

}



get_all_path<-function(one_simu,team){
    return(get_path(one_simu$tournament,team))
}

t_rank<-sapply(results,get_all_rank,"France")
table(t_rank)
#defini lesvels

t<-sapply(results,get_all_results,"England")
table(t)
#levels

t<-sapply(results,get_all_r32,"Morocco")
prop.table(table(t)[names(table(t)!="Not Qualified")])
#il faut affciher les cas ou il y a 0


t<-sapply(results,get_all_path,"France")
t_clean<-t[!is.na(t)]
t_clean[[1]]
mat<-matrix(unlist(t_clean),ncol=5,byrow = T)


opponent_round<-apply(mat,2,table)
opponent_round<-lapply(opponent_round,sort,decreasing =TRUE)
names(opponent_round)<-c("r32","r16","Quarter","Semi","Final")
opponent_round
