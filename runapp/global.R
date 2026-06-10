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
options(warn = -1)

#plan(multisession, workers = min(availableCores() - 1))
# handlers("txtprogressbar")

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
load("www/Proba_list.Rdata")
elo<-read_xlsx("www/elo.xlsx")




renderer_lock<- "function(instance, td) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = '#f0f0f0';
                }
            "








annexe$combination <- apply(annexe, 1, function(x) {
    paste(sort(x), collapse = "/")
})


#---------classement-----------


#a debugegr quand le score est vide.
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

    # MATCHES PLAYED

    
    played <- matches %>%
        filter(
            !is.na(Score_Home),
            !is.na(Score_Away)
        )
    
    # if(nrow(played) == 0){
    #     return(data.frame())
    # }
    

    # LONG FORMAT

    
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
    

    # GLOBAL TABLE

    
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

    
    
    
  
    # H2H RANKING

    
    h2h_rank <- function(block){
        

        # PAR DEFAUT

        
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






simulate_all_round<-function(tournament,elo_df){
    trnt<-simulate_round(tournament,elo_df,"Round of 32","Round of 16")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Round of 16","Quarter")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Quarter","Semi")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Semi","Final")
    trnt<-simulate_round(trnt$tournament,trnt$elo_df,"Final","test")
    return(trnt)
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
    }else if(length(index)==2){
        result<-"🥉 Third"
    }else{
        result<-finish_tournament$Match_type[index]
        if(result=="Final"){
            result<-"🥈 Second"
        }
    }
    
    
    levels<-rev(c("Group stage","Round of 32","Round of 16",
              "Quarter","Semi","🥉 Third","🥈 Second","🏆 Winner"))
    labels <- rev(c("Group stage", "Round of 32", "Round of 16",
                "Quarter", "Semi", "🥉 Third", "🥈 Second", "🏆 Winner"))
    result<-factor(result,levels,labels = labels)
    return(result)
    
}







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


get_all_rank<-function(rank_table,team){
    index<-rank_table$rank_table$team==team
    out<-paste(rank_table$rank_table$rank[index],
               rank_table$rank_table$Qualification[index],
               sep=": ")
    levels<-paste(c(1,2,3,3,4),c(rep("\U0001f7e2 Qualified",each=3),rep("\U0001f534 Eliminated",2)),sep=": ")
    out<-factor(out,levels)
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
    tournament<-tournament[1:16,]
    tournament$group_team_2[is.na(tournament$group_team_2)]<-""
    levels<-levels_r32
    levels<-c(levels,out)
    out<-factor(out,level=levels)
    
    if(bool_t1|bool_t2){
        if(bool_t1){
            out<-one_simu$tournament$Match[index_t1]
            out<-factor(levels[index_t1],level=levels)
        }
        
        if(bool_t2){
            out<-one_simu$tournament$Match[index_t2]
            out<-factor(levels[index_t2],level=levels)
        }
        
        # if(rank!=3){
        #     out<-paste0(rank,group," n°",out)
        # }else{
        #     out<-paste0(rank," n°",out)
        # }
        
    }

    return(out) 
    
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

get_all_path<-function(one_simu,team){
    return(get_path(one_simu$tournament,team))
}










build_result_graph<-function(results,team){
    
    x<-sapply(results,get_all_results,team)
    
    df <- tibble(modalite = x) |>
        count(modalite) |>
        mutate(
            prop = n / sum(n),
            label = paste0(modalite, "\n", round(100 * prop, 1), "%")
        )
    cols <- setNames(
        rev(c(
            "#B2182B",
            "#D6604D",
            "#F4A582",
            "#FDDBC7",
            "#D1E5F0",
            "#92C5DE",
            "#4393C3",
            "#2166AC"
        )),
        levels(x)
    )
    
    
    ggplot(df, aes(y = "", x = prop, fill = modalite)) +
        geom_col(width = 0.8) +
        geom_text(
            aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4
        ) +
        scale_x_continuous(labels = percent_format()) +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_fill_manual(values = cols) +
        theme_minimal() +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank()
        ) + theme(legend.position = "left")
}

draw_r32_graph<-function(results,team){
    x<-rev(sapply(results,get_all_r32,team))
    df <- tibble(modalite = x) |>
        count(modalite, .drop = FALSE) |>
        mutate(prop = n / sum(n))
    df$modalite <- factor(df$modalite, levels = rev(unique(df$modalite)))
    ggplot(df, aes(x = prop, y = modalite)) +
        geom_col(fill="royalblue") +
        scale_x_continuous(
            labels = percent_format(accuracy = 1),
            limits = c(0, 1)
        ) +
        geom_text(
            aes(label = scales::percent(prop, accuracy = 1)),
            hjust = -0.1,
            size = 4
        ) +
        labs(x = NULL, y = NULL) +
        geom_hline(yintercept = 4.5, linetype = "dotted")+
        geom_hline(yintercept = 12.5, linetype = "dotted")+
        geom_hline(yintercept = 8.5, linetype = "dashed")+theme_minimal()
    
}

draw_rank_graph<-function(results,team){
    x<-sapply(results,get_all_rank,team)
    df <- tibble(modalite = x) |>
        count(modalite, .drop = FALSE) |>
        mutate(prop = n / sum(n))
    df$modalite <- factor(df$modalite, levels = rev(unique(df$modalite)))
    ggplot(df, aes(x = prop, y = modalite)) +
        geom_col(fill=c(rep("forestgreen",3),rep("firebrick",2))) +
        scale_x_continuous(
            labels = percent_format(accuracy = 1),
            limits = c(0, 1)
        ) +
        geom_text(
            aes(label = scales::percent(prop, accuracy = 1)),
            hjust = -0.1,
            size = 4
        ) +
        labs(x = NULL, y = NULL) +
        theme_minimal()
    
}


# --------------------------test------------------------------------------------

# 
# test<-one_simulation(scores,assignment,tournament,elo)
# print(test$rank_table$rank[test$rank_table$team=="France"])
# get_path(test$tournament,"France")
# test$elo$Elo[test$elo$Team_En=="France"]
# View(test$tournament)
# 
# N_simu<-200
# t1<-Sys.time()
# results <- lapply(
#     seq_len(N_simu),
#     function(i) one_simulation(scores, assignment, tournament, elo)
# )
# t2<-Sys.time()
# difftime(t2,t1)
# #100=30s
# 
# 
# 
# 
# 
# 
# build_result_graph(results,"Japan")
# draw_r32_graph(results,"Japan")
# draw_rank_graph(results,"Japan")




#-------------path----------------------------------------
# 


draw_path<-function(results,team){
    #print("ENTER drawpath")
    

    t<-sapply(results,get_all_path,team,simplify = FALSE)
    #print(paste0("type t",typeof(t)))
    t_clean<-t[!is.na(t)]
    mat<-matrix(unlist(t_clean),ncol=5,byrow = T)
    #opponent_round<-apply(mat,2,table)
    opponent_round <- lapply(
        seq_len(ncol(mat)),
        function(i) table(mat[, i, drop = TRUE])
    )
    opponent_round<-sapply(opponent_round,sort,decreasing =TRUE,simplify = FALSE)
    
    
    names(opponent_round)<-c("r32","r16","Quarter","Semi","Final")
    
    

    # trier par frequence
    #code couleur
    df <- purrr::imap_dfr(
        opponent_round,
        ~ tibble(
            round = .y,
            opponent = names(.x),
            n = as.numeric(.x)
        ))

    
    df<-df|>
        group_by(round) |>
        mutate(
            prop = n / sum(n)
        )
    
    

    df<-df|>
        ungroup() |>
        mutate(
            opponent = ifelse(prop < prct_minimal, "OTHER", opponent)
        ) |>
        group_by(round, opponent) |>
        summarise(n = sum(n), .groups = "drop") |>
        group_by(round) |>
        mutate(prop = n / sum(n),
               label = paste0(opponent," ",round(100 * prop, 1), "%")) |>
        ungroup()
    
    
    
    
    df$round <- factor(
        df$round,
        levels = c("r32", "r16", "Quarter", "Semi", "Final")
    )
    
    
    
    p1<-ggplot(df, aes(x = round, y = prop, fill = opponent)) +
        geom_col(width = 0.8) +
        geom_text(
            aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3,
            check_overlap = TRUE
        ) +
        scale_y_continuous(labels = percent_format()) +
        labs(
            x = NULL,
            y = "Probability",
            fill = "Opponent"
        ) +
        theme_minimal()+
        scale_fill_manual(values = team_colors)
    
    return(p1)
}



# filter------------------------------------------------------------------------

filter_result<-function(result,team,rank="All",r32="All"){
    bool_r32<-TRUE
    bool_rank<-TRUE
    id_team<-which(result$rank_table$team==team)
    #print(names(result$rank_table))
    if(rank!="All"){
        bool_rank<-result$rank_table$rank[id_team]==rank
    }
    if(r32!="All"& r32!="NA"){
        
        nr32<-which(levels_r32==r32)
        bool_r32<-result$tournament$team_1[nr32]==team|
            result$tournament$team_2[nr32]==team
    }
    if(r32=="NA"){
        bool_r32<-FALSE
    }
    if(rank==4){
        bool_r32<-TRUE
    }
    # print("bool rank")
    # print(bool_rank)
    out<-bool_rank&bool_r32
    return(out)
    
}

get_r32_after_filter<-function(result,team,rank){
    t<-lapply(result,filter_result,team, rank=rank,r32="All")
    index<-which(unlist(t))
    r32<-unique(rev(sapply(result[index],get_all_r32,team)))
    index_not<-which(r32=="Not Qualified")
    if(length(index_not)>0){
        r32<-r32[-index_not]
    }

    if(length(r32)==0){
        r32<-c("NA")
    }else{
        r32<-c("All",as.character(as.vector(r32)))
    }
    
    # print(typeof(r32))
    # print(paste(r32))
    return(r32)
}

