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

# ---------------------------------
# PARAMETRES
# ---------------------------------

N_SIM <- 5000
tab_names <- c("A","B","C","D","E","F","G","H","I","J","K","L")

#test_matches<-read_xlsx("runapp/www/scores.xlsx")


renderer_lock<- "function(instance, td) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = '#f0f0f0';
                }
            "

assignment<-read_xlsx("www/round32assignment.xlsx")
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
    return(assignment)
}





compute_table <- function(matches){
    
    library(dplyr)
    
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


# matches<-scores
# test<-compute_table(matches)


#get_qualified_3_vec(test)

# ---------------------------------
# SIMULATION
# ---------------------------------

simulate_once <- function(matches){
    
    sim <- matches
    
    idx <- which(
        is.na(sim$Score_Home) |
            is.na(sim$Score_Away)
    )
    
    sim$Score_Home[idx] <- rpois(length(idx), 1.4)
    sim$Score_Away[idx] <- rpois(length(idx), 1.2)
    
    compute_table(sim)
}

run_simulations <- function(matches, n = N_SIM){
    
    sims <- map(
        1:n,
        ~simulate_once(matches)
    )
    
    winners <- map_chr(sims, ~ .x$team[1])
    
    tibble(team = winners) %>%
        count(team) %>%
        mutate(
            probability = round(100 * n / sum(n), 1)
        ) %>%
        arrange(desc(probability))
}