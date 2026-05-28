#globals
library(shiny)
library(bslib)
library(DT)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)

# ---------------------------------
# PARAMETRES
# ---------------------------------

N_SIM <- 5000

# ---------------------------------
# CLASSEMENT
# ---------------------------------



compute_table <- function(matches){
    
    played <- matches %>%
        filter(!is.na(Score_Home),
               !is.na(Score_Away))
    
    if(nrow(played) == 0){
        return(data.frame())
    }
    
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
                Score_Home == Score_Away ~ 1,
                TRUE ~ 0
            )
        )
    
    all_matches <- bind_rows(home, away)
    
    # ---- GLOBAL STATS ----
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
    
    # ---- HEAD-TO-HEAD FUNCTION ----
    h2h_rank <- function(df_group){
        
        df <- df_group
        
        # detect ties
        df <- df %>%
            mutate(tie_key = paste(Points, sep = "-"))
        
        # refine ties iteratively
        df <- df %>%
            group_by(Points) %>%
            group_modify(~{
                block <- .x
                
                if(nrow(block) <= 1) return(block)
                
                teams <- block$team
                
                h2h <- all_matches %>%
                    filter(team %in% teams, opponent %in% teams) %>%
                    group_by(team) %>%
                    summarise(
                        Points_h2h = sum(Pts),
                        GD_h2h = sum(GF - GA),
                        GF_h2h = sum(GF),
                        .groups = "drop"
                    )
                
                block %>%
                    left_join(h2h, by = "team") %>%
                    arrange(desc(Points_h2h),
                            desc(GD_h2h),
                            desc(GF_h2h))
            }) %>%
            ungroup()
        
        block <- df %>%
            arrange(desc(Points), desc(GD), desc(GF)) %>%
            mutate(rank = dense_rank(order(desc(Points),
                                           desc(Points_h2h),
                                           desc(GD_h2h),
                                           desc(GF_h2h)
                                           )))
        
        block
    }
    
    # ---- APPLY PER GROUP ----
    table_global<-table_global %>%
        group_by(Group) %>%
        group_modify(~h2h_rank(.x)) %>%
        ungroup() %>%
        arrange(Group, rank) %>% 
        mutate(across(where(is.numeric), as.integer))
    
    
    
    thirds <- table_global %>%
        filter(rank == 3) %>%
        arrange(desc(Points), desc(GD), desc(GF)) %>%
        mutate(rank_third = row_number())
    
    table_global <- table_global %>%
        left_join(thirds %>% select(team, rank_third), by = "team")
    
    table_global <- table_global %>%
        mutate(Qualified = (rank<=2)|(rank==3&rank_third<=8))
    
    return(table_global)
}


compute_table <- function(matches){
    
    library(dplyr)
    
    played <- matches %>%
        filter(!is.na(Score_Home),
               !is.na(Score_Away))
    
    if(nrow(played) == 0){
        return(data.frame())
    }
    
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
                Score_Home == Score_Away ~ 1,
                TRUE ~ 0
            )
        )
    
    all_matches <- bind_rows(home, away)
    
    # ---- GLOBAL STATS ----
    table_global <- all_matches %>%
        group_by(Group, team) %>%
        summarise(
            Points = sum(Pts, na.rm = TRUE),
            Match = n(),
            GF = sum(GF, na.rm = TRUE),
            GA = sum(GA, na.rm = TRUE),
            GD = GF - GA,
            .groups = "drop"
        )
    
    # ---- H2H SAFE RANKING ----
    h2h_rank <- function(df_group){
        
        df_group %>%
            group_modify(~{
                
                block <- .x
                
                # sécurité : toujours avoir Points
                if(!"Points" %in% names(block)){
                    block$Points <- 0
                }
                
                # PAS DE TIE → ranking direct
                if(nrow(block) <= 1){
                    return(block %>%
                               mutate(
                                   Points_h2h = Points,
                                   GD_h2h = GD,
                                   GF_h2h = GF,
                                   rank = 1
                               ))
                }
                
                teams <- block$team
                
                h2h <- all_matches %>%
                    filter(team %in% teams,
                           opponent %in% teams) %>%
                    group_by(team) %>%
                    summarise(
                        Points_h2h = sum(Pts, na.rm = TRUE),
                        GD_h2h = sum(GF - GA, na.rm = TRUE),
                        GF_h2h = sum(GF, na.rm = TRUE),
                        .groups = "drop"
                    )
                
                block %>%
                    left_join(h2h, by = "team") %>%
                    
                    # fallback si NA
                    mutate(
                        Points_h2h = ifelse(is.na(Points_h2h), Points, Points_h2h),
                        GD_h2h = ifelse(is.na(GD_h2h), GD, GD_h2h),
                        GF_h2h = ifelse(is.na(GF_h2h), GF, GF_h2h)
                    ) %>%
                    
                    arrange(desc(Points),
                            desc(Points_h2h),
                            desc(GD_h2h),
                            desc(GF_h2h)) %>%
                    
                    mutate(rank = row_number())
            }) %>%
            ungroup()
    }
    
    # ---- APPLY PER GROUP ----
    table_global <- table_global %>%
        group_by(Group) %>%
        group_modify(~h2h_rank(.x)) %>%
        ungroup() %>%
        arrange(Group, rank)%>% 
        mutate(across(where(is.numeric), as.integer))
    
    # ---- BEST 3 ----
    thirds <- table_global %>%
        filter(rank == 3) %>%
        arrange(desc(Points), desc(GD), desc(GF)) %>%
        mutate(rank_third = row_number())
    
    table_global <- table_global %>%
        left_join(thirds %>% select(team, rank_third), by = "team") %>%
        mutate(
            Qualified = (rank <= 2) | (rank == 3 & rank_third <= 8)
        )%>% 
        mutate(across(where(is.numeric), as.integer))
    
    return(table_global)
}


get_qualified_3_vec<-function(table_global){
    vec<-sort(table_global$Group[table_global$rank==3&table_global$Qualified])
    return(vec)
}


matches<-scores
test<-compute_table(matches)
get_qualified_3_vec(test)

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