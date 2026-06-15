

h2h_rank_fast<- function(block, all_matches_dt){
    
    block_dt <- as.data.table(block)
    block_dt[, `:=`(Points_h2h = 0, GD_h2h = 0, GF_h2h = 0)]
    
    # Trouver les égalités
    ties <- block_dt[, .N, by = Points][N > 1]$Points
    if (length(ties) == 0){
        block_dt <- block_dt[order(-Points, -GD, -GF)]
        block_dt[, rank := seq_len(.N)]
        return(as.data.frame(block_dt))
    }
    
    # Calcul H2H pour chaque niveau d’égalité
    for (p in ties){
        tied_teams <- block_dt[Points == p, team]
        
        h2h <- all_matches_dt[
            team %in% tied_teams & opponent %in% tied_teams,
            .(
                Points_h2h = sum(Pts),
                GD_h2h = sum(GF - GA),
                GF_h2h = sum(GF)
            ),
            by = team
        ]
        
        block_dt[h2h, `:=`(
            Points_h2h = i.Points_h2h,
            GD_h2h = i.GD_h2h,
            GF_h2h = i.GF_h2h
        ), on = "team"]
    }
    
    block_dt <- block_dt[
        order(
            -Points,
            -Points_h2h,
            -GD_h2h,
            -GF_h2h,
            -GD,
            -GF
        )
    ]
    block_dt[, rank := seq_len(.N)]
    
    as.data.frame(block_dt)
}

compute_table_fast <- function(matches){
    
    t1 <- Sys.time()
    
    matches_dt <- as.data.table(matches)
    
    # Toutes les équipes
    all_teams <- unique(rbind(
        matches_dt[, .(Group, team = home)],
        matches_dt[, .(Group, team = away)]
    ))
    
    # Matches joués
    played <- matches_dt[!is.na(Score_Home) & !is.na(Score_Away)]
    
    # Home
    home <- played[, .(
        Group, team = home, opponent = away,
        GF = Score_Home, GA = Score_Away,
        Pts = fifelse(Score_Home > Score_Away, 3,
                      fifelse(Score_Home == Score_Away, 1, 0))
    )]
    
    # Away
    away <- played[, .(
        Group, team = away, opponent = home,
        GF = Score_Away, GA = Score_Home,
        Pts = fifelse(Score_Away > Score_Home, 3,
                      fifelse(Score_Away == Score_Home, 1, 0))
    )]
    
    all_matches_dt <- rbind(home, away)
    
    # Table globale
    table_global <- all_matches_dt[
        , .(
            Points = sum(Pts),
            Match = .N,
            GF = sum(GF),
            GA = sum(GA),
            GD = sum(GF - GA)
        ),
        by = .(Group, team)
    ]
    
    table_global <- merge(
        all_teams, table_global,
        by = c("Group", "team"), all.x = TRUE
    )
    
    table_global[is.na(Points), `:=`(
        Points = 0, Match = 0, GF = 0, GA = 0, GD = 0
    )]
    
    t2 <- Sys.time()
    
    # Classement par groupe
    table_global <- table_global[
        , h2h_rank_fast(.SD, all_matches_dt),
        by = Group
    ][order(Group, rank)]
    
    t3 <- Sys.time()
    
    # Meilleurs 3e
    thirds <- table_global[rank == 3][
        order(-Points, -GD, -GF)
    ][, rank_third := seq_len(.N)]
    
    # Qualification
    table_global <- merge(
        table_global, thirds[, .(team, rank_third)],
        by = "team", all.x = TRUE
    )
    
    table_global[, Qualified :=
                     (rank <= 2) | (rank == 3 & rank_third <= 8)
    ]
    
    table_global[, Qualification :=
                     ifelse(Qualified, "🟢 Qualified", "🔴 Eliminated")
    ]
    
    t4 <- Sys.time()
    #print(diff(c(t1, t2, t3, t4)))
    
    as.data.frame(table_global)
}




#--------nouvelle version-------------------------------------------------------



h2h_rank_fast2 <- function(block_dt, h2h_all){
    
    # block_dt est déjà un data.table
    block_dt[, `:=`(Points_h2h = 0, GD_h2h = 0, GF_h2h = 0)]
    
    # Identifier les égalités
    ties <- block_dt[, .N, by = Points][N > 1]$Points
    if (length(ties) == 0){
        block_dt <- block_dt[order(-Points, -GD, -GF)]
        block_dt[, rank := seq_len(.N)]
        return(block_dt)
    }
    
    # Pour chaque bloc d’égalité, on ne fait qu’un filtrage (pas de calcul)
    for (p in ties){
        tied_teams <- block_dt[Points == p, team]
        
        h2h_block <- h2h_all[
            team %in% tied_teams & opponent %in% tied_teams
        ][
            , .(
                Points_h2h = sum(Points_h2h),
                GD_h2h = sum(GD_h2h),
                GF_h2h = sum(GF_h2h)
            ),
            by = team
        ]
        
        block_dt[h2h_block, `:=`(
            Points_h2h = i.Points_h2h,
            GD_h2h = i.GD_h2h,
            GF_h2h = i.GF_h2h
        ), on = "team"]
    }
    
    # Classement final
    block_dt <- block_dt[
        order(
            -Points,
            -Points_h2h,
            -GD_h2h,
            -GF_h2h,
            -GD,
            -GF
        )
    ]
    block_dt[, rank := seq_len(.N)]
    
    block_dt
}

compute_table_fast2 <- function(matches){
    
    t1 <- Sys.time()
    
    matches_dt <- as.data.table(matches)
    
    # Toutes les équipes
    all_teams <- unique(rbind(
        matches_dt[, .(Group, team = home)],
        matches_dt[, .(Group, team = away)]
    ))
    
    # Matches joués
    played <- matches_dt[!is.na(Score_Home) & !is.na(Score_Away)]
    
    # Home
    home <- played[, .(
        Group, team = home, opponent = away,
        GF = Score_Home, GA = Score_Away,
        Pts = fifelse(Score_Home > Score_Away, 3,
                      fifelse(Score_Home == Score_Away, 1, 0))
    )]
    
    # Away
    away <- played[, .(
        Group, team = away, opponent = home,
        GF = Score_Away, GA = Score_Home,
        Pts = fifelse(Score_Away > Score_Home, 3,
                      fifelse(Score_Away == Score_Home, 1, 0))
    )]
    
    all_matches_dt <- rbind(home, away)
    
    # Pré-calcul global du H2H (solution 1)
    h2h_all <- all_matches_dt[
        , .(
            Points_h2h = sum(Pts),
            GD_h2h = sum(GF - GA),
            GF_h2h = sum(GF)
        ),
        by = .(team, opponent)
    ]
    
    t2 <- Sys.time()
    
    # Table globale
    table_global <- all_matches_dt[
        , .(
            Points = sum(Pts),
            Match = .N,
            GF = sum(GF),
            GA = sum(GA),
            GD = sum(GF - GA)
        ),
        by = .(Group, team)
    ]
    
    table_global <- merge(
        all_teams, table_global,
        by = c("Group", "team"), all.x = TRUE
    )
    
    table_global[is.na(Points), `:=`(
        Points = 0, Match = 0, GF = 0, GA = 0, GD = 0
    )]
    
    # Classement par groupe (solution 2 : 100% data.table)
    table_global <- table_global[
        , h2h_rank_fast2(copy(.SD), h2h_all),
        by = Group
    ][order(Group, rank)]
    
    t3 <- Sys.time()
    
    # Meilleurs 3e
    thirds <- table_global[rank == 3][
        order(-Points, -GD, -GF)
    ][, rank_third := seq_len(.N)]
    
    # Qualification
    table_global <- merge(
        table_global, thirds[, .(team, rank_third)],
        by = "team", all.x = TRUE
    )
    
    table_global[, Qualified :=
                     (rank <= 2) | (rank == 3 & rank_third <= 8)
    ]
    
    table_global[, Qualification :=
                     ifelse(Qualified, "🟢 Qualified", "🔴 Eliminated")
    ]
    
    t4 <- Sys.time()
    # print(diff(c(t1, t2, t3, t4)))
    
    table_global
}




#------------versionRCPP--------------------------------------------------------

#sourceCpp("runapp/h2h_cpp.cpp")



sourceCpp("h2h_cpp.cpp")

h2h_rank_rcpp <- function(block_dt, all_matches_dt){
    
    block_dt[, `:=`(Points_h2h = 0, GD_h2h = 0, GF_h2h = 0)]
    
    ties <- block_dt[, .N, by = Points][N > 1]$Points
    if (length(ties) == 0){
        block_dt <- block_dt[order(-Points, -GD, -GF)]
        block_dt[, rank := seq_len(.N)]
        return(block_dt)
    }
    
    # Préparer les vecteurs pour Rcpp
    teams  <- all_matches_dt$team
    opp    <- all_matches_dt$opponent
    pts    <- all_matches_dt$Pts
    gf     <- all_matches_dt$GF
    ga     <- all_matches_dt$GA
    
    for (p in ties){
        tied_teams <- block_dt[Points == p, team]
        
        h2h_block <- h2h_cpp(
            teams, opp, pts, gf, ga,
            tied_teams
        )
        
        block_dt[h2h_block, `:=`(
            Points_h2h = i.Points_h2h,
            GD_h2h = i.GD_h2h,
            GF_h2h = i.GF_h2h
        ), on = "team"]
    }
    
    block_dt <- block_dt[
        order(
            -Points,
            -Points_h2h,
            -GD_h2h,
            -GF_h2h,
            -GD,
            -GF
        )
    ]
    block_dt[, rank := seq_len(.N)]
    
    block_dt
}


compute_table_fast_rcpp <- function(matches){
    
    t1 <- Sys.time()
    
    matches_dt <- as.data.table(matches)
    
    all_teams <- unique(rbind(
        matches_dt[, .(Group, team = home)],
        matches_dt[, .(Group, team = away)]
    ))
    
    played <- matches_dt[!is.na(Score_Home) & !is.na(Score_Away)]
    
    home <- played[, .(
        Group, team = home, opponent = away,
        GF = Score_Home, GA = Score_Away,
        Pts = fifelse(Score_Home > Score_Away, 3,
                      fifelse(Score_Home == Score_Away, 1, 0))
    )]
    
    away <- played[, .(
        Group, team = away, opponent = home,
        GF = Score_Away, GA = Score_Home,
        Pts = fifelse(Score_Away > Score_Home, 3,
                      fifelse(Score_Away == Score_Home, 1, 0))
    )]
    
    all_matches_dt <- rbind(home, away)
    
    t2 <- Sys.time()
    
    table_global <- all_matches_dt[
        , .(
            Points = sum(Pts),
            Match = .N,
            GF = sum(GF),
            GA = sum(GA),
            GD = sum(GF - GA)
        ),
        by = .(Group, team)
    ]
    
    table_global <- merge(
        all_teams, table_global,
        by = c("Group", "team"), all.x = TRUE
    )
    
    table_global[is.na(Points), `:=`(
        Points = 0, Match = 0, GF = 0, GA = 0, GD = 0
    )]
    
    table_global <- table_global[
        , h2h_rank_rcpp(copy(.SD), all_matches_dt),
        by = Group
    ][order(Group, rank)]
    
    t3 <- Sys.time()
    
    thirds <- table_global[rank == 3][
        order(-Points, -GD, -GF)
    ][, rank_third := seq_len(.N)]
    
    table_global <- merge(
        table_global, thirds[, .(team, rank_third)],
        by = "team", all.x = TRUE
    )
    
    table_global[, Qualified :=
                     (rank <= 2) | (rank == 3 & rank_third <= 8)
    ]
    
    table_global[, Qualification :=
                     ifelse(Qualified, "🟢 Qualified", "🔴 Eliminated")
    ]
    
    t4 <- Sys.time()
    print(diff(c(t1, t2, t3, t4)))
    
    table_global
}








#--------------------newrcpp----------------------------------------------------
sourceCpp("new_h2h_rcpp.cpp")

h2h_rank_rcpp <- function(block_dt, all_matches_dt){
    
    block_dt[, `:=`(Points_h2h = 0, GD_h2h = 0, GF_h2h = 0)]
    
    ties <- block_dt[, .N, by = Points][N > 1]$Points
    if (length(ties) == 0){
        block_dt <- block_dt[order(-Points, -GD, -GF)]
        block_dt[, rank := seq_len(.N)]
        return(block_dt)
    }
    
    # équipes du groupe (4 équipes max)
    teams_group <- block_dt$team
    team_id_map <- setNames(seq_along(teams_group) - 1L, teams_group)
    
    # matches internes au groupe
    matches_group <- all_matches_dt[
        team %in% teams_group & opponent %in% teams_group
    ]
    
    matches_group[, team_id := team_id_map[team]]
    matches_group[, opp_id  := team_id_map[opponent]]
    
    for (p in ties){
        tied_teams <- block_dt[Points == p, team]
        tied_ids   <- unname(team_id_map[tied_teams])
        
        res <- h2h_block_cpp(
            team_id = matches_group$team_id,
            opp_id  = matches_group$opp_id,
            pts     = matches_group$Pts,
            gf      = matches_group$GF,
            ga      = matches_group$GA,
            tied_ids = tied_ids
        )
        
        # res : colonnes 1=Points_h2h, 2=GD_h2h, 3=GF_h2h
        idx <- match(tied_teams, block_dt$team)
        block_dt[idx, `:=`(
            Points_h2h = res[, 1],
            GD_h2h     = res[, 2],
            GF_h2h     = res[, 3]
        )]
    }
    
    block_dt <- block_dt[
        order(
            -Points,
            -Points_h2h,
            -GD_h2h,
            -GF_h2h,
            -GD,
            -GF
        )
    ]
    block_dt[, rank := seq_len(.N)]
    
    block_dt
}


compute_table_fast_rcpp_new <- function(matches){
    
    t1 <- Sys.time()
    
    matches_dt <- as.data.table(matches)
    
    all_teams <- unique(rbind(
        matches_dt[, .(Group, team = home)],
        matches_dt[, .(Group, team = away)]
    ))
    
    played <- matches_dt[!is.na(Score_Home) & !is.na(Score_Away)]
    
    home <- played[, .(
        Group, team = home, opponent = away,
        GF = Score_Home, GA = Score_Away,
        Pts = fifelse(Score_Home > Score_Away, 3,
                      fifelse(Score_Home == Score_Away, 1, 0))
    )]
    
    away <- played[, .(
        Group, team = away, opponent = home,
        GF = Score_Away, GA = Score_Home,
        Pts = fifelse(Score_Away > Score_Home, 3,
                      fifelse(Score_Away == Score_Home, 1, 0))
    )]
    
    all_matches_dt <- rbind(home, away)
    
    t2 <- Sys.time()
    
    table_global <- all_matches_dt[
        , .(
            Points = sum(Pts),
            Match  = .N,
            GF     = sum(GF),
            GA     = sum(GA),
            GD     = sum(GF - GA)
        ),
        by = .(Group, team)
    ]
    
    table_global <- merge(
        all_teams, table_global,
        by = c("Group", "team"), all.x = TRUE
    )
    
    table_global[is.na(Points), `:=`(
        Points = 0, Match = 0, GF = 0, GA = 0, GD = 0
    )]
    
    table_global <- table_global[
        , h2h_rank_rcpp(copy(.SD), all_matches_dt),
        by = Group
    ][order(Group, rank)]
    
    t3 <- Sys.time()
    
    thirds <- table_global[rank == 3][
        order(-Points, -GD, -GF)
    ][, rank_third := seq_len(.N)]
    
    table_global <- merge(
        table_global, thirds[, .(team, rank_third)],
        by = "team", all.x = TRUE
    )
    
    table_global[, Qualified :=
                     (rank <= 2) | (rank == 3 & rank_third <= 8)
    ]
    
    table_global[, Qualification :=
                     ifelse(Qualified, "🟢 Qualified", "🔴 Eliminated")
    ]
    

    
    table_global
}


compute_table<-compute_table_fast_rcpp_new


#fonctionne mais meme temps



#-----------------------new optimiser 4 equipes--------------------------------
sourceCpp("h2h_rcpp2.cpp")

h2h_rank_rcpp <- function(block_dt, all_matches_dt){
    
    block_dt[, `:=`(Points_h2h = 0, GD_h2h = 0, GF_h2h = 0)]
    
    ties <- block_dt[, .N, by = Points][N > 1]$Points
    if (length(ties) == 0){
        block_dt <- block_dt[order(-Points, -GD, -GF)]
        block_dt[, rank := seq_len(.N)]
        return(block_dt)
    }
    
    # équipes du groupe (4 équipes)
    teams_group <- block_dt$team
    team_id_map <- setNames(0:(length(teams_group)-1), teams_group)
    
    # matches internes au groupe
    matches_group <- all_matches_dt[
        team %in% teams_group & opponent %in% teams_group
    ]
    
    matches_group[, team_id := team_id_map[team]]
    matches_group[, opp_id  := team_id_map[opponent]]
    
    for (p in ties){
        tied_teams <- block_dt[Points == p, team]
        tied_ids   <- unname(team_id_map[tied_teams])
        
        res <- h2h_cpp_fast(
            team_id = matches_group$team_id,
            opp_id  = matches_group$opp_id,
            pts     = matches_group$Pts,
            gf      = matches_group$GF,
            ga      = matches_group$GA,
            tied_ids = tied_ids
        )
        
        idx <- match(tied_teams, block_dt$team)
        block_dt[idx, `:=`(
            Points_h2h = res[, 1],
            GD_h2h     = res[, 2],
            GF_h2h     = res[, 3]
        )]
    }
    
    block_dt <- block_dt[
        order(
            -Points,
            -Points_h2h,
            -GD_h2h,
            -GF_h2h,
            -GD,
            -GF
        )
    ]
    block_dt[, rank := seq_len(.N)]
    
    block_dt
}


compute_table_fast_rcpp <- function(matches){
    
    t1 <- Sys.time()
    
    matches_dt <- as.data.table(matches)
    
    all_teams <- unique(rbind(
        matches_dt[, .(Group, team = home)],
        matches_dt[, .(Group, team = away)]
    ))
    
    played <- matches_dt[!is.na(Score_Home) & !is.na(Score_Away)]
    
    home <- played[, .(
        Group, team = home, opponent = away,
        GF = Score_Home, GA = Score_Away,
        Pts = fifelse(Score_Home > Score_Away, 3,
                      fifelse(Score_Home == Score_Away, 1, 0))
    )]
    
    away <- played[, .(
        Group, team = away, opponent = home,
        GF = Score_Away, GA = Score_Home,
        Pts = fifelse(Score_Away > Score_Home, 3,
                      fifelse(Score_Away == Score_Home, 1, 0))
    )]
    
    all_matches_dt <- rbind(home, away)
    
   
    
    table_global <- all_matches_dt[
        , .(
            Points = sum(Pts),
            Match  = .N,
            GF     = sum(GF),
            GA     = sum(GA),
            GD     = sum(GF - GA)
        ),
        by = .(Group, team)
    ]
    
    table_global <- merge(
        all_teams, table_global,
        by = c("Group", "team"), all.x = TRUE
    )
    
    table_global[is.na(Points), `:=`(
        Points = 0, Match = 0, GF = 0, GA = 0, GD = 0
    )]
    t2 <- Sys.time()
    table_global <- table_global[
        , h2h_rank_rcpp(copy(.SD), all_matches_dt),
        by = Group
    ][order(Group, rank)]
    
    t3 <- Sys.time()
    
    thirds <- table_global[rank == 3][
        order(-Points, -GD, -GF)
    ][, rank_third := seq_len(.N)]
    
    table_global <- merge(
        table_global, thirds[, .(team, rank_third)],
        by = "team", all.x = TRUE
    )
    
    table_global[, Qualified :=
                     (rank <= 2) | (rank == 3 & rank_third <= 8)
    ]
    
    table_global[, Qualification :=
                     ifelse(Qualified, "🟢 Qualified", "🔴 Eliminated")
    ]
    t4 <- Sys.time()
    print(diff(c(t2,t3)))
    print(diff(c(t1,t4)))
    table_global
}

compute_table<-compute_table_fast_rcpp
