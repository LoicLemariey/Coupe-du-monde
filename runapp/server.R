server <- function(input, output, session){
    
    matches <- reactiveVal(NULL)
    matches_init <- reactiveVal(NULL)
    rank_table <-reactiveVal()
    refresh<-reactiveVal(0)
    third_qualified<-reactiveVal()
    round32<-reactiveVal()
    tournament<-reactiveVal(tournament)
    
    
    observeEvent(matches(), {
        req(matches())
        #print(matches())
        rank_table(compute_table(matches()))
        #View(rank_table())
    })
    
    


    

    # --------------IMPORT EXCEL-----------------------------------------------

    
    
    observe(
        matches_init(readxl::read_excel("www/scores.xlsx") %>%
                    dplyr::mutate(
                        Score_Home = suppressWarnings(as.integer(Score_Home)),
                        Score_Away = suppressWarnings(as.integer(Score_Away))
                    )))
    
    
    observeEvent(input$file, {
        
        req(input$file)
        
        df <- tryCatch(
            readxl::read_excel(input$file$datapath),
            error = function(e) {
                showNotification("Erreur lecture fichier Excel", type = "error")
                return(NULL)
            }
        )
        
        if(is.null(df)) return()
        
        # colonnes attendues
        required_cols <- c(
            "Group",
            "home",
            "away",
            "Score_Home",
            "Score_Away"
        )
        
        missing_cols <- setdiff(required_cols, names(df))
        
        if(length(missing_cols) > 0){
            showNotification(
                paste("Colonnes manquantes :", paste(missing_cols, collapse = ", ")),
                type = "error",
                duration = 5
            )
            return()
        }
        

        df <- df %>%
            dplyr::mutate(
                Score_Home = suppressWarnings(as.integer(Score_Home)),
                Score_Away = suppressWarnings(as.integer(Score_Away))
            )
        
        matches_init(df)
        refresh(refresh()+1)
        showNotification("Fichier importé avec succès", type = "message")
        reset("file")
    })
    

    
    output$download_template <- downloadHandler(
        
        filename = function() {
            "scores_template.xlsx"
        },
        
        content = function(file) {
            file.copy(
                from = "www/scores.xlsx",
                to = file
            )
        }
    )
    
    
#------rankings and scores------------------------------------------------------
    
    lapply(tab_names, function(m) {
        
        local({
            mod <- m
            output[[paste0("tab_", mod)]] <- renderTable({
                
                if(!is.null(rank_table())){
                    rank_table() %>% filter(Group == mod) %>% 
                    select(rank,team,Qualification,Match,Points,GF,GA,GD,Points_h2h,GD_h2h,GF_h2h)
                }
            },class="ranking_table")
            
            
            output[[paste0("table_", mod)]] <- renderRHandsontable({
                refresh()
                rhandsontable(matches_init() %>%
                                  filter(Group == mod) %>% 
                                  select(home,Score_Home,Score_Away,away)
                              ,
                              rowHeaders = NULL) %>% 
                    hot_col(c("home","away"), readOnly = TRUE,renderer=renderer_lock) %>%
                    hot_col(2:3, className = "htCenter") %>%
                    hot_table(
                        allowInsertRow = FALSE,
                        allowRemoveRow = FALSE,
                        allowInsertColumn = FALSE,
                        allowRemoveColumn = FALSE,
                        stretchH = "none",
                        colWidths = c(120, 100, 100,120)

                    )
            })
            
            
            # ---------- Groups--------------------------------------------
            observeEvent(input[[paste0("table_", mod)]], {
                req(input[[paste0("table_", mod)]])
                req(matches_init())
                
                
                # table modifiée
                new_data <- hot_to_r(input[[paste0("table_", mod)]])
                # dataframe global
                current <- matches_init()
                
                # update des lignes correspondantes
                for (i in seq_len(nrow(new_data))) {
                    
                    
                    index<-current$home==new_data$home[i]&
                        current$away==new_data$away[i]
                    current[index, c("Score_Home","Score_Away")] <- 
                        new_data[i,c("Score_Home","Score_Away")]
                }
                
                # mise à jour globale
                matches(current)
            
        })
    })
    })
    
    
    

    
    # ---- BEST 3 ----
    output$tab_best3 <- renderTable({
        rank_table() %>%
            filter(rank == 3)%>% 
            select(rank_third,team,Qualification,Group,Match,Points,GD,GF) %>% 
            arrange(rank_third)
    })
    
    
    
    
    #------Round32-------------------------
    
    
    
    observe(third_qualified(paste(get_qualified_3_vec(rank_table()),collapse="/")))
    output$qualified_vec <- renderText({
        paste0("\u0033\uFE0F\u20E3",
               "Group of third team qualified:",
               third_qualified()
        )
    })
    
    #--building round32table from rank
    observe({
        req(rank_table())
        round32(compute_assignment(assignment,third_qualified(),rank_table()))
        tournament(rbind(round32(),tournament() %>% filter(Match_type!="Round of 32"))
                   )
# library(writexl)
#         write_xlsx(tournament(),"mon_tournoi.xlsx")
    #View(tournament())
    })
    
    # -------------------------------------------------------
    # ROUND OF 16 WINNERS
    # -------------------------------------------------------
    
    winners_r32 <- reactive({
        sapply(1:nrow(round32()), function(i){
            val <- input[[paste0("r32_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
    
    
    
    winners_r16 <- reactive({
        sapply(1:16, function(i){
            val <- input[[paste0("r16_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
    
    # -------------------------------------------------------
    # QUARTER WINNERS
    # -------------------------------------------------------
    
    winners_quarter <- reactive({
        sapply(1:4, function(i){
            val <- input[[paste0("q_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
    
    # -------------------------------------------------------
    # SEMI WINNERS
    # -------------------------------------------------------
    
    winners_semi <- reactive({
        sapply(1:2, function(i){
            val <- input[[paste0("s_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
    
    # -------------------------------------------------------
    # FINAL WINNER
    # -------------------------------------------------------
    
    winner_final <- reactive({
        val <- input[[paste0("f")]]
        if (is.null(val) || val == "") "TBD" else val
    })
    

    # --------------------------UI 16e-----------------------------
    
    output$round32_ui <- renderUI({
        tagList(
            div(class="round-title", "32èmes de finale"),
            lapply(1:nrow(round32()), function(i){
                div(class="match-box",
                    radioButtons(
                        paste0("r32_", i),
                        label = paste0("Round of 32: n°", i),
                        choices = c(round32()$team_1[i], round32()$team_2[i]),
                        selected = character(0)
                    )
                )
            })
        )
    })
    
    
    

    # -------------------------UI 8e------------------------------
    
    output$round16_ui <- renderUI({
        w<-winners_r32()
        tagList(
            div(class="round-title", "8èmes de finale"),
            lapply(1:8, function(i){
                
                t1 <- w[(i * 2) - 1]
                t2 <- w[(i * 2)]
                
                id_radio <- paste0("r16_", i)
                div(class="match-box",style="gap: 30px",
                    
                    radioButtons(
                        id_radio,
                        label = paste0("Round of 16: n°", i),
                        choices = c(t1, t2),
                        selected = isolate(input[[id_radio]])
                    )
                )
            })
        )
    })
    
    
    
    
    
    

    # --------------------------UI QUARTERS-----------------------------
    
    output$quarter_ui <- renderUI({
        w <- winners_r16()
        tagList(
            div(class="round-title", "Quarts de finale"),
            lapply(1:4, function(i){
                
                t1 <- w[(i * 2) - 1]
                t2 <- w[(i * 2)]
                
                id_radio <- paste0("q_", i)
                
                div(
                    class = "match-box",
                    
                    radioButtons(
                        id_radio,
                        label = paste0("Quarter: n°", i),
                        choices = c(t1, t2),
                        selected = isolate(input[[id_radio]])
                    ),
                    
                    
                )
            })
        )
        
    })
    
    # -----------------------UI SEMIS--------------------------------
    
    output$semi_ui <- renderUI({
        w <- winners_quarter()
        tagList(
            div(class="round-title", "Demi-finales"),
            lapply(1:2, function(i){
                t1 <- w[(i*2)-1]
                t2 <- w[(i*2)]
                div(class="match-box",
                    radioButtons(
                        paste0("s_", i),
                        label = paste0("Semi: n°", i),
                        choices = c(t1, t2),
                        selected = isolate(input[[paste0("s_", i)]])
                    )
                )
            })
        )
    })
    
    # -------------------------UI FINAL------------------------------
    
    output$final_ui <- renderUI({
        w <- winners_semi()
        div(
            class="round-title", "Finale",
            div(class="match-box",
                radioButtons(
                    "f",
                    label = "Final",
                    choices = c(w[1], w[2]),
                    selected = isolate(input[["f"]])
                ),
                
            ),
            h1(textOutput("champion"))
        )
    })
    

    # -----------------------CHAMPION--------------------------------
    
    output$champion <- renderText({
        champ <- winner_final()
        if(is.null(champ) || champ == "") return("")
        paste("🏆 Champion :", champ)
    })
    
    
    
    
    
    
    
#--------Simulations------------------------------------------------------------
    
    output$simulations <- renderTable({
        
        req(matches())
        
        df <- matches()
        
        complete <- all(
            !is.na(df$Score_Home) &
                !is.na(df$Score_Away)
        )
        
        if(complete){
            
            return(
                data.frame(
                    Info = "Tous les matchs sont complétés"
                )
            )
        }
        
        run_simulations(df)
    })
}