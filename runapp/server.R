
# ---------------------------------
# SERVER
# ---------------------------------

server <- function(input, output, session){
    
    matches <- reactiveVal(NULL)
    rank_table <-reactiveVal()
    
    
    
    
    observeEvent(matches(), {
        print("observe matches compute rank")
        req(matches())
        #print(matches())
        rank_table(compute_table(matches()))
        #View(rank_table())
    })
    

    
    # -----------------------------
    # IMPORT EXCEL
    # -----------------------------
    
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
        
        # ---- colonnes attendues ----
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
        
        # ---- optionnel : nettoyage types ----
        df <- df %>%
            dplyr::mutate(
                Score_Home = suppressWarnings(as.integer(Score_Home)),
                Score_Away = suppressWarnings(as.integer(Score_Away))
            )
        
        matches(df)
        
        showNotification("Fichier importé avec succès", type = "message")
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
    
    
    # -----------------------------
    # TABLE EDITABLE
    # -----------------------------
    
    output$scores_table <- renderDT({
        
        req(matches())
        
        datatable(
            matches()[,-6] %>% arrange(Group),
            rownames = FALSE,
            editable = TRUE,
            options = list(pageLength = 6,
                           ordering = FALSE,
                           stateSave = TRUE)
        )
    })
    
    # -----------------------------
    # SAVE EDITS
    # -----------------------------
    
    observeEvent(input$scores_table_cell_edit, {
        print("modif")
        info <- input$scores_table_cell_edit
        
        i <- info$row
        j <- info$col + 1
        
        editable_cols <- c("Score_Home", "Score_Away")
        
        col_name <- names(matches())[j]
        
        # bloque autres colonnes
        if (!(col_name %in% editable_cols)) {
            return()
        }
        
        df <- matches()
        
        df[i, j] <- as.integer(info$value)
        
        matches(df)
    })
    
    # -----------------------------
    # CLASSEMENT
    # -----------------------------
    
    lapply(tab_names, function(m) {
        
        local({
            mod <- m
            
            output[[paste0("tab_", mod)]] <- renderTable({
                rank_table() %>% filter(Group == mod) %>% 
                    select(rank,team,Qualified,Match,Points,GF,GA,GD)
            })
        })
    })
    
    # ---- BEST 3 ----
    output$tab_best3 <- renderTable({
        
        rank_table() %>%
            filter(rank == 3)%>% 
            select(rank_third,team,Qualified,Group,Match,Points,GD,GF) %>% 
            arrange(rank_third)
    })
    
    # -----------------------------
    # SIMULATIONS
    # -----------------------------
    
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