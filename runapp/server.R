server <- function(input, output, session){
    
    matches <- reactiveVal(NULL)
    matches_init <- reactiveVal(NULL)
    rank_table <-reactiveVal()
    refresh<-reactiveVal(0)
    third_qualified<-reactiveVal()
    round32<-reactiveVal()
    tournament<-reactiveVal(tournament)
    simulation_res<-reactiveVal()
    simulation_res_filtered<-reactiveVal()
    
    team_filter<-reactiveVal()
    rank_filter<-reactiveVal()
    r32_filter<-reactiveVal()
    display_res<-reactiveVal()
    
    graph_result<-reactiveVal()
    graph_path<-reactiveVal()
    table_path<-reactiveVal()
    setup<-reactiveVal()
    display_simu_res<-reactiveVal(FALSE)
    
    observe(team_filter(input$list_team))
    observe(rank_filter(input$list_rank))
    observe(r32_filter(input$list_r32))
    
    observeEvent(refresh(), {
        #print(paste0("refresh: ",refresh()))
        req(matches_init())
    
        matches(matches_init())
        #print(head(matches()))

    })
    
    
    observeEvent(matches(), {
        req(matches())
        rank_table(compute_table(matches()))
        setup(prepare_tournament(matches()))
        #View(rank_table())
    })
    
    
    # observeEvent(c(simulation_res_filtered(),input$list_team),{
    #     req(simulation_res_filtered())
    #     team_frequency(input$list_team,simulation_res_filtered())
    # })
    


    

    # --------------IMPORT EXCEL-----------------------------------------------

    
    # matches_init <- reactiveVal(NULL)
    # observe(
    #     matches_init(readxl::read_excel("www/scores.xlsx") %>%
    #                 dplyr::mutate(
    #                     Score_Home = suppressWarnings(as.integer(Score_Home)),
    #                     Score_Away = suppressWarnings(as.integer(Score_Away))
    #                 )))
    
    matches_init <- reactiveVal(
        readxl::read_excel("www/scores.xlsx") %>%
            dplyr::mutate(
                Score_Home = suppressWarnings(as.integer(Score_Home)),
                Score_Away = suppressWarnings(as.integer(Score_Away))
            )
    )
    
    
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
                
                
                
                current <- isolate(matches())
                #current <- matches_init()
                # if(!is.null(matches())){
                #     current <- matches()
                # }
                
                
                # table modifiée
                new_data <- hot_to_r(input[[paste0("table_", mod)]])
                # dataframe global

                
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
        req(rank_table())
        rank_table() %>%
            filter(rank == 3)%>% 
            select(rank_third,team,Qualification,Group,Match,Points,GD,GF) %>% 
            arrange(rank_third)
    })
    
    
    
    
    ## ------Round32-------------------------
    
    
    
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
    

    ## ------ROUND OF 16 WINNERS----------

    
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

    # QUARTER WINNERS

    
    winners_quarter <- reactive({
        sapply(1:4, function(i){
            val <- input[[paste0("q_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
 
    # SEMI WINNERS
    
    winners_semi <- reactive({
        sapply(1:2, function(i){
            val <- input[[paste0("s_", i)]]
            if (is.null(val) || val == "") "TBD" else val
        })
    })
    
    # FINAL WINNER
   
    
    winner_final <- reactive({
        val <- input[[paste0("f")]]
        if (is.null(val) || val == "") "TBD" else val
    })
    

    # --------------------------UI 16e-----------------------------
    
    output$round32_ui <- renderUI({
        tagList(
            div(class="round-title", "Round of 32"),
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
            div(class="round-title", "Round of 16"),
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
            div(class="round-title", "Quarter"),
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
            div(class="round-title", "Semi"),
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
            class="round-title", "Final",
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
    
    ## --------------Compute out-------
    
    # observeEvent(input$btn_simulation, {
    #     results <- lapply(
    #         seq_len(N_SIM),
    #         function(i) one_simulation(matches(),
    #                                    assignment,
    #                                    tournament(),
    #                                    elo)
    #     )
    #     simulation_res(results)
    # })
    
    
    
    

    # observeEvent(input$btn_simulation, {
    # 
    #     withProgress(message = "Simulation en cours...", value = 0, {
    # 
    #         results <- lapply(seq_len(N_SIM), function(i) {
    # 
    #             incProgress(
    #                 1 / N_SIM,
    #                 detail = paste(i, "/", N_SIM)
    #             )
    # 
    #             one_simulation(
    #                 matches(),
    #                 assignment,
    #                 tournament(),
    #                 elo
    #             )
    #         })
    # 
    #         simulation_res(results)
    #     })
    # })
    
    
    observeEvent(input$btn_simulation, {
        t1<-Sys.time()
        
        # res_time<-profvis({
        #     one_simulation(matches(),
        #                    assignment,
        #                    tournament(),
        #                    elo)
        # })
        display_simu_res(TRUE)
        withProgress(message = "Simulation en cours...", value = 0, {
            
            step <- ceiling(N_SIM / 20)


            results <- lapply(seq_len(N_SIM), function(i) {
                
                if (i %% step == 0 || i == N_SIM) {
                    new_progress <- i / N_SIM
                    
                    incProgress(
                        amount = step/N_SIM,
                        detail = paste0(round(new_progress * 100), "%")
                    )
                    
                    
                }

                # one_simulation(
                #     matches(),
                #     assignment,
                #     tournament(),
                #     elo
                # )
                
                
                one_simulation_fast(
                    matches(),
                    assignment,
                    tournament(),
                    elo,
                    setup()
                )
                
            })
            
            simulation_res(results)
            
        })
        t2<-Sys.time()
        my_time<-difftime(t2,t1)
        print(my_time)
    })
        
    
    
    
    #####################################
    observe(
        {
            req(simulation_res())
            choices_available<-get_r32_after_filter(simulation_res(),
                                 team_filter(),
                                 rank_filter()
            )
            
            num<- as.numeric(sub("^n°([0-9]+).*", "\\1", choices_available))
            choices_available<-choices_available[order(choices_available != "All", num)]
            updateSelectInput(session,
                              "list_r32",
                              choices = choices_available)

            r32_filter(choices_available[1])
            #qu'est ce que je met si vide ?
        }
    )
    
    
    #filtre les données
    observeEvent(c(simulation_res(),
                   team_filter(),
                   rank_filter(),
                   r32_filter()), {
                       req(simulation_res())
                       t<-lapply(simulation_res(),
                                 filter_result,
                                 team_filter(), 
                                 rank=rank_filter(),
                                 r32=r32_filter())
                       index<-which(unlist(t))
                       simulation_res_filtered(simulation_res()[index])
                     
                       
                       display_res(length(simulation_res_filtered())!=0)
                   },priority=-1)
    
    
    
    #path
    observeEvent(c(simulation_res_filtered(),
                   display_res(),
                   rank_filter(),
                   team_filter()), {
                       req(simulation_res_filtered())
                       bool<-rank_filter()!=4
                       if(bool&display_res()){
                           my_path<-draw_path(simulation_res_filtered(),
                                              team_filter())
                           graph_path(my_path$graph)
                           table_path(my_path$table)

                       }else{
                           graph_path(NA)
                           table_path(NA)
                       }
                    
                   },priority=-2)
    
    
    
    #graph result
    observeEvent(c(simulation_res_filtered(),
                   display_res(),
                   team_filter()), {
                       
                       req(simulation_res_filtered())
                       if(display_res()){
                           graph_result(build_result_graph(simulation_res_filtered(),team_filter()))
                       }else{
                           graph_result(NA)
                       }
                       
                       
                   },priority=-2)
    
    
    ## -----RENDER -----------------------
    
    output$plot_rank_group<-renderPlot({
        req(simulation_res())
        draw_rank_graph(simulation_res(),team_filter())
    })
    output$plot_r32<-renderPlot({
        req(simulation_res())
        draw_r32_graph(simulation_res(),team_filter())
    })
    
    
    #avec simulation_res filtered
    output$plot_final_result<-renderPlot({
        graph_result()
    })
    
    
    output$plot_path<-renderPlot({
        graph_path()
    })
    
    
    output$table_path <- DT::renderDT({
        req(!is.na(table_path()))
        DT::datatable(
            table_path(),
            filter = "none",
            options = list(
                dom = "t",
                paging = FALSE,
                ordering = TRUE
            )
        )
    })
    
    
    observeEvent(input$show_table, {
        
        showModal(
            modalDialog(
                title = "Possible opponents table",
                
                div(
                    style = "height:700px; overflow-y:auto;",
                    DTOutput("table_path")
                ),
                
                easyClose = TRUE,
                size = "l"
            )
        )
        
    })
    
    
    output$txt_group <- renderText({
        paste("Group:",all_teams$Group[all_teams$team==input$list_team])
    })
    
    
    output$show_panel <- reactive({
        display_simu_res()
    })
    
    outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
 
    
    
    
    


}