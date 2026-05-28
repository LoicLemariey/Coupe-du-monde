#ui
ui <- page_navbar(
    
    title = "Tournoi Dashboard",
    
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    
    # -----------------------------
    # IMPORT
    # -----------------------------
    
    nav_panel(
        
        "Score",
        
        

            fluidRow(
                
                column(
                    width = 9,
                    
                    fileInput(
                        "file",
                        "Choisir un fichier",
                        accept = ".xlsx"
                    )
                ),
                
                column(
                    width = 3,
                    br(),
                    downloadButton(
                        outputId = "download_template",
                        label = "Télécharger le template"
                    )
                )
        ),
        
        card(
            
            full_screen = TRUE,
            
            card_header("Edition des scores"),
            
            DTOutput("scores_table")
        )
        
    ),
    
    # -----------------------------
    # SCORES
    # -----------------------------
    
    # nav_panel(
    #     
    #     "Scores",
    #     
    #     card(
    #         
    #         full_screen = TRUE,
    #         
    #         card_header("Edition des scores"),
    #         
    #         DTOutput("scores_table")
    #     )
    # ),
    
    # -----------------------------
    # CLASSEMENT
    # -----------------------------
    
    nav_panel(
        
        "Tournament state",
        h5("Groups ranking + Round of 32 based on entered scores (real or fictive)"),


            
            titlePanel("States"),
            
            do.call(tabsetPanel, c(
                tabs,
                list(
                    tabPanel(
                        title = "Best 3",
                        tableOutput("tab_best3")
                    )
                )
            ))

    ),
    
    # -----------------------------
    # SIMULATIONS
    # -----------------------------
    
    nav_panel(
        "Qualifications Simulations",
        h5("Qualification and future opponents probability based on scores simulations (Monte Carlo Estimation)"),
        card(
            
            card_header("Probabilités de victoire"),
            
            tableOutput("simulations")
        )
    )
)