#ui

tabs <- lapply(tab_names, function(m) {
    tabPanel(
        title = m,
        br(),
        

        
        
        div(class = "group",
            style = "
        display: flex;
        justify-content: center;
        gap: 30px;
        align-items: flex-start;
    ",
            
            div(
                style = "
            width: 700px;
            overflow-x: hidden;
        ",
                h5("Matches:"),
                rHandsontableOutput(paste0("table_", m))
            ),
            
            div(
                style = "
            width: 1000px;
            overflow-x: hidden;
        ",
                h5("Ranking Table:"),
                tableOutput(paste0("tab_", m))
            )
        )
        
        
        
    )
})


ui <- page_navbar(
    useShinyjs(),
    header = tags$head(
        includeCSS("www/styles.css")
    ),
    
    title = "FIFA World Cup",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    
#--------------TOURNAMENT STATE-------------------------------------------------
    
    nav_panel(
        "Tournament state",
        h5("Groups ranking + Round of 32 based on entered scores (real or fictive)"),


            
            #titlePanel("States"),
        
        
        fluidRow(
            
            column(
                width = 9,
                
                fileInput(
                    "file",
                    "Enter State file",
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
        
            
            do.call(tabsetPanel, c(
                tabs,
                list(
                    tabPanel(
                        title = "Best 3",
                        br(),
                        div(
                            class = "best3",
                            style = "display: flex; justify-content: center;",
                            
                            div(
                                style = "width: 60%;",
                                h5("Ranking table:"),
                                tableOutput("tab_best3")
                            )
                        )
                    ),
                    tabPanel(
                        title = "Round of 32",
                        br(),
                        div(
                            class = "round32",
                            style = "display: flex; justify-content: center;",
                            
                            textOutput("qualified_vec"),
                            
                            
                            
                            tags$script(HTML("

function updateTBD(){

    $('.match-box').each(function(){

        let radios = $(this).find('input[type=\"radio\"]');

        let hasTBD = false;

        radios.each(function(){
            if($(this).val() === 'TBD'){
                hasTBD = true;
            }
        });

        if(hasTBD){
            radios.prop('disabled', true);
        } else {
            radios.prop('disabled', false);
        }

    });

}

// IMPORTANT : Shiny hook (pas seulement document.ready)
$(document).on('shiny:value shiny:recalculated', function(){
    setTimeout(updateTBD, 50);
});

")),
                            tags$head(
                                tags$style(HTML("
      .match-box {
        border: 1px solid #444;
        border-radius: 8px;
        padding: 3px;
        margin-bottom: 12px;
        background: #f5f5f5;
      }
      .round-column {
        float: left;
        width: 22%;
        margin-right: 2%;
      }
      .round-title {
        font-size: 15px;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .clear { clear: both; }
      



.col-round16 .match-box {
    margin-bottom: 118px;
    margin-top: 60px;
}


.col-quarter .match-box {
    margin-bottom: 330px;
    margin-top: 160px;
}


.col-semi .match-box {
    margin-bottom: 765px;
    margin-top: 360px;
}



.col-final .match-box {
    margin-top: 750px;
}

    "))
                            ),
                            
                            
                            fluidRow(
                                class = "flex-center-row",
                                column(2, uiOutput("round32_ui")),
                                column(2,class="col-round16", uiOutput("round16_ui")),
                                column(2,class = "col-quarter", uiOutput("quarter_ui")),
                                column(2, class = "col-semi", uiOutput("semi_ui")),
                                column(2,class = "col-final", uiOutput("final_ui"))#,
                                #column(2,h1(textOutput("champion")))
                            )
                        )
                    )
                )
            ))

    ),
    
#------------SIMULATIONS--------------------------------------------------------
    
    nav_panel(
        "Qualifications Simulations",
        h5("Qualification and future opponents probability based on scores simulations (Monte Carlo Estimation)"),
        card(
            
            card_header("Probabilités de victoire"),
            
            tableOutput("simulations")
        )
    )
)