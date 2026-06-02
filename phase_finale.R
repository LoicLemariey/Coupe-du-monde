library(shiny)
library(dplyr)

# =========================================================
# TABLEAU DES MATCHS (16e)
# =========================================================

bracket_table <- data.frame(
    id = 1:16,
    team_1 = rep(c("France","Brazil","Spain","Germany",
               "Italy","Portugal","England","Belgium"),2)
    ,
    team_2 = rep(c("Japan","USA","Mexico","Croatia",
               "Morocco","Netherlands","Denmark","Uruguay"),2)
)

# =========================================================
# UI
# =========================================================

ui <- fluidPage(

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
    
    titlePanel("Tournament Bracket"),
    
    fluidRow(
        class = "flex-center-row",
        column(2, uiOutput("round32_ui")),
        column(2,class="col-round16", uiOutput("round16_ui")),
        column(2,class = "col-quarter", uiOutput("quarter_ui")),
        column(2, class = "col-semi", uiOutput("semi_ui")),
        column(2,class = "col-final", uiOutput("final_ui"))#,
        #column(2,h1(textOutput("champion")))
    )
    

    #h1(textOutput("champion"))
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session){
    
    # -------------------------------------------------------
    # ROUND OF 16 WINNERS
    # -------------------------------------------------------
    
    winners_r32 <- reactive({
        sapply(1:nrow(bracket_table), function(i){
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
    
    # -------------------------------------------------------
    # UI 16e
    # -------------------------------------------------------
    
    output$round32_ui <- renderUI({
        tagList(
            div(class="round-title", "32èmes de finale"),
            lapply(1:nrow(bracket_table), function(i){
                div(class="match-box",
                    radioButtons(
                        paste0("r32_", i),
                        label = paste0("Round of 32: n°", i),
                        choices = c(bracket_table$team_1[i], bracket_table$team_2[i]),
                        selected = character(0)
                    )
                )
            })
        )
    })
    
    
    
    # -------------------------------------------------------
    # UI 16e
    # -------------------------------------------------------
    
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
    
    
    
    
    
    
    # -------------------------------------------------------
    # UI QUARTERS
    # -------------------------------------------------------
    
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
    
    # -------------------------------------------------------
    # UI SEMIS
    # -------------------------------------------------------
    
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
    
    # -------------------------------------------------------
    # UI FINAL
    # -------------------------------------------------------
    
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
    
    # -------------------------------------------------------
    # CHAMPION
    # -------------------------------------------------------
    
    output$champion <- renderText({
        champ <- winner_final()
        if(is.null(champ) || champ == "") return("")
        paste("🏆 Champion :", champ)
    })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
