
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