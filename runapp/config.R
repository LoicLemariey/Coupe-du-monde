#config


prct_minimal<-0.03
date_update_proba<-"2026-06-22"

N_SIM <- 10000
tab_names <- c("A","B","C","D","E","F","G","H","I","J","K","L")

scores<-read_xlsx("www/scores.xlsx")
all_teams <- bind_rows(
    scores %>% select(Group, team = home),
    scores %>% select(Group, team = away)
) %>%
    distinct()


renderer_lock<- "function(instance, td) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    td.style.background = '#f0f0f0';
                }
            "



team_colors <- c(
    "Mexico" = "#006847",
    "South Korea" = "#E60026",
    "Czech Republic" = "#11457E",
    "South Africa" = "#007A4D",
    "Canada" = "#D80621",
    "Qatar" = "#8A1538",
    "Switzerland" = "#FF0000",
    "Bosnia" = "#002F6C",
    "Brazil" = "#FFDF00",
    "Haiti" = "#0038A8",
    "Scotland" = "#2A5CAA",
    "Morocco" = "#C1272D",
    "United States" = "#3C3B6E",
    "Australia" = "#FFB81C",
    "Turkey" = "#E30A17",
    "Paraguay" = "#D52B1E",
    "Netherlands" = "#FF6F00",
    "Sweden" = "#006AA7",
    "Tunisia" = "#E70013",
    "Japan" = "#001489",
    "Belgium" = "#FFD100",
    "Iran" = "#239F40",
    "New Zealand" = "grey90",
    "Egypt" = "#CE1126",
    "Spain" = "#AA151B",
    "Saudi Arabia" = "#006C35",
    "Uruguay" = "#6CB4EE",
    "Cape Verde" = "#003893",
    "France" = "#0055A4",
    "Iraq" = "#00843D",
    "Norway" = "#BA0C2F",
    "Senegal" = "#00853F",
    "Argentina" = "#75AADB",
    "Austria" = "#ED2939",
    "Jordan" = "#007A3D",
    "Algeria" = "#006233",
    "Portugal" = "#046A38",
    "Uzbekistan" = "#0099B5",
    "Colombia" = "#FCD116",
    "DR Congo" = "#00A3E0",
    "England" = "#C8102E",
    "Ghana" = "#F9423A",
    "Panama" = "#005EB8",
    "Croatia" = "#D00027",
    "Germany" = "yellow",
    "Ivory Coast" = "#F77F00",
    "Ecuador" = "#F4D000",
    "Curacao" = "#002B7F"
)