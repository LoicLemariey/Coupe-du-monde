#recover proba


library(jsonlite)
library(httr)
library(dplyr)
library(stringr)
library(readxl)
library(rvest)
library(readr)
library(xml2)
library(purrr)
setwd("~/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026")
rm(list=ls())






#----------translate team score-------------------------------------------------
scores<-read_xlsx("runapp/www/scores.xlsx")
countries<-read_xlsx("runapp/www/countries.xlsx")
scores2<-merge(scores,countries[,1:2],by.x="home",by.y="Team_Fr")
scores2<-merge(scores2,countries[,1:2],by.x="away",by.y="Team_Fr")
names(scores2)<-c("away_fr","home_fr","Group","Score_Home","Score_Away","match_id","home","away")
scores2<-scores2 %>% select(Group,match_id,home,Score_Home,Score_Away,away)
write_xlsx(scores2,"runapp/www/scores.xlsx")


load("runapp/www/elo_and_proba/2026-06-15Proba_list.Rdata")
# for(i in seq_along(list)) {
#     
#     scores_num <- do.call(
#         rbind,
#         strsplit(list[[i]]$name, ":", fixed = TRUE)
#     )
#     
#     list[[i]]$home_score <- as.integer(scores_num[,1])
#     list[[i]]$away_score <- as.integer(scores_num[,2])
# }
old_proba<-list





recover_proba<-function(url_infos,url_markets,match_ids){
    
    n_match<-length(url_infos)
    away<-character(n_match)
    home<-character(n_match)
    matchs_df<-data.frame(home,away,match_id=match_ids)
    list_proba_score<-list()
    
    for(i in 1:n_match){
        
        res <- GET(url_infos[i], add_headers(
            `User-Agent` = "Mozilla/5.0",
            `Referer` = "https://www.winamax.fr/",
            `Accept` = "application/json"
        ))
        
        content <- content(res, as = "text", encoding = "UTF-8")
        data <- fromJSON(content)
        matchs_df$home[i]<-data$doc$data$teams$home$team$name
        matchs_df$away[i]<-data$doc$data$teams$away$team$name
        
        print(matchs_df[i,])
        #match_market
        res <- GET(url_markets[i], add_headers(
            `User-Agent` = "Mozilla/5.0",
            `Referer` = "https://www.winamax.fr/",
            `Accept` = "application/json"
        ))
        
        content <- content(res, as = "text", encoding = "UTF-8")
        data_market <- fromJSON(content)
        market<-data_market$doc$data$markets[[1]]
        market_score<-market[market$`_marketId`==45,"outcomes"][[1]][,c("name","probability")]#41 ou 45
        
        list_proba_score[[i]]<-market_score_clean<-market_score %>% 
            filter(name!="autre") %>% 
            mutate(probability=probability/sum(probability))
    }
    names(list_proba_score)<-matchs_df$match_id
    
    list<-list_proba_score
    for(i in seq_along(list)) {
        
        scores_num <- do.call(
            rbind,
            strsplit(list[[i]]$name, ":", fixed = TRUE)
        )
        
        list[[i]]$home_score <- as.integer(scores_num[,1])
        list[[i]]$away_score <- as.integer(scores_num[,2])
    }
    
    
    
    out<-list(matchs_df=matchs_df,
              list_proba_score = list)
    return(out)
}


# 1) RECOVER PROBA WINAMAX -----------------------------------------------------
#a changer car elle change avec le temps.
new_url_stats<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/stats_match_form/66457028?T=exp=1781955648~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=d78848d8e6f13ff1acfad6f3252ea73bb1ec9a8ed0ca2a3ba0e00eef02c2a9b2"
new_url_market<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/match_markets/66457028?T=exp=1781955648~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=d78848d8e6f13ff1acfad6f3252ea73bb1ec9a8ed0ca2a3ba0e00eef02c2a9b2"



#41 jamais plus de 6 buts en tout : 28 valeurs
#45 jamais  plus de 4 buts pour une equipe: 25 valeurs






match_ids<-scores$match_id[is.na(scores$Score_Home)]
n_match<-6*12




url_infos <- sapply(match_ids, function(id) {
    stringr::str_replace(new_url_stats, "66457028", id)
})
url_markets <- sapply(match_ids, function(id) {
    stringr::str_replace(new_url_market, "66457028", id)
})


res<-recover_proba(url_infos,url_markets,match_ids)
res$list_proba_score[[match_ids[3]]]
list_new<-res$list_proba_score


n<-n_match-length(list_new)
list <- c(
    list_new,
    old_proba[!(names(old_proba) %in% names(list_new))]
)

save(list,file=paste0(Sys.Date(),"Proba_list.Rdata"))




#-----------2) recover elo----------------------------------


#ou a partir url

url_elo<-'https://www.eloratings.net/World.tsv?_=1780235318230'
res_elo <- GET(url_elo, add_headers(
    `User-Agent` = "Mozilla/5.0",
    `Referer` = "https://www.winamax.fr/",
    `Accept` = "application/json"
))
content_text_elo <- content(res_elo, as = "text", encoding = "UTF-8")


df_elo <- read_tsv(url_elo, col_names = FALSE) %>% mutate(id_iso = X3,
                                                      Elo = X4) %>% 
    select(id_iso,Elo)

coutries<-read_xlsx("runapp/www/countries.xlsx")
df_elo_world_cup<-merge(coutries,df_elo)
writexl::write_xlsx(df_elo_world_cup,paste0(Sys.Date(),"elo.xlsx"))



#----------------------------ARCHIV---------------------------------------------
#pour recuperer les ids des matches fifa
url3<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/match_info/66456904?T=exp=1779968448~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=1aad6e03f16d52a3f0c6a895c6073a6bbb2f358853945d033de2958ea11ed909"



recover_fifa_matchs<-function(all_matchs_id,url3){
    url_tournoi<- sapply(all_matchs_id, function(id) {
        stringr::str_replace(url3, "66456904", id)
    })
    tournament<-character(length(all_matchs_id))
    for (i in 1:90) {
        tournament[i] <- tryCatch({
            res <- GET(url_tournoi[i], add_headers(
                `User-Agent` = "Mozilla/5.0",
                `Referer` = "https://www.winamax.fr/",
                `Accept` = "application/json"
            ))
            
            #if (status_code(res) != 200) stop("HTTP error")
            
            content_text <- content(res, as = "text", encoding = "UTF-8")
            data <- fromJSON(content_text)
            
            # sécurité structure
            if (is.null(data$doc) ||
                is.null(data$doc$data) ||
                is.null(data$doc$data$tournament) ||
                is.null(data$doc$data$tournament$name)) {
                NA_character_
            }else{
                data$doc$data$tournament$name
            }
            
            
        }, error = function(e) {
            NA_character_
        })
    }
    bool <- stringr::str_detect(tournament, "Coupe du Monde FIFA")
    bool[is.na(bool)] <- FALSE
    matchs_ids <- all_matchs_id[bool]
    return(matchs_ids)
}

all_matchs_id<-as.character(seq(66456904,by=2,length.out=90))
match_ids<-recover_fifa_matchs(all_matchs_id,url3)



# soit un fichier local html (plus le dossier)
doc <- read_html("World Football Elo Ratings.html")
rows <- html_elements(doc, "div.slick-row")
elo_df <- map_dfr(rows, function(x) {
    
    cells <- html_elements(x, ".slick-cell") |>
        html_text2()
    
    # adapter selon le nombre de colonnes
    if(length(cells) >= 3) {
        tibble(
            rank   = cells[1],
            team   = cells[2],
            rating = cells[3]
        )
    }
})

elo_df <- elo_df |>
    mutate(
        rank = as.integer(rank),
        rating = as.numeric(rating)
    )


#ou a partir du fichier excel




