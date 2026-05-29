#recover proba


library(jsonlite)
library(httr)
library(dplyr)
library(stringr)
library(readxl)
setwd("~/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026")
rm(list=ls())



scores<-read_xlsx("runapp/www/scores.xlsx")

match_ids<-scores$match_id[is.na(scores$Score_Home)]




url1<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/stats_match_form/66456904?T=exp=1779959808~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=8f45fb6ece1746b05544fc0bd9751ffc61340d74dd2e2e7e34345312d6ad759f"
url2<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/match_markets/66456904?T=exp=1779951168~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=37902744ac124612db7c35368bbf7eb7df78c47acc15cce9a507cc2aa13d7acf"
url3<-"https://lmt.fn.sportradar.com/common/fr/Etc:UTC/gismo/match_info/66456904?T=exp=1779968448~acl=/*~data=eyJvIjoiaHR0cHM6Ly93d3cud2luYW1heC5mciIsImEiOiJjMWQ4ODE1MWJiNjE0MGNjZjk2NzU5ZjEzM2RiYTAyZiIsImFjdCI6Im9yaWdpbmlnbm9yZWQiLCJvc3JjIjoib3JpZ2luIn0~hmac=1aad6e03f16d52a3f0c6a895c6073a6bbb2f358853945d033de2958ea11ed909"

n_match<-6*12



all_matchs_id<-as.character(seq(66456904,by=2,length.out=90))

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
    out<-list(matchs_df=matchs_df,
              list_proba_score = list_proba_score)
    return(out)
}

match_ids<-recover_fifa_matchs(all_matchs_id,url3)



url_infos <- sapply(match_ids, function(id) {
    stringr::str_replace(url1, "66456904", id)
})
url_markets <- sapply(match_ids, function(id) {
    stringr::str_replace(url2, "66456904", id)
})

#41 jamais plus de 6 buts en tout : 28 valeurs
#45 jamais  plus de 4 buts pour une equipe: 25 valeurs


res<-recover_proba(url_infos,url_markets,match_ids)
res$matchs_df
res$list_proba_score[[2]]

res$list_proba_score[[match_ids[3]]]
list<-res$list_proba_score

save(list,file=paste0(Sys.Date(),"Proba_list.Rdata"))
