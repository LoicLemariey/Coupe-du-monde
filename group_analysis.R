#group_analysis
assignment<-read_xlsx("runapp/www/round32assignment.xlsx")
annexe <- read.csv(
    "runapp/www/third.txt",
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE
)[,-1]



assignment$R16<-rep(1:8,each=2)#huitieme
assignment$R8<-rep(1:4,each=4)
assignment$R4<-rep(1:2,each=8)#demi

lettres <- LETTERS[1:12]  # A à L

count_letter<-function(vec, letter){
    return(sum(vec==letter))
}


resultat <- sapply(lettres, function(l) {
    apply(annexe, 2, count_letter, letter = l)
})

resultat <- as.data.frame(resultat)
colnames(resultat) <- lettres


lettres_par_colonne <- apply(resultat, 2, function(x) {
    names(x)[x > 0]
})
lettres_par_colonne


group<-"A"
adversaire<-"E"
next_match<-function(group,adversaire){
    res<-"final"
    index<-which(is.na(assignment$group_team_2)&assignment$group_team_1==adversaire)
    index_group<-which(assignment$group_team_1==group|assignment$group_team_2==group)
    
    if(assignment$R4[index]%in%assignment$R4[index_group]){
        res<-"semi"
    }

    
    if(assignment$R8[index]%in%assignment$R8[index_group]){
        res<-"quarter"
    }
    
    if(assignment$R16[index]%in%assignment$R16[index_group]){
        res<-"8e"
    }

    return(res)
}




frequence_next_match<-function(resultat,group){
    tab<-resultat[,group]
    index<-which(tab!=0)
    letter<-row.names(resultat)[index]
    frequence<-tab[index]
    next_matches<-sapply(letter,next_match,group=group)
    t<-tapply(frequence, next_matches, sum)
    out<-paste(names(t),t)
    return(paste(out,collapse = "/"))
}


#build final data
all_groups<-names(resultat)
Group_rencontre<-sapply(all_groups,frequence_next_match,resultat=resultat)
possible_adversaire_group<-sapply(lettres_par_colonne,paste,collapse="/")
third<-data.frame(Group=all_groups,
                  Possible_oponnent=possible_adversaire_group,
                  next_match_same_group=Group_rencontre)

