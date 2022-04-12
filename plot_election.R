library(data.table)
library(ggplot2)
election<-fread("resultats-par-niveau-subcom-t1-france-entiere.txt",header = F,dec=",")
colnames(election)<-c("dep_num","dep","com_num","commune","etat_saisi","inscrits","abstention","abs_inscr","votants","vote_inscr","blancs","blancs_inscr","blancs_vote","nuls","nuls_inscr","nuls_vote","exprime","exprime_inscr","exprime_vote",paste(rep(c("num","s","Nom","Prenom","voix","voix_inscr","voix_expr"),12),rep(LETTERS[1:12],each=7),sep="_"))#,rep(c("","8"),12)

candidats<-foreach(col=c(paste0("_",LETTERS[1:12]),"blanc"),.combine = rbind,.multicombine = T)%do%{
  if(col!="blanc"){
    coltoget<-grep(col,colnames(election),value = T)
    colcom<-c("dep_num","dep","com_num","commune","etat_saisi","inscrits","abstention","abs_inscr","votants","vote_inscr","blancs","blancs_inscr","blancs_vote","nuls","nuls_inscr","nuls_vote","exprime","exprime_inscr","exprime_vote")
    coltoget<-c(colcom,coltoget)
    dt_res<-election[,..coltoget]
    colnames(dt_res)<-c(colcom,"num","s","Nom","Prenom","voix","voix_inscr","voix_expr")
    dt_res
  }else{
    
    colcom<-c("dep_num","dep","com_num","commune","etat_saisi","inscrits","abstention","abs_inscr","votants","vote_inscr","blancs","blancs_inscr","blancs_vote","nuls","nuls_inscr","nuls_vote","exprime","exprime_inscr","exprime_vote")
    dt_res<-election[,..colcom]
    dt_res$num<-13
    dt_res$s<-"B"
    dt_res$Nom<-"1_Blanc+nul"
    dt_res$Prenom<-"  "
    dt_res$voix<- dt_res$blancs + dt_res$nuls
    dt_res$voix_inscr<-dt_res$voix / dt_res$inscrits
    dt_res$voix_expr<-dt_res$voix / dt_res$exprime
    dt_res
  }
  
}


candidats<-candidats[order(dep_num,com_num)]
candidats[,nom_complet:=paste(Prenom , Nom)]
candidats[,voix_votans:=round((voix/votants)*100,2)]
candidats[,nom_complet_pour:=paste(Prenom , Nom,"=",voix_votans,"%")]

plot_commune<-function(name){
  commune_data<-candidats[commune==name,]
  legend_elec<-data.table(nom_complet=unique(commune_data$nom_complet_pour),couleur=c("brown","deeppink3","goldenrod1","skyblue4","royalblue1","royalblue4","orangered3","orchid1","yellowgreen","lightsteelblue","red4","steelblue1","white"))
  ordre<-c(3 , 5 , 7 , 6, 10 , 9,  4 , 2, 13, 12,  8 ,11,  1) # ordre du resultat final national
  legend_elec<-legend_elec[ordre,]
  print(commune_data[,.(nom_complet,voix,voix_votans)])
  
  ggplot(commune_data, aes(x="", y=voix, fill=nom_complet_pour)) + geom_bar(stat="identity", width=1,color="black") +coord_polar("y", start=0)+theme_void()+ggtitle(name)+scale_fill_manual(breaks = legend_elec$nom_complet,values=legend_elec$couleur)+labs(fill="Candidats",subtitle = paste0("Votants=",unique(commune_data$votants)))
  
}

plot_commune("Chambéry")
plot_commune("Le Bourget-du-Lac")
plot_commune("Aix-les-Bains")
plot_commune("Grenoble")



