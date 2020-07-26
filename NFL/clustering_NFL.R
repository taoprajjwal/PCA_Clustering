library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)
seasons <- 2010:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

pbp_pass<-pbp%>%filter(pass==T)
pbp_rush<-pbp%>%filter(qb_scramble==T)

games<-pbp_pass%>%group_by(passer,game_id)%>%summarize(dropbacks=n(),epa=sum(qb_epa,na.rm=T),epa_success=sum(success,na.rm=T),completions=sum(complete_pass),fumbles=sum(fumble),sacks=sum(sack),qb_hit=sum(qb_hit),td=sum(touchdown),comp_air_epa=sum(comp_air_epa,na.rm=T),comp_yac_epa=sum(comp_yac_epa,na.rm=T),air_yards=sum(air_yards,na.rm=T),int=sum(interception),cpoe=sum(cpoe,na.rm=T),season=sum(season)/n())
season<-games%>%group_by(passer,season)%>%summarize_if(is.numeric,sum)
season_filtered<-season%>%filter(dropbacks>200)
season_filtered<-season_filtered%>%mutate(epa_p_dropback=epa/dropbacks,success_rate=epa_success/dropbacks,competions_rate=completions/dropbacks,fumble_rate=fumbles/dropbacks,sack_rate=sacks/dropbacks,td_rate=td/dropbacks,comp_air_epa_rate=comp_air_epa/completions,comp_yac_epa_rate=comp_yac_epa/completions,air_yards_per_completion=air_yards/completions,int_rate=int/dropbacks,cpoe_per_dropback=cpoe/dropbacks)%>%select(passer,season,epa_p_dropback,success_rate,competions_rate,fumble_rate,sack_rate,td_rate,comp_air_epa_rate,comp_yac_epa_rate,air_yards_per_completion,int_rate)

rush_game<-pbp_rush%>%group_by(passer,game_id)%>%summarize(carries=n(),yards=sum(yards_gained,na.rm = T),rushing_epa=sum(epa),season=sum(season)/n())
rush_season<-rush_game%>%group_by(passer,season)%>%summarize(carries=sum(carries),yards=sum(yards),rushing_epa=sum(rushing_epa),games=n())
rush_all<-rush_season%>%mutate(carries_per_game=carries/games,yards_per_carry=yards/carries,epa_per_carry=rushing_epa/carries)%>%select(passer,season,carries_per_game,yards_per_carry,epa_per_carry)

qb_df<-left_join(season_filtered,rush_all,by=c('passer','season'))
qb_df[is.na(qb_df)]<-0

###View Scaled for PCA interpretability
qb_df$season<-as.character(qb_df$season)
qb_df%>%ungroup()%>%mutate_if(is.numeric,scale)%>%view()


###PCA and Clustering
pca_values<-prcomp(qb_df[,3:15],scale=T)
ggplot(as.data.frame(pca_values$rotation),aes(x=PC1,y=PC2))+geom_point()+geom_text_repel(aes(label=rownames(pca_values$rotation)))+ggsave("nfl_pca_components.png")
plot(x=1:13,y=cumsum(pca_values$sdev^2/sum(pca_values$sdev^2)),type="o",xlab = "PCA components",ylab = "Variance Explained",xaxp=(c(1,13,12)))
hcluster_all<-hclust(dist(pca_values$x[,1:6]),method='single')
hcluster_all_df<-data.frame(players=paste(qb_df$passer,qb_df$season,sep=" "),PC1=pca_values$x[,1],PC2=pca_values$x[,2],cluster=cutree(hcluster_all,k=11))
ggplot(hcluster_all_df,aes(x=PC1,y=PC2))+geom_point(aes(color=cluster))+geom_text_repel(aes(label=players,color=cluster))+scale_color_identity()+ggsave("nfl_hcluster_all_10.png",width=25,height=10)

