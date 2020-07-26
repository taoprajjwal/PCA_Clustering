library(tidyverse)
library(ggplot2)
library(ggrepel)
library(stringr)


passing<-read_csv("18-19_player_passing_p90.csv")

players_names<-str_split_fixed(passing$Player,"\\\\",2)
passing$Player_Name<-players_names[,1]
passing$Player_ID<-players_names[,2]
passing$Pos1<-str_sub(passing$Pos,start=0,end=2)
passing$Pos2<-str_sub(passing$Pos,start=-2)


passing<-passing%>%mutate("Short%"=ShortAtt/TotAtt,"Medium%"=MediumAtt/TotAtt,"Long%"=LongAtt/TotAtt)

shooting<-read_csv("18-19_player_shooting_p90.csv")
shot_creation<-read_csv("18_19_player_shot_goal_creation.csv")
possession<-read_csv("18_19_player_possession_p90.csv")

df<-list(passing,possession,shooting,shot_creation)%>%reduce(inner_join,by=c("Player","Squad","90s"))

df_filtered<-data.frame(Name=df$Player_Name,Player_ID=df$Player_ID,Pos1=df$Pos1,Pos2=df$Pos2,GamesOver90=df$`90s`,ShortAtt=df$ShortAtt,Shortpct=df$`ShortCmp%`,Mediumatt=df$MediumAtt,Mediumpct=df$`MediumCmp%`,LongAtt=df$LongAtt,Longpct=df$`LongCmp%`,xA=df$xA,PassPrgDist=df$Prog,npXG=df$npxG,GxG=df$`np:G-xG`,SCA=df$SCA90,GCA=df$GCA90,DribbAtt=df$Att,DribleSucc=df$`Succ%`,Disposs=df$Dispos,PossPrgDist=df$PrgDist.y)
df_fw<-df_filtered%>%filter(GamesOver90>15 & ((Pos1=="FW") | Pos2=="FW"))
df_fw[is.na(df_fw)]<-0

###Every variable with PCA_hierarichal clustering single method

pca_values<-prcomp(df_fw[,6:21],scale=T)
ggplot(as.data.frame(pca_values$rotation),aes(x=PC1,y=PC2))+geom_point()+geom_text_repel(aes(label=rownames(pca_values$rotation)))+ggsave("pca_components.png")
plot(x=1:16,y=cumsum(pca_values$sdev^2/sum(pca_values$sdev^2)),type="o",xlab = "PCA components",ylab = "Variance Explained",xaxp=(c(1,16,15)))
hcluster_all<-hclust(dist(pca_values$x[,1:7]),method='single')
hcluster_all_df<-data.frame(players=df_fw$Name,PC1=pca_values$x[,1],PC2=pca_values$x[,2],cluster=cutree(hcluster_all,k=7))
dfggplot(hcluster_all_df,aes(x=PC1,y=PC2))+geom_point(aes(color=cluster))+geom_text_repel(aes(label=players,color=cluster))+scale_color_identity()+ggsave("hcluster_all_5.png",width=25,height=10)


### Bonus--> these were not included in the blog post!!
####Passing variables PCA with herirarichal clustering

df_fw_passing<-df_fw[,c(1:11,13,18:21)]
pca_values<-prcomp(df_fw_passing[,6:16],scale=T)
plot(x=1:11,y=cumsum(pca_values$sdev^2/sum(pca_values$sdev^2)),type="o")
ggplot(as.data.frame(pca_values$rotation),aes(x=PC1,y=PC2))+geom_point()+geom_text_repel(aes(label=rownames(pca_values$rotation)))+ggsave("pca_components_passing_map.png")
hcluster_passing<-hclust(dist(pca_values$x[,1:4]),method='single')
hcluster_passing_df<-data.frame(players=df_fw$Name,PC1=pca_values$x[,1],PC2=pca_values$x[,2],cluster=cutree(hcluster_passing,k=6))
ggplot(hcluster_passing_df,aes(x=PC1,y=PC2))+geom_point(aes(color=cluster))+geom_text_repel(aes(label=players,color=cluster))+scale_color_identity()+ggsave("hcluster_passing_single_6.png",width=25,height=10)

####Shooting variables PCA with heirarrichal clustering

df_fw_shooting<-df_fw[,c(1:5,12,14:17)]
pca_values<-prcomp(df_fw_shooting[,6:10],scale=T)
ggplot(as.data.frame(pca_values$rotation),aes(x=PC1,y=PC2))+geom_point()+geom_text_repel(aes(label=rownames(pca_values$rotation)))+ggsave("pca_components_shooting_map.png")
hcluster_shooting<-hclust(dist(scale(df_fw_shooting[,6:10])),method='single')
hcluster_shooting_df<-data.frame(players=df_fw$Name,PC1=pca_values$x[,1],PC2=pca_values$x[,2],cluster=cutree(hcluster_shooting,k=5))
ggplot(hcluster_shooting_df,aes(x=PC1,y=PC2))+geom_point(aes(color=cluster))+geom_text_repel(aes(label=players,color=cluster))+scale_color_identity()+ggsave("hcluster_shooting_complete_nonPCA.png",width=25,height=10)
