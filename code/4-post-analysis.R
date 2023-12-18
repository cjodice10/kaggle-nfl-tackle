#- library
options(warn=-1)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(magick)
library(REdaS)
library(zoo)
library(caret)
library(keras)
library(tensorflow)
library(pROC)

#- source helpers
source(paste0(my_dir,"/code/helpers.R"))

`%ni%`<- Negate(`%in%`)

#- bring in model if needed
#model<- keras::load_model_hdf5(paste0(getwd(),"/model/model-prob-tackle-v3.hdf5"))

#- bring in other info
all_pred_probs_f<- merge( x = ex_play_tmp
                         ,y =  all_pred_probs
                         ,by.x = c("gameIdplayId","nflId","frameid_new")
                         ,by.y = c("gameidplayid","reference_nflid","frameid_new")
                         ,all.x=TRUE)
all_pred_probs_f %>% str 


#- if missing, it just means they were on offense or there was a play with no tackle
all_pred_probs_f$prob_tackle<- ifelse(is.na(all_pred_probs_f$prob_tackle),0,all_pred_probs_f$prob_tackle)

#- for plotting, scale the probs
all_pred_probs_f$prob_tackle_scale<- (all_pred_probs_f$prob_tackle+0.5) *3

#- merge with games
all_pred_probs_f<- merge( x=all_pred_probs_f
                         ,y=games %>% dplyr::select(gameId,gameDate) %>% dplyr::mutate(gameDate=as.Date(gameDate,"%m/%d/%Y"))
                         ,by="gameId"
                         ,all.x=TRUE)
all_pred_probs_f %>% str

#- create score for each player, for each play
#- create a rolling avg score
#-    use a 30-play rolling avg
defensive_player_preds_agg<- all_pred_probs_f %>%
  dplyr::filter(defense_ind == 1) %>%
  dplyr::group_by(gameId,gameDate,playId,nflId) %>%
  dplyr::summarize(max_score = max(prob_tackle)
                   ,actual = max(dv.x)) %>%
  dplyr::mutate(pred_tackle = ifelse(max_score>=0.95,1,0)) %>%
  dplyr::mutate( play_score = actual - pred_tackle
                ,play_score_prob = actual - max_score) %>%
  dplyr::arrange(nflId,gameDate,playId) %>%
  dplyr::group_by(nflId) %>%
  dplyr::mutate( score_final = rollapply(play_score,30,mean,align='right',fill=NA)
                ,score_final_prob = rollapply(play_score_prob,30,mean,align='right',fill=NA) ) %>%
  data.frame

summary(defensive_player_preds_agg[,c("play_score","score_final","play_score_prob","score_final_prob")])
defensive_player_preds_agg %>% str


#- merge to get player info
defensive_player_preds_agg_f<- merge(x = defensive_player_preds_agg
                                     ,y = players %>% dplyr::select(nflId,position,displayName)
                                     ,by = "nflId"
                                     ,all.x=TRUE)

#-----------------------------#
#- create standardized score -#
#-----------------------------#
score_mean<- mean(defensive_player_preds_agg_f$score_final,na.rm=TRUE)
score_sd<- sd(defensive_player_preds_agg_f$score_final,na.rm=TRUE)
score_mean;score_sd;

#- zscores
defensive_player_preds_agg_f$zscore_final<- (defensive_player_preds_agg_f$score_final - score_mean) / score_sd

#- histogram of z-score
ggplot(defensive_player_preds_agg_f,aes(x=zscore_final)) +
  geom_histogram(bins=20,color="white",fill="steelblue",alpha=0.7) +
  geom_vline(xintercept=c(-3,-2,-1,1,2,3),linetype='dashed',color="grey")+
  labs(title="Final (Z) Score Distribution",x="Score","Freq") +
  theme_minimal()
  

#- write it out
write.csv(defensive_player_preds_agg_f,paste0(getwd(),"/scored-data/defensive_player_preds_agg.csv"),row.names=FALSE)


#------------#
#- ANALYSIS -#
#------------#
#- for each person get last score
defensive_player_recent_score<- defensive_player_preds_agg_f %>%
  dplyr::group_by(nflId) %>%
  dplyr::arrange(gameId,playId) %>%
  dplyr::filter(row_number()==n()) %>%
  data.frame
defensive_player_recent_score %>% str

#- histogram of recent z-scores
ggplot(defensive_player_recent_score,aes(x=zscore_final)) +
  geom_histogram(bins=12,color="white",fill="steelblue",alpha=0.7) +
  geom_vline(xintercept=c(-2,-1,-0.5,0.5,1,2),linetype='dashed',color="grey")+
  labs(title="Most Recent Scores",x="Score","Freq") +
  theme_minimal()


#- get some plots
defensive_player_playcount<- defensive_player_preds_agg_f %>%
  dplyr::group_by(nflId) %>%
  dplyr::summarize(n=n()) %>%
  data.frame
defensive_player_playcount %>% str

#- merge to get total play count
defensive_player_recent_score %>% nrow
defensive_player_recent_score<- merge(x = defensive_player_recent_score,y=defensive_player_playcount,by="nflId",all.x=TRUE)
defensive_player_recent_score %>% str

#- top players
top_players<- defensive_player_recent_score %>% dplyr::filter(n>=61) %>% dplyr::arrange(desc(zscore_final)) %>% head(3) %>% dplyr::select(nflId)
top_players<- top_players$nflId

#- bottom players
bottom_players<- defensive_player_recent_score %>% dplyr::filter(n>=61) %>% dplyr::arrange(zscore_final) %>% head(3) %>% dplyr::select(nflId)
bottom_players<- bottom_players$nflId

#- others
set.seed(321)
other_players<- defensive_player_recent_score %>% dplyr::filter(n>=61 & nflId %ni% c(top_players,bottom_players)) %>% dplyr::sample_n(3) %>% dplyr::select(nflId)
other_players<- other_players$nflId

c(top_players,bottom_players,other_players) %>% unique


#- get last 30 plays
testing<- defensive_player_preds_agg_f %>%
  dplyr::filter(nflId %in% c(top_players,bottom_players,other_players)) %>%
  dplyr::group_by(nflId) %>%
  dplyr::mutate(id = 1:n()) %>%
  data.frame

testing<- testing %>%
  dplyr::group_by(nflId) %>%
  dplyr::filter(id >= (n() - 30)) %>%
  data.frame
testing %>% str

testing<- testing %>% dplyr::group_by(nflId) %>% dplyr::mutate(id=row_number()) %>% data.frame
testing$displayName %>% unique

#- plot
ggplot(data=testing %>% dplyr::mutate(nflId=as.character(nflId)), aes(x=id, y=zscore_final, group=displayName)) +
  geom_line(aes(color=displayName))+
  geom_point(aes(color=displayName)) +
  geom_hline(yintercept=c(-0.5,0.5),linetype='dashed')+
  geom_hline(yintercept=c(-2,2),linetype='dashed')+
  theme_minimal()

library(gganimate)
ggplot(data=testing, aes(x=id-30, y=zscore_final, group=displayName,color=displayName)) +
  geom_line()+
  geom_point() +
  geom_hline(yintercept=c(-0.5,0.5),linetype='dashed')+
  geom_hline(yintercept=c(-2,2),linetype='dashed')+
  labs(title="Player Performance - last 30 plays",x="Play (0 is most recent play)",y="Performance")+
  theme_bw()+
  transition_reveal(id)

# Save at gif:

anim_save(paste0(getwd(),"/pictures/player-performance-example.gif"))


