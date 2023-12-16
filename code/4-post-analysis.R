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
#model<- keras::load_model_hdf5(paste0(getwd(),"/model/model-prob-tackle-v2.hdf5"))

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

