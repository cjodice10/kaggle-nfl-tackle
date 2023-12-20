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

###- READ IN PROCESSED DATA -###
analytic_weeks_6  <- read.csv(paste0(getwd(),"/data/02-intermediate/analytic-weeks-6.csv"),stringsAsFactors=FALSE)
analytic_weeks_7  <- read.csv(paste0(getwd(),"/data/02-intermediate/analytic-weeks-7.csv"),stringsAsFactors=FALSE)
analytic_weeks_8_9<- read.csv(paste0(getwd(),"/data/02-intermediate/analytic-weeks-8-9.csv"),stringsAsFactors=FALSE)

#- bind it
analytic_weeks    <- dplyr::bind_rows(analytic_weeks_6,analytic_weeks_7,analytic_weeks_8_9)
analytic_weeks_6  <- NULL
analytic_weeks_7  <- NULL
analytic_weeks_8_9<- NULL

##- create new frame ids, 1 through n for each play
analytic_weeks<- analytic_weeks %>%
  dplyr::group_by(gameidplayid,reference_nflid) %>%
  dplyr::arrange(frameid)%>%
  dplyr::mutate(frameid_new = row_number()) %>%
  data.frame

#- main id
analytic_weeks$id<- 1:nrow(analytic_weeks)

#- create gameidplayid_playerid
analytic_weeks$gameidplayid_playerid<- paste0(analytic_weeks$gameidplayid,"-",analytic_weeks$reference_nflid)

#---------------#
#-- CREATE DV --#
#---------------#
#- re-work the DV
#- For those who made tackle (or assist)
#-    make 1 for all steps 20 frames before tackle
dvs_playerid_gameids<- analytic_weeks %>% 
  dplyr::group_by(gameidplayid_playerid) %>% 
  dplyr::summarize(dv_max=max(dv),frameid_new_max=max(frameid_new)) %>% 
  dplyr::filter(dv_max==1) %>%
  dplyr::mutate(strt=frameid_new_max-20) %>%  #- 10
  dplyr::mutate(strt=ifelse(strt<=1,1,strt)) %>%
  data.frame
dvs_playerid_gameids %>% str


#- new dv_f
ids<- dvs_playerid_gameids$gameidplayid_playerid %>% unique

tmpdf<- data.frame()
for(i in ids){
  st<- dvs_playerid_gameids[which(dvs_playerid_gameids$gameidplayid_playerid==i),]$strt
  ed<- dvs_playerid_gameids[which(dvs_playerid_gameids$gameidplayid_playerid==i),]$frameid_new_max
  
  for(j in st:ed){
    tmpdf<- dplyr::bind_rows(tmpdf,data.frame(gameidplayid_playerid=i,frameid_new=j,dv_f=1))
  }
}


#- merge back with data
analytic_weeks %>% nrow
analytic_weeks<- merge(x    = analytic_weeks
                      ,y    = tmpdf
                      ,by.x = c("gameidplayid_playerid","frameid_new")
                      ,by.y = c("gameidplayid_playerid","frameid_new")
                      ,all.x= TRUE)
analytic_weeks %>% nrow

#- finalize DV
analytic_weeks$dv_f<- ifelse(is.na(analytic_weeks$dv_f),0,analytic_weeks$dv_f)
table(analytic_weeks$dv_f,exclude=NULL)


#- sort
analytic_weeks <- analytic_weeks %>%
  dplyr::arrange(gameidplayid,reference_nflid,frameid_new)
gc()


#- list of player - play - gameid
gameidplayid_playerid<- paste0(analytic_weeks$gameidplayid,"-",analytic_weeks$reference_nflid) %>% unique
gameidplayid_playerid %>% length
#- 45,562


#------------------------#
#- split into dev / val -#
#------------------------#
set.seed(321)
dev_ids<- sample(gameidplayid_playerid,round(length(gameidplayid_playerid)*0.7),replace=FALSE)

dev_df<- analytic_weeks %>% dplyr::filter(gameidplayid_playerid %in% dev_ids);print(nrow(dev_df))#- 1,173,499
val_df<- analytic_weeks %>% dplyr::filter(gameidplayid_playerid %ni% dev_ids);print(nrow(val_df))#- 504,067


#- predictors
var_2_use<- colnames(dev_df)
var_2_use<- var_2_use[var_2_use %ni% c("gameidplayid","reference_nflid","frameid","id","dv","dv_f","gameidplayid_playerid")] #frameid_new
var_2_use %>% length %>% print

#- dv
dv<- "dv_f"


#------------------#
#- PRE-PROCESSING -#
#------------------#
#- center and scale
preProcValues <- preProcess(dev_df[,var_2_use], method = "range")

#- transform it
#- dev
dev_transf <- predict(preProcValues, dev_df[,c(var_2_use)])

#- val
val_transf <- predict(preProcValues, val_df[,c(var_2_use)])

#- save preProcess
saveRDS(preProcValues, paste0(getwd(),"/model/preProcValues.rds"))
#- load it
#readRDS(paste0(getwd(),"/model/preProcValues.rds"))


#- bring back key variables
#- dev
dev_transf$gameidplayid     <- dev_df$gameidplayid
dev_transf$reference_nflid  <- dev_df$reference_nflid
dev_transf$frameid_new      <- dev_df$frameid_new
dev_transf$dv               <- dev_df$dv_f
dev_transf$gameidplayidnflid<- as.numeric(as.factor(paste0(dev_transf$gameidplayid,"-",dev_transf$reference_nflid)))

#- val
val_transf$gameidplayid     <- val_df$gameidplayid
val_transf$reference_nflid  <- val_df$reference_nflid
val_transf$frameid_new      <- val_df$frameid_new
val_transf$dv               <- val_df$dv_f
val_transf$gameidplayidnflid<- as.numeric(as.factor(paste0(val_df$gameidplayid,"-",val_df$reference_nflid)))


#- set up
dev_num_ids        <- dev_transf$gameidplayidnflid %>% unique %>% length
num_frames         <- max((dev_transf$frameid_new %>% unique %>% length),(val_transf$frameid_new %>% unique %>% length))
dev_unique_players <- dev_transf$reference_nflid %>% unique %>% sort
dev_unique_combinations<- dev_transf$gameidplayidnflid %>% unique
num_vars           <- length(var_2_use)

val_num_ids        <- val_transf$gameidplayidnflid %>% unique %>% length
val_unique_players <- val_transf$reference_nflid %>% unique %>% sort
val_unique_combinations<- val_transf$gameidplayidnflid %>% unique

message("Max frames: ",num_frames)


#- DEV
dev_x <- dev_transf[,var_2_use] %>% matrix %>% simplify2array
dev_y <- dev_transf[,"dv"] %>% to_categorical

#- VAL
val_x <- val_transf[,var_2_use] %>% matrix %>% simplify2array
val_y <- val_transf[,"dv"] %>% to_categorical


#- weights
#- 1's:   0.5 / (78419/1173499) = 7.5
#- 0's:   0.5 / (1095080/1173499) = 0.54


#- KERAS MODEL
gc()
k_clear_session()

model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = num_vars) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = 'sigmoid')

model %>%
  compile(optimizer = 'adam', 
          metrics = list( 'binary_accuracy'
                          ,'auc'=tf$keras$metrics$AUC()
                          ,'prec'=tf$keras$metrics$Precision()
                          ,'recall'=tf$keras$metrics$Recall()
          ), 
          loss = "binary_crossentropy"
  )

model %>% summary

#- FIT MODEL
model_history<- model %>% fit(
   x = dev_x
  ,y = dev_y
  ,batch_size = 50000
  #,validation_split=0.7
  ,epochs = 100
  ,verbose = 1
  ,class_weight =  list("0"=0.54,"1"=7.5)
  ,validation_data = list(val_x,val_y)
  ,shuffle = FALSE
)

plot(model_history,title='Metrics During Training')

model_history
#- get predictions

#- DEV
dev_pred_probs                  <- round(model %>% predict(dev_x,verbose=1) %>% data.frame %>% dplyr::rename(prob_tackle=X2) %>% dplyr::select(prob_tackle),4)
dev_pred_probs$dv               <- dev_transf[,"dv"]
dev_pred_probs$gameidplayid     <- dev_transf[,"gameidplayid"]
dev_pred_probs$reference_nflid  <- dev_transf[,"reference_nflid"]
dev_pred_probs$frameid_new      <- dev_transf[,"frameid_new"]
dev_pred_probs$gameidplayidnflid<- dev_transf[,"gameidplayidnflid"]

#- VAL
val_pred_probs                  <- round(model %>% predict(val_x,verbose=1) %>% data.frame %>% dplyr::rename(prob_tackle=X2) %>% dplyr::select(prob_tackle),4)
val_pred_probs$dv               <- val_transf[,"dv"]
val_pred_probs$gameidplayid     <- val_transf[,"gameidplayid"]
val_pred_probs$reference_nflid  <- val_transf[,"reference_nflid"]
val_pred_probs$frameid_new      <- val_transf[,"frameid_new"]
val_pred_probs$gameidplayidnflid<- val_transf[,"gameidplayidnflid"]


#- aggregate to player
dev_df_aggr_to_player_play<- aggr_to_player_play(dev_pred_probs,has_dv=TRUE)
val_df_aggr_to_player_play<- aggr_to_player_play(val_pred_probs,has_dv=TRUE)

#- ROC
dev_aggr_roc<-pROC::roc(dev_df_aggr_to_player_play$dv,dev_df_aggr_to_player_play$prob_tackle)
val_aggr_roc<-pROC::roc(val_df_aggr_to_player_play$dv,val_df_aggr_to_player_play$prob_tackle)

p_dev<-plot(dev_aggr_roc, ,main=paste0('Dev - Player/Play Level - AUROC: ',round(dev_aggr_roc$auc,3)))
p_val<-plot(val_aggr_roc, ,main=paste0('Val - Player/Play Level - AUROC: ',round(val_aggr_roc$auc,3)))

#- bind dev and val predictions
all_pred_probs<- dplyr::bind_rows(dev_pred_probs,val_pred_probs)
all_pred_probs %>% str %>% print

#- assign predicted class
dev_df_aggr_to_player_play<- get_pred_class(dev_df_aggr_to_player_play,0.95)
val_df_aggr_to_player_play<- get_pred_class(val_df_aggr_to_player_play,0.95)

dev_df_aggr_to_player_play %>% get_metrics
val_df_aggr_to_player_play %>% get_metrics

#- save model
save_model_hdf5(model,paste0(getwd(),"/model/model-prob-tackle-v3.hdf5"))


dev_df_aggr_to_player_play %>% str
dev_df_aggr_to_player_play %>% dplyr::group_by(dv,pred_class) %>% tally

