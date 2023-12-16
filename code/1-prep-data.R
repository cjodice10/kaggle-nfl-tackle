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

#- dir
my_dir<- getwd()

#- bring in data
games          <- read.csv(paste0(my_dir,"/data/01-raw/games.csv")  ,stringsAsFactors=FALSE)
players        <- read.csv(paste0(my_dir,"/data/01-raw/players.csv"),stringsAsFactors=FALSE)
plays          <- read.csv(paste0(my_dir,"/data/01-raw/plays.csv")  ,stringsAsFactors=FALSE)
tackles        <- read.csv(paste0(my_dir,"/data/01-raw/tackles.csv"),stringsAsFactors=FALSE)

#- only bring in a few weeks worth of data
tracking_week_1<-  data.frame()
for(i in 6:9){
  df<-NULL
  message("File: ",paste0(my_dir,"/data/01-raw/tracking_week_",i,".csv"))
  df<- read.csv(paste0(my_dir,"/data/01-raw/tracking_week_",i,".csv"),stringsAsFactors=FALSE)
  tracking_week_1<- dplyr::bind_rows(tracking_week_1,df)
}


#- adjust height and weights
players<- players %>%
  dplyr::mutate(height_temp = height) %>%
  tidyr::separate(height_temp,c('feet','inches'),"-",convert=TRUE) %>%
  dplyr::mutate(height_inches = (12*feet + inches)) %>%
  dplyr::select(-feet,-inches)%>%
  data.frame();

#- bmi
players$bmi<- round(703*players$weight / (players$height_inches*players$height_inches),1)


#- check those plays with tackles / assists
tackles_aggr<- tackles %>%
  dplyr::mutate(tackle_or_assist = tackle + assist) %>%
  dplyr::group_by(gameId,playId) %>%
  dplyr::summarise( n = n()
                    ,sum_tackle = sum(tackle)
                    ,sum_assist = sum(assist)
                    ,sum_tackle_or_assist = sum(tackle_or_assist)
                    ,nflIds = paste(nflId,collapse=",")) %>%
  dplyr::arrange(gameId,playId) %>%
  data.frame


#- make adjustments - standardize x,y,direction,etc
tracking_week_1 <- get_adjustments(tracking_week_1)

#- game ids
ingame_id<- get_gameids(tracking_week_1)
df<- get_subsets(tracking_week_1,ingame_id)

#- prep
ex_play<- get_explay(df);nrow(ex_play)


#- first check that teams are abbrev the same across datasets
tracking_df_clubs<- table(tracking_week_1$club %>% unique) %>% data.frame %>% dplyr::filter(Var1 !='football') %>% dplyr::arrange(Var1)
plays_off        <- table(plays$possessionTeam %>% unique) %>% data.frame %>% dplyr::filter(Var1 !='football') %>% dplyr::arrange(Var1)
plays_def        <- table(plays$defensiveTeam  %>% unique) %>% data.frame %>% dplyr::filter(Var1 !='football') %>% dplyr::arrange(Var1)

cbind(tracking_df_clubs,plays_off,plays_def)
#- good - they match across set by visual inspection!


#- create defensive indicator
ex_play_tmp<- get_possTeam(ex_play)

#- subset to plays with tackle
ex_play_tmp<- get_playsWithTackle(ex_play_tmp)

#- subset to frameIds between snap and the tackle (no more, no less)
frame_ids_start<- get_startend(ex_play_tmp)
ex_play_tmp<- get_merge_startend(ex_play_tmp,frame_ids_start)

#- create an ID
ex_play_tmp  <- create_gameidplayid(ex_play_tmp)

#-------------------------#
#-- GENERATE ATTRIBUTES --#
#-------------------------#
#-- THIS WILL TAKE TIME --#
#-------------------------#
generate_attributes(ex_play_tmp_1)

#-- Attributes generated! --#

#- write it out
#- list of files in wd
filenames     <- list.files(paste0(getwd(),"/data/02-intermediate/"))
analytic_week1<- data.frame()

#- loop through each csv file
for(i in filenames){
  message("File: ",i)
  analytic_week1<- dplyr::bind_rows(analytic_week1,read.csv(i,stringsAsFactors = TRUE))
}

write.csv(analytic_week1,paste0(getwd(),"/analytic-weeks.csv"),row.names=FALSE)

#- DONE
