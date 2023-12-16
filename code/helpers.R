#- helper functions

#- functions -#

#- function for adjustments
get_adjustments<- function(indf){
  within(indf,{
    dir_rad           <- ((90-dir) %% 360) *pi/180;
    o_rad             <- ((90-o)   %% 360) *pi/180;
    
    std_X              <- ifelse(playDirection=="right",120-x,x);
    std_Y              <- ifelse(playDirection=="right",(160/3)-y,y);
    std_Yardline       <- ifelse(playDirection=="right",120-x,x);
    std_Orientation    <- ifelse(playDirection=="right",360-o,o);
    std_Orientation_rad<- ifelse(playDirection=="right",((pi+o_rad) %% (2*pi)),o_rad);
    std_Dir            <- ifelse(playDirection=="right",360-dir,dir);
    std_Dir_rad        <- ifelse(playDirection=="right",((pi+dir_rad) %% (2*pi)),dir_rad);
    
    #normalize the standard x and y
    norm_std_Y        <- ifelse(std_Y>(160/3),1,std_Y/(160/3));
    norm_std_X        <- ifelse(std_X>120,1,std_X/120);
  })
}

#- function to sort
get_gameids<- function(indf){indf[,"gameId"] %>% unique %>% sort}

#- function for filtering
get_subsets<- function(indf,ingameid){
  indf[which(indf[,"gameId"] %in% ingameid),] %>%
    dplyr::arrange(gameId,playId,frameId,nflId)
}

#- function to merge with tackles
get_explay<- function(indf){
  d<- merge(x = indf
            ,y = tackles %>% dplyr::select(gameId,playId,nflId,tackle,assist)
            ,by.x = c("gameId","playId","nflId")
            ,by.y = c("gameId","playId","nflId")
            ,all.x = TRUE) %>%
    dplyr::arrange(gameId,playId,nflId,frameId) %>%
    dplyr::mutate(plyr_tackle = ifelse(is.na(tackle),0,tackle)
                  ,plyr_assist = ifelse(is.na(assist),0,assist)) %>%
    dplyr::mutate(dv_plyr = ifelse(plyr_tackle==1,1,
                                   ifelse(plyr_assist==1,1,0))) %>%
    dplyr::mutate(dv = ifelse(is.na(event),0,
                              ifelse(event=='tackle' & dv_plyr==1,1,0))) %>%
    dplyr::select(-plyr_tackle,-plyr_assist,-dv_plyr)
  
  #- get player info
  d<- merge(x = d
            ,y = players %>% dplyr::select(nflId,height_inches,weight,bmi)
            ,by.x = c('nflId')
            ,by.y = c('nflId')
            ,all.x= TRUE)
  return(d)
}


#- function to get defense offense indicator
get_possTeam<- function(indf){
  merge(x = indf
        ,y = plays %>% dplyr::select(gameId,playId,possessionTeam,defensiveTeam)
        ,by.x = c('gameId','playId')
        ,by.y = c('gameId','playId')
        ,all.x=TRUE)  %>%
    dplyr::mutate(defense_ind = ifelse(club == defensiveTeam,1,0) )%>%
    data.frame
}

#- function merge with tackles
get_playsWithTackle<- function(indf){
  merge(x = indf
        ,y = tackles_aggr %>% dplyr::filter(sum_tackle_or_assist>0) %>% dplyr::select(gameId,playId)
        ,by.x = c('gameId','playId')
        ,by.y = c('gameId','playId'))%>% 
    dplyr::arrange(gameId,playId,nflId,frameId)
}

#- function get start/end frames
get_startend<- function(indf){
  #- start
  frame_ids_st<- indf %>% 
    dplyr::select(gameId,playId,frameId,event) %>% 
    unique %>%
    dplyr::filter(event %in% c('ball_snap','autoevent_ballsnap')) %>%
    dplyr::group_by(gameId,playId,) %>%
    dplyr::summarise(frameId_start = min(frameId)
                     ,.groups = 'drop')
  
  #- end
  frame_ids_en<- indf %>% 
    dplyr::select(gameId,playId,frameId,event) %>% 
    unique %>%
    dplyr::filter(event %in% c('tackle')) %>%
    dplyr::group_by(gameId,playId,) %>%
    dplyr::summarise(frameId_end   = max(frameId)
                     ,.groups = 'drop')
  
  #- bring together
  frame_ids_sten<- merge(x = frame_ids_st
                         ,y = frame_ids_en
                         ,by.x = c("gameId","playId")
                         ,by.y = c("gameId","playId")
                         ,all.y = TRUE)
  
  #- replace NA with 1
  frame_ids_sten$frameId_start<- ifelse(is.na(frame_ids_sten$frameId_start),1,frame_ids_sten$frameId_start)
  return(frame_ids_sten)
}

#- function to merge back w/ startend
get_merge_startend<- function(indf,instrtend){
  merge(x = indf
        ,y = instrtend
        ,by.x = c('gameId','playId')
        ,by.y = c('gameId','playId')) %>%
    dplyr::filter(frameId>=frameId_start & frameId<=frameId_end) %>%
    dplyr::arrange(gameId,playId,nflId,frameId) 
}

#- function to create gameidplayid
create_gameidplayid<- function(indf){
  d<- within(indf,{
    gameIdplayId<- paste(gameId,"-",playId)
    gameidplayid_playerid<- paste0(gameIdplayId,"-",nflId)
    nflId<- ifelse(is.na(nflId),9999999,nflId)
  })
  d %>%
    dplyr::group_by(gameIdplayId,nflId) %>%
    dplyr::arrange(frameId)%>%
    dplyr::mutate(frameid_new = row_number()) %>%
    data.frame
}

generate_attributes<- function(indf){
  #- then create metrics for each frameId|player combo for every play
  ex_play_tm<- indf %>% dplyr::arrange(gameId,playId)
  
  #- unique games
  games<- ex_play_tm$gameId %>% unique %>% sort
  
  len_of_games<- length(games)
  len_i<- 0
  
  #- loop through each gameId
  for(game in games){
    len_i<- len_i+1
    message("  ",len_i, " out of ",len_of_games)
    
    
    start_time<- Sys.time()
    message("gameId: ",game)
    
    #- create empty matrices
    frameid_f           <- matrix()
    gameidplayid_f      <- matrix()
    reference_nflid_f   <- matrix()
    other_nflid_f       <- matrix()
    other_def_ind_f     <- matrix()
    relative_distances_f<- matrix()
    relative_angles_f   <- matrix()
    diff_speed_f        <- matrix()
    diff_accel_f        <- matrix()
    diff_orien_f        <- matrix()
    diff_direc_f        <- matrix()
    rel_ht_f            <- matrix()
    rel_wt_f            <- matrix()
    rel_bmi_f           <- matrix()
    
    #- unique gameIds
    gameIdplayIds<- ex_play_tm[which(ex_play_tm$gameId %in% game),]$gameIdplayId %>% unique %>% sort
    message(" Total plays: ",length(gameIdplayIds))
    
    len_of_gameplays<- length(gameIdplayIds)
    
    #- iterate through each gameidplayid
    for(gameplay in gameIdplayIds){
      
      #- subset to gameplayid
      gameplay_df<- ex_play_tm %>% dplyr::filter(gameIdplayId == gameplay)
      
      #- unique frameIds
      timestamps <- gameplay_df$frameId %>% unique %>% sort
      timestamps<- timestamps
      
      #- iterate through each timestamp
      for(timestamp in timestamps){
        #- data at timestamp
        data_at_timestamp <- gameplay_df %>% filter(frameId == timestamp)
        
        #- nflIds on defense
        nflIds_def<- data_at_timestamp[which(data_at_timestamp$defense_ind==1),]$nflId %>% unique %>% sort
        
        #- iterate through defensive nflids
        for(nflid in nflIds_def){
          player_data<- data_at_timestamp %>% dplyr::filter(nflId == nflid)
          
          # Calculate the relative distances and angles for nflId
          gameidplayid        <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1) #itself and football
          frameid             <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          reference_nflid     <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          other_nflid         <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          other_def_ind       <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          relative_distances  <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          relative_distancesxy<- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=2)
          relative_angles     <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          diff_speed          <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          diff_accel          <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          diff_orien          <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          diff_direc          <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          rel_ht              <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          rel_wt              <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          rel_bmi             <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          dv                  <- matrix(0,nrow=nrow(data_at_timestamp)-1,ncol=1)
          
          #- iterate to calculate measures between each off player
          for(i in 1:nrow(player_data)){
            #- other plays
            other_players_data <- data_at_timestamp %>% filter(nflId != nflid)
            
            for(j in 1:nrow(other_players_data)){     
              frameid[j,]        <- c(timestamp)
              gameidplayid[j,]   <- c(gameplay)
              reference_nflid[j,]<- c(nflid)
              other_nflid[j,]    <- c(other_players_data[j,"nflId"])
              other_def_ind[j,]  <- c(other_players_data[j,"defense_ind"])
              #dv[j,]             <- c(other_players_data[j,"dv"])
              
              #- calc distance and angles
              relative_distances[j, ]  <-c(sqrt((player_data[i, "x"] - other_players_data[j, "x"])**2 + (player_data[i, "y"] - other_players_data[j, "y"])**2))
              relative_distancesxy[j, ]<-c(player_data[i, "x"] - other_players_data[j, "x"], player_data[i, "y"] - other_players_data[j, "y"])
              relative_angles[j,]      <-atan2(relative_distancesxy[j, 2], relative_distancesxy[j, 1])
              diff_speed[j, ]          <-c(player_data[i, "s"] - other_players_data[j, "s"])
              diff_accel[j, ]          <-c(player_data[i, "a"] - other_players_data[j, "a"])
              diff_orien[j, ]          <-c(player_data[i, "std_Orientation_rad"] - other_players_data[j, "std_Orientation_rad"])
              diff_direc[j, ]          <-c(player_data[i, "std_Dir_rad"] - other_players_data[j, "std_Dir_rad"])
              rel_ht[j, ]              <-c(player_data[i, "height_inches"] / other_players_data[j, "height_inches"])
              rel_wt[j, ]              <-c(player_data[i, "weight"] / other_players_data[j, "weight"])
              rel_bmi[j, ]             <-c(player_data[i, "bmi"] / other_players_data[j, "bmi"])
            }
            
            #- set it
            frameid_f           <- c(frameid_f,frameid)
            gameidplayid_f      <- c(gameidplayid_f,gameidplayid)
            reference_nflid_f   <- c(reference_nflid_f,reference_nflid)
            other_nflid_f       <- c(other_nflid_f,other_nflid)
            other_def_ind_f     <- c(other_def_ind_f,other_def_ind)
            #dv_f                <- c(dv_f,dv)
            relative_distances_f<- c(relative_distances_f,round(relative_distances,4))
            relative_angles_f   <- c(relative_angles_f,round(relative_angles,4))
            
            diff_speed_f<- c(diff_speed_f,round(diff_speed,4))
            diff_accel_f<- c(diff_accel_f,round(diff_accel,4))
            diff_orien_f<- c(diff_orien_f,round(diff_orien,4))
            diff_direc_f<- c(diff_direc_f,round(diff_direc,4))
            rel_ht_f    <- c(rel_ht_f,round(rel_ht,2))
            rel_wt_f    <- c(rel_wt_f,round(rel_wt,2))
            rel_bmi_f   <- c(rel_bmi_f,round(rel_bmi,2))
          }
        }
      }
    }
    
    message("   Created attributes.")
    tmpdf      <- NULL
    tmpdf2_off <- NULL
    tmpdf2_def <- NULL
    tmpdf2_foot<- NULL
    
    tmpdf<- data.frame(gameidplayid=gameidplayid_f
                       ,reference_nflid=reference_nflid_f
                       ,frameid=frameid_f
                       ,other_nflid=other_nflid_f
                       ,other_def_ind=other_def_ind_f
                       ,relative_distances=relative_distances_f
                       ,relative_angles=relative_angles_f
                       ,diff_speed=diff_speed_f
                       ,diff_accel=diff_accel_f
                       ,diff_orien=diff_orien_f
                       ,diff_direc=diff_direc_f
                       ,rel_ht=rel_ht_f
                       ,rel_wt=rel_wt_f
                       ,rel_bmi=rel_bmi_f
    )
    
    
    #- closest offensive players for each defensive player
    tmpdf2_off<- tmpdf %>%
      dplyr::filter(other_def_ind == 0 & other_nflid != 9999999) %>%
      dplyr::arrange(gameidplayid,reference_nflid,frameid,relative_distances) %>%
      dplyr::group_by(gameidplayid,reference_nflid,frameid) %>%
      dplyr::mutate(close_rank = row_number()) %>%
      dplyr::filter(close_rank <= 5)
    
    #- closest defensive players for each defensive player
    tmpdf2_def<- tmpdf %>%
      dplyr::filter(other_def_ind == 1 & other_nflid != 9999999) %>%
      dplyr::arrange(gameidplayid,reference_nflid,frameid,relative_distances) %>%
      dplyr::group_by(gameidplayid,reference_nflid,frameid) %>%
      dplyr::mutate(close_rank = row_number()) %>%
      dplyr::filter(close_rank <= 5)
    
    #- football
    tmpdf2_foot<- tmpdf %>%
      dplyr::filter(other_nflid == 9999999) %>%
      dplyr::arrange(gameidplayid,reference_nflid,frameid,relative_distances) 
    
    #- create empty dataframes
    foot<- data.frame()
    def1<- data.frame()
    def2<- data.frame()
    def3<- data.frame()
    def4<- data.frame()
    def5<- data.frame()
    off1<- data.frame()
    off2<- data.frame()
    off3<- data.frame()
    off4<- data.frame()
    off5<- data.frame()
    
    #- defense
    def1<- dplyr::bind_rows(def1, tmpdf2_def %>% dplyr::filter(close_rank == 1))
    def2<- dplyr::bind_rows(def2, tmpdf2_def %>% dplyr::filter(close_rank == 2))
    def3<- dplyr::bind_rows(def3, tmpdf2_def %>% dplyr::filter(close_rank == 3))
    def4<- dplyr::bind_rows(def4, tmpdf2_def %>% dplyr::filter(close_rank == 4))
    def5<- dplyr::bind_rows(def5, tmpdf2_def %>% dplyr::filter(close_rank == 5))
    
    #- offense
    off1<- dplyr::bind_rows(off1, tmpdf2_off %>% dplyr::filter(close_rank == 1))
    off2<- dplyr::bind_rows(off2, tmpdf2_off %>% dplyr::filter(close_rank == 2))
    off3<- dplyr::bind_rows(off3, tmpdf2_off %>% dplyr::filter(close_rank == 3))
    off4<- dplyr::bind_rows(off4, tmpdf2_off %>% dplyr::filter(close_rank == 4))
    off5<- dplyr::bind_rows(off5, tmpdf2_off %>% dplyr::filter(close_rank == 5))
    
    #- football
    foot<- dplyr::bind_rows(foot,tmpdf2_foot )
    
    #- colnames for defensive df
    colnames(def1)<- c("gameidplayid_d1","reference_nflid_d1","frameid_d1","other_nflid_d1","other_def_ind_d1","relative_distance_d1","relative_angle_d1","diff_speed_d1","diff_accel_d1","diff_orien_d1","diff_direc_d1","rel_ht_d1","rel_wt_d1","rel_bmi_d1","close_rank_d1")
    colnames(def2)<- c("gameidplayid_d2","reference_nflid_d2","frameid_d2","other_nflid_d2","other_def_ind_d2","relative_distance_d2","relative_angle_d2","diff_speed_d2","diff_accel_d2","diff_orien_d2","diff_direc_d2","rel_ht_d2","rel_wt_d2","rel_bmi_d2","close_rank_d2")
    colnames(def3)<- c("gameidplayid_d3","reference_nflid_d3","frameid_d3","other_nflid_d3","other_def_ind_d3","relative_distance_d3","relative_angle_d3","diff_speed_d3","diff_accel_d3","diff_orien_d3","diff_direc_d3","rel_ht_d3","rel_wt_d3","rel_bmi_d3","close_rank_d3")
    colnames(def4)<- c("gameidplayid_d4","reference_nflid_d4","frameid_d4","other_nflid_d4","other_def_ind_d4","relative_distance_d4","relative_angle_d4","diff_speed_d4","diff_accel_d4","diff_orien_d4","diff_direc_d4","rel_ht_d4","rel_wt_d4","rel_bmi_d4","close_rank_d4")
    colnames(def5)<- c("gameidplayid_d5","reference_nflid_d5","frameid_d5","other_nflid_d5","other_def_ind_d5","relative_distance_d5","relative_angle_d5","diff_speed_d5","diff_accel_d5","diff_orien_d5","diff_direc_d5","rel_ht_d5","rel_wt_d5","rel_bmi_d5","close_rank_d5")
    
    #- colnames for offense df
    colnames(off1)<- c("gameidplayid_o1","reference_nflid_o1","frameid_o1","other_nflid_o1","other_off_ind_o1","relative_distance_o1","relative_angle_o1","diff_speed_o1","diff_accel_o1","diff_orien_o1","diff_direc_o1","rel_ht_o1","rel_wt_o1","rel_bmi_o1","close_rank_o1")
    colnames(off2)<- c("gameidplayid_o2","reference_nflid_o2","frameid_o2","other_nflid_o2","other_off_ind_o2","relative_distance_o2","relative_angle_o2","diff_speed_o2","diff_accel_o2","diff_orien_o2","diff_direc_o2","rel_ht_o2","rel_wt_o2","rel_bmi_o2","close_rank_o2")
    colnames(off3)<- c("gameidplayid_o3","reference_nflid_o3","frameid_o3","other_nflid_o3","other_off_ind_o3","relative_distance_o3","relative_angle_o3","diff_speed_o3","diff_accel_o3","diff_orien_o3","diff_direc_o3","rel_ht_o3","rel_wt_o3","rel_bmi_o3","close_rank_o3")
    colnames(off4)<- c("gameidplayid_o4","reference_nflid_o4","frameid_o4","other_nflid_o4","other_off_ind_o4","relative_distance_o4","relative_angle_o4","diff_speed_o4","diff_accel_o4","diff_orien_o4","diff_direc_o4","rel_ht_o4","rel_wt_o4","rel_bmi_o4","close_rank_o4")
    colnames(off5)<- c("gameidplayid_o5","reference_nflid_o5","frameid_o5","other_nflid_o5","other_off_ind_o5","relative_distance_o5","relative_angle_o5","diff_speed_o5","diff_accel_o5","diff_orien_o5","diff_direc_o5","rel_ht_o5","rel_wt_o5","rel_bmi_o5","close_rank_o5")
    
    #- colnames for football df
    colnames(foot)<- c("gameidplayid_football","reference_nflid_football","frameid_football","other_nflid_football","other_def_ind_football","relative_distance_football","relative_angle_football","diff_speed_football","diff_accel_football","diff_orien_football","diff_direc_football","rel_ht_football","rel_wt_football","rel_bmi_football")
    
    analytic_df<- NULL
    analytic_df<- cbind(def1,def2,def3,def4,def5,off1,off2,off3,off4,off5,foot)
    #analytic_df %>% str
    
    #- remove columns
    analytic_df<- analytic_df %>%
      dplyr::select(-rel_ht_football,rel_wt_football,rel_bmi_football
                    #- offense
                    ,-gameidplayid_d2,-gameidplayid_d3,-gameidplayid_d4,-gameidplayid_d5
                    ,-reference_nflid_d2,-reference_nflid_d3,-reference_nflid_d4,-reference_nflid_d5
                    ,-frameid_d2,-frameid_d3,-frameid_d4,-frameid_d5
                    ,-other_nflid_d1,-other_nflid_d2,-other_nflid_d3,-other_nflid_d4,-other_nflid_d5
                    ,-other_def_ind_d1,-other_def_ind_d2,-other_def_ind_d3,-other_def_ind_d4,-other_def_ind_d5
                    ,-close_rank_d1,-close_rank_d2,-close_rank_d3,-close_rank_d4,-close_rank_d4,-close_rank_d5
                    
                    #- defense
                    ,-gameidplayid_o1,-gameidplayid_o2,-gameidplayid_o3,-gameidplayid_o4,-gameidplayid_o5
                    ,-reference_nflid_o1,-reference_nflid_o2,-reference_nflid_o3,-reference_nflid_o4,-reference_nflid_o5
                    ,-frameid_o1,-frameid_o2,-frameid_o3,-frameid_o4,-frameid_o5
                    ,-other_nflid_o1,-other_nflid_o2,-other_nflid_o3,-other_nflid_o4,-other_nflid_o5
                    ,-other_off_ind_o1,-other_off_ind_o2,-other_off_ind_o3,-other_off_ind_o4,-other_off_ind_o5
                    ,-close_rank_o1,-close_rank_o2,-close_rank_o3,-close_rank_o4,-close_rank_o5
                    
                    #- football
                    ,-gameidplayid_football
                    ,-reference_nflid_football
                    ,-frameid_football
                    ,-other_nflid_football
                    ,-other_def_ind_football    
                    ,-diff_orien_football
                    ,-diff_direc_football
                    ,-rel_wt_football
                    ,-rel_bmi_football
      )
    
    #- change first few colnames
    colnames(analytic_df)[which(colnames(analytic_df) == "gameidplayid_d1")]   <-"gameidplayid"
    colnames(analytic_df)[which(colnames(analytic_df) == "reference_nflid_d1")]<-"reference_nflid"
    colnames(analytic_df)[which(colnames(analytic_df) == "frameid_d1")]        <-"frameid"
    
    #- merge with dv
    analytic_df<- merge(x = analytic_df
                        ,y = ex_play_tm %>% dplyr::select(gameIdplayId,nflId,frameId,dv)
                        ,by.x = c("gameidplayid","reference_nflid","frameid") 
                        ,by.y = c("gameIdplayId","nflId","frameId")
                        ,all.x = TRUE)
    
    #- write it out
    write.csv(analytic_df,paste0(getwd(),"/data/02-intermediate/analytic-df1-",game,".csv"),row.names=FALSE)
    
    message("  Time to complete (mins): ",round(difftime(Sys.time(),start_time, units="mins")))
    message("  Completed game: ", game)
    
  }#- end games
  
}


#- aggregate
aggr_to_player_play<- function(df,has_dv=FALSE){
  if(has_dv){
    dff<-df %>%
      dplyr::group_by(gameidplayid,reference_nflid) %>%
      dplyr::summarize(prob_tackle = max(prob_tackle)
                       ,dv = max(dv)) %>%
      data.frame
  }
  
  if(!has_dv){
    dff<-df %>%
      dplyr::group_by(gameidplayid,reference_nflid) %>%
      dplyr::summarize(prob_tackle = max(prob_tackle)) %>%
      data.frame
  } 
  return(dff)
}


#- annimation plot of play
get_plot_anim<- function(ingame_id,inplay_id){
  
  df<- all_pred_probs_f[which(all_pred_probs_f$playId==inplay_id & all_pred_probs_f$gameId==ingame_id),] %>%
    dplyr::arrange(frameId,nflId)
  teams<- df$club %>% unique
  teams<- teams[teams %ni% 'football']
  
  df1<- df %>% dplyr::filter(club==teams[1])
  df2<- df %>% dplyr::filter(club==teams[2])
  dff<- df %>% dplyr::filter(club=='football')
  
  p<- ggplot(data=df, aes(x=std_X,y=std_Y))+
    gg_field(direction="horiz") +
    geom_point(data=df1,
               aes(x = std_X, y = std_Y,fill = club, color = club), shape = 21, alpha = 0.9, size = round(df1$prob_tackle_scale,2)) +
    geom_point(data=df2,
               aes(x = std_X, y = std_Y,fill = club, color = club), shape = 21, alpha = 0.9, size = round(df2$prob_tackle_scale,2)) +
    geom_point(data=dff,
               aes(x = std_X, y = std_Y,fill = club, color = club), shape = 18, alpha = 0.9, size = 3) +
    geom_text(size=2, data=df1,aes(x = std_X, y = std_Y,label=jerseyNumber))+
    geom_text(size=2, data=df2,aes(x = std_X, y = std_Y,label=jerseyNumber))+
    #geom_spoke(aes(angle = df$std_Dir_rad)                   , radius = df$s/2, arrow=arrow(length = unit(0.2,"cm")),alpha=0.3)+
    #geom_spoke(aes(angle = df$std_Orientation_rad,color=club), radius = 2   , arrow=arrow(length = unit(0.2,"cm")),alpha=0.5)+
    theme(legend.position='none') +
    transition_time(frameId) +
    ease_aes('linear') + 
    exit_fade()
  
  play_len <- df$frameId %>% unique %>% length
  gganimate::animate(plot=p,fps = 5, nframe = play_len)
  
  #Save
  anim_save(paste0("anim_play_",ingame_id,"_",inplay_id,".gif"), animation = last_animation())
}



#- for below function -#
## Repo: https://github.com/mlfurman3/gg_field

## gg_field function - set up as a list of annotations
gg_field <- function(yardmin=0, yardmax=120, buffer=5, direction="vert",
                     field_color="forestgreen",line_color="white",
                     sideline_color=field_color, endzone_color="darkgreen"){
  
  ## field dimensions (units=yards)
  xmin <- 0
  xmax <- 120
  
  ymin <- 0
  ymax <- 53.33
  
  
  ## distance from sideline to hash marks in middle (70 feet, 9 inches)
  hash_dist <- (70*12+9)/36
  
  ## yard lines locations (every 5 yards) 
  yd_lines <- seq(15,105,by=5)
  
  ## hash mark locations (left 1 yard line to right 1 yard line)
  yd_hash <- 11:109
  
  ## field number size
  num_size <- 5
  
  ## rotate field numbers with field direction
  ## first element is for right-side up numbers, second for upside-down
  angle_vec <- switch(direction, "horiz" = c(0, 180), "vert" = c(270, 90))
  num_adj <- switch(direction, "horiz" = c(-1, 1), "vert" = c(1, -1))
  
  ## list of annotated geoms
  p <- list(
    
    ## add field background 
    annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin-buffer, ymax=ymax+buffer, 
             fill=field_color),
    
    ## add end zones
    annotate("rect", xmin=xmin, xmax=xmin+10, ymin=ymin, ymax=ymax, fill=endzone_color),
    annotate("rect", xmin=xmax-10, xmax=xmax, ymin=ymin, ymax=ymax, fill=endzone_color),
    
    ## add yardlines every 5 yards
    annotate("segment", x=yd_lines, y=ymin, xend=yd_lines, yend=ymax,
             col=line_color),
    
    ## add thicker lines for endzones, midfield, and sidelines
    annotate("segment",x=c(0,10,60,110,120), y=ymin, xend=c(0,10,60,110,120), yend=ymax,
             lwd=1.3, col=line_color),
    annotate("segment",x=0, y=c(ymin, ymax), xend=120, yend=c(ymin, ymax),
             lwd=1.3, col=line_color) ,
    
    ## add field numbers (every 10 yards)
    ## field numbers are split up into digits and zeros to avoid being covered by yard lines
    ## numbers are added separately to allow for flexible ggplot stuff like facetting
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[2], y=ymin+12, label=0, angle=angle_vec[1],
             col=line_color, size=num_size),
    
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    
    
    ## upside-down numbers for top of field
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[1], y=ymax-12, angle=angle_vec[2],
             label=0, col=line_color, size=num_size),
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    
    
    ## add hash marks - middle of field
    annotate("segment", x=yd_hash, y=hash_dist - 0.5, xend=yd_hash, yend=hash_dist + 0.5,
             color=line_color),
    annotate("segment", x=yd_hash, y=ymax - hash_dist - 0.5, 
             xend=yd_hash, yend=ymax - hash_dist + 0.5,color=line_color),
    
    ## add hash marks - sidelines
    annotate("segment", x=yd_hash, y=ymax, xend=yd_hash, yend=ymax-1, color=line_color),
    annotate("segment", x=yd_hash, y=ymin, xend=yd_hash, yend=ymin+1, color=line_color),
    
    ## add conversion lines at 2-yard line
    annotate("segment",x=12, y=(ymax-1)/2, xend=12, yend=(ymax+1)/2, color=line_color),
    annotate("segment",x=108, y=(ymax-1)/2, xend=108, yend=(ymax+1)/2, color=line_color),
    
    ## cover up lines outside of field with sideline_color
    annotate("rect", xmin=0, xmax=xmax, ymin=ymax, ymax=ymax+buffer, fill=sideline_color),
    annotate("rect",xmin=0, xmax=xmax, ymin=ymin-buffer, ymax=ymin, fill=sideline_color),
    
    ## remove axis labels and tick marks
    labs(x="", y=""),
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()),
    
    ## clip axes to view of field
    if(direction=="horiz"){
      coord_cartesian(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), 
                      expand = FALSE)
      
    } else if (direction=="vert"){
      ## flip entire plot to vertical orientation
      coord_flip(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), expand = FALSE)
      
    }
  )
  
  return(p)
  
}
