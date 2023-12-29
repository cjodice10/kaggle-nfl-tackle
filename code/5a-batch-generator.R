
#- dir
my_dir<- getwd()

#- source helpers
source(paste0(my_dir,"/code/helpers.R"))

dev_lstm_x<- dev_transf %>% 
  dplyr::mutate(myid = paste0(gameidplayid,"-",reference_nflid))

#- create unique ids for testing
unique_myids       <- dev_lstm_x$myid %>% unique
unique_myids %>% str
unique_myids_length<- unique_myids %>% length
unique_myids_length
#- 31,893

#- maxframes
maxframes<- max(dev_lstm_x$frameid_new %>% unique)
maxframes

batch_size <- 500
epochs     <- 50
steps_per_epoch <- round(unique_myids_length / batch_size)
steps_per_epoch


#- weights
dev_lstm_x %>%
  dplyr::group_by(myid) %>%
  dplyr::summarise(maxdv=max(dv)) %>%
  dplyr::group_by(maxdv) %>%
  tally
#- 0: 0.5 / (27997/31893) = 0.57
#- 1: 0.5 / (3896/31893) = 4.0

batch_generator <- function(indata, inbatch_size,inmax_frames,inunique_ids=unique_myids){

  function() {
    ids <- sample(x=inunique_ids, size=inbatch_size, replace = FALSE)
    
    #- subset to certain ids
    df<- indata %>%
      dplyr::filter(myid %in% ids ) %>%
      dplyr::arrange(reference_nflid, gameidplayid, frameid_new)
    
    #- nbr nflids gameids
    unique_seq<- df %>% dplyr::select(gameidplayid,reference_nflid) %>% unique %>% nrow
    
    #-numfeat
    numfeatures<-95
    
    #- ensure all have max frames
    x_df_pad<-df %>%
      dplyr::select(-gameidplayid,-reference_nflid) %>%
      tidyr::complete(  myid
                      , frameid_new = 1:inmax_frames
                      , fill = list(   diff_orien_d1 	=(-1)
                                      ,relative_distance_d2 	=(-1)
                                      ,diff_direc_d2 	=(-1)
                                      ,relative_angle_d3 	=(-1)
                                      ,rel_ht_d3 	=(-1)
                                      ,diff_speed_d4 	=(-1)
                                      ,rel_wt_d4 	=(-1)
                                      ,diff_accel_d5 	=(-1)
                                      ,rel_bmi_d5 	=(-1)
                                      ,diff_orien_o1 	=(-1)
                                      ,relative_distance_o2 	=(-1)
                                      ,diff_direc_o2 	=(-1)
                                      ,relative_angle_o3 	=(-1)
                                      ,rel_ht_o3 	=(-1)
                                      ,diff_speed_o4 	=(-1)
                                      ,rel_wt_o4 	=(-1)
                                      ,diff_accel_o5 	=(-1)
                                      ,rel_bmi_o5 	=(-1)
                                      ,gameidplayid 	=(-1)
                                      ,relative_distance_d1 	=(-1)
                                      ,diff_direc_d1 	=(-1)
                                      ,relative_angle_d2 	=(-1)
                                      ,rel_ht_d2 	=(-1)
                                      ,diff_speed_d3 	=(-1)
                                      ,rel_wt_d3 	=(-1)
                                      ,diff_accel_d4 	=(-1)
                                      ,rel_bmi_d4 	=(-1)
                                      ,diff_orien_d5 	=(-1)
                                      ,relative_distance_o1 	=(-1)
                                      ,diff_direc_o1 	=(-1)
                                      ,relative_angle_o2 	=(-1)
                                      ,rel_ht_o2 	=(-1)
                                      ,diff_speed_o3 	=(-1)
                                      ,rel_wt_o3 	=(-1)
                                      ,diff_accel_o4 	=(-1)
                                      ,rel_bmi_o4 	=(-1)
                                      ,diff_orien_o5 	=(-1)
                                      ,relative_distance_football 	=(-1)
                                      ,relative_angle_d1 	=(-1)
                                      ,rel_ht_d1 	=(-1)
                                      ,diff_speed_d2 	=(-1)
                                      ,rel_wt_d2 	=(-1)
                                      ,diff_accel_d3 	=(-1)
                                      ,rel_bmi_d3 	=(-1)
                                      ,diff_orien_d4 	=(-1)
                                      ,relative_distance_d5 	=(-1)
                                      ,diff_direc_d5 	=(-1)
                                      ,relative_angle_o1 	=(-1)
                                      ,rel_ht_o1 	=(-1)
                                      ,diff_speed_o2 	=(-1)
                                      ,rel_wt_o2 	=(-1)
                                      ,diff_accel_o3 	=(-1)
                                      ,rel_bmi_o3 	=(-1)
                                      ,diff_orien_o4 	=(-1)
                                      ,relative_distance_o5 	=(-1)
                                      ,diff_direc_o5 	=(-1)
                                      ,relative_angle_football 	=(-1)
                                      ,dv 	=(-1)
                                      ,diff_speed_d1 	=(-1)
                                      ,rel_wt_d1 	=(-1)
                                      ,diff_accel_d2 	=(-1)
                                      ,rel_bmi_d2 	=(-1)
                                      ,diff_orien_d3 	=(-1)
                                      ,relative_distance_d4 	=(-1)
                                      ,diff_direc_d4 	=(-1)
                                      ,relative_angle_d5 	=(-1)
                                      ,rel_ht_d5 	=(-1)
                                      ,diff_speed_o1 	=(-1)
                                      ,rel_wt_o1 	=(-1)
                                      ,diff_accel_o2 	=(-1)
                                      ,rel_bmi_o2 	=(-1)
                                      ,diff_orien_o3 	=(-1)
                                      ,relative_distance_o4 	=(-1)
                                      ,diff_direc_o4 	=(-1)
                                      ,relative_angle_o5 	=(-1)
                                      ,rel_ht_o5 	=(-1)
                                      ,diff_speed_football 	=(-1)
                                      ,diff_accel_d1 	=(-1)
                                      ,rel_bmi_d1 	=(-1)
                                      ,diff_orien_d2 	=(-1)
                                      ,relative_distance_d3 	=(-1)
                                      ,diff_direc_d3 	=(-1)
                                      ,relative_angle_d4 	=(-1)
                                      ,rel_ht_d4 	=(-1)
                                      ,diff_speed_d5 	=(-1)
                                      ,rel_wt_d5 	=(-1)
                                      ,diff_accel_o1 	=(-1)
                                      ,rel_bmi_o1 	=(-1)
                                      ,diff_orien_o2 	=(-1)
                                      ,relative_distance_o3 	=(-1)
                                      ,diff_direc_o3 	=(-1)
                                      ,relative_angle_o4 	=(-1)
                                      ,rel_ht_o4 	=(-1)
                                      ,diff_speed_o5 	=(-1)
                                      ,rel_wt_o5 	=(-1)
                                      ,diff_accel_football 	=(-1)
                      )
      )
    
    #- make it long
    long_data <- x_df_pad %>%
      tidyr::pivot_longer(cols = c(  'diff_orien_d1'
                              ,'relative_distance_d2'
                              ,'diff_direc_d2'
                              ,'relative_angle_d3'
                              ,'rel_ht_d3'
                              ,'diff_speed_d4'
                              ,'rel_wt_d4'
                              ,'diff_accel_d5'
                              ,'rel_bmi_d5'
                              ,'diff_orien_o1'
                              ,'relative_distance_o2'
                              ,'diff_direc_o2'
                              ,'relative_angle_o3'
                              ,'rel_ht_o3'
                              ,'diff_speed_o4'
                              ,'rel_wt_o4'
                              ,'diff_accel_o5'
                              ,'rel_bmi_o5'
                              #,'gameidplayid'
                              ,'relative_distance_d1'
                              ,'diff_direc_d1'
                              ,'relative_angle_d2'
                              ,'rel_ht_d2'
                              ,'diff_speed_d3'
                              ,'rel_wt_d3'
                              ,'diff_accel_d4'
                              ,'rel_bmi_d4'
                              ,'diff_orien_d5'
                              ,'relative_distance_o1'
                              ,'diff_direc_o1'
                              ,'relative_angle_o2'
                              ,'rel_ht_o2'
                              ,'diff_speed_o3'
                              ,'rel_wt_o3'
                              ,'diff_accel_o4'
                              ,'rel_bmi_o4'
                              ,'diff_orien_o5'
                              ,'relative_distance_football'
                              ,'relative_angle_d1'
                              ,'rel_ht_d1'
                              ,'diff_speed_d2'
                              ,'rel_wt_d2'
                              ,'diff_accel_d3'
                              ,'rel_bmi_d3'
                              ,'diff_orien_d4'
                              ,'relative_distance_d5'
                              ,'diff_direc_d5'
                              ,'relative_angle_o1'
                              ,'rel_ht_o1'
                              ,'diff_speed_o2'
                              ,'rel_wt_o2'
                              ,'diff_accel_o3'
                              ,'rel_bmi_o3'
                              ,'diff_orien_o4'
                              ,'relative_distance_o5'
                              ,'diff_direc_o5'
                              ,'relative_angle_football'
                              ,'dv'
                              ,'diff_speed_d1'
                              ,'rel_wt_d1'
                              ,'diff_accel_d2'
                              ,'rel_bmi_d2'
                              ,'diff_orien_d3'
                              ,'relative_distance_d4'
                              ,'diff_direc_d4'
                              ,'relative_angle_d5'
                              ,'rel_ht_d5'
                              ,'diff_speed_o1'
                              ,'rel_wt_o1'
                              ,'diff_accel_o2'
                              ,'rel_bmi_o2'
                              ,'diff_orien_o3'
                              ,'relative_distance_o4'
                              ,'diff_direc_o4'
                              ,'relative_angle_o5'
                              ,'rel_ht_o5'
                              ,'diff_speed_football'
                              ,'diff_accel_d1'
                              ,'rel_bmi_d1'
                              ,'diff_orien_d2'
                              ,'relative_distance_d3'
                              ,'diff_direc_d3'
                              ,'relative_angle_d4'
                              ,'rel_ht_d4'
                              ,'diff_speed_d5'
                              ,'rel_wt_d5'
                              ,'diff_accel_o1'
                              ,'rel_bmi_o1'
                              ,'diff_orien_o2'
                              ,'relative_distance_o3'
                              ,'diff_direc_o3'
                              ,'relative_angle_o4'
                              ,'rel_ht_o4'
                              ,'diff_speed_o5'
                              ,'rel_wt_o5'
                              ,'diff_accel_football'
      )
      ,names_to  = "variable"
      ,values_to = "value")
    
    # Reshape data for LSTM
    n_individuals<- length(unique(long_data$myid))
    n_timeframes <- length(unique(long_data$frameid_new))
    n_features   <- numfeatures  # Number of predictor variables
    
    # Create a 3D array (tensor)
    X <- array(
       data = matrix(long_data$value, nrow = n_individuals * n_timeframes * n_features)
      ,dim  = c(n_individuals, n_timeframes, n_features)
    )
    
    y<- long_data %>% 
      dplyr::filter(variable=='dv') %>%
      dplyr::group_by(myid) %>% 
      dplyr::summarise(dv=max(value)) %>% 
      data.frame %>% 
      dplyr::select(dv) %>% 
      as.matrix()
    y<- to_categorical(y)
    
    list(X,y)
    
  }#- end function()
  
}#- end batch_generator



#---------#
#- LSTM - #
#---------#
lstm_model<- NULL
gc()
k_clear_session()
lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 64, input_shape = c(107, 95)) %>%
  #layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 2,activation = "softmax")

# Compile the model
lstm_model %>%
  compile(optimizer = 'adam', 
          metrics = list( #'binary_accuracy' #
            'auc'=tf$keras$metrics$AUC()
            ,'prec'=tf$keras$metrics$Precision()
            ,'recall'=tf$keras$metrics$Recall()
          ), 
          loss = "categorical_crossentropy"  #"sparse_categorical_crossentropy" #   "binary_crossentropy"
  )
# Print the model summary
summary(lstm_model)

# Fit the model
## This code works
gen <- keras:::as_generator.function(batch_generator( indata=dev_lstm_x
                                                     ,inbatch_size=batch_size
                                                     ,inmax_frames=maxframes
                                                     ,inunique_ids=unique_myids))

lstm_model_history<- lstm_model %>%
  keras::fit_generator(gen
                      ,steps_per_epoch = steps_per_epoch
                      ,epochs = epochs
                      ,class_weight =  list("0"=0.57,"1"=10.0))


