#- Prepare for LSTM Model #-

dev_transf %>% str

dev_lstm_x<- dev_transf %>% 
  #dplyr::select(gameidplayid
  #              ,reference_nflid
  #              ,frameid_new,dv
  #              ,relative_distance_o1
  #              ,diff_speed_o1
  #              ,relative_distance_football
  #              ,diff_speed_football) %>%
  dplyr::mutate(myid = paste0(gameidplayid,"-",reference_nflid))
dev_lstm_x %>% str #1,173,499

#- create unique ids for testing
unique_myids<- dev_lstm_x$myid %>% unique %>% head(500)

#ingameids<- c("2022101300 - 1000","2022101300 - 1000","2022101300 - 1021","2022101300 - 1088")
#innflids<- c(41599,46077,41595,54504)

#- subset to certain ids
dev_lstm_x<- dev_lstm_x %>%
  dplyr::filter(myid %in% unique_myids ) %>%
  dplyr::arrange(reference_nflid, gameidplayid, frameid_new)
str(dev_lstm_x)
colnames(dev_lstm_x)
dev_lstm_x$dv %>% table 

#- maxframes
maxframes<- max(dev_lstm_x$frameid_new %>% unique)
maxframes

#- nbr nflids gameids
unique_seq<- dev_lstm_x %>% dplyr::select(gameidplayid,reference_nflid) %>% unique %>% nrow
unique_seq
#- 

#-numfeat
numfeatures<-95

#- ensure all have max frames
x_df_pad<-dev_lstm_x %>%
  dplyr::select(-gameidplayid,-reference_nflid) %>%
  tidyr::complete(myid
                  , frameid_new = 1:maxframes
                  , fill = list(	   diff_orien_d1 	 =(-1)
                                  ,	 relative_distance_d2 	 =(-1)
                                  ,	 diff_direc_d2 	 =(-1)
                                  ,	 relative_angle_d3 	 =(-1)
                                  ,	 rel_ht_d3 	 =(-1)
                                  ,	 diff_speed_d4 	 =(-1)
                                  ,	 rel_wt_d4 	 =(-1)
                                  ,	 diff_accel_d5 	 =(-1)
                                  ,	 rel_bmi_d5 	 =(-1)
                                  ,	 diff_orien_o1 	 =(-1)
                                  ,	 relative_distance_o2 	 =(-1)
                                  ,	 diff_direc_o2 	 =(-1)
                                  ,	 relative_angle_o3 	 =(-1)
                                  ,	 rel_ht_o3 	 =(-1)
                                  ,	 diff_speed_o4 	 =(-1)
                                  ,	 rel_wt_o4 	 =(-1)
                                  ,	 diff_accel_o5 	 =(-1)
                                  ,	 rel_bmi_o5 	 =(-1)
                                  ,	 gameidplayid 	 =(-1)
                                  ,	 relative_distance_d1 	 =(-1)
                                  ,	 diff_direc_d1 	 =(-1)
                                  ,	 relative_angle_d2 	 =(-1)
                                  ,	 rel_ht_d2 	 =(-1)
                                  ,	 diff_speed_d3 	 =(-1)
                                  ,	 rel_wt_d3 	 =(-1)
                                  ,	 diff_accel_d4 	 =(-1)
                                  ,	 rel_bmi_d4 	 =(-1)
                                  ,	 diff_orien_d5 	 =(-1)
                                  ,	 relative_distance_o1 	 =(-1)
                                  ,	 diff_direc_o1 	 =(-1)
                                  ,	 relative_angle_o2 	 =(-1)
                                  ,	 rel_ht_o2 	 =(-1)
                                  ,	 diff_speed_o3 	 =(-1)
                                  ,	 rel_wt_o3 	 =(-1)
                                  ,	 diff_accel_o4 	 =(-1)
                                  ,	 rel_bmi_o4 	 =(-1)
                                  ,	 diff_orien_o5 	 =(-1)
                                  ,	 relative_distance_football 	 =(-1)
                                  ,	 relative_angle_d1 	 =(-1)
                                  ,	 rel_ht_d1 	 =(-1)
                                  ,	 diff_speed_d2 	 =(-1)
                                  ,	 rel_wt_d2 	 =(-1)
                                  ,	 diff_accel_d3 	 =(-1)
                                  ,	 rel_bmi_d3 	 =(-1)
                                  ,	 diff_orien_d4 	 =(-1)
                                  ,	 relative_distance_d5 	 =(-1)
                                  ,	 diff_direc_d5 	 =(-1)
                                  ,	 relative_angle_o1 	 =(-1)
                                  ,	 rel_ht_o1 	 =(-1)
                                  ,	 diff_speed_o2 	 =(-1)
                                  ,	 rel_wt_o2 	 =(-1)
                                  ,	 diff_accel_o3 	 =(-1)
                                  ,	 rel_bmi_o3 	 =(-1)
                                  ,	 diff_orien_o4 	 =(-1)
                                  ,	 relative_distance_o5 	 =(-1)
                                  ,	 diff_direc_o5 	 =(-1)
                                  ,	 relative_angle_football 	 =(-1)
                                  ,	 dv 	 =0
                                  ,	 diff_speed_d1 	 =(-1)
                                  ,	 rel_wt_d1 	 =(-1)
                                  ,	 diff_accel_d2 	 =(-1)
                                  ,	 rel_bmi_d2 	 =(-1)
                                  ,	 diff_orien_d3 	 =(-1)
                                  ,	 relative_distance_d4 	 =(-1)
                                  ,	 diff_direc_d4 	 =(-1)
                                  ,	 relative_angle_d5 	 =(-1)
                                  ,	 rel_ht_d5 	 =(-1)
                                  ,	 diff_speed_o1 	 =(-1)
                                  ,	 rel_wt_o1 	 =(-1)
                                  ,	 diff_accel_o2 	 =(-1)
                                  ,	 rel_bmi_o2 	 =(-1)
                                  ,	 diff_orien_o3 	 =(-1)
                                  ,	 relative_distance_o4 	 =(-1)
                                  ,	 diff_direc_o4 	 =(-1)
                                  ,	 relative_angle_o5 	 =(-1)
                                  ,	 rel_ht_o5 	 =(-1)
                                  ,	 diff_speed_football 	 =(-1)
                                  ,	 diff_accel_d1 	 =(-1)
                                  ,	 rel_bmi_d1 	 =(-1)
                                  ,	 diff_orien_d2 	 =(-1)
                                  ,	 relative_distance_d3 	 =(-1)
                                  ,	 diff_direc_d3 	 =(-1)
                                  ,	 relative_angle_d4 	 =(-1)
                                  ,	 rel_ht_d4 	 =(-1)
                                  ,	 diff_speed_d5 	 =(-1)
                                  ,	 rel_wt_d5 	 =(-1)
                                  ,	 diff_accel_o1 	 =(-1)
                                  ,	 rel_bmi_o1 	 =(-1)
                                  ,	 diff_orien_o2 	 =(-1)
                                  ,	 relative_distance_o3 	 =(-1)
                                  ,	 diff_direc_o3 	 =(-1)
                                  ,	 relative_angle_o4 	 =(-1)
                                  ,	 rel_ht_o4 	 =(-1)
                                  ,	 diff_speed_o5 	 =(-1)
                                  ,	 rel_wt_o5 	 =(-1)
                                  ,	 diff_accel_football 	 =(-1)
                               )
                  )

#- make it long
long_data <- x_df_pad %>%
  pivot_longer(cols = c(  'diff_orien_d1'
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
                        ), names_to = "variable", values_to = "value")

long_data %>% str
long_data %>% head(50)

# Reshape data for LSTM
n_individuals <- length(unique(long_data$myid))
n_timeframes <- length(unique(long_data$frameid_new))
n_features <- numfeatures  # Number of predictor variables

n_individuals
n_timeframes
n_features

# Create a 3D array (tensor)
X <- array(
  data = matrix(long_data$value, nrow = n_individuals * n_timeframes * n_features),
  dim = c(n_individuals, n_timeframes, n_features)
)
X %>% dim

long_data$variable %>% unique
n_individuals

#y<- long_data %>% dplyr::group_by(myid) %>% dplyr::summarise(dv=max(dv)) %>% data.frame %>% dplyr::select(dv) %>% as.matrix()
y_tmp<- long_data %>% dplyr::select(myid,frameid_new,dv) %>% unique %>% dplyr::arrange(myid,frameid_new,dv)
y <- matrix(y_tmp$dv, nrow = n_individuals, ncol = n_timeframes,byrow = TRUE)
str(y)
#y<- to_categorical(y)
dim(y)
sum(y)


#---------#
#- LSTM - #
#---------#
#gc()
rm(model)
model_history<- NULL
k_clear_session()
model <- keras_model_sequential()

model %>%
  layer_lstm(units = 50, input_shape = c(n_timeframes, n_features)) %>%
  layer_dense(units = n_timeframes,activation = "sigmoid")

# Compile the model
model %>%
  compile(optimizer = 'adam', 
          metrics = list(  'binary_accuracy' #
                          ,'auc'=tf$keras$metrics$AUC()
                          ,'prec'=tf$keras$metrics$Precision()
                          ,'recall'=tf$keras$metrics$Recall()
          ), 
          loss = "binary_crossentropy" # "categorical_crossentropy"  #"sparse_categorical_crossentropy" #   "binary_crossentropy"
  )
# Print the model summary
summary(model)

# Fit the model
model_history<- model %>% fit(X, y, epochs = 100, batch_size = 50,weight_class=list("0"=0.5,"1"=50))
model_history

#- Generator -#
?keras::fit_generator
