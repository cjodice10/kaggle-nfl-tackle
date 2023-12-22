#- get Trending Component
trending<- get_mk(defensive_player_preds_agg_f)
table(trending$Trend)

#- merge with recent score
defensive_player_recent_score %>% str
utpm_trending<- merge (x = defensive_player_recent_score %>% dplyr::select(nflId,zscore_final,displayName)
                      ,y = trending %>% dplyr::select(nflId,Trend)
                      ,by="nflId"
                      ,all.x=TRUE) %>%
  dplyr::mutate(Trend = ifelse(Trend==-1,"Down",ifelse(Trend==1,"Up","Steady"))
                ,UTPM = zscore_final
                ,UTPM_cat = ifelse(zscore_final < (-2), "Poor",
                                   ifelse(zscore_final < (-1), "Below average",
                                          ifelse(zscore_final < 1, "Average",
                                                 ifelse(zscore_final < 2, "Above average",
                                                        ifelse(zscore_final<100, "Excellent",NA)))))
                ) %>%
  dplyr::select(-zscore_final) %>%
  dplyr::filter(!is.na(UTPM))
utpm_trending %>% str
utpm_trending %>% View
table(utpm_trending$UTPM_cat,utpm_trending$Trend,exclude=NULL)


#---------------------------------#
#- look at some players trending -#
#---------------------------------#

#- trending down
trending_down<- defensive_player_preds_agg_f %>% dplyr::filter(nflId %in% trending[which(trending$Trend==-1),]$nflId)
trending_down_nflids<- trending_down$nflId %>% unique %>% head

#- get last 20 plays
trending_down<- trending_down %>%
  dplyr::filter(nflId %in% c(52706,54513)) %>%
  dplyr::group_by(nflId) %>%
  dplyr::arrange(nflId,gameDate,playId) %>%
  dplyr::mutate(id = 1:n()) %>%
  data.frame

trending_down<- trending_down %>%
  dplyr::group_by(nflId) %>%
  dplyr::filter(id >= (n() - 20)) %>%
  data.frame
trending_down<- trending_down %>% dplyr::group_by(nflId) %>% dplyr::mutate(id=row_number()) %>% data.frame

#- trending up
trending_up<- defensive_player_preds_agg_f %>% dplyr::filter(nflId %in% trending[which(trending$Trend==1),]$nflId)

#- get last 20 plays
trending_up<- trending_up %>%
  dplyr::filter(nflId %in% c(42427,52471)) %>%
  dplyr::group_by(nflId) %>%
  dplyr::arrange(nflId,gameDate,playId) %>%
  dplyr::mutate(id = 1:n()) %>%
  data.frame

trending_up<- trending_up %>%
  dplyr::group_by(nflId) %>%
  dplyr::filter(id >= (n() - 20)) %>%
  data.frame

trending_up<- trending_up %>% dplyr::group_by(nflId) %>% dplyr::mutate(id=row_number()) %>% data.frame

#- combine
trending_downup<- dplyr::bind_rows(trending_up,trending_down)

ggplot() +
  geom_rect(aes(xmin = -21, xmax = 0, ymin = -2, ymax = -Inf),fill = "orange", alpha = 0.2) +
  geom_rect(aes(xmin = -21, xmax = 0, ymin = -1, ymax = -2),fill = "orange", alpha = 0.1) +
  geom_rect(aes(xmin = -21, xmax = 0, ymin = 1, ymax = 2),fill = "green", alpha = 0.1) +
  geom_rect(aes(xmin = -21, xmax = 0, ymin = 2, ymax = Inf),fill = "green", alpha = 0.2) +
  geom_line(data=trending_downup,aes(x=id-21, y=zscore_final,color=displayName))+
  geom_point(data=trending_downup,aes(x=id-21, y=zscore_final,color=displayName)) +
  labs(title="Player UTPM - last 20 plays",x="Play (0 is most recent play)",y="UTPM")+
  theme_minimal() +
  transition_reveal(id)

# Save at gif:
anim_save(paste0(getwd(),"/pictures/utpm-trend-example.gif"))

trending_downup %>% View
