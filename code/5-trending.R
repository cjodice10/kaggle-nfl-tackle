?MannKendall

#- get Trending Component
trending<- get_mk(defensive_player_preds_agg_f)
table(trending$Trend)
trending %>% dplyr::arrange(tau) %>% head
trending %>% dplyr::arrange(desc(tau)) %>% dplyr::filter(Trend==1) %>% head

#- look at some players trending

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
