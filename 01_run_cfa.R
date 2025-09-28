#~ Group Inclusion - Otago
belonging.otago.model <- '
             belongingOtg =~ belongingotago1 + belongingotago2 + belongingotago3
            '
# These are the variables for the halls:  + 

belonging_otago_fit <- sem(belonging.otago.model, cluster="id", data = new_df)
summary(belonging_otago_fit, fit.measures = TRUE) 

belonging_otago_df <- bind_cols(
  new_df %>%
    select(id, time, contains("belongingotago")) %>%
    na.omit() %>%
    select(-contains("belongingotago")),
  predict(belonging_otago_fit)
  )


#~ Group Inclusion - Hall
belonging.hall.model <- '
             belongingHall =~ belonginghall1 + belonginghall2 + belonginghall3
            '
belonging_hall_fit <- sem(belonging.hall.model, cluster="id", data = new_df)
summary(belonging_hall_fit, fit.measures = TRUE) 

belonging_hall_df <- bind_cols(
  new_df %>%
    select(id, time, contains("belonginghall")) %>%
    na.omit() %>%
    select(-contains("belonginghall")),
  predict(belonging_hall_fit)
)


#~ Warwick–Edinburgh Mental Well-being Scale (WMWS)
warw_edin.model <- '
            wmws =~ wmws1 + wmws2 + wmws3 + wmws4 + wmws5 + wmws6 + wmws7 + wmws8 + wmws9 + wmws10 + wmws11 + wmws12 + wmws13 + wmws14
                        '
warw_edin_fit <- sem(warw_edin.model, cluster="id", data = new_df)
summary(warw_edin_fit, fit.measures = TRUE)

warw_edin_df <- bind_cols(
  new_df %>%
    select(id, time, contains("wmws")) %>%
    na.omit() %>%
    select(-contains("wmws")),
  predict(warw_edin_fit)
)

#Exeter Identity Transition Scale (EXITS)
exits.model <- '
            exits =~ exits1 + exits2 + exits3 + exits4
                       '
exits_fit <- sem(exits.model, cluster = "id", data = new_df)
summary(exits_fit, fit.measures = TRUE)

exits_df <- bind_cols(
  new_df %>%
    select(id, time, exits1, exits2, exits3, exits4) %>%
    na.omit() %>%
    select(-contains("exits")),
  predict(exits_fit)
)

#Exeter Identity Transition Scale (EXITS) - Continuity
exits_cont.model <- '
            continuity =~ exits5 + exits6 + exits7
                       '
exits_cont_fit <- sem(exits_cont.model, cluster = "id", data = new_df)
summary(exits_cont_fit, fit.measures = TRUE)

exits_cont_df <- bind_cols(
  new_df %>%
    select(id, time, exits5, exits6, exits7) %>%
    na.omit() %>%
    select(-contains("exits")),
  predict(exits_cont_fit)
)

#Exeter Identity Transition Scale (EXITS) - Gain
exits_gain.model <- '
            gain =~ exits8 + exits9 + exits10
                       '
exits_gain_fit <- sem(exits_gain.model, cluster = "id", data = new_df)
summary(exits_gain_fit, fit.measures = TRUE)

exits_gain_df <- bind_cols(
  new_df %>%
    select(id, time, exits8, exits9, exits10) %>%
    na.omit() %>%
    select(-contains("exits")),
  predict(exits_gain_fit)
)

#UCLA Loneliness Scale 
loneliness.model <- '
             loneliness=~ ucla2 + ucla1 + ucla3 + ucla4 + ucla5 + ucla6 + ucla7 + ucla8 + ucla9 + ucla10 + ucla11 + ucla12 + ucla13 + ucla14 + ucla15 + ucla16 + ucla17 + ucla18 + ucla19 + ucla20
                       '
loneliness_fit <- sem(loneliness.model, cluster = "id", data = new_df)
summary(loneliness_fit, fit.measures = TRUE)

loneliness_df <- bind_cols(
  new_df %>%
    select(id, time, ucla1 , ucla2 , ucla3 , ucla4 , ucla5 , ucla6 , ucla7 , ucla8 , ucla9 , ucla10 , ucla11 , ucla12 , ucla13 , ucla14 , ucla15 , ucla16 , ucla17 , ucla18 , ucla19 , ucla20) %>%
    na.omit() %>%
    select(-contains("ucla")),
  predict(loneliness_fit)
)

new_df_latent_vars <- new_df %>%
  select(id, gender, time, exits5, exits8, exits10) %>%
  left_join(belonging_otago_df, by=c("id", "time")) %>%
  left_join(belonging_hall_df, by=c("id", "time")) %>%
  left_join(warw_edin_df, by=c("id", "time")) %>%
  left_join(exits_df, by=c("id", "time")) %>%
  left_join(exits_cont_df, by=c("id", "time")) %>%
  left_join(exits_gain_df, by=c("id", "time")) %>%
  left_join(loneliness_df, by=c("id", "time"))

# calculate cronbach's alpha

library(ltm)

#Warwick Edinburgh Mental Wellbeing Scale
wemws_proto_alpha <- new_df %>%
  dplyr::select(contains("wmws"), time) %>% #the "dplyr::" is there bc there is another package that has a select function and this serves to specify
  filter(time == "s3") %>% #filter out relevant timepoint so that there aren't mutliple responses from one participant
  na.omit()

wemws_alpha <- wemws_proto_alpha %>% #this is to get the responses we want but without the time variable so the cronbach.alpha doesn't get confused
  dplyr::select(contains("wmws"))

cronbach.alpha(warw_edin_alpha, CI=TRUE, standardized=FALSE) #idk if either of these have to be false tbh - lauren

#EXITS

###Total
exits_total_proto <- new_df %>%
  dplyr::select(exits1, exits2, exits3, exits4, time) %>%
  filter(time == "s1") %>%
  na.omit()

exits_total_alpha <- exits_total_proto %>%
  dplyr::select(exits1, exits2, exits3, exits4)

cronbach.alpha(exits_total_alpha, CI=TRUE, standardized=FALSE)

###Continuity
exits_cont_proto <- new_df %>%
  dplyr::select(exits5, exits6, exits7, time) %>%
  filter(time == "s3") %>%
  na.omit()

exits_cont_alpha <- exits_cont_proto %>%
  dplyr::select(exits5, exits6, exits7)

cronbach.alpha(exits_cont_alpha, CI=TRUE, standardized=FALSE)

###Gain
exits_gain_proto <- new_df %>%
  dplyr::select(exits8, exits9, exits10, time) %>%
  filter(time == "s3") %>%
  na.omit()

exits_gain_alpha <- exits_gain_proto %>%
  dplyr::select(exits8, exits9, exits10)

cronbach.alpha(exits_gain_alpha, CI=TRUE, standardized=FALSE)