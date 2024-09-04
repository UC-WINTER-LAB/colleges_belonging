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
  select(id, gender, time, exits5, exits8) %>%
  left_join(belonging_otago_df, by=c("id", "time")) %>%
  left_join(belonging_hall_df, by=c("id", "time")) %>%
  left_join(warw_edin_df, by=c("id", "time")) %>%
  left_join(exits_df, by=c("id", "time"))%>%
  left_join(loneliness_df, by=c("id", "time"))
