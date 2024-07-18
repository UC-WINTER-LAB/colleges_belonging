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
             belongingOtg =~ belonginghall1 + belonginghall2 + belonginghall3
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
            exits =~ exits1 + exits2 + exits3 + exits4 + exits5 + exits6 + exits7 + exits8 + exits9 + exits10 + exits11 + exits12 + exits13
           
                       '
exits_fit <- sem(exits.model, cluster="id", cluster="id", data = new_df)
summary(exits_fit, fit.measures = TRUE)

exits_df <- bind_cols(
  new_df %>%
    select(id, time, contains("exits")) %>%
    na.omit() %>%
    select(-contains("exits")),
  predict(exits_fit)
)


new_df %>%
  select(id, time) %>%
  left_join(belonging_otago_df, belonging_hall_df, warw_edin_df, exits_df, by=c("id", "time"))
