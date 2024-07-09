#~ Group Inclusion (Sheldon & Bettencourt, 2002)
belonging.model <- '
             belongingOtg =~ belongingotago1 + belongingotago2 + belongingotago3 + belonginghall1 + belonginghall2 + belonginghall3
            '

belonging_fit <- sem(belonging.model, data = new_df)
summary(belonging_fit, fit.measures = TRUE) 



#~ Self-categorisation (Ellemers et al., 1999)
centrality.model <- '
            centralityOtg =~ centralityotago1 + centralityotago2 + centralityotago3 + centralityhall1 + centralityhall2 + centralityhall3
            '
centrality_fit <- sem(centrality.model, data = new_df)
summary(centrality_fit, fit.measures = TRUE)



#~ Mental Health Continuum (short form)
mental_health_cont.model <- '
            mhcsf =~ mhcsf1 + mhcsf2 + mhcsf3 + mhcsf4 + mhcsf5 + mhcsf6 + mhcsf7 + mhcsf8 + mhcsf9 + mhcsf10 + mhcsf11 + mhcsf12 + mhcsf13 + mhcsf14
                        '
mental_health_cont_fit <- sem(mental_health_cont.model, data = new_df)
summary(mental_health_cont_fit, fit.measures = TRUE) 



#~ Warwick–Edinburgh Mental Well-being Scale (WMWS)
warw_edin.model <- '
            wmws =~ wmws1 + wmws2 + wmws3 + wmws4 + wmws5 + wmws6 + wmws7 + wmws8 + wmws9 + wmws10 + wmws11 + wmws12 + wmws13 + wmws14
                        '
warw_edin_fit <- sem(warw_edin.model, data = new_df)
summary(warw_edin_fit, fit.measures = TRUE)



#~ Patient Health Questionnaire (PHQ-9)
patient_health.model <- '
            phq =~ phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9
            
            '
patient_health_fit <- sem(patient_health.model, data = new_df)
summary(patient_health_fit, fit.measures = TRUE) 



#Belonging Uncertainty Scale
#~no longer needed// uncertainty.model <- '
#            bus =~ bus1 + bus2 + bus3 + bus4 + bus5 + bus6 + bus7 + bus8'
#uncertainty_fit <- sem(uncertainty.model, data = new_df)
#summary(uncertainty_fit, fit.measures = TRUE) 



#Exeter Identity Transition Scale (EXITS)
ident_trans.model <- '
            exits =~ exits1 + exits2 + exits3 + exits4 + exits5 + exits6 + exits7 + exits8 + exits9 + exits10 + exits11 + exits12 + exits13
           
                       '
ident_trans_fit <- sem(ident_trans.model, data = new_df)
summary(ident_trans_fit, fit.measures = TRUE)



### Example to get the data
#~ idk if i was supposed to have the variable(?) names down here be the same as the ones used in the xxx =~ xxx1 + xxx2 etc.
data.frame(
  belongingOtg = predict(belonging_fit),
  centralityOtg = predict(centrality_fit),
  mhcsf = predict(mental_health_cont_fit),
  wmws = predict(warw_edin_fit),
  phq = predict(patient_health_fit),
  exits = predict(ident_trans_fit)
)