df_added_vars <- new_df %>%
  mutate(
    centralityOtg = centralityotago1 + centralityotago2 + centralityotago3 + centralityhall1 + centralityhall2 + centralityhall3,
    belongingOtg = belongingotago1 + belongingotago2 + belongingotago3 + belonginghall1 + belonginghall2 + belonginghall3,
    mhcsf = mhcsf1 + mhcsf2 + mhcsf3 + mhcsf4 + mhcsf5 + mhcsf6 + mhcsf7 + mhcsf8 + mhcsf9 + mhcsf10 + mhcsf11 + mhcsf12 + mhcsf13 + mhcsf14,
    wmws = wmws1 + wmws2 + wmws3 + wmws4 + wmws5 + wmws6 + wmws7 + wmws8 + wmws9 + wmws10 + wmws11 + wmws12 + wmws13 + wmws14,
    phq = phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9,
    bus = bus1 + bus2 + bus3 + bus4 + bus5 + bus6 + bus7 + bus8,
    exits = exits1 + exits2 + exits3 +  + exits4 + exits5 + exits6 + exits7 + exits8 + exits9 + exits10 + exits11 + exits12 + exits13
  ) %>%
  select(centralityOtg, belongingOtg, mhcsf, wmws, phq, exits) %>%
  na.omit()

df_added_vars

lm(wmws ~ belongingOtg * exits, data = df_added_vars) %>%
  summary()

sum(!is.na(df_added_vars$exits))