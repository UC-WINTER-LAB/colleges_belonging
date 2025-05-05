#### Correlation Matrix

library(Hmisc)
library(lme4)
library(lmerTest)

df_Cor <- new_df_latent_vars %>%
  select(contains("belongingOtg"), contains("belonginghall"), contains("wmws"), contains("exits"))

rcorr(as.matrix(df_Cor))

#### Regression


belongingOtago_regression <- lmer(wmws ~ belongingOtg + exits + gender + (1 | id), data = new_df_latent_vars)
summary(belongingOtago_regression)

belongingHalls_regression <- lmer(wmws ~ belongingHall + exits + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)

######## Taylor playing around with models - wmws

belongingHalls_regression <- lmer(wmws ~ exits * time + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)

exits_over_time <- lmer(exits ~ time + gender + (1 | id), data = new_df_latent_vars)
summary(exits_over_time)

exits_over_time <- lmer(exits ~ exits5 + exits8 + gender + (1 | id), data = new_df_latent_vars)
summary(exits_over_time)

######## Taylor playing around with models - loneliness

belongingHalls_regression <- lmer(loneliness ~ exits * time + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)

exits_over_time <- lmer(loneliness ~ time + gender + (1 | id), data = new_df_latent_vars)
summary(exits_over_time)

#####################################################

# Exits predicts wellbeing / loneliness
lmer(wmws ~ exits + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()
lmer(loneliness ~ exits + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()

# Exits is moderated by time
lmer(wmws ~ exits * time + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()
lmer(loneliness ~ exits * time + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()

# Exits is moderated by sex

lmer(wmws ~ exits * gender + (1 | id), data = new_df_latent_vars) %>%
  summary()
lmer(loneliness ~ exits * gender + (1 | id), data = new_df_latent_vars) %>%
  summary()

# Does exits5 and exits8 predict wellbeing when controlling for exits
lmer(wmws ~ exits5 + exits8 + exits + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()
lmer(loneliness ~ exits5 + exits8 + exits + gender + (1 | id), data = new_df_latent_vars) %>%
  summary()

# Look at exits5 and exits8 descriptively
new_df_latent_vars %>%
  select(time, exits5, exits8) %>%
  pivot_longer(cols = c(exits5, exits8), names_to = "var", values_to = "val") %>%
  ggplot(aes(x=time, y=val, group=var, color=var)) +
  geom_point() +
  geom_line() +
  theme_classic()



