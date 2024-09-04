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

######## Taylor playing around with models.

belongingHalls_regression <- lmer(wmws ~ exits * time + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)

exits_over_time <- lmer(exits ~ time + gender + (1 | id), data = new_df_latent_vars)
summary(exits_over_time)

exits_over_time <- lmer(exits ~ exits5 + exits8 + gender + (1 | id), data = new_df_latent_vars)
summary(exits_over_time)
