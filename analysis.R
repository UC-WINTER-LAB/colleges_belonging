#### Correlation Matrix

library(Hmisc)
library(lme4)
library(lmerTest)

df_Cor <- new_df_latent_vars %>%
  select(contains("belongingOtg"), contains("belonginghall"), contains("wmws"), contains("exits"))

rcorr(as.matrix(df_Cor))$P

#### Regression


belongingOtago_regression <- lmer(wmws ~ belongingOtg * exits + gender + (1 | id), data = new_df_latent_vars)
summary(belongingOtago_regression)

belongingHalls_regression <- lmer(wmws ~ belongingHall * exits + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)

########

belongingHalls_regression <- lmer(wmws ~ belongingHall * time + gender + (1 | id), data = new_df_latent_vars)
summary(belongingHalls_regression)