#### Correlation Matrix

library(Hmisc)
library(lme4)
library(lmerTest)

df_Cor <- new_df_latent_vars %>%
  select(contains("belongingOtg"), contains("belonginghall"), contains("wmws"), contains("exits"))

rcorr(as.matrix(df_Cor))

#### Regression


#### SEM
sem_data <- new_df_latent_vars %>%
  filter(time == "s1") %>%
  select(id, gender, exits) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s2") %>%
      select(id, continuity, gain),
    by="id"
  ) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s2") %>%
      select(id, wmws_old = wmws),
    by="id"
  ) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s3") %>%
      select(id, wmws),
    by="id"
  )

test_sem <- sem(
  "
  wmws ~ b1*continuity + b2*gain + c*exits + gender + wmws_old
  continuity ~ a1*exits + gender
  gain ~ a2*exits + gender
  
  m1 := a1*b1
  m2 := a2*b2
  
  total := (a1*b1) + (a2*b2) + c
  
  m1_prop := m1/total
  m2_prop := m2/total
  ",
  data=sem_data
)

summary(test_sem)  
