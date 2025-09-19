#### Correlation Matrix

library(Hmisc)
library(lme4)
library(lmerTest)

######## Get longitudinal SEM data #######################################

sem_data <- new_df_latent_vars %>%
  filter(time == "s1") %>%
  dplyr::select(id, gender, exits) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s2") %>%
      dplyr::select(id, continuity, gain),
    by="id"
  ) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s2") %>%
      dplyr::select(id, wmws_old = wmws),
    by="id"
  ) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s3") %>%
      dplyr::select(id, wmws),
    by="id"
  )

##### Correlation matrix ##############################

sem_data %>%
  dplyr::select(-wmws_old, -id, -gender) %>%
  as.matrix() %>%
  rcorr()

#### Regression #######################################

jtools::summ(lm(wmws ~ exits + gender, data = sem_data))
jtools::summ(lm(wmws ~ exits + continuity + gain + gender, data = sem_data))

#### SEM ##############################################

simic_sem <- sem(
  "
  wmws ~ b1*continuity + b2*gain + c*exits + gender
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

# Note summary(simic_sem)$pe
summary(simic_sem)  

lavaanPlot(
  model = simic_sem,
  coefs = TRUE,
  stand = TRUE,
  graph_options = list(rankdir = "LR"),
  stars = "covs",
  sig = 0.05
)
