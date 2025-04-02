library(lavaan)

sem_data <- new_df_latent_vars %>%
  filter(time == "s1") %>%
  select(id, gender, exits) %>%
  left_join(
    new_df_latent_vars %>%
      filter(time == "s2") %>%
      select(id, exits5, exits8),
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
  wmws ~ b1*exits5 + b2*exits8 + c*exits
  exits5 ~ a1*exits
  exits8 ~ a2*exits
  
  m1 := a1*b1
  m2 := a2*b2
  
  total := a1+b1+a2+b2+c
  
  m1_prop := m1/total
  m2_prop := m2/total
  ",
  data=sem_data
)

summary(test_sem)  
  
