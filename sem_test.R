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

  
  
