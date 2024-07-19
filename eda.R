

new_df_latent_vars

# Check belonging change over time

new_df_latent_vars %>%
  select(id, time) %>%
  pivot_longer(names_to = "belong_type", values_to = "belong_val") %>%
  na.omit() %>%
  group_by(time, belong_type) %>%
  summarise(belong_val = mean(belong_val)) %>%
  ggplot(aes(x=time, y=belong_val, group = belong_type, color=belong_type)) +
  geom_line()

# Correlation matrix

# Multilevel regression comparing belonging to wellbeing

# Multilevel regresssion seeing it there is an interaction between belonging and exits

