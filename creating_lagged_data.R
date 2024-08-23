dat_lag <- new_df_latent_vars %>%
  group_by(id) %>%
  mutate(exits_lag = lead(exits, n=1)) %>% 
  ungroup()

ids <- na.omit(dat_lag) %>%
  group_by(id) %>%
  summarise(cnt = n()) %>%
  filter(cnt > 1) %>%
  select(id) %>%
  as.vector()

test <- mice::complete(mice::mice(dat_lag))

test_lm <- lmer(exits ~ exits_lag + exits5 * exits8 + gender + (1|id), data=filter(dat_lag, id %in% ids))
test_lm <- lmer(exits ~ exits_lag + exits5 * exits8 + gender + (1|id), data=test)
  
test_lm <- lm(exits ~ exits_lag + exits5 * exits8 + gender, data=dat_lag)

summary(test_lm)

ggplot(dat_lag, aes(x=time, y=exits)) +
  geom_boxplot()

dat_lag %>%
  filter(exits < exits_lag) %>%
  group_by(time) %>%
  summarise(exits5 = mean(exits5, na.rm=TRUE),
            exits8 = mean(exits8, na.rm=TRUE))

test %>%
  ggplot(aes(x=exits)) +
  geom_density()

test %>%
  mutate(grp = case_when(
    exits < -1 & time == "s1" ~ 0,
    exits < 1 & time == "s1" ~ 1,
    time == "s1" ~ 2)) %>%
  mutate(grp = as.factor(mean(grp, na.rm=TRUE)), .by="id") %>%
  ggplot(aes(x=time, y=exits, color=grp, group=id)) +
  geom_point() +
  geom_line()

test %>%
  mutate(grp = case_when(
    exits < -1 & time == "s1" ~ 0,
    exits < 1 & time == "s1" ~ 1,
    time == "s1" ~ 2)) %>%
  mutate(grp = as.factor(mean(grp, na.rm=TRUE)), .by="id") %>%
  summarise(
    exits5 = mean(exits5, na.rm=TRUE),
    exits8 = mean(exits8, na.rm=TRUE),
    .by=c("grp", "time")
  ) %>%
  filter(time != "s1") %>%
  pivot_longer(cols = c(exits5, exits8), names_to = "name", values_to = "vals") %>%
  ggplot(aes(x=time, y=vals, group=grp, color=grp)) +
  geom_line() +
  facet_wrap(~name)
