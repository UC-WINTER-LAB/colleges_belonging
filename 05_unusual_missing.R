# Even when using fiml there was one extra case in simic_sem than cor_fit
# Here I explore the discrepancy

item_vars <- c(
  paste0("exits", 1:10),
  paste0("wmws", 1:14, "_t1"),
  paste0("wmws", 1:14, "_t3")
)

sem_data %>%
  mutate(row_id = row_number()) %>%
  filter(
    if_all(all_of(item_vars), is.na),
    !is.na(gender)
  ) %>%
  select(row_id, gender)

lavInspect(cor_fit, "empty.idx")
lavInspect(simic_sem, "empty.idx")
