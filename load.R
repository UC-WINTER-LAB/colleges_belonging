library(tidyverse)
library(rdrop2)

drop_auth(new_user = TRUE)

raw_df <- drop_read_csv("/Winter Data/Longitudinal Colleges Data - Otago/Belonging_Dataset_Deidentified.csv") %>%
  as_tibble() %>%
  rename_all(tolower)

# Convert the data from wide format to long format
raw_df_long <- raw_df %>%
  mutate(id = coalesce(id, id_s1, id_s2, id_s3)) %>% # Take ID from a later wave if its not in base ID variable
  select(id, -ends_with("_s1"), -ends_with("_s2"), -ends_with("_s3")) %>%
  # Join the base demographic data onto each waves variables
  left_join(
    # Stack each timepoint on top of each other and trim the suffix
    bind_rows(
      raw_df %>%
        select(id, ends_with("_s1"), -id_s1) %>% # Remove each waves ID to use the general coalesced one
        rename_with(~str_remove(., "_s1")) %>%
        mutate(time = "s1"),
      raw_df %>%
        select(id, ends_with("_s2"), -id_s2) %>%
        rename_with(~str_remove(., "_s2")) %>%
        select(-college) %>%
        mutate(time = "s2"), # This wave had a college variable in it for some reason
      raw_df %>%
        select(id, ends_with("_s3"), -id_s3) %>%
        rename_with(~str_remove(., "_s3")) %>%
        mutate(time = "s3")
    ),
    by="id" # Assuming each waves ID variable is complete and consistent
  )
