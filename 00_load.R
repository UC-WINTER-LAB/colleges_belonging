library(tidyverse)
library(rdrop2)
library(lavaan)

if(rdrop2::drop_acc()$error_summary %in% c("expired_access_token/", "invalid_access_token/")) {
  drop_auth(new_user = TRUE)
}

raw_df <- drop_read_csv("Longitudinal Colleges Data - Otago/Belonging_Dataset_Deidentified.csv") %>%
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
        select(id, gender, ends_with("_s1"), -id_s1) %>% # Remove each waves ID to use the general coalesced one
        rename_with(~str_remove(., "_s1")) %>%
        mutate(time = "s1"),
      raw_df %>%
        select(id, gender, ends_with("_s2"), -id_s2) %>%
        rename_with(~str_remove(., "_s2")) %>%
        select(-college) %>%
        mutate(time = "s2"), # This wave had a college variable in it for some reason
      raw_df %>%
        select(id, gender, ends_with("_s3"), -id_s3) %>%
        rename_with(~str_remove(., "_s3")) %>%
        mutate(time = "s3")
    ),
    by="id" # Assuming each waves ID variable is complete and consistent
  )

new_df <- raw_df_long %>%
  select(id, 
         time,
         gender,
         contains("wmws"),
         belongingotago1,belongingotago2,belongingotago3,
         belonginghall1,belonginghall2,belonginghall3,
         contains("exits"),
         contains("ucla")## loneliness
         ) %>%
  filter(!is.na(time)
         )
