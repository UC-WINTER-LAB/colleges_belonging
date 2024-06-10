library(tidyverse)
library(rdrop2)
library(lavaan)

drop_auth(new_user = TRUE)

raw_df <- drop_read_csv("/Longitudinal Colleges Data - Otago/Belonging_Dataset_Deidentified.csv") %>%
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
new_df <- raw_df_long %>%
  select(mhcsf1,mhcsf2,mhcsf3,mhcsf4,mhcsf5,mhcsf6,mhcsf7,mhcsf8,mhcsf9,mhcsf10,mhcsf11,mhcsf12,mhcsf13,mhcsf14,
         wmws1,wmws2,wmws3,wmws4,wmws5,wmws6,wmws7,wmws8,wmws9,wmws10,wmws11,wmws12,wmws13,wmws14,
         phq9_1,phq9_2,phq9_3,phq9_4,phq9_5,phq9_6,phq9_7,phq9_8,phq9_9,
         centralityotago1,centralityotago2,centralityotago3,centralityhall1,centralityhall2,centralityhall3,
         belongingotago1,belongingotago2,belongingotago3,belonginghall1,belonginghall2,belonginghall3,
         bus1,bus2,bus3,bus4,bus5,bus6,bus7,bus8,
         exits1,exits2,exits3,,exits4,exits5,exits6,exits7,exits8,exits9,exits10,exits11,exits12,exits13)
belonging.model <- '
            centralityOtg =~ centralityotago1 + centralityotago2 + centralityotago3 + centralityhall1 + centralityhall2 + centralityhall3
            belongingOtg =~ belongingotago1 + belongingotago2 + belongingotago3 + belonginghall1 + belonginghall2 + belonginghall3
            
            #the overarching score
            overall_belonging =~ 1*centralityOtg + 1*belongingOtg
            '
belonging_fit <- sem(belonging.model, data = new_df)
summary(belonging_fit, fit.measures = TRUE)    

            
wellbeing.model <- '
            mhcsf =~ mhcsf1 + mhcsf2 + mhcsf3 + mhcsf4 + mhcsf5 + mhcsf6 + mhcsf7 + mhcsf8 + mhcsf9 + mhcsf10 + mhcsf11 + mhcsf12 + mhcsf13 + mhcsf14
            wmws =~ wmws1 + wmws2 + wmws3 + wmws4 + wmws5 + wmws6 + wmws7 + wmws8 + wmws9 + wmws10 + wmws11 + wmws12 + wmws13 + wmws14
            phq =~ phq9_1 + phq9_2 + phq9_3 + phq9_4 + phq9_5 + phq9_6 + phq9_7 + phq9_8 + phq9_9
            
            #the overarching score
            overall_wellbeing =~ 1*mhcsf1 + 1*wmws + 1*phq
            '
wellbeing_fit <- sem(wellbeing.model, data = new_df)
summary(wellbeing_fit, fit.measures = TRUE) 

uncertainty.model <- '
            bus =~ bus1 + bus2 + bus3 + bus4 + bus5 + bus6 + bus7 + bus8
           
                       '
uncertainty_fit <- sem(uncertainty.model, data = new_df)
summary(uncertainty_fit, fit.measures = TRUE) 

ident_trans.model <- '
            exits =~ exits1 + exits2 + exits3 +  + exits4 + exits5 + exits6 + exits7 + exits8 + exits9 + exits10 + exits11 + exits12 + exits13
           
                       '
ident_trans_fit <- sem(ident_trans.model, data = new_df)
summary(ident_trans_fit, fit.measures = TRUE)
