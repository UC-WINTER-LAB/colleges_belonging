# Run after 00_load(1).R, which creates new_df.
# This replaces the factor-score preparation and simic_sem fit in 02_analysis.R.
# No need to run 01_run_cfa.R for this model.

# Prevent joins from multiplying participants when ID/wave pairs are duplicated.
if (anyNA(new_df$id) || anyDuplicated(new_df[c("id", "time")])) {
  stop("new_df must have non-missing IDs and only one row per ID/time pair.")
}

wmws_items <- paste0("wmws", 1:14)

# One row per participant, retaining the wave-1 cohort as in the original code.
sem_data <- new_df %>%
  filter(time == "s1") %>%
  select(id, gender, exits1, exits2, exits3, exits4, all_of(wmws_items)) %>%
  rename_with(~ paste0(.x, "_t1"), all_of(wmws_items)) %>%
  left_join(
    new_df %>%
      filter(time == "s2") %>%
      select(id, exits5, exits6, exits7, exits8, exits9, exits10),
    by = "id"
  ) %>%
  left_join(
    new_df %>%
      filter(time == "s3") %>%
      select(id, all_of(wmws_items)) %>%
      rename_with(~ paste0(.x, "_t3"), all_of(wmws_items)),
    by = "id"
  )

total_effect_model <- '
  # Measurement model: all five constructs estimated within this SEM
  exits =~ exits1 + exits2 + exits3 + exits4

  wmws_t1 =~ wmws1_t1 + wmws2_t1 + wmws3_t1 + wmws4_t1 +
             wmws5_t1 + wmws6_t1 + wmws7_t1 + wmws8_t1 +
             wmws9_t1 + wmws10_t1 + wmws11_t1 + wmws12_t1 +
             wmws13_t1 + wmws14_t1

  wmws_t3 =~ wmws1_t3 + wmws2_t3 + wmws3_t3 + wmws4_t3 +
             wmws5_t3 + wmws6_t3 + wmws7_t3 + wmws8_t3 +
             wmws9_t3 + wmws10_t3 + wmws11_t3 + wmws12_t3 +
             wmws13_t3 + wmws14_t3

  # Original structural model
  wmws_t3 ~ exits + gender + wmws_t1

  # Correlated residuals for the same wellbeing item across waves
  wmws1_t1 ~~ wmws1_t3
  wmws2_t1 ~~ wmws2_t3
  wmws3_t1 ~~ wmws3_t3
  wmws4_t1 ~~ wmws4_t3
  wmws5_t1 ~~ wmws5_t3
  wmws6_t1 ~~ wmws6_t3
  wmws7_t1 ~~ wmws7_t3
  wmws8_t1 ~~ wmws8_t3
  wmws9_t1 ~~ wmws9_t3
  wmws10_t1 ~~ wmws10_t3
  wmws11_t1 ~~ wmws11_t3
  wmws12_t1 ~~ wmws12_t3
  wmws13_t1 ~~ wmws13_t3
  wmws14_t1 ~~ wmws14_t3
'

# Items are treated as continuous, consistent with the original CFA code.
# MLR provides robust SEs; FIML uses available item responses.
# With the default fixed.x = TRUE, missing gender causes case exclusion.
# Wellbeing loadings/intercepts are freely estimated across waves: this is
# not a test of longitudinal measurement invariance.
total_effect__sem <- lavaan::sem(
  model = total_effect_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml"
)

summary(total_effect__sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

simic_effects <- parameterEstimates(total_effect__sem, standardized = TRUE, ci = TRUE) %>%
  filter(op %in% c("~", ":=")) %>%
  select(lhs, op, rhs, label, est, se, z, pvalue, ci.lower, ci.upper, std.all)

print(simic_effects)

