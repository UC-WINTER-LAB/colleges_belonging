

##################################### MISSINGNESS ##############################

library(naniar)

# Observed variables used in your SEM
sem_vars <- c(
  paste0("exits", 1:10),
  paste0("wmws", 1:14, "_t1"),
  paste0("wmws", 1:14, "_t3"),
  "gender"
)

mcar_data <- sem_data %>%
  select(all_of(sem_vars))

# Check that all variables are numeric
stopifnot(all(map_lgl(mcar_data, is.numeric)))

# Missingness by variable
missing_summary <- mcar_data %>%
  miss_var_summary()

# Little's MCAR test
mcar_result <- mcar_data %>%
  mcar_test()

mcar_result

##################################### Measurement Invariance ##################

# Items at each wave
items_t1 <- paste0("wmws", 1:14, "_t1")
items_t3 <- paste0("wmws", 1:14, "_t3")

# Use the same participants and variables for all models
wellbeing_data <- sem_data %>%
  select(all_of(c(items_t1, items_t3))) %>%
  filter(if_any(everything(), ~ !is.na(.x)))

# Correlated residuals for the same item across waves
residual_correlations <- paste(
  paste0(items_t1, " ~~ ", items_t3),
  collapse = "\n"
)

# Factor loadings:
# First loading fixed to 1 at each wave for identification
make_factor <- function(factor_name, items, equal_loadings = FALSE) {
  terms <- if (equal_loadings) {
    c(
      paste0("1*", items[1]),
      paste0("l", 2:14, "*", items[-1])
    )
  } else {
    c(paste0("1*", items[1]), items[-1])
  }
  
  paste0(factor_name, " =~ ", paste(terms, collapse = " + "))
}

# 1. Configural invariance
model_configural <- paste(
  make_factor("wmws_t1", items_t1),
  make_factor("wmws_t3", items_t3),
  "wmws_t1 ~~ wmws_t3",
  residual_correlations,
  "wmws_t1 ~ 0*1",
  "wmws_t3 ~ 0*1",
  sep = "\n"
)

# 2. Metric invariance: same loading labels at both waves
model_metric <- paste(
  make_factor("wmws_t1", items_t1, equal_loadings = TRUE),
  make_factor("wmws_t3", items_t3, equal_loadings = TRUE),
  "wmws_t1 ~~ wmws_t3",
  residual_correlations,
  "wmws_t1 ~ 0*1",
  "wmws_t3 ~ 0*1",
  sep = "\n"
)

# 3. Scalar invariance: equal loadings and intercepts
equal_intercepts <- paste(
  c(
    paste0(items_t1, " ~ i", 1:14, "*1"),
    paste0(items_t3, " ~ i", 1:14, "*1")
  ),
  collapse = "\n"
)

model_scalar <- paste(
  make_factor("wmws_t1", items_t1, equal_loadings = TRUE),
  make_factor("wmws_t3", items_t3, equal_loadings = TRUE),
  "wmws_t1 ~~ wmws_t3",
  residual_correlations,
  equal_intercepts,
  "wmws_t1 ~ 0*1",       # Reference-wave mean fixed to zero
  "wmws_t3 ~ mean_t3*1", # Follow-up mean freely estimated
  sep = "\n"
)

# Fit all three models
invariance_models <- list(
  Configural = model_configural,
  Metric = model_metric,
  Scalar = model_scalar
)

invariance_fits <- map(
  invariance_models,
  ~ cfa(
    model = .x,
    data = wellbeing_data,
    estimator = "MLR",
    missing = "ML",
    meanstructure = TRUE
  )
)

# Check convergence
map_lgl(invariance_fits, ~ lavInspect(.x, "converged"))

# Fit indices and changes relative to the preceding model
invariance_table <- imap_dfr(
  invariance_fits,
  function(fit, model_name) {
    fm <- fitMeasures(
      fit,
      c(
        "chisq.scaled", "df", "pvalue.scaled",
        "cfi.robust", "tli.robust", "rmsea.robust", "srmr"
      )
    )
    
    tibble(
      Model = model_name,
      Chi_square = unname(fm["chisq.scaled"]),
      df = unname(fm["df"]),
      p = unname(fm["pvalue.scaled"]),
      CFI = unname(fm["cfi.robust"]),
      TLI = unname(fm["tli.robust"]),
      RMSEA = unname(fm["rmsea.robust"]),
      SRMR = unname(fm["srmr"])
    )
  }
) %>%
  mutate(
    CFI_drop = lag(CFI) - CFI,
    RMSEA_increase = RMSEA - lag(RMSEA),
    SRMR_increase = SRMR - lag(SRMR)
  )

print(invariance_table, width = Inf)

# Robust nested-model comparisons
lavTestLRT(
  invariance_fits$Configural,
  invariance_fits$Metric,
  invariance_fits$Scalar
)

################################## Correlation matrix ##########################

items_t1 <- paste0("wmws", 1:14, "_t1")
items_t3 <- paste0("wmws", 1:14, "_t3")

# Equal wellbeing loadings across waves; first loading fixed to 1
wellbeing_factor <- function(factor_name, items) {
  paste0(
    factor_name, " =~ ",
    paste(
      c(
        paste0("1*", items[1]),
        paste0("l", 2:14, "*", items[-1])
      ),
      collapse = " + "
    )
  )
}

# Measurement model only
cor_model <- paste(
  "
  exits =~ exits1 + exits2 + exits3 + exits4
  continuity =~ exits5 + exits6 + exits7
  gain =~ exits8 + exits9 + exits10
  ",
  
  wellbeing_factor("wmws_t1", items_t1),
  wellbeing_factor("wmws_t3", items_t3),
  
  # Equal wellbeing intercepts across waves
  paste(paste0(items_t1, " ~ i", 1:14, "*1"), collapse = "\n"),
  paste(paste0(items_t3, " ~ i", 1:14, "*1"), collapse = "\n"),
  
  # Allow the wellbeing mean to differ across waves
  "wmws_t1 ~ 0*1",
  "wmws_t3 ~ mean_t3*1",
  
  # Correlated residuals for matching items
  paste(paste0(items_t1, " ~~ ", items_t3), collapse = "\n"),
  
  sep = "\n"
)

# CFA freely estimates all correlations between the five factors
cor_fit <- cfa(
  model = cor_model,
  data = sem_data,
  estimator = "MLR",
  missing = "fiml",
  meanstructure = TRUE
)

# Check convergence and model fit
lavInspect(cor_fit, "converged")
summary(cor_fit, fit.measures = TRUE, standardized = TRUE)

# Extract latent correlations in the required order
factor_order <- c(
  "exits", "wmws_t1", "continuity", "gain", "wmws_t3"
)

latent_cor <- lavInspect(cor_fit, "cor.lv")[
  factor_order, factor_order
]

# Format the lower triangle
cor_display <- matrix(
  sprintf("%.2f", latent_cor),
  nrow = 5,
  ncol = 5
)

colnames(cor_display) <- paste0("(", 1:5, ")")

cor_table <- bind_cols(
  tibble(
    Variable = c(
      "Pre-university identity T1 (1)",
      "Wellbeing T1 (2)",
      "Identity continuity T2 (3)",
      "Identity gain T2 (4)",
      "Wellbeing T3 (5)"
    )
  ),
  as_tibble(cor_display[, 1:4, drop = FALSE])
)

print(cor_table, width = Inf)

latent_cor_tests <- standardizedSolution(
  cor_fit,
  type = "std.all"
) %>%
  filter(
    op == "~~",
    lhs %in% factor_order,
    rhs %in% factor_order,
    lhs != rhs
  ) %>%
  transmute(
    Variable_1 = lhs,
    Variable_2 = rhs,
    r = est.std,
    SE = se,
    z = z,
    p = pvalue,
    CI_lower = ci.lower,
    CI_upper = ci.upper
  )

lavInspect(cor_fit, "nobs")
