### Load Required Libraries
library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(beepr)
library(ggplot2)
library(data.table)

### Create Log-Transformed Variables (add 1 to avoid log(0)) ----
parcels_ag_hmst_acres_df_2$log_CITY_LNTC_TAX <- log(parcels_ag_hmst_acres_df_2$CITY_LNTC_TAX + 1)
parcels_ag_hmst_acres_df_2$log_REAL_CITY_TAX_DOLLAR <- log(parcels_ag_hmst_acres_df_2$REAL_CITY_TAX_DOLLAR + 1)
parcels_ag_hmst_acres_df_2$log_EMV_LAND <- log(parcels_ag_hmst_acres_df_2$EMV_LAND + 1)
parcels_ag_hmst_acres_df_2$log_REAL_EMV_LAND <- log(parcels_ag_hmst_acres_df_2$REAL_EMV_LAND + 1)
parcels_ag_hmst_acres_df_2$log_ESTIMATED_MARKET_VALUE <- log(parcels_ag_hmst_acres_df_2$ESTIMATED_MARKET_VALUE + 1)
parcels_ag_hmst_acres_df_2$log_REAL_ESTIMATED_MARKET_VALUE <- log(parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE + 1)

### Plot Histogram of Real Estimated Market Value
hist(parcels_ag_hmst_acres_df_2$REAL_ESTIMATED_MARKET_VALUE)

### Prepare General Dataset for Analysis
data_general <- parcels_ag_hmst_acres_df_2 %>%
  arrange(PID_SUBRECORD, TAX_YEAR) %>%
  group_by(PID_SUBRECORD) %>%
  filter(any(AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1)) %>%
  mutate(
    switch_year = min(TAX_YEAR[AG_PRESERVE == 1 | G_Green_Acres == 1 | R_Rural_Preserve == 1], na.rm = TRUE),
    pre_post = ifelse(TAX_YEAR < switch_year, "Pre", "Post")
  ) %>%
  ungroup()

### Ensure Paired Observations (retain IDs with both Pre and Post data) ----
paired_ids <- data_general %>%
  group_by(PID_SUBRECORD) %>%
  filter(all(c("Pre", "Post") %in% pre_post)) %>%
  pull(PID_SUBRECORD) %>%
  unique()

data_general <- data_general %>%
  filter(PID_SUBRECORD %in% paired_ids)

table(data_general$pre_post)

### Define Variables for Analysis (original and log-transformed)
vars <- c(
  "LNTC_CITY_SHARE",
  "log_CITY_LNTC_TAX",
  "log_REAL_CITY_TAX_DOLLAR",
  "log_EMV_LAND",
  "log_REAL_EMV_LAND",
  "log_ESTIMATED_MARKET_VALUE",
  "log_REAL_ESTIMATED_MARKET_VALUE"
)

#### Perform General Paired t-tests ----
general_ttest_data <- data_general %>%
  group_by(PID_SUBRECORD, pre_post) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(
    names_from = pre_post,
    values_from = all_of(vars),
    names_sep = "_"
  ) %>%
  drop_na()

ttest_results <- map(vars, ~ {
  pre_col <- paste0(.x, "_Pre")
  post_col <- paste0(.x, "_Post")
  ttest <- t.test(general_ttest_data[[pre_col]], general_ttest_data[[post_col]], paired = TRUE)
  list(variable = .x, ttest = ttest)
})

ttest_results

#### Fit General Linear Models (with controls and fixed effects) ----
general_lm_data <- data_general %>%
  mutate(pre_post = ifelse(pre_post == "Pre", 0, 1))

lm_results <- map(vars, ~ {
  formula <- as.formula(paste0(.x, " ~ pre_post + factor(PID_SUBRECORD) + factor(CTU_NAME) + factor(SCHOOL_ID) + factor(super_property_type) + factor(HOMESTEAD) + ACREAGE + factor(year)"))
  model <- lm(formula, data = general_lm_data)
  list(variable = .x, model = summary(model))
})

lm_results[1:3]
lm_results

results_df <- map_dfr(lm_results, function(res) {
  tidy_model <- broom::tidy(res$model) %>%
    filter(term == "pre_post")
  tidy_model %>%
    mutate(
      program = res$program,
      variable = res$variable,
      r_squared = res$model$r.squared,
      adj_r_squared = res$model$adj.r.squared
    )
})

write_xlsx(results_df, "lm_results.xlsx")

### Prepare Program-Specific Entry Data ----
data_program <- parcels_ag_hmst_acres_df_2 %>%
  group_by(PID_SUBRECORD) %>%
  mutate(
    entry_year_ag_preserv = year[AG_PRESERVE - lag(AG_PRESERVE, default = 0) == 1][1],
    entry_year_rural_pres = year[R_Rural_Preserve - lag(R_Rural_Preserve, default = 0) == 1][1],
    entry_year_green_acre = year[G_Green_Acres - lag(G_Green_Acres, default = 0) == 1][1]
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("entry_year_"),
    names_to = "program",
    values_to = "entry_year"
  ) %>%
  filter(!is.na(entry_year)) %>%
  mutate(period = ifelse(year < entry_year, "Pre", "Post"))

#### Perform Program-Specific Paired t-tests ----
program_ttest_data <- data_program %>%
  group_by(PID_SUBRECORD, program, period) %>%
  summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  group_by(PID_SUBRECORD, program) %>%
  filter(all(c("Pre", "Post") %in% period)) %>%
  pivot_wider(
    names_from = period,
    values_from = all_of(vars),
    names_sep = "_"
  ) %>%
  drop_na()

programs <- unique(program_ttest_data$program)

program_ttest_results <- expand.grid(program = programs, variable = vars) %>%
  split(., seq(nrow(.))) %>%
  map(~{
    p <- .x$program
    v <- .x$variable
    pre_col <- paste0(v, "_Pre")
    post_col <- paste0(v, "_Post")
    df_sub <- program_ttest_data %>% filter(program == p)
    ttest <- t.test(df_sub[[pre_col]], df_sub[[post_col]], paired = TRUE)
    list(program = p, variable = v, ttest = ttest)
  })

program_ttest_results

#### Fit Program-Specific Linear Models ----
program_lm_data <- data_program %>%
  filter(PID_SUBRECORD %in% unique(program_ttest_data$PID_SUBRECORD)) %>%
  mutate(period = ifelse(period == "Pre", 0, 1))

programs <- unique(program_lm_data$program)

program_lm_results <- expand.grid(program = programs, variable = vars) %>%
  split(., seq(nrow(.))) %>%
  map(~ {
    p <- .x$program
    v <- .x$variable
    df_sub <- program_lm_data %>% filter(program == p)
    formula <- as.formula(paste0(v, " ~ period + factor(PID_SUBRECORD) + factor(CTU_NAME) + factor(SCHOOL_ID) + factor(super_property_type) + factor(HOMESTEAD) + ACREAGE + factor(year)"))
    model <- lm(formula, data = df_sub)
    list(program = p, variable = v, model = summary(model))
  })

results_df <- map_dfr(program_lm_results, function(res) {
  tidy_model <- broom::tidy(res$model) %>%
    filter(term == "period")
  tidy_model %>%
    mutate(
      program = res$program,
      variable = res$variable,
      r_squared = res$model$r.squared,
      adj_r_squared = res$model$adj.r.squared
    )
})

write_xlsx(results_df, "program_lm_results.xlsx")
