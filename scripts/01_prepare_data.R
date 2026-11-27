#### HELIOS Nova scenarios: data preparation
####
#### Starting datasets:
####   df_0  - one row per HELIOS participant (core and health and lifestyle questionnaire data)
####   ffq_0 - one row per participant who completed the baseline FFQ
####   ffq2_0 - one row per participant who completed the repeat FFQ, same variables as ffq0
####   nova - FFQ item classification (data/nova_classification.csv)
####
#### 1. Load HELIOS data and apply exclusions (baseline and repeat FFQ)
#### 2. Derive Nova group intakes under the three classification scenarios (baseline and repeat FFQ)
#### 3. Covariates (age, sex, ethnicity, education, BMI)
#### 4. Merge repeat FFQ with the baseline analytic sample



## ===== Baseline dataset ===================================================================================================


# Exclusions:
# 1. No FFQ data
# 2. Implausible energy intake: < 500 kcal/day or > sex-specific mean + 3 SD
#    (mean and SD among participants with FFQ data, before exclusion 3)
# 3. Ethnicity other than Chinese, Indian or Malay

df <- df_0 %>%
  inner_join(ffq_0, by = "NOVA_PID") %>%   # 8501 after merging FFQ dataset
  mutate(energy = DFFQB3_Energy_per_day)

energy_upper <- df %>%
  group_by(Sex) %>%
  summarise(cutoff = mean(energy, na.rm = TRUE) + 3 * sd(energy, na.rm = TRUE)) %>%
  deframe()

df <- df %>%
  filter(energy >= 500,
         energy <= energy_upper[Sex],
         Ethnicity != "OTHERS") %>%        # 8229
  mutate(Ethnicity = droplevels(Ethnicity))


## ===== Repeat FFQ ===================================================================================================

ffq2 <- ffq2_0 %>%
  inner_join(df[, c("NOVA_PID", "Sex")], by = "NOVA_PID") %>%
  filter(energy2 >= 500, energy2 <= energy_upper[Sex])


# ===== [PART 2] NOVA GROUP INTAKES ==================================================================================

# This part only needs (i) a data frame with one column per FFQ item in g/day and energy (kcal/day), and
# (ii) the item classification file (data/nova_classification.csv), with columns:
#   Variable_g - FFQ item variable name (g/day)
#   FG_Generic - generic food group
#   FG_UPF     - UPF subtype name
#   nova_pri, nova_lo, nova_hi - Nova group (1-4) under the primary, low and high processing scenarios

## ===== Load classification ==========================================================================================

fg_order <- c(
  "Refined_grains", "Whole_grains", "Ethnic_breads",
  "Vegetables", "Legumes", "Fruits", "Soy_products", "Dairy_products",
  "Egg_and_egg_dishes", "Fish_and_seafood", "Poultry", "Red_meat",
  "Gravies_and_sauces", "Soup", "Mixed_dishes_or_ingredients",
  "Spreads", "Snacks_and_desserts", "Western_fast_food", "Coffee_or_tea",
  "Malted_drink", "Sugar_added_to_drinks", "Fruit_juice", "Sweet_beverages", "Powdered_nutrition_drink"
)

nova <- read_csv(here("data", "nova_classification.csv")) %>%
  mutate(FG_Generic = factor(str_replace_all(FG_Generic, " |/", "_"), levels = fg_order))

stopifnot(nrow(nova) == 253, !anyNA(nova$FG_Generic))       # NA = food group name not in fg_order
stopifnot(all(nova$Variable_g %in% names(df)), all(nova$Variable_g %in% names(ffq2)))

scenarios <- c("pri", "lo", "hi")
novas <- paste0("nova", 1:4)

# Lists of FFQ items in each Nova group by food group and scenario
# Format: list_gpvars[[scenario]][[nova_group]][[fg]]
list_gpvars <- list()

for (scenario in scenarios)
  for (i in 1:4)
    for (fg in fg_order) {
      idx <- nova[[paste0("nova_", scenario)]] == i & nova$FG_Generic == fg
      list_gpvars[[scenario]][[paste0("nova", i)]][[fg]] <- nova$Variable_g[idx]
    }


## ===== Derive variables (baseline and repeat FFQ) ===================================================================

# Derive g/day, % contribution by weight, and energy-density-adjusted (g/2000 kcal) intakes
# for each Nova group and food group under all scenarios.

ffq_list <- list(base = df, ffq2 = ffq2)

for (d in names(ffq_list)) {

  dat <- ffq_list[[d]]
  dat$energy <- dat$DFFQB3_Energy_per_day

  # Food group within Nova group
  for (scenario in scenarios)
    for (nova_group in novas)
      for (fg in fg_order) {
        vars <- list_gpvars[[scenario]][[nova_group]][[fg]]
        dat[[paste0(fg, "_", nova_group, scenario, "_g_day")]] <-
          if (length(vars) == 0) 0 else rowSums(dat[, vars, drop = FALSE])
      }

  # Nova groups across all foods
  for (scenario in scenarios)
    for (nova_group in novas) {
      vars <- unlist(list_gpvars[[scenario]][[nova_group]], use.names = FALSE)
      dat[[paste0("all_", nova_group, scenario, "_g_day")]] <- rowSums(dat[, vars, drop = FALSE])
    }

  # Processed animal products (UPF subtype)
  for (scenario in scenarios)
    dat[[paste0("Processed_animal_products_nova4", scenario, "_g_day")]] <-
      rowSums(dat[, paste0(c("Fish_and_seafood", "Poultry", "Red_meat"), "_nova4", scenario, "_g_day")])

  # Total food intake and % contribution of each Nova group
  dat$total_g_day <- rowSums(dat[, nova$Variable_g])

  for (scenario in scenarios)
    for (nova_group in novas)
      dat[[paste0("p_", nova_group, scenario, "_g_day")]] <-
        dat[[paste0("all_", nova_group, scenario, "_g_day")]] / dat$total_g_day * 100

  # g per 2000 kcal/day (derived g/day vars only, not the FFQ items or percentages)
  gvars <- grep("_g_day$", names(dat), value = TRUE)
  gvars <- gvars[!grepl("^DFFQ|^p_nova", gvars)]
  dat[sub("_g_day$", "_g_p2k", gvars)] <- dat[gvars] / dat$energy * 2000

  ffq_list[[d]] <- dat
}


# ===== [PART 3] MERGE NEW VARIABLES INTO FULL DATASET ======================

df <- ffq_list$base

# Rename ffq2 variables with f2_ prefix before merging with baseline full dataset
ffq2 <- ffq_list$ffq2 %>%
  select(NOVA_PID, energy, total_g_day, total_g_p2k, matches("_nova[1-4](pri|lo|hi)_g_(day|p2k)$")) %>%
  rename_with(~ paste0("f2_", .), -NOVA_PID)
df_f2 <- ffq_list$base %>%
  inner_join(ffq2, by = "NOVA_PID")

stopifnot(!anyDuplicated(df_f2$NOVA_PID))


# ===== SAVE =========================================================================================================

dir.create(here("data", "derived"), showWarnings = FALSE, recursive = TRUE)
save(df, df_f2, nova, file = here("data", "derived", "analysis_data.RData"))


