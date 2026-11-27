# HELIOS Nova scenarios - SUPPLEMENTARY TABLE 6
# Consumption of Nova groups, and food groups within each Nova group, by ethnicity (primary scenario),
# expressed as energy-standardised intake (g/2000 kcal/day), with R2 for ethnicity and p-values for ethnic differences
# Output: sheet "STable6" in output/tables/STables.xlsx (STable 5 is written to the same file)

library(tidyverse)
library(here)

source(here("R", "functions.R"))   # write_sheets(), fmt_pval()
load(here("data", "derived", "analysis_data.RData"))   # provides df and nova

dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
out_file <- here("output", "tables", "STables.xlsx")


# ===== 1. Settings and labels ==================================================================

nova_lab <- c(nova1 = "Nova-1 Minimally processed foods",
              nova2 = "Nova-2 Processed culinary ingredients",
              nova3 = "Nova-3 Processed foods",
              nova4 = "Nova-4 Ultra-processed foods")

# Order of food groups - add processed animal products after fish and seafood
fg25 <- append(levels(nova$FG_Generic), "Processed_animal_products",
               after = match("Fish_and_seafood", levels(nova$FG_Generic)))

# Colours for shading of relative difference from overall mean
shade_cols <- c(pos50 = "#FBE2D5",   # > 50%
                pos   = "#FEF8F4",   # 0-50%
                neg   = "#F2F7FC",   # -50%-0
                neg50 = "#DAE3F3")   # < -50%

# Primary scenario, energy-standardised: Nova group totals (all_nova*pri) + food groups within each Nova group
st6_vars <- grep("^(?:[A-Z]|all)\\w*_nova[1-4]pri_g_p2k$", names(df), value = TRUE, perl = TRUE)

# Sample sizes
n_eth <- table(df$Ethnicity)
n_all <- nrow(df)


# ===== 2. Mean ± SD by ethnicity and overall ===================================================

st6_long <- df %>%
  select(Ethnicity, all_of(st6_vars)) %>%
  mutate(Ethnicity = as.character(Ethnicity)) %>%
  pivot_longer(-Ethnicity, names_to = "var", values_to = "value")

st6_sum <- st6_long %>%
  bind_rows(mutate(st6_long, Ethnicity = "ALL")) %>%
  group_by(Ethnicity, var) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(fmt = ifelse(mean > 100,
                      sprintf("%.0f ± %.0f", mean, sd),
                      sprintf("%.1f ± %.1f", mean, sd)))

# Drop food groups with no intake in the whole population (Nova group has no items of that food group)
vars_keep <- st6_sum %>% filter(Ethnicity == "ALL", mean > 0) %>% pull(var)

st6_wide <- st6_sum %>%
  filter(var %in% vars_keep) %>%
  select(Ethnicity, var, mean, fmt) %>%
  pivot_wider(names_from = Ethnicity, values_from = c(mean, fmt))


# ===== 3. R2 and p-values for ethnic differences ===============================================

# P from one-way ANOVA; R2 from linear regression of intake on ethnicity
st6_stats <- st6_long %>%
  filter(var %in% vars_keep) %>%
  mutate(Ethnicity = factor(Ethnicity)) %>%
  group_by(var) %>%
  summarise(
    r2   = summary(lm(value ~ Ethnicity, data = tibble(value, Ethnicity)))$r.squared,
    pval = oneway.test(value ~ Ethnicity, data = tibble(value, Ethnicity), var.equal = TRUE)$p.value,
    .groups = "drop"
  ) %>%
  # BH correction across tests reported in this table (primary scenario: Nova totals + food groups)
  mutate(p_adj = p.adjust(pval, method = "BH"))


# ===== 4. Assemble table ========================================================================

st6_tab <- st6_wide %>%
  left_join(st6_stats, by = "var") %>%
  mutate(
    nova = str_extract(var, "nova[1-4]"),
    fg   = sub("_nova[1-4]pri_g_p2k$", "", var),
    is_nova = as.integer(fg == "all"),
    label = ifelse(is_nova == 1, nova_lab[nova], paste0("   ", gsub("_", " ", fg))),
    # Relative difference of each ethnic group mean vs overall mean, for shading
    across(c(mean_CHINESE, mean_INDIAN, mean_MALAY),
           ~ case_when((.x - mean_ALL) / mean_ALL >  0.5 ~ shade_cols[["pos50"]],
                       (.x - mean_ALL) / mean_ALL >  0   ~ shade_cols[["pos"]],
                       (.x - mean_ALL) / mean_ALL < -0.5 ~ shade_cols[["neg50"]],
                       (.x - mean_ALL) / mean_ALL <  0   ~ shade_cols[["neg"]],
                       TRUE ~ NA_character_),
           .names = "fill_{.col}")
  ) %>%
  arrange(nova, factor(fg, levels = c("all", fg25)))

# Columns written out (r2 in %)
st6_out <- st6_tab %>%
  transmute(label, fmt_ALL, fmt_CHINESE, fmt_INDIAN, fmt_MALAY, r2 = r2 * 100, is_nova)

# Fill matrix aligned with written columns (label, ALL, CHINESE, INDIAN, MALAY, r2)
st6_fills <- cbind(NA, NA,
                   st6_tab$fill_mean_CHINESE, st6_tab$fill_mean_INDIAN, st6_tab$fill_mean_MALAY,
                   NA)


# ===== 5. Numbers for footnotes ================================================================

# Tests with BH-adjusted P < 0.05 and < 0.0001, out of all tests in the table
cat("Significant (adjusted P < 0.05):", sum(st6_tab$p_adj < 0.05), "of", nrow(st6_tab),
    "| adjusted P < 0.0001:", sum(st6_tab$p_adj < 0.0001), "\n")

# Non-significant comparisons
st6_tab %>%
  filter(p_adj >= 0.05) %>%
  transmute(nova, food_group = ifelse(is_nova == 1, "total", gsub("_", " ", fg)), p_adj = fmt_pval(p_adj)) %>%
  print(n = Inf)


# ===== 6. Export ===============================================================================

st6_header <- rbind(
  c("Nova group\n   Food group", "Mean ± SD consumption, grams per 2000 kcal/day", "", "", "", "R² (%)"),
  c("", paste0("All (N = ", n_all, ")"),
    paste0(str_to_title(names(n_eth)), " (N = ", as.integer(n_eth), ")"),
    "")
)

write_sheets(
  st6_out, out_file, sheet_name = "STable6",
  title = "Supplementary Table 6. Consumption of Nova groups by food group in the HELIOS study in the primary scenario.",
  header = st6_header,
  header_merges = list(c(1, 1, 2, 1), c(1, 2, 1, 5), c(1, 6, 2, 6)),
  flag_cols = "is_nova",
  bold_col = "is_nova",
  cell_fills = st6_fills,
  num_fmt = c(r2 = "0.0"),
  col_widths = c(47, 16, 20, 18, 18, 12)
)
