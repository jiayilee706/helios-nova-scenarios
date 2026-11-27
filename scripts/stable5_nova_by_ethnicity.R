# HELIOS Nova scenarios - SUPPLEMENTARY TABLE 5
# Consumption of Nova groups by ethnicity, under the three classification scenarios,
# expressed as energy-standardised intake (g/2000 kcal/day) and % of total food weight (% g/day).
# Output: sheet "STable5" in output/tables/STables.xlsx (STable 6 is written to the same file)

library(tidyverse)
library(lme4)
library(here)

source(here("R", "functions.R"))
load(here("data", "derived", "analysis_data.RData"))   # provides df

dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
out_file <- here("output", "tables", "STables.xlsx")


# ===== 1. Settings and labels ==================================================================

scen <- c(pri = "Primary scenario", lo = "Low processing scenario", hi = "High processing scenario")
nova_lab <- c(nova1 = "Nova-1 Minimally processed foods",
              nova2 = "Nova-2 Processed culinary ingredients",
              nova3 = "Nova-3 Processed foods",
              nova4 = "Nova-4 Ultra-processed foods")
eth_levels <- c("ALL", "CHINESE", "INDIAN", "MALAY")

# Variable names: absolute (E-standardised, g/2000kcal/day) and % of total food weight (% g/day)
st5_grid <- expand_grid(nova = names(nova_lab), scenario = names(scen)) %>%
  mutate(abs_var = paste0("all_", nova, scenario, "_g_p2k"),
         pct_var = paste0("p_", nova, scenario, "_g_day"))

# Sample sizes by ethnicity (for footnote)
n_eth <- table(df$Ethnicity)


# ===== 2. Mean ± SD by ethnicity and overall ===================================================

st5_long <- df %>%
  select(Ethnicity, all_of(c(st5_grid$abs_var, st5_grid$pct_var))) %>%
  mutate(Ethnicity = as.character(Ethnicity)) %>%
  pivot_longer(-Ethnicity, names_to = "var", values_to = "value")

st5_sum <- st5_long %>%
  # Add ALL group as a copy of everyone
  bind_rows(mutate(st5_long, Ethnicity = "ALL")) %>%
  group_by(Ethnicity, var) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd   = sd(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    vartype  = ifelse(grepl("_p2k$", var), "g", "pct"),
    nova     = str_extract(var, "nova[1-4]"),
    scenario = str_extract(var, "(pri|lo|hi)(?=_g_)"),
    # Absolute means >100 with no decimals, otherwise 1 decimal
    fmt = ifelse(vartype == "g" & mean > 100,
                 sprintf("%.0f ± %.0f", mean, sd),
                 sprintf("%.1f ± %.1f", mean, sd))
  )

st5_wide <- st5_sum %>%
  select(nova, Ethnicity, scenario, vartype, fmt) %>%
  pivot_wider(names_from = c(scenario, vartype), values_from = fmt)


# ===== 3. P for scenario x ethnicity interaction (mixed models, unadjusted) ====================

# For each Nova group: long format across 3 scenarios, random intercept for participant
# LRT comparing models with vs without scenario x ethnicity interaction (ML, not REML)
# Same interaction models as Figure 3
st5_pint <- tibble(nova = names(nova_lab), p_int = NA_real_)

for (k in names(nova_lab)) {

  vars_k <- paste0("all_", k, names(scen), "_g_p2k")

  df_long <- df %>%
    select(NOVA_PID, Ethnicity, all_of(vars_k)) %>%
    pivot_longer(all_of(vars_k), names_to = "scenario", values_to = "intake") %>%
    mutate(scenario = factor(str_extract(scenario, "(pri|lo|hi)(?=_g_)"), levels = names(scen)))

  mixed_base <- lmer(intake ~ scenario + Ethnicity + (1 | NOVA_PID), data = df_long, REML = FALSE)
  mixed_int  <- lmer(intake ~ scenario * Ethnicity + (1 | NOVA_PID), data = df_long, REML = FALSE)

  st5_pint$p_int[st5_pint$nova == k] <- anova(mixed_base, mixed_int)$`Pr(>Chisq)`[2]
}

# BH correction across the 4 Nova groups
st5_pint <- st5_pint %>%
  mutate(p_adj = p.adjust(p_int, method = "BH"))


# ===== 4. Assemble table ========================================================================

st5_tab <- NULL

for (k in names(nova_lab)) {

  # Ethnic group rows
  body_k <- st5_wide %>%
    filter(nova == k) %>%
    arrange(factor(Ethnicity, levels = eth_levels)) %>%
    transmute(label = ifelse(Ethnicity == "ALL", "All", str_to_title(Ethnicity)),
              pri_g, pri_pct, sp1 = NA_character_,
              lo_g,  lo_pct,  sp2 = NA_character_,
              hi_g,  hi_pct)

  # P-value row
  p_k <- st5_pint$p_adj[st5_pint$nova == k]
  p_txt <- if (p_k < 1e-300) {
    "< 1 × 10⁻³⁰⁰"   # below the smallest P that can be stored
  } else if (p_k < 0.001) {
    sci <- formatC(p_k, format = "e", digits = 1)   # e.g. "3.2e-08"
    paste0("= ", sub("e.*", "", sci), " × 10",
           chartr("-0123456789", "⁻⁰¹²³⁴⁵⁶⁷⁸⁹", as.character(as.integer(sub(".*e", "", sci)))))
  } else if (p_k < 0.01) {
    paste("=", sprintf("%.3f", p_k))
  } else {
    paste("=", sprintf("%.2f", p_k))
  }
  p_lab <- paste0("P for scenario × ethnicity ", p_txt)

  st5_tab <- bind_rows(
    st5_tab,
    tibble(label = nova_lab[[k]], is_nova = 1),   # Nova group heading row
    body_k,
    tibble(label = p_lab, is_p = 1)
  )
}

st5_tab <- st5_tab %>%
  mutate(across(c(is_nova, is_p), ~ replace_na(.x, 0)))


# ===== 5. Export ================================================================================

st5_header <- rbind(
  c("Nova group", "Mean ± SD consumption", rep("", 7)),
  c("Ethnicity", scen[["pri"]], "", "", scen[["lo"]], "", "", scen[["hi"]], ""),
  c("", "g/2000kcal/day", "% g/day", "", "g/2000kcal/day", "% g/day", "", "g/2000kcal/day", "% g/day")
)

write_sheets(
  st5_tab, out_file, sheet_name = "STable5",
  title = paste0("Supplementary Table 5. Consumption of Nova groups by ethnicity expressed as energy-standardised ",
                 "intake (g/2000 kcal/day) and proportion of total food weight (% g/day)."),
  header = st5_header,
  header_merges = list(c(1, 2, 1, 9), c(2, 2, 2, 3), c(2, 5, 2, 6), c(2, 8, 2, 9)),
  flag_cols = c("is_nova", "is_p"),
  bold_col = "is_nova",
  italic_col = "is_p", merge_col = "is_p",
  col_widths = c(30, 17, 13, 3, 17, 13, 3, 17, 13)
)
