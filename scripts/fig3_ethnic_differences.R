# HELIOS Nova scenarios - FIGURE 3
# Scenario-specific mean differences in Nova group intakes (g per 2,000 kcal per day)
# for Indian and Malay vs Chinese participants, with P for interaction
# (ethnicity x scenario interaction) shown under each Nova group heading.

library(tidyverse)
library(lme4)
library(ggtext)
library(scales)
library(ragg)
library(here)

source(here("R", "functions.R"))
load(here("data", "derived", "analysis_data.RData"))   # provides df

fig_dir    <- here("output", "figures")
source_dir <- here("output", "source_data")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(source_dir, showWarnings = FALSE, recursive = TRUE)

nova_ids    <- paste0("nova", 1:4)
nova_labels <- c(nova1 = "Nova-1 MPF", nova2 = "Nova-2 PCI",
                 nova3 = "Nova-3 PF",  nova4 = "Nova-4 UPF")
scenario_ids    <- c("pri", "lo", "hi")
scenario_labels <- c(pri = "Primary", lo = "Low processing", hi = "High processing")

# Bar fill and a darker outline of the same hue (Chinese is the reference, not drawn)
ethnic_fill    <- c(Indian = "#9B5E82", Malay = "#009E73")
ethnic_outline <- c(Indian = "#6B3D58", Malay = "#006B4E")

base_font <- "Arial"


# ===== 1. P for interaction: mixed models for scenario x ethnicity ==========
# LRT comparing intake ~ scenario + Ethnicity vs intake ~ scenario * Ethnicity,
# random intercept per participant, one model per Nova group (unadjusted).
# BH correction across the four Nova groups.

pval_df <- tibble(nova = nova_ids, p_int = NA_real_)

for (k in nova_ids) {

  vars <- paste0("all_", k, scenario_ids, "_g_p2k")

  df_long <- df %>%
    select(NOVA_PID, Ethnicity, all_of(vars)) %>%
    pivot_longer(cols = all_of(vars), names_to = "scenario", values_to = "intake") %>%
    mutate(scenario = factor(scenario, levels = vars, labels = c("primary", "low", "high")))

  mixed_base <- lmer(intake ~ scenario + Ethnicity + (1 | NOVA_PID), data = df_long, REML = FALSE)
  mixed_int  <- lmer(intake ~ scenario * Ethnicity + (1 | NOVA_PID), data = df_long, REML = FALSE)

  pval_df$p_int[pval_df$nova == k] <- anova(mixed_base, mixed_int)$`Pr(>Chisq)`[2]
}

pval_df <- pval_df %>%
  mutate(p_int_bh = p.adjust(p_int, method = "BH"),
         p_lab = fmt_p_sci(p_int_bh))


# ===== 2. Scenario-specific mean differences vs Chinese =======================
# Difference in means with unpooled SE, 95% CI = beta +/- 1.96 * SE.

intake_vars <- as.vector(outer(nova_ids, scenario_ids,
                               function(n, s) paste0("all_", n, s, "_g_p2k")))

scenario_diff <- df %>%
  select(Ethnicity, all_of(intake_vars)) %>%
  pivot_longer(-Ethnicity, names_to = "var", values_to = "intake") %>%
  mutate(
    nova     = str_extract(var, "nova[1-4]"),
    scenario = str_match(var, "nova[1-4](pri|lo|hi)_")[, 2]
  ) %>%
  group_by(nova, scenario, Ethnicity) %>%
  summarise(mean = mean(intake, na.rm = TRUE),
            sd   = sd(intake, na.rm = TRUE),
            n    = sum(!is.na(intake)),
            .groups = "drop") %>%
  group_by(nova, scenario) %>%
  mutate(
    beta = mean - mean[Ethnicity == "CHINESE"],
    SE   = sqrt(sd^2 / n + (sd^2 / n)[Ethnicity == "CHINESE"])
  ) %>%
  ungroup() %>%
  filter(Ethnicity != "CHINESE") %>%
  transmute(
    nova,
    scenario  = factor(scenario_labels[scenario], levels = scenario_labels),
    ethnicity = factor(str_to_title(as.character(Ethnicity)), levels = names(ethnic_fill)),
    beta,
    lcl = beta - qnorm(.975) * SE,
    ucl = beta + qnorm(.975) * SE
  )


# ===== 3. Facet headings: Nova group + P_interaction ====================================

nova_strip <- pval_df %>%
  mutate(strip = paste0(
    "<b>", nova_labels[nova], "</b><br>",
    "<span style='font-size:6.5pt'><i>P</i><sub>interaction</sub> ", p_lab, "</span>"
  ))
nova_strip <- set_names(nova_strip$strip, nova_strip$nova)

scenario_diff <- scenario_diff %>%
  mutate(nova_strip = factor(nova_strip[nova], levels = nova_strip[nova_ids]))


# ===== 4. Figure ==============================================================

fig3 <- ggplot(scenario_diff, aes(x = beta, y = fct_rev(ethnicity))) +

  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.3) +

  geom_col(aes(fill = ethnicity, colour = ethnicity), width = 0.5, linewidth = 0.25) +

  geom_errorbar(
    aes(xmin = lcl, xmax = ucl),
    width = 0.15, linewidth = 0.3, colour = "grey15",
    orientation = "y"
  ) +

  facet_grid(scenario ~ nova_strip, scales = "free_x", switch = "y") +
  scale_x_continuous(
    labels = label_number(style_negative = "minus", big.mark = ","),
    expand = expansion(mult = 0.08)
  ) +
  scale_fill_manual(values = ethnic_fill) +
  scale_colour_manual(values = ethnic_outline) +

  labs(
    y = NULL,
    x = "Mean difference in intake vs Chinese participants (g/2,000 kcal/day)"
  ) +

  theme_minimal(base_size = 7, base_family = base_font) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.25),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.25),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.y = element_blank(),

    strip.text.x = element_markdown(size = 7, colour = "black", lineheight = 1.3,
                                    halign = 0.5, margin = margin(b = 4)),
    strip.text.y.left = element_text(size = 7, face = "bold", colour = "black", angle = 90,
                                     margin = margin(r = 7)),
    strip.placement  = "outside",
    strip.background = element_blank(),

    panel.spacing.y = unit(10, "pt"),
    panel.spacing.x = unit(10, "pt"),   # keeps adjacent panels' axis labels apart

    axis.text.x  = element_text(size = 6, colour = "black"),
    axis.text.y  = element_text(size = 7, hjust = 1, colour = "black",
                                margin = margin(r = 2)),
    axis.title.x = element_text(size = 7, margin = margin(t = 5)),

    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 3, r = 5, b = 3, l = 3)
  )

fig_w <- 183
fig_h <- 90

ggsave(file.path(fig_dir, "Figure3_ethnic_differences.pdf"), fig3,
       width = fig_w, height = fig_h, units = "mm", device = cairo_pdf)
ggsave(file.path(fig_dir, "Figure3_ethnic_differences.png"), fig3,
       width = fig_w, height = fig_h, units = "mm", dpi = 600,
       device = ragg::agg_png, bg = "white")


# ===== Source data: Figure 3 ===================================================
# Scenario-specific mean differences vs Chinese (g per 2,000 kcal per day) with
# 95% CI, and BH-adjusted P for interaction (ethnicity x scenario) per Nova group.

source_fig3 <- scenario_diff %>%
  left_join(select(pval_df, nova, p_int_bh), by = "nova") %>%
  arrange(nova, scenario, ethnicity) %>%
  transmute(
    nova_group = unname(nova_labels[nova]),
    scenario   = as.character(scenario),
    ethnicity  = as.character(ethnicity),
    reference  = "Chinese",
    mean_difference_g_per_2000kcal = round(beta, 4),
    lower_95ci = round(lcl, 4),
    upper_95ci = round(ucl, 4),
    p_interaction_bh = signif(p_int_bh, 3)
  )

openxlsx::write.xlsx(source_fig3, file.path(source_dir, "SourceData_Fig3.xlsx"), sheetName = "Fig3", overwrite = TRUE)
