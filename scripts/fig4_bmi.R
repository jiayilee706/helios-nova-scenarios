# HELIOS Nova scenarios - FIGURE 4
# Association of Nova group intake (per 100 g per 2,000 kcal per day) with BMI
# under each processing scenario, with P for interaction (intake x scenario)
# and the number of FFQ items in each Nova group.
# Illustrative downstream analysis only (not aetiological).

library(tidyverse)
library(sandwich)
library(car)
library(ggtext)
library(scales)
library(ragg)
library(here)

source(here("R", "functions.R"))
load(here("data", "derived", "analysis_data.RData"))   # provides df (participants) and nova (FFQ items)

fig_dir    <- here("output", "figures")
source_dir <- here("output", "source_data")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(source_dir, showWarnings = FALSE, recursive = TRUE)

nova_ids    <- paste0("nova", 1:4)
nova_labels <- c(nova1 = "Nova-1 MPF", nova2 = "Nova-2 PCI",
                 nova3 = "Nova-3 PF",  nova4 = "Nova-4 UPF")
scenario_ids    <- c("pri", "lo", "hi")
scenario_labels <- c(primary = "Primary", low = "Low processing", high = "High processing")
scenario_order  <- c("Primary", "Low processing", "High processing")   # top to bottom

nfood_col <- "#8B6A3E"
base_font <- "Arial"


# ===== 1. BMI ~ intake x scenario, cluster-robust SEs =========================
# Pooled long data (3 rows per participant), adjusted for age, sex, ethnicity
# and education; SEs clustered by participant. p_int = robust Wald F test of
# the two intake x scenario interaction terms.
# Scenario-specific slopes = intake + interaction term, with robust SEs.

slopes_df <- NULL

for (k in nova_ids) {

  # Intake variables for this Nova group under the three scenarios (e.g. all_nova4pri_g_p2k, all_nova4lo_g_p2k, all_nova4hi_g_p2k)
  vars <- paste0("all_", k, scenario_ids, "_g_p2k")

  # Long format: 3 rows per participant (one per scenario), same BMI and covariates in each row
  df_long <- df %>%
    select(NOVA_PID, Age, Sex, Ethnicity, Education.fc, bmi, all_of(vars)) %>%
    pivot_longer(cols = all_of(vars), names_to = "scenario", values_to = "intake") %>%
    mutate(scenario = factor(scenario, levels = vars, labels = c("primary", "low", "high")))

  # intake * scenario = separate intake-BMI slope for each scenario
  # Coefficients: intake = slope in primary; intake:scenariolow / intake:scenariohigh = difference in slope vs primary
  model_full <- lm(bmi ~ intake * scenario + Age + Sex + Ethnicity + Education.fc, data = df_long)

  # Cluster-robust variance-covariance matrix: allows for the 3 rows from the same participant being correlated
  V <- vcovCL(model_full, cluster = ~NOVA_PID)
  b <- coef(model_full)

  # p_int: Wald test that both slope differences are 0 at the same time (= same slope in all 3 scenarios), using robust V
  # Output table has 2 rows (restricted model, full model); the P-value is in row 2
  p_int <- linearHypothesis(model_full,
                            c("intake:scenariolow = 0", "intake:scenariohigh = 0"),
                            vcov = V, test = "F")$`Pr(>F)`[2]

  # Scenario-specific slopes and robust SEs
  # Low/high slope = primary slope + difference; Var(a + b) = Var(a) + Var(b) + 2 Cov(a, b)
  slopes_df <- bind_rows(slopes_df, tibble(
    nova     = k,
    scenario = c("primary", "low", "high"),
    slope    = c(b["intake"],
                 b["intake"] + b["intake:scenariolow"],
                 b["intake"] + b["intake:scenariohigh"]),
    se       = sqrt(c(V["intake", "intake"],
                      V["intake", "intake"] + V["intake:scenariolow", "intake:scenariolow"] + 2 * V["intake", "intake:scenariolow"],
                      V["intake", "intake"] + V["intake:scenariohigh", "intake:scenariohigh"] + 2 * V["intake", "intake:scenariohigh"])),
    p_int    = p_int
  ))
}

# BH correction of p_int across the four Nova groups
p_int_adj <- slopes_df %>%
  distinct(nova, p_int) %>%
  mutate(p_int_bh = p.adjust(p_int, method = "BH")) %>%
  select(-p_int)

slopes_df <- slopes_df %>%
  mutate(
    beta = slope * 100,
    lcl = (slope - qnorm(.975) * se) * 100,
    ucl = (slope + qnorm(.975) * se) * 100,
    beta_ci = fmt_betaci(beta, lcl, ucl),
    scenario = factor(scenario_labels[scenario], levels = scenario_order)
  ) %>%
  left_join(p_int_adj, by = "nova")


# ===== 2. Number of FFQ items in each Nova group per scenario =================

nova_N <- nova %>%
  select(nova_pri, nova_lo, nova_hi) %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "nova_group") %>%
  count(scenario, nova_group, name = "n_foods") %>%
  transmute(
    nova     = paste0("nova", nova_group),
    scenario = factor(scenario_labels[c(nova_pri = "primary", nova_lo = "low", nova_hi = "high")[scenario]],
                      levels = scenario_order),
    n_foods
  )

slopes_df <- slopes_df %>%
  left_join(nova_N, by = c("nova", "scenario")) %>%
  print()


# ===== 3. Facet headings and axis title text ==================================

nova_strip <- slopes_df %>%
  distinct(nova, p_int_bh) %>%
  mutate(strip = paste0(
    "<b>", nova_labels[nova], "</b><br>",
    "<span style='font-size:6.5pt'><i>P</i><sub>interaction</sub> ", fmt_p_sci(p_int_bh), "</span>"
  ))
nova_strip <- set_names(nova_strip$strip, nova_strip$nova)

slopes_df <- slopes_df %>%
  mutate(nova_strip = factor(nova_strip[nova], levels = nova_strip[nova_ids]))

bmi_mean <- mean(df$bmi, na.rm = TRUE)
bmi_sd   <- sd(df$bmi, na.rm = TRUE)

x_title <- paste0(
  "β (95% CI) for BMI (kg/m<sup>2</sup> per 100 g/2,000 kcal/day)<br>",
  "<span style='font-size:6.5pt;color:#444444'>Mean ± SD BMI = ",
  sprintf("%.1f ± %.1f", bmi_mean, bmi_sd), " kg/m<sup>2</sup></span>"
)

# "N FFQ items" column header, one per panel
nfoods_head <- slopes_df %>% distinct(nova_strip) %>% mutate(label = "N FFQ items")

# x axis per panel: ticks run from a round number at or below the lowest CI
# (or 0) to one at or above the highest, so the axis covers every CI.
# geom_blank() at those ends sets each panel's range; the extra expansion on the
# left makes room for the N FFQ items column.
x_expand_l <- 0.45
x_expand_r <- 0.2

# Evenly spaced ticks at a "pretty" step, always including 0 and extending
# outwards to the first multiple of the step beyond lo and hi
panel_breaks <- slopes_df %>%
  group_by(nova_strip) %>%
  summarise(lo = min(lcl, 0), hi = max(ucl, 0), .groups = "drop") %>%
  mutate(step   = map2_dbl(lo, hi, ~ diff(pretty(c(.x, .y), n = 3))[1]),
         x_lo   = floor(lo / step + 1e-9) * step,
         x_hi   = ceiling(hi / step - 1e-9) * step,
         breaks = pmap(list(x_lo, x_hi, step), seq))

axis_ends <- panel_breaks %>%
  select(nova_strip, x_lo, x_hi) %>%
  pivot_longer(c(x_lo, x_hi), values_to = "x")

# ggplot passes each panel's *expanded* limits to the breaks function, so undo
# the expansion to recover the axis ends set above and return that panel's ticks.
data_breaks <- function(lims) {
  w   <- diff(lims) / (1 + x_expand_l + x_expand_r)
  rng <- c(lims[1] + x_expand_l * w, lims[2] - x_expand_r * w)
  i   <- which.min(abs(panel_breaks$x_lo - rng[1]) + abs(panel_breaks$x_hi - rng[2]))
  panel_breaks$breaks[[i]]
}


# ===== 4. Figure ==============================================================

fig4 <- ggplot(slopes_df, aes(x = beta, y = fct_rev(scenario))) +

  geom_blank(data = axis_ends, aes(x = x), inherit.aes = FALSE) +   # sets axis ends

  geom_vline(xintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.3) +

  geom_linerange(aes(xmin = lcl, xmax = ucl), linewidth = 0.35, colour = "black") +
  geom_point(size = 1.3, colour = "black") +

  # beta (95% CI) above each estimate, on white so the zero line doesn't cut through
  geom_label(aes(label = beta_ci), nudge_y = 0.32, size = 5.5 / .pt,
             family = base_font, colour = "grey15", fill = "white",
             linewidth = 0, label.padding = unit(0.5, "pt")) +

  # N FFQ items left-aligned at the left edge of each panel. geom_label (invisible
  # box) gives a fixed padding in pt, so 1-, 2- and 3-digit counts line up.
  geom_label(aes(x = -Inf, label = n_foods),
             hjust = -0.1, size = 6 / .pt,
             family = base_font, colour = nfood_col, fill = NA,
             linewidth = 0, label.padding = unit(3, "pt")) +

  geom_label(data = nfoods_head, aes(x = -Inf, y = Inf, label = label),
             inherit.aes = FALSE, hjust = 0, vjust = 1, size = 5.5 / .pt,
             family = base_font, fontface = "italic", colour = nfood_col, fill = NA,
             linewidth = 0, label.padding = unit(3, "pt")) +

  facet_wrap(~ nova_strip, nrow = 1, scales = "free_x") +
  coord_cartesian(clip = "off") +   # CI labels near the panel edge are not cut off

  scale_x_continuous(
    labels = label_number(style_negative = "minus", drop0trailing = TRUE),
    breaks = data_breaks,
    expand = expansion(mult = c(x_expand_l, x_expand_r))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.85))) +   # top room for labels/header
  guides(x = guide_axis(cap = "both")) +     # axis line spans the ticks only

  labs(y = NULL, x = x_title) +

  theme_minimal(base_size = 7, base_family = base_font) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.25),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.25),
    axis.ticks.length.x = unit(1.5, "pt"),
    axis.ticks.y = element_blank(),

    strip.text = element_markdown(size = 7, colour = "black", lineheight = 1.3,
                                  halign = 0.5, margin = margin(t = 3, b = 3)),
    strip.background = element_rect(fill = NA, colour = NA),

    panel.spacing.x = unit(15, "pt"),

    axis.text.x  = element_text(size = 6, colour = "black"),
    axis.text.y  = element_text(size = 7, hjust = 1, colour = "black",
                                margin = margin(r = 2)),
    axis.title.x = element_markdown(size = 7, lineheight = 1.4, halign = 0.5,
                                    margin = margin(t = 5)),

    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 3, r = 5, b = 3, l = 3)
  )

fig_w <- 183
fig_h <- 62

ggsave(file.path(fig_dir, "Figure4_bmi.pdf"), fig4,
       width = fig_w, height = fig_h, units = "mm", device = cairo_pdf)
ggsave(file.path(fig_dir, "Figure4_bmi.png"), fig4,
       width = fig_w, height = fig_h, units = "mm", dpi = 600,
       device = ragg::agg_png, bg = "white")


# ===== Source data: Figure 4 ===================================================
# Scenario-specific beta (95% CI) for BMI per 100 g of intake per 2,000 kcal per
# day, number of FFQ items, and BH-adjusted P for interaction.

source_fig4 <- slopes_df %>%
  arrange(nova, scenario) %>%
  transmute(
    nova_group  = unname(nova_labels[nova]),
    scenario    = as.character(scenario),
    n_ffq_items = n_foods,
    beta_bmi_per_100g_per_2000kcal = round(beta, 4),
    lower_95ci  = round(lcl, 4),
    upper_95ci  = round(ucl, 4),
    p_interaction_bh = signif(p_int_bh, 3),
    bmi_mean    = round(bmi_mean, 2),     # same for all rows: shown under the x-axis title
    bmi_sd      = round(bmi_sd, 2)
  )

openxlsx::write.xlsx(source_fig4, file.path(source_dir, "SourceData_Fig4.xlsx"), sheetName = "fig4", overwrite = TRUE)
