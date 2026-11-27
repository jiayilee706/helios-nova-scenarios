# HELIOS Nova scenarios - FIGURE 2
# Number and share of FFQ items in each Nova group, by food group and processing
# scenario (primary, low, high). Rows comprise all FFQ items followed by food
# groups nested within Foods (Grains, Plant-based, Animal-based, Mixed and other)
# and Beverages, sorted by % UPF in the primary scenario

library(tidyverse)
library(scales)
library(ggtext)
library(ggh4x)
library(ragg)
library(here)

load(here("data", "derived", "analysis_data.RData"))   # provides nova (FFQ items)

fig_dir    <- here("output", "figures")
source_dir <- here("output", "source_data")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(source_dir, showWarnings = FALSE, recursive = TRUE)

nova_colours4_G <- c(G1 = "#C7E9C0", G2 = "#FEEBC8", G3 = "#FDC086", G4 = "#E07A5F")
nova_labels     <- c(G1 = "Nova-1 MPF", G2 = "Nova-2 PCI", G3 = "Nova-3 PF", G4 = "Nova-4 UPF")

base_font <- "Arial"


# ===== 1. Food group sub-groups ===============================================

fg_grains <- c("Refined grains", "Whole grains", "Ethnic breads")
fg_plant  <- c("Vegetables", "Fruits", "Legumes", "Soy products")
fg_animal <- c("Dairy products", "Egg and egg dishes", "Fish and seafood", "Poultry", "Red meat")
fg_other  <- c("Mixed dishes or ingredients", "Soup", "Western fast food",   # meals
               "Gravies and sauces", "Spreads",                              # condiments
               "Snacks and desserts")
fg_bev    <- c("Coffee or tea", "Sugar added to drinks",                     # sugar sits with coffee/tea
               "Malted drink", "Powdered nutrition drink",                   # powdered drinks together
               "Fruit juice", "Sweet beverages")

fgorder <- c(fg_grains, fg_plant, fg_animal, fg_other, fg_bev)

# All food groups assigned to a sub-group
stopifnot(setequal(gsub("_", " ", levels(nova$FG_Generic)), fgorder))


# ===== 2. Counts and shares by food group, scenario and Nova group ============

# Long format: one row per FFQ item x scenario
nova_long <- nova %>%
  select(FG_Generic, nova_lo, nova_pri, nova_hi) %>%
  pivot_longer(
    cols = starts_with("nova_"),
    names_to = "scenario",
    values_to = "nova_group"
  ) %>%
  mutate(
    scenario = dplyr::recode(scenario,
                             nova_lo  = "Low processing scenario",
                             nova_pri = "Primary scenario",
                             nova_hi  = "High processing scenario") %>%
      factor(levels = c("Primary scenario",
                        "Low processing scenario",
                        "High processing scenario")),
    nova_group = factor(paste0("G", nova_group),
                        levels = c("G1", "G2", "G3", "G4")),
    FG_Generic = gsub("_", " ", as.character(FG_Generic))
  )

# Total row + food group rows.
# Strip labels: TOTAL gets blank labels in both strip layers, beverages a blank
# inner label. Blank labels use distinct whitespace so each stays its own level;
# their strips are hidden with element_blank() in the figure below.
nova_total_df <- nova_long %>%
  count(scenario, nova_group, name = "n") %>%
  mutate(FG_Generic = "TOTAL") %>%
  bind_rows(
    nova_long %>% count(FG_Generic, scenario, nova_group, name = "n")
  ) %>%
  group_by(FG_Generic, scenario) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    FG_Generic = factor(FG_Generic, levels = c("TOTAL", fgorder)),
    # outer strip
    type = case_when(
      FG_Generic == "TOTAL"  ~ " ",
      FG_Generic %in% fg_bev ~ "Beverages",
      TRUE                   ~ "Foods") %>%
      factor(levels = c(" ", "Foods", "Beverages")),
    # inner strip
    subgroup = case_when(
      FG_Generic == "TOTAL"     ~ " ",
      FG_Generic %in% fg_grains ~ "Grains",
      FG_Generic %in% fg_plant  ~ "Plant-based",
      FG_Generic %in% fg_animal ~ "Animal-based",
      FG_Generic %in% fg_other  ~ "Mixed and other",
      FG_Generic %in% fg_bev    ~ "  ") %>%
      factor(levels = c(" ", "Grains", "Plant-based", "Animal-based", "Mixed and other", "  "))
  )


# ===== 3. Sort food groups by % UPF within each sub-group =====================
# Highest UPF share at the top of each sub-group, based on the primary scenario;
# ties broken by % PF (Nova 3).

fgorder_upf <- nova_total_df %>%
  filter(scenario == "Primary scenario", FG_Generic != "TOTAL") %>%
  group_by(type, subgroup, FG_Generic) %>%
  summarise(
    upf = sum(prop[nova_group == "G4"]),
    pf  = sum(prop[nova_group == "G3"]),
    .groups = "drop"
  ) %>%
  arrange(type, subgroup, desc(upf), desc(pf)) %>%
  pull(FG_Generic) %>%
  as.character()

nova_total_df <- nova_total_df %>%
  mutate(FG_Generic = factor(as.character(FG_Generic), levels = c("TOTAL", fgorder_upf)))


# ===== 4. Per-strip styling ===================================================
# With by_layer_y = FALSE, ggh4x styles strips one by one: outer layer top to bottom
# (TOTAL, Foods, Beverages), then inner layer top to bottom
# (TOTAL, Grains, Plant-based, Animal-based, Mixed and other, Beverages).

strip_fill_outer <- "#F5EFE6"
strip_fill_inner <- "#F7F7F7"

outer_bg  <- element_rect(fill = strip_fill_outer, colour = NA)
inner_bg  <- element_rect(fill = strip_fill_inner, colour = NA)
outer_txt <- element_text(colour = "grey10", face = "bold", size = 7.5, angle = 90,
                          margin = margin(l = 3, r = 3))
inner_txt <- element_text(colour = "grey30", size = 6.5, angle = 90,
                          margin = margin(l = 3, r = 3))

strip_bg_y  <- list(element_blank(), outer_bg, outer_bg,                    # outer: TOTAL, Foods, Beverages
                    element_blank(), inner_bg, inner_bg, inner_bg, inner_bg, # inner: TOTAL, 4 food sub-groups
                    element_blank())                                        # inner: Beverages
strip_txt_y <- list(element_blank(), outer_txt, outer_txt,
                    element_blank(), inner_txt, inner_txt, inner_txt, inner_txt,
                    element_blank())


# ===== 5. Figure ==============================================================

fgscenarios <- ggplot(nova_total_df, aes(y = fct_rev(FG_Generic), x = prop, fill = nova_group)) +
  geom_col(width = 0.8, colour = "white", linewidth = 0.2) +

  facet_nested(
    type + subgroup ~ scenario,
    scales = "free_y", space = "free_y", switch = "y",
    strip = strip_nested(
      by_layer_y   = FALSE,
      background_y = strip_bg_y,
      text_y       = strip_txt_y
    )
  ) +

  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    expand = expansion(mult = c(0, 0.01))
  ) +

  scale_y_discrete(
    labels = function(x) {
      x_clean <- str_trim(as.character(x))
      if_else(x_clean == "TOTAL", "**All FFQ items**", x_clean)
    }
  ) +

  scale_fill_manual(
    values = nova_colours4_G,
    labels = nova_labels,
    breaks = c("G4", "G3", "G2", "G1"),   # match left-to-right stacking order
    guide = guide_legend(
      label.position = "bottom",
      nrow = 1,
      byrow = TRUE
    )
  ) +
  labs(
    x = "Share of FFQ items in food group",
    y = NULL,
    fill = "Nova group"
  ) +
  theme(
    text = element_text(family = base_font),

    panel.background = element_blank(),
    plot.background  = element_blank(),
    strip.background.x = element_blank(),
    strip.placement = "outside",
    strip.switch.pad.grid = unit(3, "pt"),   # gap between strips and y-axis labels
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "pt"),
    axis.ticks.x = element_line(colour = "grey20", linewidth = 0.25),
    axis.ticks.length.x = unit(2, "pt"),

    strip.text.x = element_text(colour = "grey10", face = "bold", size = 7.5,
                                margin = margin(b = 4)),
    axis.title.x = element_text(margin = margin(t = 5), size = 7),
    axis.text.x  = element_text(size = 6, colour = "grey20"),
    axis.text.y  = element_markdown(
      margin = margin(l = 4, r = 4),         # gap between y-axis labels and bars
      colour = "grey10", size = 7),

    panel.grid = element_blank(),
    panel.spacing.x = unit(16, "pt"),   # avoid 100%/0% label collision
    panel.spacing.y = unit(4, "pt"),    # small gap between sub-groups

    legend.title = element_text(margin = margin(b = 7, r = 6), size = 7.5),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width  = unit(1.1, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.key.spacing.x = unit(0.35, "cm"),
    legend.text = element_text(size = 7),
    plot.margin = margin(t = 3, r = 7, b = 3, l = 3)
  )


# ===== 6. Add counts ==========================================================
# Count of FFQ items in the middle of each segment; hidden for segments < 6%.

fg_counts <- nova_total_df %>%
  group_by(FG_Generic, scenario) %>%
  arrange(desc(nova_group), .by_group = TRUE) %>%
  mutate(
    cum_prop = cumsum(prop),
    mid_prop = cum_prop - prop / 2
  ) %>%
  ungroup()

fig2 <- fgscenarios +
  geom_text(
    data = fg_counts,
    aes(
      y = fct_rev(FG_Generic),
      x = mid_prop,
      label = ifelse(prop < 0.06, "", n),
      fontface = ifelse(FG_Generic == "TOTAL", "bold", "plain")
    ),
    size = 5.5 / .pt,
    family = base_font,
    colour = "grey15",
    inherit.aes = FALSE
  )

fig_w <- 183
fig_h <- 153

ggsave(file.path(fig_dir, "Figure2_items_by_scenario.pdf"), fig2,
       width = fig_w, height = fig_h, units = "mm", device = cairo_pdf, bg = "white")
ggsave(file.path(fig_dir, "Figure2_items_by_scenario.png"), fig2,
       width = fig_w, height = fig_h, units = "mm", dpi = 600,
       device = ragg::agg_png, bg = "white")


# ===== Source data: Figure 2 ===================================================
# One row per food group x scenario x Nova group: number and share of FFQ items.

source_fig2 <- nova_total_df %>%
  # include Nova groups with no items in a food group/scenario as explicit zeros
  complete(nesting(FG_Generic, type, subgroup), scenario, nova_group,
           fill = list(n = 0L, prop = 0)) %>%
  arrange(FG_Generic, scenario, nova_group) %>%
  transmute(
    ffq_group       = if_else(FG_Generic == "TOTAL", "All FFQ items", as.character(FG_Generic)),
    category        = na_if(str_trim(as.character(type)), ""),       # Foods / Beverages
    subcategory     = na_if(str_trim(as.character(subgroup)), ""),   # Grains, Plant-based, ...
    scenario        = as.character(scenario),
    nova_group      = unname(nova_labels[as.character(nova_group)]),
    n_ffq_items     = n,
    share_ffq_items = round(prop, 4)
  )

openxlsx::write.xlsx(source_fig2, file.path(source_dir, "SourceData_Fig2.xlsx"), sheetName = "Fig2", overwrite = TRUE)
