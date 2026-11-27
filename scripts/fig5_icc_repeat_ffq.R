# HELIOS Nova scenarios - FIGURE 5
# Reproducibility of Nova group intakes (g per 2,000 kcal per day, primary scenario)
# between baseline and repeat FFQ: ICC(3,1) with 95% CI, overall and by ethnicity.
# Rows: the four Nova groups, then UPF intake from each food group.
#
# Food group rows use the same labels, sub-groups and order as Figure 2: sorted by
# % UPF within each sub-group, primary scenario. Food groups with no UPF items are
# not shown (listed in the console output, for the caption). "Processed animal
# products" is not a Figure 2 food group; it is placed at the top of Animal-based.

library(tidyverse)
library(psych)
library(ggh4x)   # facet_nested()
library(ggtext)
library(ragg)
library(here)

load(here("data", "derived", "analysis_data.RData"))   # provides df_f2 and nova

fig_dir    <- here("output", "figures")
source_dir <- here("output", "source_data")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(source_dir, showWarnings = FALSE, recursive = TRUE)

base_font <- "Arial"

# Overall, Chinese, Indian, Malay (same colours as Figure 3)
ethnic_colours <- c(All = "#2C1A12", CHINESE = "#E69F00", INDIAN = "#9B5E82", MALAY = "#009E73")

nova_total_vars <- c(
  "Nova-1 MPF" = "all_nova1pri_g_p2k",
  "Nova-2 PCI" = "all_nova2pri_g_p2k",
  "Nova-3 PF"  = "all_nova3pri_g_p2k",
  "Nova-4 UPF" = "all_nova4pri_g_p2k"
)

# Food groups with UPF items in the primary scenario
# (fish, poultry and red meat as processed animal products)
upf_fg <- nova %>% filter(nova_pri == 4) %>% pull(FG_Generic) %>% as.character() %>% unique()
upf_fg <- unique(if_else(upf_fg %in% c("Fish_and_seafood", "Poultry", "Red_meat"),
                         "Processed_animal_products", upf_fg))

# Display labels: UPF subtype names from the classification file, by food group
upf_labels <- nova %>%
  filter(nova_pri == 4) %>%
  distinct(fg = gsub("_", " ", as.character(FG_Generic)), FG_UPF)
upf_labels <- set_names(upf_labels$FG_UPF, upf_labels$fg)
upf_labels["Coffee or tea"] <- "RTD/speciality coffee and tea"   # 20260909 rename


# ===== 1. Food group order and sub-groups, as in Figure 2 =====================

fg_grains <- c("Refined grains", "Whole grains", "Ethnic breads")
fg_plant  <- c("Vegetables", "Fruits", "Legumes", "Soy products")
fg_animal <- c("Processed animal products",   # not in Figure 2; top of Animal-based
               "Dairy products", "Egg and egg dishes", "Fish and seafood", "Poultry", "Red meat")
fg_other  <- c("Mixed dishes or ingredients", "Soup", "Western fast food",
               "Gravies and sauces", "Spreads", "Snacks and desserts")
fg_bev    <- c("Coffee or tea", "Sugar added to drinks", "Malted drink",
               "Powdered nutrition drink", "Fruit juice", "Sweet beverages")

fg_subgroup <- c(
  set_names(rep("Grains", length(fg_grains)), fg_grains),
  set_names(rep("Plant-based", length(fg_plant)), fg_plant),
  set_names(rep("Animal-based", length(fg_animal)), fg_animal),
  set_names(rep("Mixed and other", length(fg_other)), fg_other),
  set_names(rep("Beverages", length(fg_bev)), fg_bev)
)
subgroup_levels <- c("Grains", "Plant-based", "Animal-based", "Mixed and other", "Beverages")

# Figure 2 order: % UPF (then % PF) of FFQ items, primary scenario, within sub-group
fg_order <- nova %>%
  transmute(fg = gsub("_", " ", as.character(FG_Generic)), nova_pri) %>%
  group_by(fg) %>%
  summarise(upf = mean(nova_pri == 4), pf = mean(nova_pri == 3), .groups = "drop") %>%
  mutate(subgroup = factor(fg_subgroup[fg], levels = subgroup_levels)) %>%
  arrange(subgroup, desc(upf), desc(pf)) %>%
  pull(fg)

# Insert Processed animal products at the top of Animal-based
fg_order <- append(fg_order, "Processed animal products",
                   after = max(which(fg_subgroup[fg_order] %in% c("Grains", "Plant-based"))))


# ===== 2. ICC between baseline and repeat FFQ =================================
# ICC(3,1) (95% CI) between baseline and repeat FFQ, g per 2,000 kcal per day,
# overall and within each ethnic group.

measures <- tibble(
  var   = c(unname(nova_total_vars), paste0(upf_fg, "_nova4pri_g_p2k")),
  label = c(names(nova_total_vars), gsub("_", " ", upf_fg))
)

strata <- c("All", levels(df_f2$Ethnicity))

icc_df <- NULL

for (i in seq_len(nrow(measures)))
  for (s in strata) {

    d <- if (s == "All") df_f2 else df_f2[df_f2$Ethnicity == s, ]
    x <- d[[measures$var[i]]]
    y <- d[[paste0("f2_", measures$var[i])]]
    keep <- complete.cases(x, y)
    x <- x[keep]; y <- y[keep]

    # Not estimable if no variation (e.g. no one in the stratum consumed the food group)
    if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) {
      res <- tibble(ICC3 = NA_real_, LCL = NA_real_, UCL = NA_real_)
    } else {
      icc <- psych::ICC(cbind(x, y))$results
      res <- tibble(ICC3 = icc$ICC[3], LCL = icc$`lower bound`[3], UCL = icc$`upper bound`[3])
    }

    icc_df <- bind_rows(icc_df, tibble(measures[i, ], Subgroup = s, res, N = length(x)))
  }

# Food groups from Figure 2 without UPF items (for the caption)
cat("\nFigure 2 food groups not shown (no UPF items in the primary scenario):\n  ",
    paste(setdiff(names(fg_subgroup), gsub("_", " ", upf_fg)), collapse = ", "), "\n")

dropped <- unique(icc_df$label[is.na(icc_df$ICC3)])
if (length(dropped) > 0) cat("ICC not estimable (dropped):", paste(dropped, collapse = ", "), "\n")


# ===== 3. Layout: Nova groups, then UPF subtypes ==============================
# Bold section headings in the label column ("Nova groups", "Nova-4 UPF by subtype") with rows indented under them.
# Each heading sits in its own one-row panel; food sub-groups get strips as in Figure 2.

band_fill  <- "#F5EFE6"   # alternating row bands (as Figure 2)
strip_fill <- "#F7F7F7"   # food sub-group strips (as Figure 2)

head_nova <- "Nova groups"
head_upf  <- "Nova-4 UPF by subtype"

n_by_group <- icc_df %>% group_by(Subgroup) %>% summarise(N = max(N), .groups = "drop")
legend_labels <- set_names(paste0(str_to_title(n_by_group$Subgroup), " (N = ", n_by_group$N, ")"),
                           n_by_group$Subgroup)

# Panels: each heading is a one-row panel; "  " = the four Nova group rows, no strip
panel_levels <- c(head_nova, "  ", head_upf, subgroup_levels)
row_levels   <- c(head_nova, names(nova_total_vars), head_upf, fg_order)

plot_df <- icc_df %>%
  filter(!is.na(ICC3)) %>%
  mutate(
    panel = if_else(label %in% names(nova_total_vars), "  ", fg_subgroup[label]),
    # dodge order: All on top, then Chinese, Indian, Malay
    Subgroup = factor(Subgroup, levels = rev(names(ethnic_colours)))
  )
heading_rows <- tibble(label = c(head_nova, head_upf), panel = c(head_nova, head_upf))

keep_levels <- row_levels[row_levels %in% c(plot_df$label, heading_rows$label)]

plot_df <- plot_df %>%
  mutate(panel = factor(panel, levels = panel_levels), label = factor(label, levels = keep_levels))
heading_rows <- heading_rows %>%
  mutate(panel = factor(panel, levels = panel_levels), label = factor(label, levels = keep_levels))

panels <- bind_rows(distinct(plot_df, panel), distinct(heading_rows, panel)) %>%
  distinct() %>% arrange(panel) %>% pull(panel)

# Labels: heading rows left empty (the heading is drawn separately, section 6); the other
# rows are indented a little (invisible text, as ggtext collapses spaces and &nbsp;)
indent   <- "<span style='color:transparent'>M</span>"
label_md <- function(x) if_else(x %in% c(head_nova, head_upf), "",
                                paste0(indent, coalesce(unname(upf_labels[x]), x)))

# Row bands: alternate from the top across panels
band_df <- plot_df %>%
  distinct(panel, label) %>%
  arrange(label) %>%
  mutate(band = row_number() %% 2 == 1) %>%
  group_by(panel) %>%
  mutate(pos = n() - row_number() + 1) %>%    # y position within panel (fct_rev)
  ungroup() %>%
  filter(band)

# Grid and zero lines only in panels with estimates (not the heading panels)
vline_df <- plot_df %>% distinct(panel) %>%
  expand_grid(x = seq(-0.25, 1, 0.25)) %>%
  mutate(zero = x == 0)


# ===== 4. Strips and panel gaps ===============================================
# by_layer_y = FALSE: strips styled one by one, top to bottom.
#  - heading panels and Nova group rows: no strip
#  - food sub-groups: grey strip, rotated text (as Figure 2)

strip_txt <- element_text(colour = "grey30", size = 6.5, angle = 90,
                          margin = margin(l = 3, r = 3))

is_subgroup <- as.character(panels) %in% subgroup_levels
strip_bg_y  <- map(is_subgroup, ~ if (.x) element_rect(fill = strip_fill, colour = NA) else element_blank())
strip_txt_y <- map(is_subgroup, ~ if (.x) strip_txt else element_blank())

# Gaps: none between a heading and its rows, wide after the Nova groups
panel_gaps <- case_when(
  as.character(panels[-length(panels)]) == "  " ~ 12,                   # Nova group rows -> UPF heading
  as.character(panels[-length(panels)]) %in% c(head_nova, head_upf) ~ 0, # heading -> its rows
  TRUE ~ 4
)
panel_gaps <- unit(panel_gaps, "pt")


# ===== 5. Figure ==============================================================

dodge <- position_dodge(width = 0.93, orientation = "y")

fig5 <- ggplot(plot_df, aes(x = ICC3, y = fct_rev(label))) +

  geom_blank(data = heading_rows, aes(x = 0)) +   # heading rows (no estimates)

  geom_rect(data = band_df,
            aes(xmin = -Inf, xmax = Inf, ymin = pos - 0.5, ymax = pos + 0.5),
            inherit.aes = FALSE, fill = band_fill, alpha = 0.7) +

  geom_vline(data = filter(vline_df, !zero), aes(xintercept = x),
             colour = "grey88", linewidth = 0.25) +
  geom_vline(data = filter(vline_df, zero), aes(xintercept = x),
             colour = "grey55", linetype = "dashed", linewidth = 0.3) +

  geom_linerange(aes(xmin = LCL, xmax = UCL, group = Subgroup),
                 colour = "grey40", linewidth = 0.3, position = dodge) +
  geom_point(aes(fill = Subgroup), shape = 21, colour = "black", size = 1.3,
             stroke = 0.25, position = dodge) +

  facet_nested(
    panel ~ .,
    scales = "free_y", space = "free_y", switch = "y",
    strip = strip_nested(by_layer_y = FALSE,
                         background_y = strip_bg_y, text_y = strip_txt_y)
  ) +

  scale_x_continuous(
    limits = c(-0.25, 1), breaks = seq(-0.25, 1, 0.25),
    labels = scales::label_number(style_negative = "minus", drop0trailing = TRUE),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_discrete(labels = label_md, expand = expansion(add = 0.5)) +
  scale_fill_manual(values = ethnic_colours, breaks = names(ethnic_colours),
                    labels = legend_labels) +

  labs(x = "Intraclass correlation coefficient (95% CI)\nIntakes in g/2,000 kcal/day",
       y = NULL, fill = NULL) +

  theme_minimal(base_size = 7, base_family = base_font) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),

    strip.placement = "outside",
    strip.switch.pad.grid = unit(3, "pt"),
    strip.clip = "off",                  # short panels: don't cut long strip labels
    panel.spacing.y = panel_gaps,

    axis.text.x  = element_text(size = 6, colour = "black"),
    # set on .left: theme_minimal() defines axis.text.y.left, which would override
    axis.text.y.left = element_markdown(size = 7, colour = "black", hjust = 0,
                                        margin = margin(r = 3)),
    axis.title.x = element_text(size = 7, margin = margin(t = 4)),

    legend.position = "top",
    legend.location = "plot",            # centre over the whole figure, not the panel
    legend.text = element_text(size = 6.5, margin = margin(l = 1)),
    legend.key.size = unit(7, "pt"),
    legend.key.spacing.x = unit(5, "pt"),
    legend.margin = margin(b = 0),

    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 3, r = 6, b = 3, l = 3)
  )


# ===== 6. Add section headings and save =========================================
# Headings are drawn on the finished layout, in each heading panel's row,
# starting at the left edge of the strip column (same x as the Grains/Beverages
# strips). The layout is built inside the output device so that text is
# measured with the font it is drawn in.

draw_fig5 <- function() {
  g <- ggplotGrob(fig5)

  panel_cells <- g$layout %>% filter(grepl("^panel", name)) %>% arrange(t)
  stopifnot(nrow(panel_cells) == length(panels))

  strip_cols <- g$layout %>% filter(grepl("strip", name), l < min(panel_cells$l))
  left_col <- if (nrow(strip_cols) > 0) min(strip_cols$l) else min(panel_cells$l) - 1

  for (h in c(head_nova, head_upf)) {
    cell <- panel_cells[which(as.character(panels) == h), ]
    g <- gtable::gtable_add_grob(
      g,
      grid::textGrob(h, x = unit(0, "npc"), hjust = 0,
                     gp = grid::gpar(fontfamily = base_font, fontface = "bold",
                                     fontsize = 7, col = "black")),
      t = cell$t, b = cell$b, l = left_col, r = cell$l - 1,
      clip = "off", name = paste0("heading-", h)
    )
  }
  grid::grid.draw(g)
}

fig_w <- 120
fig_h <- 190

cairo_pdf(file.path(fig_dir, "Figure5_icc_repeat_ffq.pdf"), width = fig_w / 25.4, height = fig_h / 25.4)
draw_fig5()
invisible(dev.off())

ragg::agg_png(file.path(fig_dir, "Figure5_icc_repeat_ffq.png"), width = fig_w, height = fig_h, units = "mm",
              res = 600, background = "white")
draw_fig5()
invisible(dev.off())


# ===== Source data: figure 5 ===================================================
# ICC(3,1) (95% CI) between baseline and repeat FFQ, g per 2,000 kcal per day,
# by ethnic group, in the row order of the figure.
# ffq_group = Figure 2 food group; label = label shown.

source_fig5 <- icc_df %>%
  filter(!is.na(ICC3)) %>%
  mutate(
    is_total = label %in% names(nova_total_vars),
    row      = factor(label, levels = c(names(nova_total_vars), fg_order)),
    Subgroup = factor(Subgroup, levels = names(ethnic_colours))
  ) %>%
  arrange(row, Subgroup) %>%
  transmute(
    section     = if_else(is_total, head_nova, head_upf),
    subcategory = if_else(is_total, NA_character_, unname(fg_subgroup[label])),
    ffq_group   = if_else(is_total, NA_character_, label),
    label       = if_else(is_total, label, coalesce(unname(upf_labels[label]), label)),
    ethnicity   = str_to_title(as.character(Subgroup)),
    n           = N,
    icc         = round(ICC3, 4),
    lower_95ci  = round(LCL, 4),
    upper_95ci  = round(UCL, 4)
  )

openxlsx::write.xlsx(source_fig5, file.path(source_dir, "SourceData_Fig5.xlsx"), sheetName = "fig5", overwrite = TRUE)

# ===== Source data: figure 5 WIDE ===================================================

# Convert source data to wide format
source_fig5_wide <- source_fig5 %>%
  mutate(icc = sprintf("%.2f (%.2f, %.2f)", icc, lower_95ci, upper_95ci)) %>%
  select(-n, -lower_95ci, -upper_95ci) %>%
  pivot_wider(names_from = ethnicity, values_from = icc) %>%
  select(label, All, Chinese, Indian, Malay)

openxlsx::write.xlsx(source_fig5_wide, file.path(source_dir, "SourceData_Fig5_wide.xlsx"), sheetName = "fig5_wide", overwrite = TRUE)

