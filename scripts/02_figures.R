library(tidyverse)
library(reshape2)

chr <- readRDS("data/chr_clean.rds") |>
  filter(!is.na(life_expectancy))

heatmap_vars <- chr |>
  select(
    `Life\nExpectancy` = life_expectancy,
    `Median\nIncome` = median_income,
    `Adult\nSmoking` = adult_smoking,
    `Excessive\nDrinking` = excessive_drinking,
    `Teen\nBirths` = teen_births,
    `Food\nInsecurity` = food_insecurity
  ) |>
  drop_na()

cor_matrix <- cor(heatmap_vars, use = "pairwise.complete.obs")

var_order <- c("Life\nExpectancy", "Median\nIncome",
               "Excessive\nDrinking", "Adult\nSmoking",
               "Teen\nBirths", "Food\nInsecurity")
cor_matrix <- cor_matrix[var_order, var_order]

cor_long <- melt(cor_matrix) |>
  rename(x = Var1, y = Var2, corr = value)

cor_long$x <- factor(cor_long$x, levels = var_order)
cor_long$y <- factor(cor_long$y, levels = var_order)

fig5 <- ggplot(cor_long, aes(x = x, y = y, fill = corr)) +
  geom_tile(color = NA) +
  geom_text(aes(label = round(corr, 2)),
            size = 7, color = "black", fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#1565C0", "#FFFFFF", "#F4A100"),
    values = scales::rescale(c(-1, 0, 1)),
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),
    guide = guide_colorbar(
      title = "Correlation (r)",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 2,
      barheight = 10,
      ticks = TRUE,
      ticks.colour = "black",
      ticks.linewidth = 0.8,
      frame.colour = "black",
      frame.linewidth = 0.8,
      nbin = 200
    )
  ) +
  annotate("rect",
           xmin = 0.5, xmax = 6.5,
           ymin = 0.5, ymax = 6.5,
           fill = NA, color = "black", linewidth = 0.5) +
  scale_x_discrete(position = "bottom") +
  scale_y_discrete() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 14, color = "black",
      lineheight = 0.8, vjust = 1,
      face = "bold",
      margin = margin(t = 6)
    ),
    axis.text.y = element_text(
      size = 16, color = "black",
      lineheight = 0.8, hjust = 1,
      face = "bold",
      margin = margin(r = 6)
    ),
    axis.title = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.18, "cm"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    legend.margin = margin(t = 30),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave("figures/fig5_heatmap.png", fig5,
       width = 10, height = 9,
       dpi = 600,
       units = "in")






# Fig 4(violin plot sleep vs insurance)

make_split_violin <- function(data, group_var,
                               low_label, high_label, title_text) {

  low_data  <- data |> filter(.data[[group_var]] == "Low")  |> mutate(side = "Low")
  high_data <- data |> filter(.data[[group_var]] == "High") |> mutate(side = "High")
  combined  <- bind_rows(low_data, high_data) |>
    mutate(side = factor(side, levels = c("Low", "High")))

  ggplot(combined, aes(x = side, y = life_expectancy, fill = side)) +
    geom_violin(trim = TRUE, alpha = 0.85, color = NA,
                scale = "width") +
    geom_boxplot(width = 0.08, fill = "white",
                 color = "black", outlier.shape = NA,
                 linewidth = 0.6) +
    stat_summary(fun = median, geom = "text",
                 aes(label = paste0(round(after_stat(y), 1), " yrs")),
                 position = position_nudge(x = 0.18),
                 size = 4.2, fontface = "bold", color = "black") +
    scale_fill_manual(values = c("Low" = better_color,
                                  "High" = worse_color)) +
    scale_x_discrete(labels = c("Low" = low_label,
                                 "High" = high_label)) +
    scale_y_continuous(limits = c(65, 88),
                       breaks = seq(65, 88, 5)) +
    labs(title = title_text,
         y = "Life Expectancy (Years)",
         x = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold",
                                hjust = 0.5, color = "black"),
      axis.text.x = element_text(size = 13, face = "bold",
                                  color = "black", lineheight = 0.85),
      axis.text.y = element_text(size = 13, face = "bold",
                                  color = "black"),
      axis.title.y = element_text(size = 14, face = "bold",
                                   color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey85",
                                         linewidth = 0.4),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      plot.margin = margin(10, 40, 10, 40)
    )
}

p_sleep <- make_split_violin(
  chr_violin,
  group_var  = "sleep_group",
  low_label  = "Low Sleep\nDeprivation",
  high_label = "High Sleep\nDeprivation",
  title_text = "Sleep Deprivation"
)

p_insured <- make_split_violin(
  chr_violin,
  group_var  = "insured_group",
  low_label  = "Low\nUninsured Rate",
  high_label = "High\nUninsured Rate",
  title_text = "Uninsured Rate"
)

fig4 <- p_sleep / p_insured +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 10, 15, 10)
    )
  )

ggsave("figures/fig4_violin.png", fig4,
       width = 7, height = 11,
       dpi = 600,
       units = "in")

# ============================================
# FIGURE 5b — Regression Lollipop Chart
# ============================================

library(broom)
library(tidyverse)

chr <- readRDS("data/chr_clean.rds") |>
  filter(!is.na(life_expectancy))

model <- lm(life_expectancy ~
              median_income +
              adult_smoking +
              excessive_drinking +
              teen_births +
              food_insecurity,
            data = chr)

coef_raw <- tidy(model, conf.int = TRUE) |>
  filter(term != "(Intercept)")

coef_df <- coef_raw |>
  mutate(
    multiplier = case_when(
      term == "median_income"      ~ 10000,
      term == "adult_smoking"      ~ 0.10,
      term == "excessive_drinking" ~ 0.04,
      term == "teen_births"        ~ 10,
      term == "food_insecurity"    ~ 0.08
    ),
    estimate_scaled  = estimate  * multiplier,
    conf.low_scaled  = conf.low  * multiplier,
    conf.high_scaled = conf.high * multiplier,
    label = case_when(
      term == "median_income"      ~ "Median Income\n(+$10,000)",
      term == "adult_smoking"      ~ "Adult Smoking\n(+10% prevalence)",
      term == "excessive_drinking" ~ "Excessive Drinking\n(+4% prevalence)",
      term == "teen_births"        ~ "Teen Births\n(+10 per 1,000)",
      term == "food_insecurity"    ~ "Food Insecurity\n(+8% prevalence)"
    ),
    direction = ifelse(estimate_scaled > 0, "Beneficial", "Harmful")
  ) |>
  arrange(estimate_scaled)

coef_df$label <- factor(coef_df$label, levels = coef_df$label)

fig5b <- ggplot(coef_df,
                aes(x = estimate_scaled, y = label, color = direction)) +

  # Background zones
  annotate("rect",
           xmin = -Inf, xmax = 0,
           ymin = -Inf, ymax = Inf,
           fill = "#EF476F", alpha = 0.11) +
  annotate("rect",
           xmin = 0, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "#4CAF50", alpha = 0.11) +

  # Zero reference line
  geom_vline(xintercept = 0, color = "black", linewidth = 0.7) +

  # Confidence interval lines
  geom_segment(aes(x = conf.low_scaled, xend = conf.high_scaled,
                   y = label, yend = label),
               linewidth = 0.6, alpha = 0.45) +

  # Lollipop stem
  geom_segment(aes(x = 0, xend = estimate_scaled,
                   y = label, yend = label),
               linewidth = 1.5) +

  # Lollipop head
  geom_point(size = 6) +

  # Effect size labels
  geom_text(aes(label = paste0(ifelse(estimate_scaled > 0, "+", ""),
                               round(estimate_scaled, 2), " yrs")),
            nudge_y = 0.38,
            size = 5, fontface = "bold", color = "black") +

  scale_color_manual(
    values = c("Beneficial" = "#4CAF50", "Harmful" = "#EF476F")
  ) +

  # Expand x-axis right side so +0.83 yrs label doesn't clip
  scale_x_continuous(
    breaks = seq(-2.5, 1.0, 0.5),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x),
    expand = expansion(mult = c(0.03, 0.07))
  ) +

  labs(
    x = "Independent Effect on Life Expectancy (Years)",
    y = NULL
  ) +

  theme_minimal(base_size = 14) +
  theme(
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.y      = element_text(size = 13, face = "bold",
                                    color = "black", lineheight = 0.85),
    axis.text.x      = element_text(size = 12, color = "black"),
    axis.title.x     = element_text(size = 13, face = "bold",
                                    color = "black", margin = margin(t = 9)),
    axis.ticks       = element_line(color = "black", linewidth = 0.4),
    axis.line        = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey82", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "none",
    plot.margin      = margin(18, 28, 18, 18)
  )

ggsave("figures/fig5b_lollipop.png", fig5b,
       width = 8, height = 5.5,
       dpi = 600,
       units = "in")

message("Figure 5b saved to figures/fig5b_lollipop.png")


# ============================================
# FIGURE 3 — Outlier Scatter Plot
# ============================================

library(tidyverse)
library(ggrepel)

chr <- readRDS("data/chr_clean.rds") |>
  filter(!is.na(life_expectancy))

model <- lm(life_expectancy ~
              median_income +
              adult_smoking +
              excessive_drinking +
              teen_births +
              food_insecurity,
            data = chr)

chr <- chr |>
  mutate(
    predicted_le = predict(model, newdata = chr),
    residual     = life_expectancy - predicted_le
  )

resort_counties <- c(
  "Pitkin County_CO", "Summit County_CO",
  "Eagle County_CO",  "Mono County_CA"
)

reservation_counties <- c(
  "Buffalo County_SD", "Dewey County_SD",
  "Roosevelt County_MT", "Menominee County_WI",
  "Todd County_SD"
)

mystery_counties <- c("Presidio County_TX")

chr <- chr |>
  mutate(
    county_key  = paste0(county, "_", state),
    county_type = case_when(
      county_key %in% resort_counties      ~ "Above",
      county_key %in% reservation_counties ~ "Below",
      county_key %in% mystery_counties     ~ "Mystery",
      TRUE                                  ~ "Other"
    )
  )

label_counties <- c(resort_counties, reservation_counties, mystery_counties)

chr <- chr |>
  mutate(
    label_name = ifelse(county_key %in% label_counties,
                        paste0(sub(" County", "", county), ", ", state),
                        NA_character_)
  )

chr <- chr |>
  arrange(desc(county_type == "Other"),
          desc(county_type == "Below"),
          desc(county_type == "Above"),
          desc(county_type == "Mystery"))

axis_limits <- c(53, 97)
axis_breaks <- seq(55, 95, 5)

type_colors <- c(
  "Above"   = "#F4A100",
  "Below"   = "#43A047",
  "Mystery" = "#9C5FC5",
  "Other"   = "#CCCCCC"
)

type_sizes <- c(
  "Above"   = 4.0,
  "Below"   = 4.0,
  "Mystery" = 4.5,
  "Other"   = 1.2
)

type_alpha <- c(
  "Above"   = 1,
  "Below"   = 1,
  "Mystery" = 1,
  "Other"   = 0.45
)

fig3 <- ggplot(chr, aes(x = predicted_le, y = life_expectancy)) +

  geom_abline(slope = 1, intercept = 0,
              color = "black", linewidth = 0.8, linetype = "dashed") +

  geom_point(aes(color = county_type,
                 size  = county_type,
                 alpha = county_type)) +

  geom_label_repel(
    data               = chr |> filter(!is.na(label_name)),
    aes(label = label_name, color = county_type),
    size               = 4.2,
    fontface           = "bold",
    fill               = "white",
    label.size         = 0.25,
    label.padding      = unit(0.25, "lines"),
    box.padding        = unit(0.6,  "lines"),
    point.padding      = unit(0.5,  "lines"),
    max.overlaps       = 30,
    min.segment.length = 0.2,
    show.legend        = FALSE
  ) +

  scale_color_manual(
    values = type_colors,
    breaks = c("Above", "Below", "Mystery"),
    labels = c("Significantly Above Prediction",
               "Significantly Below Prediction",
               "Unexplained Outlier")
  ) +
  scale_size_manual(values = type_sizes, guide = "none") +
  scale_alpha_manual(values = type_alpha, guide = "none") +

  scale_x_continuous(breaks = axis_breaks, limits = axis_limits) +
  scale_y_continuous(breaks = axis_breaks, limits = axis_limits) +

  coord_fixed(ratio = 1) +

  labs(
    x     = "Predicted Life Expectancy (Years)",
    y     = "Actual Life Expectancy (Years)",
    color = NULL
  ) +

  theme_minimal(base_size = 14) +
  theme(
    panel.border       = element_rect(color = "black", fill = NA,
                                      linewidth = 0.8),
    axis.text          = element_text(size = 12, color = "black"),
    axis.title         = element_text(size = 13, face = "bold",
                                      color = "black"),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.line          = element_blank(),
    panel.grid.major   = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    legend.position    = "right",
    legend.direction   = "vertical",
    legend.background  = element_rect(fill = "white", color = "grey75",
                                      linewidth = 0.4),
    legend.text        = element_text(size = 10.5, face = "bold",
                                      color = "black"),
    legend.key.size    = unit(0.55, "cm"),
    legend.spacing.y   = unit(0.1, "cm"),
    legend.margin      = margin(6, 8, 6, 8),
    plot.margin        = margin(18, 10, 18, 18)
  ) +

  guides(color = guide_legend(override.aes = list(size = 4.5)))

ggsave("figures/fig3_scatter.png", fig3,
       width = 8.5, height = 7,
       dpi = 600,
       units = "in")

message("Figure 3 saved to figures/fig3_scatter.png")

# ============================================
# FIGURE 2 — Regional Bar Chart (Faceted)
# ============================================

library(tidyverse)
library(scales)

chr <- readRDS("data/chr_clean.rds") |>
  filter(!is.na(life_expectancy))

chr <- chr |>
  mutate(
    high_smoking  = adult_smoking      > median(adult_smoking,      na.rm = TRUE),
    high_drinking = excessive_drinking > median(excessive_drinking, na.rm = TRUE),
    beh_group = case_when(
      high_smoking  & !high_drinking ~ "Smoke-Dominant",
      !high_smoking &  high_drinking ~ "Drink-Dominant",
      high_smoking  &  high_drinking ~ "Both High",
      !high_smoking & !high_drinking ~ "Both Low"
    ),
    region = case_when(
      state %in% c("ME","NH","VT","MA","RI","CT","NY","NJ","PA") ~ "Northeast",
      state %in% c("OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS") ~ "Midwest",
      state %in% c("DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX") ~ "South",
      state %in% c("MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI") ~ "West",
      TRUE ~ "Other"
    )
  ) |>
  filter(region != "Other", !is.na(beh_group))

summary_df <- chr |>
  group_by(beh_group, region) |>
  summarise(
    avg_le     = mean(life_expectancy, na.rm = TRUE),
    avg_income = mean(median_income,   na.rm = TRUE),
    .groups    = "drop"
  )

group_order <- chr |>
  group_by(beh_group) |>
  summarise(avg_le = mean(life_expectancy, na.rm = TRUE)) |>
  arrange(avg_le) |>
  pull(beh_group)

region_order <- c("Northeast", "West", "Midwest", "South")

summary_df <- summary_df |>
  mutate(
    beh_group = factor(beh_group, levels = group_order),
    region    = factor(region, levels = region_order)
  )

# ── Dual axis ───────────────────────────────────────────────
le_min <- 68; le_max <- 82
inc_min <- 50000; inc_max <- 92000

scale_income   <- function(x) le_min + (x - inc_min) / (inc_max - inc_min) * (le_max - le_min)
unscale_income <- function(y) inc_min + (y - le_min) / (le_max - le_min) * (inc_max - inc_min)

summary_df <- summary_df |>
  mutate(income_scaled = scale_income(avg_income))

y_ceil <- max(max(summary_df$avg_le), max(summary_df$income_scaled)) + 1.2

group_colors <- c(
  "Smoke-Dominant" = "#FFD166",
  "Both High"      = "#EF476F",
  "Both Low"       = "#4CAF50",
  "Drink-Dominant" = "#118AB2"
)

# Dummy data for legend entry for the income dot+line
income_legend_df <- data.frame(
  x = NA_integer_, y = NA_real_,
  label = "Avg. Median Income"
)

fig2 <- ggplot(summary_df,
               aes(x = beh_group)) +

  # Bars
  geom_col(aes(y = avg_le, fill = beh_group),
           width = 0.72, color = "black", linewidth = 0.4) +

  # LE label inside bar
  geom_text(aes(y = avg_le - 0.8, label = round(avg_le, 1)),
            vjust = 1, size = 4.6,
            fontface = "bold", color = "black") +

  # Income connecting line
  geom_line(aes(y = income_scaled, group = region,
                linetype = "Avg. Median Income"),
            color = "grey55", linewidth = 0.8) +

  # Income dots
  geom_point(aes(y = income_scaled, shape = "Avg. Median Income"),
             fill = "white", color = "grey40",
             size = 3.8, stroke = 1.3) +

  facet_wrap(~ region, nrow = 1) +

  scale_fill_manual(values = group_colors, name = NULL) +

  # Income dot in legend via shape scale
  scale_shape_manual(
    name   = NULL,
    values = c("Avg. Median Income" = 21)
  ) +

  # Income line in legend via linetype scale
  scale_linetype_manual(
    name   = NULL,
    values = c("Avg. Median Income" = "solid")
  ) +

  scale_y_continuous(
    name   = "Avg. Life Expectancy (Years)",
    breaks = seq(68, 82, 2),
    expand = c(0, 0),
    sec.axis = sec_axis(
      transform = ~ unscale_income(.),
      name      = "Avg. Median Income ($)",
      labels    = label_dollar(scale = 1e-3, suffix = "K"),
      breaks    = seq(50000, 90000, 5000)
    )
  ) +

  coord_cartesian(ylim = c(68, y_ceil)) +

  labs(x = NULL) +

  theme_minimal(base_size = 15) +
  theme(
    panel.border       = element_rect(color = "black", fill = NA,
                                      linewidth = 0.8),
    strip.text         = element_text(size = 14, face = "bold",
                                      color = "black"),
    strip.background   = element_rect(fill = "#EAF2FB",
                                      color = "black",
                                      linewidth = 0.6),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.text.y        = element_text(size = 12, face = "bold",
                                      color = "black"),
    axis.text.y.right  = element_text(size = 12, face = "bold",
                                      color = "black"),
    axis.title.y.left  = element_text(size = 13, face = "bold",
                                      color = "black",
                                      margin = margin(r = 8)),
    axis.title.y.right = element_text(size = 13, face = "bold",
                                      color = "black", angle = -90,
                                      vjust = 0.5,
                                      margin = margin(l = 10)),
    axis.ticks.y       = element_line(color = "black", linewidth = 0.4),
    axis.line          = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 12, face = "bold",
                                      color = "black"),
    legend.key.size    = unit(0.65, "cm"),
    legend.key         = element_rect(color = "black", linewidth = 0.5,
                                      fill = NA),
    legend.spacing.x   = unit(0.4, "cm"),
    plot.margin        = margin(15, 25, 10, 18)
  ) +

  guides(
    fill     = guide_legend(nrow = 1, order = 1),
    shape    = guide_legend(order = 2,
                            override.aes = list(size = 4, fill = "white",
                                                color = "#444444")),
    linetype = "none"   # suppress separate linetype entry
  )

ggsave("figures/fig2_bar.png", fig2,
       width = 12, height = 7.5,
       dpi = 600,
       units = "in")

message("Figure 2 saved to figures/fig2_bar.png")

# ============================================
# FIGURE 1 — US County Choropleth Map
# ============================================

library(tidyverse)
library(tigris)
library(sf)
library(scales)

options(tigris_use_cache = TRUE)

chr <- readRDS("data/chr_clean.rds") |>
  filter(!is.na(life_expectancy))

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2023) |>
  shift_geometry() |>
  filter(!STATEFP %in% c("72","78","66","69","60"))

states_sf <- states(cb = TRUE, resolution = "20m", year = 2023) |>
  shift_geometry() |>
  filter(!STUSPS %in% c("PR","VI","GU","MP","AS"))

chr_map <- counties_sf |>
  left_join(
    chr |> select(fips, life_expectancy),
    by = c("GEOID" = "fips")
  )

le_low  <- 60
le_mid  <- 75.3
le_high <- 90

fig1 <- ggplot() +

  # County fill
  geom_sf(data = chr_map,
          aes(fill = life_expectancy),
          color = NA, linewidth = 0) +

  # Medium grey county borders
  geom_sf(data = chr_map,
          fill = NA,
          color = "grey5",
          linewidth = 0.05) +

  # State borders on top
  geom_sf(data = states_sf,
          fill = NA,
          color = "black",
          linewidth = 0.5) +

  scale_fill_gradientn(
    colors   = c("#CC0000", "#FF4500", "#FFFAF6", "#1A6FD4", "#00008B"),
    values   = rescale(c(60, 64, 75.3, 86, 90)),
    limits   = c(60, 90),
    na.value = "#FFFAF6",
    name     = "Life Expectancy\n(Years)",
    breaks   = c(60, 65, 70, 75.3, 80, 85, 90),
    labels   = c("60", "65", "70", "75 (Median)", "80", "85", "90"),
    guide    = guide_colorbar(
      barwidth        = unit(0.8, "cm"),
      barheight       = unit(5.5, "cm"),
      title.position  = "top",
      title.hjust     = 0.5,
      ticks.colour    = "black",
      ticks.linewidth = 0.6,
      frame.colour    = "black",
      frame.linewidth = 0.8
    )
  ) +

  coord_sf(crs = st_crs("ESRI:102003"), clip = "on") +

  theme_void(base_size = 13) +
  theme(
    legend.position      = "right",
    legend.box.margin    = margin(0, 0, 0, -15),
    legend.justification = c(0, 0.5),
    legend.title         = element_text(size = 11, face = "bold",
                                        color = "black", lineheight = 1.1),
    legend.text          = element_text(size = 10, face = "bold",
                                        color = "black"),
    legend.background    = element_rect(fill = "white", color = "black",
                                        linewidth = 0.7),
    legend.margin        = margin(6, 8, 6, 8),
    plot.background      = element_rect(fill = "white", color = NA),
    panel.background     = element_rect(fill = "white", color = NA),
    plot.margin          = margin(5, 5, 5, 5)
  )

ggsave("figures/fig1_choropleth.png", fig1,
       width  = 10,
       height = 6.2,
       dpi    = 600,
       units  = "in")

message("Figure 1 saved to figures/fig1_choropleth.png")
