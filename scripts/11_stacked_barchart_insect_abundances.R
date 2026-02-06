# load libraries 

library(tidyverse)  # ggplot2 
library(forcats)    # for ordering in descending order 
library(scales) 
library(viridis)    # for colorblind palette 


#################################
#Setup
#################################

d = read.csv(barchart_piechart_filepath)


################################################
#  Best graph   
#################################################

# sort 

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

# Compute totals per (year, order) for the bar tops
bar_totals <- d %>%
  group_by(year, order) %>%
  summarise(total = sum(photo_or_abundance), .groups = "drop")

# Compute totals per year for percent-of-year labels
year_totals <- d %>%
  group_by(year) %>%
  summarise(year_total = sum(photo_or_abundance), .groups = "drop")

# Build label table with percent text
labels <- bar_totals %>%
  left_join(year_totals, by = "year") %>%
  mutate(
    pct = total / year_total,
    label_txt = percent(pct, accuracy = 0.1)
  )

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, ncol = 2) +
  # Put labels at the bar tops (use the summarised data)
  geom_text(
    data = labels,
    aes(x = order, y = total, label = label_txt),
    vjust = -0.2,           # nudge above the bar
    size = 3,
    inherit.aes = FALSE
  ) +
  # Give a bit of headroom so labels aren't clipped
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_fill_viridis_d(option = "D", end = 0.95) + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
  theme_minimal()

p

####################################################################
# asking copilot to hlep clean up the graph 
####################################################################

# load libraries
library(tidyverse)
library(forcats)
library(scales)

#################################
# Setup
#################################
d <- read.csv(barchart_piechart_filepath)

################################################
# Sort orders by total abundance (descending)
################################################
d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

################################################
# Compute totals for bar-top labels
################################################
bar_totals <- d %>%
  group_by(year, order) %>%
  summarise(total = sum(photo_or_abundance), .groups = "drop")

year_totals <- d %>%
  group_by(year) %>%
  summarise(year_total = sum(photo_or_abundance), .groups = "drop")

labels <- bar_totals %>%
  left_join(year_totals, by = "year") %>%
  mutate(
    pct = total / year_total,
    label_txt = percent(pct, accuracy = 0.1)
  )

################################################
# Grayscale palette for families
################################################
# Make sure families have a stable order (optional but recommended)
family_levels <- d %>%
  count(family, sort = TRUE) %>%
  pull(family)

d <- d %>%
  mutate(family = factor(family, levels = family_levels))

n_fam <- nlevels(d$family)

# A readable grayscale ramp (avoid pure white to keep visibility)
gray_pal <- gray.colors(n_fam, start = 0.2, end = 0.85)

################################################
# Plot
################################################
p <- ggplot(d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_col(position = "stack", color = "grey20", linewidth = 0.2) +  # thin outlines help in grayscale
  facet_wrap(~ year, ncol = 1) +
  geom_text(
    data = labels,
    aes(x = order, y = total, label = label_txt),
    vjust = -0.3,
    size = 4,                # BIGGER label text
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = gray_pal) +
  labs(
    y = "Insect Families Abundance \n(# photos in 2019–2022, # captured in 2024)",
    x = "Order",
    fill = "Family"
  ) +
  theme_minimal(base_size = 14) +  # BIGGER overall text
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # readable x labels
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),           # facet year labels
    legend.position = "bottom",                                    # legend underneath
    legend.direction = "horizontal",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.6, "lines"),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    fill = guide_legend(
      nrow = 15,               # put legend items in multiple rows
      byrow = TRUE
    )
  )

p


####################################################################
# Failed graphs (not as pretty, not included in man)
####################################################################


#  bargraph 1: All years, orders, families 

# sort 

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

p <- ggplot(data = d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis_d(option = "D", end = 0.95) + 
  ggtitle("Orders and families") +
  ylab("Insect Families") +
  xlab("Order")

p


#  bargraph 2:  orders, families divided by year 


# sort 

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

p <- ggplot(data = d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~year, ncol = 1) +      # panel per year 
  scale_fill_viridis_d(option = "D", end = 0.95) + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order")
p


#  bargraph 4:  orders, families as a percentage 


# sort 

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

p <- ggplot(data = d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~year, ncol = 2) +      # panel per year
  scale_fill_viridis_d(option = "D", end = 0.95) + 
  ggtitle("Orders and families") +
  ylab("Insect Families  \n(As a Percentage)") +
  xlab("Order")
p

