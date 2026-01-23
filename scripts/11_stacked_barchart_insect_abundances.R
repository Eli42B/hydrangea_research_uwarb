# load libraries 

library(tidyverse)  # ggplot2 
library(forcats)    # for ordering in descending order 
library(scales) 
library(viridis)    # for colorblind palette 


#################################
#Setup
#################################

d = read.csv(piechart_filepath)

################################################
#  bargraph 1: All years, orders, families 
#################################################

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

################################################
#  bargraph 2:  orders, families divided by year 
#################################################

# sort 

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

p <- ggplot(data = d, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~year, ncol = 2) +      # panel per year 
  scale_fill_viridis_d(option = "D", end = 0.95) + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order")
p


################################################
#  bargraph 3:  orders, families divided by year
#  plus percentages on top  
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


################################################
#  bargraph 4:  orders, families as a percentage 
#################################################

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
