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
    legend.position = "bottom",  
    # legend underneath
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


# plot without legend 

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
    legend.position = "none",  
    # legend gone
  )

p


# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = insect_piechart_but_not, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 6.5, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)


########################################################
# Graphing by Order individually  
########################################################

# Hymenoptera 
#---------------------------------------------------------------------------

d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_hymenoptera = d[d$order == "Hymenoptera",]  # only hymenoptera
d_hymenoptera = d_hymenoptera[order(-d_hymenoptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_hymenoptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "none",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = hymenoptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)


# Diptera 
#-----------------------------------------------------------------------


d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_diptera = d[d$order == "Diptera",]  # only Diptera
d_diptera = d_diptera[order(-d_diptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_diptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "right",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = diptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)



# Diptera 
#-----------------------------------------------------------------------


d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_diptera = d[d$order == "Diptera",]  # only Diptera
d_diptera = d_diptera[order(-d_diptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_diptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "right",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = diptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)


# Coleoptera 
#-----------------------------------------------------------------------


d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_coleoptera = d[d$order == "Coleoptera",]  # only Coleoptera
d_coleoptera = d_coleoptera[order(-d_coleoptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_coleoptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "right",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = coleoptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)


# Lepidoptera 
#-----------------------------------------------------------------------


d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_lepidoptera = d[d$order == "Lepidoptera",]  # only Lepidoptera
d_lepidoptera = d_lepidoptera[order(-d_lepidoptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_lepidoptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "right",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = lepidoptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)


# /Hemiptera 
#-----------------------------------------------------------------------


d <- d %>%
  mutate(order = fct_reorder(order, photo_or_abundance, .fun = sum, .desc = TRUE))

d_hemiptera = d[d$order == "Hemiptera",]  # only Hemiptera
d_hemiptera = d_hemiptera[order(-d_hemiptera$photo_or_abundance), ] # arrange by descending order 

# Plot: stacked bars + facet by year + percent labels on top of each bar
p <- ggplot(d_hemiptera, aes(x = order, y = photo_or_abundance, fill = family)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ year, nrow = 2) +
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
  scale_color_viridis() + 
  ggtitle("Orders and families") +
  ylab("Insect Families Abundance \n(# photos in 2019-2022, # captured in 2024)") +
  xlab("Order") +
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
    legend.position = "right",  
    # legend at bottom, can also do 'right' or 'none'
  )

p

# Save the plot manually (must do this or will save at lower dpi) 

ggsave(
  filename = hemiptera_chart, 
  plot = p, 
  dpi = 600, # 300 to 600 best 
  width = 7, 
  height = 6.5, 
  units = "in" # Specify units (inches, cm, mm, or px)
)





