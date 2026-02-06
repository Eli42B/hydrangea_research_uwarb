############## Stacked Barcharts ############################
# but with richness and abundance per day instead of per season 


#-----------------------------load libraries

library(ggplot2)
library(tidyverse)
library(tidytext)
library(lubridate)
library(vegan)
library(grid)
library(cowplot)        # graph ggplots side by sisde 
library(ggpattern)      # add patterns to a barchart 

#------------------------------- Richness Plot 

# Load data 
dmerge_richness = read.csv(glm_richness_filepath)

# set up dataframes 

dmerge_richness = dmerge_richness %>% 
  filter(Cultivar != "Silver Leaf")

# summarize by cultivar to find avg. richness per cultivar 
d_richness = dmerge_richness %>% 
  dplyr::select(Cultivar, Flower.Type, InsectRichness) %>% 
  group_by(Cultivar,Flower.Type) %>% 
  summarise(avg_richness = mean(InsectRichness, na.rm = TRUE), .groups = "drop") 

# clean names 
d_richness = d_richness %>% 
  clean_names()

# graphs 

#Insect Richness x Species + Flower Type

p_richness = ggplot(data = d_richness,
                  aes(x = reorder(cultivar, avg_richness), y = avg_richness, fill = flower_type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
p_richness = p_richness + 
  scale_fill_discrete(name = "Legend") + 
  ggtitle("Insect Richness Per Day by Cultivar") + 
  ylab("Avg. # of Unique Insects / Day") + 
  xlab("Hydrangea Cultivar") + 
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9))

#------------------------------- Abundance Plot 

# Load data 
dmerge_abundance = read.csv(glm_abundance_filepath)

# set up dataframes 

dmerge_abundance = dmerge_abundance %>% 
  filter(Cultivar != "Silver Leaf")

# summarize by cultivar to find avg. richness per cultivar 
d_abundance = dmerge_abundance %>% 
  dplyr::select(Cultivar, Flower.Type, TotalInsects) %>% 
  group_by(Cultivar,Flower.Type) %>% 
  summarise(avg_abundance = mean(TotalInsects, na.rm = TRUE), .groups = "drop") 

# clean names 
d_abundance = d_abundance %>% 
  clean_names()

#Insect Abundance x Species + Flower Type

p_abundance = ggplot(data = d_abundance,
                    aes(x = reorder(cultivar, avg_abundance),
                        y = avg_abundance, 
                        fill = flower_type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)

p_abundance = p_abundance +
  scale_fill_discrete(name = "Legend") +
  ggtitle("Insect Abundance Per Day by Cultivar") +
  ylab("Avg. # of Insects / Day") + 
  xlab("Hydrangea Cultivar") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9))

# graph them side by side 

plot_grid(p_abundance, p_richness, nrow = 2, ncol = 1, labels = "AUTO")


############################################################################
# Asking copilot to make the plots more readable and change colors a bit 
############################################################################

library(tidyverse)

#  enforce factor levels so the legend order is consistent
d_abundance <- d_abundance %>%
  mutate(
    flower_type = factor(
      flower_type,
      levels = c("Mop", "Lacy", "Mop-like panicle", "Lacy panicle")
    )
  )

# Manual colors 
flower_cols <- c(
  "Mop"          = "#08306B",  # dark blue
  "Lacy"         = "#9CCBFF",  # light blue
  "Mop-like panicle"  = "#8B0000",  # dark red
  "Lacy panicle" = "#F4A3A3"   # light red
)

p_abundance <- ggplot(
  data = d_abundance,
  aes(
    x = reorder(cultivar, avg_abundance),
    y = avg_abundance,
    fill = flower_type
  )
) +
  geom_col(
    width = 0.55,              # thinner bars 
    color = "grey20",          # outline them 
    linewidth = 0.2
  ) +
  coord_flip() +
  scale_fill_manual(values = flower_cols, drop = FALSE) + # colors 
  
  # padding 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  # x and y labels 
  labs(
    x = "Cultivar Name",
    y = "Average Daily Insect Abundance",
    fill = "Flower type"
  ) +
  
  theme_minimal(base_size = 14) +
  
  # legend on bottom, bigger text 
  theme(
    axis.text.y = element_text(size = 13),  # cultivar names (after flip)
    axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),         # remove title 
    legend.text = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

p_abundance


