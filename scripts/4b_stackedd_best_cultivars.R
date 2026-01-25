############## Stacked Barcharts ############################
# but with richness and abundance per day instead of per season 


#-----------------------------load libraries

library(ggplot2)
library(tidyverse)
library(tidytext)
library(lubridate)
library(vegan)
library(grid)

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
p_richness + 
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
                    aes(x = reorder(cultivar, avg_abundance), y = avg_abundance, fill = flower_type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)

p_abundance +
  scale_fill_discrete(name = "Legend") +
  ggtitle("Insect Abundance Per Day by Cultivar") +
  ylab("Avg. # of Insects / Day") + 
  xlab("Hydrangea Cultivar") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 9))

