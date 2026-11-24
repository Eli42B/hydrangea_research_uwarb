#########################################################################
# NMDS Plots 
#########################################################################

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 
library(tidyverse)   # for pivoting to long format and stacaked barchart 


# load data 
#--------------------------------------------

d = read.csv(glm_abundance_filepath, header = TRUE) # make sure to re-load this, we want the cultivar to appear as a column name 
# we'll need to aggregate by functional groups of insects 
# And then aggregate by functional groups of flowers 
# - species, structure, color, species + structure, # fertile flowers 
# Then create stacked barcharts of bugs per flower category (out of 100%)
# then do nmds as a supplementary chart 

# Group the data by guilds of insects 

d_guilds <- d %>%
  mutate(
    
    small_dark_bees = Protandrena + Melissodes + Ceratina + Hylaeus + 
      Halictidae + Halictus + Lasioglossum, 
    
    green_sweatbees = Agapostemon + Augochlora + Green.Sweatbee, 
    
    large_dark_bees = Xylocopa + Andrenidae, 
    
    honeybee = Apis.mellifera, 
    
    bumble_bees = Bombus.affinis + Bombus.auricomus + Bombus.bimaculatus + Bombus.impatiens + Bombus.griseocollis + Bombus.vagans, 
    
    flies = Diptera + Bombyliidae + Calliphoridae + Conopidae + Cecidomyiidae + Culicidae + Dolichopodidae + Drosophilidae + Empididae + Muscidae + Oestroidea + Syrphidae_total + Unknown_Bristly_Flies + Tipulidae + Ulidiidae,
    
    true_bugs = Hemiptera + Aphididae + Cicadellidae + Miridae + Reduviidae, 
    
    butterflies = Lepidoptera + Hesperiidae + Celastrina.ladon +  Epargyreus.clarus + Libythean.carinenta + Pieris.rapae + Polygonia.comma + Vanessa.atalanta, 
    
    wasps = Symmorphus + Eurytoma + Braconidae + Apocrita + Scolia + Sphecius.speciosus + Sphex + Vespidae + Dolichovespula.maculata + Dolichovespula + Astata + Cerceris + Chrysididae, 
    
    beetles = Chauliognathus + Cerambycidae + Chrysomelidae + Curculionidae + Mordellidae + Scarabaeoidea, 
    
    unknown_hymenoptera = Hymenoptera, 
  )

# now aggrregate by tag number + cultivar + species + flower shape 

d_plant_level = d_guilds %>% 
  group_by(
    Tag.Number, 
    Cultivar, 
    Species, 
    Flower.shape,
    Flower.Type, 
    Color
  ) %>% 
  summarise(
    across(
      c(small_dark_bees, green_sweatbees, large_dark_bees, honeybee, bumble_bees, flies, true_bugs, butterflies, wasps, beetles, unknown_hymenoptera), 
      ~ mean(.x, na.rm = TRUE)), 
    n_days = n(), 
    .groups = "drop")

# Aggregating by different plant categories 
# --------------------------------------------------------------------------------------

#  aggrregating by cultivar 
d_plant_level = d_guilds %>% 
  group_by(
    Cultivar, 
    Species, 
    Flower.shape,
    Flower.Type, 
    Color
  ) %>% 
  summarise(
    across(
      c(small_dark_bees, green_sweatbees, large_dark_bees, honeybee, bumble_bees, flies, true_bugs, butterflies, wasps, beetles, unknown_hymenoptera), 
      ~ mean(.x, na.rm = TRUE)), 
    n_days = n(), 
    .groups = "drop")
# gives insect abundance per day on average by cultivar 

d_plant_level_small = d_guilds %>% 
  group_by(
    Species, 
    Flower.shape,
  ) %>% 
  summarise(
    across(
      c(small_dark_bees, green_sweatbees, large_dark_bees, honeybee, bumble_bees, flies, true_bugs, butterflies, wasps, beetles, unknown_hymenoptera), 
      ~ mean(.x, na.rm = TRUE)), 
    n_days = n(), 
    .groups = "drop") %>% 
  mutate(
    total_bugs = rowSums(across(small_dark_bees:unknown_hymenoptera))
  )
# gives insect abundance per day on average by plant species and shape. 
# Helpful for determining the relative percentage by category fo rthe results. 

# convert the d_plant_level to longform 

d_shape_long = d_plant_level %>% 
  pivot_longer(
    cols = c(small_dark_bees, green_sweatbees, large_dark_bees, honeybee, bumble_bees, flies, true_bugs, butterflies, wasps, beetles, unknown_hymenoptera), 
    names_to = "guild", 
    values_to = "abundance"
  ) 


# Graphing a NMDS 
# ---------------------------------------------------------------------------
# https://library.virginia.edu/data/articles/starting-non-metric-multidimensional-scaling-nmds 
# NMDS requires replication so we need the original dataframe with the aggregated columns of bug types but separated dates 
# we want things organized as a 'community matrix,' which our d_plant_level dataframe already is 

# nmds = metaMDS(d_plant_level[,6:16], 
#                distance = "bray", 
#                k = 2) 
# get an error because some dates had zero insects 

# data cleaning 

d_community = d_plant_level[,6:16] 
d_community = d_community %>% 
  mutate(across(everything(), as.numeric))   # convert all to numeric 
d_community[is.na(d_community)] = 0          # turn NA values into zeroes 

#diagnostics  
sum(rowSums(d_community) == 0)    # looking for rows where everything equals zero, if we have > 0, we have a problem row 
sum(is.na(d_community))           # hunting NA values 

# removing rows where everything equals zero 
keep <- rowSums(d_community) > 0
d_community <- d_community[keep, ]
d_plant_nmds <- d_plant_level[keep, ]

sum(rowSums(d_community) == 0)  # looks like it worked, 0 rows where everything equals zero now 
colSums(d_community)            # Making sure column sums also don't have any zeroes 

# re-running nmds 

set.seed(42) 
nmds = metaMDS(d_community, 
               distance = "bray", 
               k = 2, 
               trymax = 100) 

print(nmds)
# stress < 0.2 is a fair fit, so ours is a fair fit but not great. Usable, but poor interpretation. 
# But it didn't get a best fit more than once after 100 iterations, that's not great. 

stressplot(nmds)  
# OK, we want the little blue dots (pairwise comparisons, actual data) to follow the red line (ideal relationship between points) 
# This is actually looking pretty good. 

plot(nmds) 
# We don't really have distinct groups, everything's in one cluster. Which matches the percent graphs that indicated a lo tof overlap. 
# The circles are our rows / plants and the + signs are the bug guilds 

# Trying to plot prettier

# Extract site scores
nmds_df <- as.data.frame(scores(nmds, display = "sites"))

# add the metadata to nmds scores 
nmds_df$Flower.shape <- d_plant_nmds$Flower.shape
nmds_df$Species <- d_plant_nmds$Species
nmds_df$Cultivar = d_plant_nmds$Cultivar
nmds_df$Flower.Type = d_plant_nmds$Flower.Type

# plot with ggplot2 
ggplot(nmds_df, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Plot by species 
ggplot(nmds_df,
       aes(NMDS1, NMDS2, color = Species)) +
  geom_point(size = 3) +
  coord_equal() +
  theme_bw()

# Plot by Flower Shape 
ggplot(nmds_df,
       aes(NMDS1, NMDS2, color = Flower.shape)) +
  geom_point(size = 3) +
  coord_equal() +
  theme_bw()

# Plot by Flower Type 
ggplot(nmds_df,
       aes(NMDS1, NMDS2, color = Flower.Type)) +
  geom_point(size = 3) +
  coord_equal() +
  theme_bw()

# Plot by both 
p = ggplot(nmds_df,
       aes(NMDS1, NMDS2, 
           shape = Flower.shape, 
           color = Species)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(3, 16)) + 
  scale_color_manual(values = c(
    "Hydrangea arborescens" = "#08306B",
    "Hydrangea paniculata" = "#8B0000")) + 
  coord_equal() +
  theme_bw() + 
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

plot(p) 



# Trying again with higher trymax values 
# Commented out because runnning it 500 times tends to make the laptop pause for a while 

# set.seed(42) 
# nmds = metaMDS(d_community, 
#                distance = "bray", 
#                k = 2, 
#                trymax = 500) 
# print(nmds)
# # best solution was repeated one time, that is not good 
# 
# # Trying with a third dimension 
# 
# set.seed(42) 
# nmds = metaMDS(d_community, 
#                distance = "bray", 
#                k = 3, 
#                trymax = 500) 
# # best solution was repeated 1 time 
# print(nmds)

# Stress was 0.124 which is better than 0.18 

