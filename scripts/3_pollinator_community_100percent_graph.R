#########################################################################
# new pca analysis aggregated by WiBee categories + plant categories 
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

# convert the d_plant_level to longform 

d_shape_long = d_plant_level %>% 
  pivot_longer(
    cols = c(small_dark_bees, green_sweatbees, large_dark_bees, honeybee, bumble_bees, flies, true_bugs, butterflies, wasps, beetles, unknown_hymenoptera), 
    names_to = "guild", 
    values_to = "abundance"
  ) 

# graphing a 100% stacked barchart with ggplot2 
# ------------------------------------------------------------------------------

# colorblind palette 
cb_palette <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#999999", # gray
  "#332288", # dark indigo
  "#88CCEE", # light cyan
  "#000000" # black 
) 

# By cultivar 
ggplot(d_shape_long, aes(fill=guild, y=abundance, x=Cultivar)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = cb_palette) + 
  theme_classic()

# By plant species 
ggplot(d_shape_long, aes(fill=guild, y=abundance, x=Species)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = cb_palette) + 
  theme_classic()

# by shape 
ggplot(d_shape_long, aes(fill=guild, y=abundance, x=Flower.shape)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = cb_palette) + 
  theme_classic()

# by species + shape 
ggplot(d_shape_long, aes(fill=guild, y=abundance, x=Flower.Type)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_manual(values = cb_palette) + 
  theme_classic()

# Graphing a NMDS 
# ---------------------------------------------------------------------------
# https://library.virginia.edu/data/articles/starting-non-metric-multidimensional-scaling-nmds 
# NMDS requires replication so we need the original dataframe with the aggregated columns of bug types but separated dates 
# we want things organized as a 'community matrix,' which our d_plant_level dataframe already is 

nmds = metaMDS(d_plant_level[,6:16], 
               distance = "bray", 
               k = 2) 
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

# Trying again with higher trymax values 

set.seed(42) 
nmds = metaMDS(d_community, 
               distance = "bray", 
               k = 2, 
               trymax = 500) 
print(nmds)
# best solution was repeated one time, that is not good 

# Trying with a third dimension 

set.seed(42) 
nmds = metaMDS(d_community, 
               distance = "bray", 
               k = 3, 
               trymax = 500) 
# best solution was repeated 1 time 
print(nmds)
# Stress was 0.124 which is better than 0.18 

# Fancy nmds plot 
# --------------------------------------------------------------

# plotting ordination by habitat type 

# get habitat type  
flower_type <- d_guilds %>% 
  distinct(X, Flower.Type) 

# Extract NMDS scores for sites 
nmds_FlowerTypeScores <-
  # get nmds scores 
  as.data.frame(scores(nmds)$sites) %>%
  # change rownames (site) to a column 
  rownames_to_column(var = "Site") %>% 
  # join our habitat type (grouping variable) to each site 
  left_join(flower_type)
# Extract NMDS scores for Flower Type  
nmds_SpeciesScores <- 
  as.data.frame(scores(nmds, "Flower Type"))

# create a column of species, from the rownames of species.scores
nmds_SpeciesScores$species <- rownames(nmds_SpeciesScores) 

ggplot() + 
  
  # add site scores
  geom_point(data = nmds_SiteScores, 
             aes(x=NMDS1, y=NMDS2, colour = Habitat), 
             size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  theme_classic()

# Plotting with convex hulls  

# get centroid of each hull 
Habitat_Centroid <- 
  nmds_SiteScores %>% 
  group_by(Habitat) %>% 
  summarise(axis1 = mean(NMDS1),
            axis2 = mean(NMDS2)) %>% 
  ungroup()

# extract convex hull
habitat.hull <- 
  nmds_SiteScores %>% 
  group_by(Habitat) %>%
  slice(chull(NMDS1, NMDS2))

nmds_stress <- nmds$stress

# use ggplot to plot 
ggplot() + 
  
  # add site scores
  geom_point(data = nmds_SiteScores, 
             aes(x=NMDS1, y=NMDS2, colour = Habitat), size = 2) + 
  
  # add species scores 
  geom_text(data = nmds_SpeciesScores, 
            aes(x=NMDS1, y=NMDS2, label = species)) +
  
  # add centroid 
  geom_point(data = Habitat_Centroid, 
             aes(x = axis1, y = axis2, color = Habitat), 
             size = 5, shape = 17) +
  
  # add convex hull
  geom_polygon(data = habitat.hull, 
               aes(x = NMDS1, y = NMDS2, fill = Habitat, group = Habitat), 
               alpha = 0.30) +
  
  # add stress value
  annotate("text", x = 0.75, y = 0.65, 
           label = paste("2d stress =", round(nmds_stress, 3))) +
  # edit theme
  labs(x = "NMDS1", y = "NMDS2") + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, linewidth = .5),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(.25, "cm")) 


########################################################################
# Running PERNOVA analysis 
########################################################################

# what is our dataframe? 
d_guilds2 = d_guilds %>% 
  dplyr::select(
    X, Date, Cultivar, Species, Flower.shape, small_dark_bees:unknown_hymenoptera)

# Cleaning things 
d_guilds2[is.na(d_guilds2)] = 0 # turn NA values into zeroes 

#diagnostics  
sum(rowSums(d_guilds2[,6:16]) == 0)    # looking for rows where everything equals zero, if we have > 0, we have a problem row 
sum(is.na(d_guilds2[,6:16]))            # hunting NA values 

# removing rows where everything equals zero 
keep <- rowSums(d_guilds2[,6:16]) > 0
d_guilds2 <- d_guilds2[keep, ]

#diagnostics (checking if it worked)  
sum(rowSums(d_guilds2[,6:16]) == 0)    # looking for rows where everything equals zero, if we have > 0, we have a problem row 
sum(is.na(d_guilds2[,6:16]))            # hunting NA values 

# Community matrix
head(d_guilds2)

# Bray-Curtis distances
bc <- vegdist(d_guilds2[,6:16], method = "bray")

# NMDS
set.seed(42) 
nmds = metaMDS(d_guilds2[,6:16], 
               distance = "bray", 
               k = 2, 
               trymax = 500) 
# PERMANOVA 
adonis2(bc ~ Species + Flower.shape,
        data = d_guilds2,
        by = "terms")

adonis2(bc ~ Cultivar,
        data = d_guilds2,
        by = "terms")
