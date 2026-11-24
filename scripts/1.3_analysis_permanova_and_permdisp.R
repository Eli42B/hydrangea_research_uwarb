#########################################################################
# PERMANOVA and PERMDISP ANALYSES 
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
               trymax = 100) 

# PERMANOVA 
adonis2(bc ~ Species + Flower.shape,
        data = d_guilds2,
        by = "terms")

adonis2(bc ~ Cultivar,
        data = d_guilds2,
        by = "terms")

########################################################################
# Running PERMDISP analysis 
########################################################################
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/permdisp/ 

# 2. Run PERMDISP on the same distance matrix and grouping factor
disp <- betadisper(dist_mat, group = metadata$group, type = "centroid") # or "median"

# 3. Test for significant differences in dispersion
permutest(disp, permutations = 999)

# 4. Pairwise comparisons & visual check (if >2 groups or significant result)
TukeyHSD(disp)
boxplot(disp)
plot(disp)

