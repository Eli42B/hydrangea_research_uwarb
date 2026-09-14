############## PCA analysis graph ################

#-----------------------------load libraries

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 
library(tidyverse)   # for pivoting to long format and stacaked barchart 

# load data 
#--------------------------------------------

# we are using raw data rather than ranks here. Originally we tried ranking everything, but in the final manuscript we decided to go back to just a PCA plot and not giving out 'star' ratings  

d = read.csv(pca_filepath, header = TRUE) # make sure to re-load this, we want the cultivar to appear as a column name 

# Cleaning data 

rownames(d) <- iconv(rownames(d), from = "latin1", to = "UTF-8")
rownames(d) <- iconv(rownames(d), from = "WINDOWS-1252", to = "UTF-8")
# there are badly encoded characters that R is having a fit about, trying to fix
d = d %>% 
  clean_names()     # clean the names 
d[26,1] = "Invincibelle Wee White" # rename weird characters 
d = d[-28,] # remove Tardiva, looks like we lack data for it 

# Replace NA values with the mean photo insect count 

mean_insect_count = mean(d$photo_insect_count, na.rm = TRUE)
mean_insect_count 
# calculate hte mean of d$photo_insect_count 
# = 161.5588 
d$photo_insect_count[is.na(d$photo_insect_count)] <- mean_insect_count
# set the NAs as the mean 
# the days are already set with the average; I did it in Excel befor eI realized that doing it in R is a better best practice 

# Run the pca 
# ---------------------------------------------------------

#Run PCA 
res.pca = PCA(d[,2:6], graph = FALSE, scale.unit = TRUE)
# note: the scaling is important or it looks really funky
# only do a pca of column 1, that's the cultivars  

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val 
#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)

#Graphing individuals 
ind <- get_pca_ind(res.pca)

#simplified pca biplot where abundance > 2 and colors are simplified 
p = fviz_pca_biplot(res.pca, repel = TRUE,
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969", # Individuals color
)
p # looks terrible 

# Trying to make it prettier 
####################################################################

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# graphing by correlative power of each datapoint 
# -------------------------------------------------

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21,    # let's have them be circles
             fill = '#E7B800',   # coloring circles
             repel = TRUE        # avoid text overlapping 
)

# graphing by category 
# ------------------------------------------------

fviz_pca_ind(res.pca, 
             geom.ind = "point", 
             addEllipses = TRUE, 
             legend.title = "Species"
)  # oh, it's one giant ellipse 

# graph by two categories (species and shape) 
# ---------------------------------------------------

# we need to do this in ggplot2, fviz_pca_ind function can't do it 

# Extract PCA scores 
pca_scores = as_tibble(res.pca$ind$coord)

# we are going to have to change this dataframe pretty significantly 

pca_scores <- pca_scores %>%      
  rename(shannon = Dim.1, 
         abundance = Dim.2, 
         richness = Dim.3, 
         days = Dim.4) 
# rename the pca scores 

pca_scores$cultivar = d$cultivar
# add back the cultivar type 

dmerge = read.csv(dmergeFINAL_filepath, header = TRUE)
dmerge = dmerge %>%
  clean_names() %>% 
  dplyr::select(cultivar, species, flower_shape) %>% 
  distinct(cultivar, .keep_all = TRUE) 
pca_scores = pca_scores %>% 
  left_join(
    dmerge %>%  dplyr::select(cultivar, species, flower_shape),
    by = "cultivar"
  )
# bring in dmerge to get the cultivar species and flower shape
# clean it to remove duplicates 
# left join the pca scores and dmerge to add the cultivar species nad flower shape to the pca_scores dataframe 

# adding back our arrows (ie the loadings) 

var_coords = as_tibble(res.pca$var$coord)
var_coords$variable = rownames(var_coords)
var_coords$variable = c("shannon","abundance","richness","days", "photo_insect_count") 
# extracting arrow coordinates and saving as tibble

arrow_scale = 3          
var_coords = var_coords %>% 
  mutate(
    Dim.1 = Dim.1*arrow_scale, 
    Dim.2 = Dim.2*arrow_scale, 
    Dim.3 = Dim.3*arrow_scale, 
    Dim.4 = Dim.4*arrow_scale, 
    Dim.5 = Dim.5*arrow_scale
  )
# these arrows will look tiny on our plot 
# let's make them bigger by 3x, we can change this later 

# plotting just points and saving as a map 

points_plot = ggplot() + 
  # points 
  geom_point( 
    data = pca_scores, 
    aes(x = shannon, y = abundance, 
        color = species, 
        shape = flower_shape), 
    size = 3, alpha = 0.6) + 
  scale_color_manual(values = c("brown", "#00AFBB"))

points_plot

#adding arrows for variables 

arrows_plot = 
  geom_segment(
    data = var_coords, 
    aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
    arrow = arrow(length = unit(0.3,"cm")), 
    color = "black"
  ) 

points_plot + arrows_plot

# adding labels for the arrows 

label_plot = 
  geom_text(
    data = var_coords, 
    aes(x = Dim.1, y = Dim.2, 
        label = variable), 
    color = "black", 
    vjust = -0.5
  )
label_plot

points_plot + arrows_plot + label_plot
# pretty plot 

p
# basic pca plot 

points_plot + 
  arrows_plot + 
  labs(
    title = "Hydrangea Cultivar PCA Plot",
    subtitle = "Top performers clustered along the righthand sides"
  )


#########################################################################
# new pca analysis aggregated by WiBee categories + plant categories 
#########################################################################


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
stressplot(nmds)  
# OK, we want the little blue dots (pairwise comparisons, actual data) to follow the red line (ideal relationship between points) 
# This is OK. Not great but not terrible. 

plot(nmds) 
# We don't really have distinct groups, everything's in one cluster. Which matches the percent graphs that indicated a lo tof overlap. 
# The circles are our rows / plants and the + signs are the bug guilds 
