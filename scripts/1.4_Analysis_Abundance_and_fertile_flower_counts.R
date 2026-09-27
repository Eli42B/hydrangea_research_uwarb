# ###############################################################################
# # Analysis for Insect Abundance and Fertile Flower Counts 
# ###############################################################################

#-------------------------------------------------------------------------------

########################################33
#SETUP 
##########################################

#Loading libaries
library(jtools)       # for making regression analyses easier
library(broom)        # for converting df to tibbles 
library(ggstance)     # so we can coord_flip() 
library(interactions) # for interactions 
library(car)          # for calculating VIF 
library(MASS)         # for negative binomial models 
library(emmeans)      # for post-hoc analysis of NB models 

#organizing data 
dmerge = read.csv(glm_abundance_filepath) 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) 

#take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship, plus 1 so we don't get infinite values  

# removing silver leaf 
dmerge = dmerge %>% 
  filter(Cultivar != "Silver Leaf") 

# use dplyr to get averge abundance by cultivar 

dmerge_cultivar <- dmerge %>%
  group_by(Cultivar, Species, Flower.shape) %>%
  summarise(
    MeanInflorescences = mean(TotalInflorescences_AB, na.rm = TRUE),
    MeanInsects = mean(TotalInsects, na.rm = TRUE),
    .groups = "drop"
  )

plot(dmerge_cultivar$MeanInsects ~ dmerge_cultivar$MeanInflorescences)

