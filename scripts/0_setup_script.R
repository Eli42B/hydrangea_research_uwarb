################################################################################
# run t his script first to load libraries and set up 
# relative pathing 
# 
# Elizabeth Braatz 2025 
###############################################################################

#############################################
# Libraries 
#############################################

# script 0  (data prep and exploration)
library(here)         # for relative pathing 
library(tidyverse)    # for dplyr and ggplot2 
library(janitor)      # for converting names to snakecase
library(vegan)        # for calculating shannon diversity index 

# Script 1 (speccaccum plots) 
library(lubridate)    # for managing dates 
library(iNEXT)        # for spec accumulations 
library(tidytext)     # not sure I think visualization for text 

# script 1.1_ Analysis abundance anova glm interactions.R to 1.3 

library(jtools)       # for making regression analyses easier
library(broom)        # for converting df to tibbles 
library(ggstance)     # so we can coord_flip() 
library(interactions) # for interactions 
library(car)          # for calculating VIF 
library(MASS)         # for negative binomial models 
library(emmeans)      # for post-hoc analysis of NB models 

# script 1.4 _ PCA Analysis (we will rename this as supplementary later)

library(FactoMineR)   # for pca analysis
library(factoextra)   # for pca analysis
library(corrplot)     # for plotting correlations 

# script 5_ supplementary graph of timeline 

library(vistime)      # for the stacked bargraph timeline 

# 9_ pca analysis_graph 

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(FactoMineR)  # for pca analysis 
library(factoextra)  # for ggplot visualization of pca analysis 
library("corrplot")  # for graphing correlations in pca 
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 

####################################################################
# Relative pathing  
####################################################################

# general base links 
# ---------------------------------------------------------------

working_directory = here()
input_filepath = here("input")
output_filepath = here("output") 
scripts_filepath = here("scripts") 

# script 1_specaccum plots Analysis 
# ----------------------------------------------------------------

specaccum_vaccum_filepath = file.path(input_filepath, "specaccum.csv") 
specaccum_photo_filepath = file.path(input_filepath, "specaccum_2019to2022.csv") 
specaccum_vials_filepath = file.path(input_filepath, "spec_accum_vials.csv")

# script 1.1_ Analysis abundance anova glm 
# -----------------------------------------------------------------

glm_abundance_filepath = file.path(input_filepath, "dmergeFINAL.csv")

# script 1.2_ Analysis richness anova glm 
# -----------------------------------------------------------------

glm_richness_filepath = file.path(input_filepath, "dmergeFINALRichness.csv")

# script 1.3_ Analysis diversity anova glm 
# -----------------------------------------------------------------

glm_diversity_filepath = file.path(input_filepath, "dmergeFINAL.csv")

# script 1.4_ PCA Analysis and ranking 
# -----------------------------------------------------------------

pca_analysis_filepath = file.path(input_filepath, "pca_analysis.csv")
pca_ranks_filepath = file.path(input_filepath, "Ranking_cultivars.csv")
pca_dpearson_filepath = file.path(input_filepath, "pearson correlation abundance data.csv") 

# script 2_ Monthly Abundance multicolored graph 
# -----------------------------------------------------------------

beemachine_photos_filepath = file.path(input_filepath, "BeeMachineAI_Photos_3.csv")

# script 3_ Rates of Increase Figure 
# -----------------------------------------------------------------

dmergeFINAL_filepath = file.path(input_filepath, "dmergeFINAL.csv")

# script 4_ stacked best cultivars 
# ------------------------------------------------------------------

dAgg3_filepath = file.path(input_filepath, "DAgg3.csv") 
# data for species abundance 

dAgg4_filepath = file.path(input_filepath, "DAgg4.csv") 
# data for species richness 

dSA_filepath = file.path(input_filepath, "SA_cultivar.csv") 
# surface area by cultivar 

# Script 5 supplementary graph of timeline 
# ------------------------------------------------------------------

timeline_all_filepath = file.path(input_filepath, "Timeline_All.csv") 

# Script 9 supplementary pca analysis 
#-------------------------------------------------------------------

pca_filepath = file.path(input_filepath, "Ranking_cultivars.csv")








