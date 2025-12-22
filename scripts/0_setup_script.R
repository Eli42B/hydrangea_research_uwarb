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

# script 1.1_ Analysis abundance anova glm interactions.R 

library(jtools)       # for making regression analyses easier
library(broom)        # for converting df to tibbles 
library(ggstance)     # so we can coord_flip() 
library(interactions) # for interactions 
library(car)          # for calculating VIF 
library(MASS)         # for negative binomial models 
library(emmeans)      # for post-hoc analysis of NB models 

# 9_ supplementary pca analysis 

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#install.packages("ggplot2")
library(ggplot2)
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library("corrplot")
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



# Script 9 supplementary pca analysis 
#-------------------------------------------------------------------

pca_filepath = file.path(input_filepath, "Ranking_cultivars.csv")











