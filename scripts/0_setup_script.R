################################################################################
# run t his script first to load libraries and set up 
# relative pathing 
# 
# Elizabeth Braatz 2025 
###############################################################################

rm(list = ls())   # blank out your environment 
if (!is.null(dev.list())) dev.off()       # Blank out your plots if they aren't already blanked out 


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
library(cowplot)      # for graphing ggplots side by side 
library(ggpattern)    # add patterns to a barchart 


# script 1.4 _ PCA Analysis (we will rename this as supplementary later)

library(FactoMineR)   # for pca analysis
library(factoextra)   # for pca analysis
library(corrplot)     # for plotting correlations 

# script 5_ supplementary graph of timeline 

library(vistime)      # for the stacked bargraph timeline 

# 4 Confusion Matrix graph 

library(caret)        # for the confusion matrix 

# 9_ pca analysis_graph 

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(FactoMineR)  # for pca analysis 
library(factoextra)  # for ggplot visualization of pca analysis 
library("corrplot")  # for graphing correlations in pca 
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 

# 10 _ piecharts 

library(janitor)     # snake case clean names
library(tidyverse)   # dplyr and ggplot2


####################################################################
# Relative pathing  
####################################################################

# general base links 
# ---------------------------------------------------------------

working_directory = here()
input_filepath = here("input")
output_filepath = here("output") 
scripts_filepath = here("scripts") 

#############################
# Filepaths to scripts 
# ###########################
# ----------------------------------------------------------------

# Scripts are approximately numbered to run in order, but all should be able to be run independently as well 

# Script 1 always goes first, it's the setup script 

script_0_filepath = file.path(scripts_filepath, "0_setup_script.R") 

# Scripts with 1.# denotation are scripts used to run core analyses 

script_1.1_filepath = file.path(scripts_filepath,"1.1_Analysis_Abundance_anova_glm_interactions.R") 
script_1.2_filepath = file.path(scripts_filepath,"1.2_Analysis_richness_anova_glm_interactions.R") 
script_1.3_filepath = file.path(scripts_filepath,"1.3_analysis_permanova_and_permdisp.R") 


# Scripts with 2.# are scripts that generate graphs for the main text

script_2.1_filepath = file.path(scripts_filepath,"2.1_rates_of_increase_graph.R") 
script_2.2_filepath = file.path(scripts_filepath,"2.2_stacked_barchart_insect_abundances.R") 
script_2.3_filepath = file.path(scripts_filepath,"2.3_stacked_best_cultivars.R") 
script_2.4_filepath = file.path(scripts_filepath,"2.4_pollinator_community_100percent_graph.R") 
script_2.5_filepath = file.path(scripts_filepath,"2.5_nmds_plots.R") 



# Scripts that start with supplementary_ are scripts that generate graphs or run analyses for the supplementary text 



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

# Script 4 confusion matrix 
# ------------------------------------------------------------------

heatmap_morphospecies_path = file.path(input_filepath, "300_image_subset_morphospecies.csv") 
heatmap_lowest_taxa_path = file.path(input_filepath, "300_image_subset_lowest_taxonomic.csv")

# Script 5 supplementary graph of timeline 
# ------------------------------------------------------------------

timeline_all_filepath = file.path(input_filepath, "Timeline_All.csv") 

# Script 9 supplementary pca analysis 
#-------------------------------------------------------------------

pca_filepath = file.path(input_filepath, "Ranking_cultivars.csv")

# Script 10 piechart
#-------------------------------------------------------------------

barchart_piechart_filepath = file.path(input_filepath, "pie_chart_df.csv") 

# Outputs of graphs 
# -------------------------------------------------------

# Graphs of spp richness and abundance x floral abundance

interaction_abundance_filepath = file.path(output_filepath, "interaction_abundance_graph.png") 
interaction_richness_filepath = file.path(output_filepath, "interaction_richness_graph.png") 

# Graphs of which cultivars had most insects 

stacked_abundance_cultivars_filepath = file.path(output_filepath, "stacked_abundance_cultivars_graph.png") 
stacked_richness_cultivars_filepath = file.path(output_filepath, "stacked_richness_cultivars_graph.png") 

# Insect piechart 

insect_piechart_but_not = file.path(output_filepath, "insect_piechart_but_not.png") 
hymenoptera_chart = file.path(output_filepath, "hymenoptera_chart.png") 
diptera_chart = file.path(output_filepath, "diptera_chart.png") 
coleoptera_chart = file.path(output_filepath, "coleoptera_chart.png") 
lepidoptera_chart = file.path(output_filepath, "lepidoptera_chart.png") 
hemiptera_chart = file.path(output_filepath, "hemiptera_chart.png") 

