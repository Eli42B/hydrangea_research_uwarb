################################################################################
# run this script first to run all other scripts
# 
# Elizabeth Braatz 2026 
###############################################################################

rm(list = ls())   # blank out your environment 
if (!is.null(dev.list())) dev.off()         # Blank out your plots if not already blanked out 


# For Setup Script 0 , the first time we run it, we need to manually select it 
# --------------------------------------------------------------------

source(file.choose()) # to run the setup script the first time, you have to find it on your computer because the 'here' package and relative paths aren't set up. Find the 1_setup_script.R script and run it
source(script_0_filepath) # run script0_setup script 


# Analyses for main text 
#-----------------------------------------------------------------------

source(script_1.1_filepath, print.eval = FALSE) # Run script 1.1_Analysis_Abundance_anova_glm_interactions.R 
source(script_1.2_filepath, print.eval = TRUE) # Run script 1.2_Analaysis_richness_anova_glm_interactions.R 
source(script_1.3_filepath)

# graphs for main text 
#-----------------------------------------------------------------------

source(script_2.1_filepath) # run script 2.1_rates_of_increase_graph.R
# note that print.eval = TRUE is required if you have ggplot2 graphs, otherwise source will just skip right over them 
source(script_2.2_filepath) # run scripti 2.2_stacked_best_cultivars.R graph 
source(script_2.3_filepath)
source(script_2.4_filepath)
source(script_2.5_filepath)
