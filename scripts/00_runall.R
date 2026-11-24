################################################################################
# run this script first to run all other scripts
# 
# Elizabeth Braatz 2026 
###############################################################################

rm(list = ls())                       # blank out your environment 
if (!is.null(dev.list())) dev.off()   # Blank out your plots if not already blanked out 


# For Setup Script 0 , the first time we run it, we need to manually select it 
# --------------------------------------------------------------------

source(file.choose()) # to run the setup script the first time, you have to find it on your computer because the 'here' package and relative paths aren't set up. Find the 1_setup_script.R script and run it
source(script_0_filepath) # run script0_setup script 


# Analyses for main text 
#-----------------------------------------------------------------------

source(script_1.1_filepath, print.eval = FALSE) # ANOVA GLM for insect abundance
source(script_1.2_filepath, print.eval = TRUE)  # ANOVA GLM for insect richness 
source(script_1.3_filepath)                     # PERMANOVA and PERMDISP analysis 

# graphs for main text 
#-----------------------------------------------------------------------

source(script_2.1_filepath)  #  scatterplots of insect abundance x fertile flower count
source(script_2.2_filepath)  # rainbow stacked barchart of insect families
source(script_2.3_filepath)  # best cultivars for bugs, incl. both abundance and richness 
source(script_2.4_filepath)  # 100% graph of pollinator taxa
source(script_2.5_filepath)  # NMDS plots 
