# ###############################################################################
# # Analysis for Insect Diversity  Linearity, Interactions, GLM, ANOVA 
# ###############################################################################


###############################################33
# Data setup 
###############################################

#Loading libaries
library(jtools)       # for making regression analyses easier
library(broom)        # for converting df to tibbles 
library(ggstance)     # so we can coord_flip() 
library(interactions) # for interactions 
library(car)          # for calculating VIF 
library(emmeans)      # for post-hoc analysis of NB models 

#organizing data 
dmerge = read.csv(glm_diversity_filepath, header = TRUE) 

dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) 
#take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship, plus 1 so we don't get infinite values  

############################################
# Creating the Diversity Index 
############################################

#Create the diversity index 
#We're going to use Shannon diversity index because we aren't particularly concerned about a handful of rare species, we just want an overall picture of diversity 
#Shannon diversity index is based on number of species and evenness. It's am easure of hte diversiety of species in a community. 
#Higher H = higher diversity of species in a community
# Lower H means lower diversity 
# H = 0 means no diversity, the community only has one species 

#Visualized data with boxplots and scatterplots 

dmerge$shannon = diversity(dmerge[,29:96], index = "shannon", MARGIN = 1, base = exp(1))

dmerge = dmerge[-178,] #there's a single day where shannon diversity index calculated as NA, not sure why, let's just remove it 
dmerge$shannon

##################################################
# Clean the names 
##################################################

dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)
dmerge$Flower.shape = as.factor(dmerge$Flower.shape)

clean_dmerge = dmerge %>% 
  dplyr::select(Cultivar, Species, Date, Tag.Number, TotalInflorescences_AB, Flower.Type, Flower.shape, Inflorescence.Type, False.Sepals, Wind, Cloud, TempF, Month, shannon) %>% 
  na.omit() %>% 
  clean_names() 
# note: dplyr::select is needed because MASS package also has a select command and will confuse R since we are loading MASS too 


######################################################
# What model is appropriate? 
######################################################

#Checking Assumptions of linearity 
#is insect abundance distributed normally? 

hist(clean_dmerge$shannon, breaks = 15) #oh no 
qqnorm(clean_dmerge$shannon)
var(clean_dmerge$shannon)
mean(clean_dmerge$shannon)
var(clean_dmerge$shannon)/mean(clean_dmerge$shannon)

#it's not zero inflated, these are true zeroes (when shannon diversity = 0, because we only saw one species, very common at a flower) 
# It's approximately normal, besides the pile up of zeroes at the left hand side 
# We can just use Gaussian 

#####################################
#DATA VISUALIZATION AND INTERACTIONS 
#####################################

# We just did this for the predictor values for the 1.1_ Analysis of abundance, so no need to do this again 

#############################################################
# GENREAL LINEAR MODELS 
# (Gaussian)
#############################################################

#Just included all variables without cultivar 

model = glm(shannon ~ total_inflorescences_ab*flower_type + inflorescence_type + false_sepals + wind + cloud + temp_f + month, data = clean_dmerge, family = gaussian)  
summary(model)

# Only total inflorescences were relevant 
# false sepals don't make sense biologically with our data, let's remove  

model = glm(shannon ~ total_inflorescences_ab*flower_type + inflorescence_type + wind + cloud + temp_f + month, data = clean_dmerge, family = gaussian)  
summary(model)

# model with all variables including cultivar

model = glm(shannon ~ total_inflorescences_ab*flower_type + inflorescence_type + wind + cloud + temp_f + month + cultivar, data = clean_dmerge, family = gaussian)  
summary(model)
rm(model) # cleaning environment

# Unsurprisingly it's just garbage, way too many cultivars to extract meaningful results 
# we could include cultivar as a random effect but that's getting really complex and we do talk about cultivar later by graphing a pca plot
# Let's keep things simple and defensible and omit cultivar and be transparent about it 

# Final model 
# (everything from full model minus the factors that did not make sense biologically and the cultivars) 

model = glm(shannon ~ total_inflorescences_ab*flower_type + wind + cloud + temp_f + month, data = clean_dmerge, family = gaussian)  
summary(model)

# confidence intervals for appendix 

confint(model)

#########################################################
# Assumption checking 
#########################################################

plot(model)  

# something weird's going on with residuals vs fitted but it's OK, especially past 0. the main issue is below  zero, which can't happen anyway for shannon.

#####################################################
# Post hoc tests 
#####################################################

# ----- emmeans ---------# 

# Can't do tukey post-hoc directly for  poisson / quasipoisson 

emm <- emmeans(model, "flower_type")
# Get estimated marginal means from emmeans package 

pairs(emm, adjust = "tukey")
# Pairwise comparisons with Tukey adjustment

# Significant
# Lacy H. arborescens < Lacy H. paniculata ** 
# Lacy H. arborescens <  Mop H. paniculata * 
# Lacy H. paniculata > Mop H. arborescens *** 
# Mop H. arborescens < Mop H. paniculata *** 

# Not significant 
# Lacy H. arborescens vs. Mop H. arborescesns 
# Lacy H. paniculata vs. Mop H. paniculata

pairs_resp <- pairs(emm, type = "response")
pairs_resp 

# getting the confidence intervals 
ci_resp <- confint(pairs_resp)
ci_resp 


# ---------- Post Hoc Tukey ---------- # 

# We can actually do a proper Tukey post-hoc test too 

model = aov(shannon ~ total_inflorescences_ab * flower_type + wind + cloud + temp_f + month, data = clean_dmerge)

TukeyHSD(model, "flower_type")  # Specify the factor you want comparisons for

# I'ts a bit mad about the continuous factors but it ignores them which we want for the  post hoc test 


# Significant 
# Lacy H. paniculata > Lacy H. arborescens ** 
# Mop H. paniculata > Lacy H. arborescens * 
# Mop H. arborescens < Lacy H. paniculata ***
# Mop H. paniculata > Mop H. arborescens *** 

# Non-significant 
# Mop H. arborescens vs. Lacy H. arborescens
# Mop H. paniculata vs. Lacy H. paniculata 

