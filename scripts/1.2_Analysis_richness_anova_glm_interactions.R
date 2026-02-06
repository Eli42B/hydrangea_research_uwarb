# ###############################################################################
# # Analysis for Insect Richness  Linearity, Interactions, GLM, ANOVA 
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
library(MASS)         # for negative binomial models 
library(emmeans)      # for post-hoc analysis of NB models 

#organizing data 
dmerge = read.csv(glm_richness_filepath, header = TRUE) 

dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) 
#take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship, plus 1 so we don't get infinite values  

dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)
dmerge$Flower.shape = as.factor(dmerge$Flower.shape)

# removing silver leaf 
dmerge = dmerge %>% 
  filter(Cultivar != "Silver Leaf") 

clean_dmerge = dmerge %>% 
  dplyr::select(Cultivar, Species, Date, Tag.Number, InsectRichness, TotalInflorescences_AB, Flower.Type, Flower.shape, Inflorescence.Type, False.Sepals, Wind, Cloud, TempF, Month) %>% 
  na.omit() %>% 
  clean_names() 
# note: dplyr::select is needed because MASS package also has a select command and will confuse R since we are loading MASS too


######################################################
# What model is appropriate? 
######################################################

#Checking Assumptions of linearity 
#is insect abundance distributed normally? 

hist(dmerge$InsectRichness, breaks = 15) #oh no 
qqnorm(dmerge$InsectRichness)
var(dmerge$InsectRichness)
mean(dmerge$InsectRichness)
var(dmerge$InsectRichness)/mean(dmerge$InsectRichness)
# Var > mean, but not by that much  

#it's not zero inflated, these are true zeroes 
# It's overdispersed 
#for poisson, variance = mean. 
#Let's do quasi-poisson since it's a bit larger but not huge  


####################################### 
# INTERACTIONS BETWEEN FACTORS 
####################################### 

# -------- flower type x inflorescnces  -------------# 

model = glm.nb(insect_richness ~ flower_type + total_inflorescences_ab, data = clean_dmerge)
summary(model)

model = glm.nb(insect_richness ~ flower_type*total_inflorescences_ab, data = clean_dmerge)
summary(model) 
# the interaction is not significant 

rm(model)

# -------- hydrangea spp  x inflorescnces  -------------#

model = glm.nb(insect_richness ~ species*total_inflorescences_ab, data = clean_dmerge)
summary(model) 
# nope not interacting, though it's close to significant


# -------- Flower shape x inflorescnces  -------------# 

model = glm.nb(insect_richness ~ flower_shape*total_inflorescences_ab, data = clean_dmerge)
summary(model) 
# Nope, flower shape doesn't matter, huh 

rm(model) # removing the model from the enviroinment 

#############################################################
# GENREAL LINEAR MODELS 
# (quasi-Poisson)
#############################################################

#Just included all variables without cultivar 

model = glm(insect_richness ~ total_inflorescences_ab + flower_type + inflorescence_type + false_sepals + wind + cloud + temp_f + month, data = clean_dmerge, family = quasipoisson(link = "log"))  
summary(model)

# false sepals and color don't make sense biologically with our data, let's remove  

# model with all relevant variables including cultivar

model = glm(insect_richness ~ total_inflorescences_ab + flower_type + inflorescence_type + wind + cloud + temp_f + month + cultivar, data = clean_dmerge, family = quasipoisson(link = "log"))  
summary(model)
rm(model) # cleaning environment

# Unsurprisingly it's just garbage, way too many cultivars to extract meaningful results 
# we could include cultivar as a random effect but that's getting really complex and we do talk about cultivar later by graphing a pca plot
# Let's keep things simple and defensible and omit cultivar and be transparent about it 

# Final model 
# (everything from full model minus the factors that did not make sense biologically and the cultivars) 

model = glm(insect_richness ~ total_inflorescences_ab + flower_type + wind + cloud + temp_f + month, data = clean_dmerge, family = quasipoisson(link = "log"))  
summary(model)


# Preparing the final model for output in the paper 
# -------------------------------------------------------

summary_model = summary(model)             # save summary
coef_table = summary_model$coefficients    # extract coefficients

estimates = coef_table[,"Estimate"]        # get the estimates 
exp_est = exp(estimates)              # take the exp(estimates) aka IRR 
exp_est 

summary(model)$coefficients[, "Std. Error"]
# SE(log) 
# how variable would our coefficients be if we repeatedly sampled? 

ci_log <- confint(model)               # est. confidence intervals
ci_irr <- exp(ci_log)                  # extract esp(conf int)
ci_irr                                # CIs for IRR 

#########################################################
# Assumption checking 
#########################################################

plot(model)  # residuals vs leverage isn't great but I can accept 

#####################################################
# Post hoc tests 
#####################################################

# Can't do tukey post-hoc directly for  poisson / quasipoisson 

emm <- emmeans(model, "flower_type")
# Get estimated marginal means from emmeans package 

pairs(emm, adjust = "tukey")
# Pairwise comparisons with Tukey adjustment

# Significant 
# Lacy H. arborescens vs. Lacy H. paniculata *
# Lacy H. paniculata vs. Mop H. arborescens *** 
# Lacy H. paniculata vs. Mop H. paniculata * 
# Mop H. arborescens vs. Mop H. paniculata *** 

# Not significant 
# Lacy h. arborescens vs. Mop H. arborescens 
# Lacy H. arborescens vs. Mop H. paniculata 

# Preparing the post-hoc test for the paper 
# -------------------------------------------------------
# getting the exp(est) 

pairs_resp <- pairs(emm, type = "response")
pairs_resp 

# getting the confidence intervals 
ci_resp <- confint(pairs_resp)
ci_resp
