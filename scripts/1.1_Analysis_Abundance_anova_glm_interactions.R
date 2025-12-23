# ###############################################################################
# # Analysis for Insect Abundance Linearity, Interactions, GLM, ANOVA 
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


#citation('interactions')
#-------------------------------------------------------------------------------

###############################
#Checking Assumptions of linearity 
###############################
#checking assumptions -- is insect abundance distributed normally? 

hist(dmerge$TotalInsects, breaks = 15) #oh no 
qqnorm(dmerge$TotalInsects)
var(dmerge$TotalInsects)/mean(dmerge$TotalInsects)
#yep, does not look like a normal distribution. There are a lot of models we can use to sort through this, including: 
# Poisson: often used, esp. for count models 
# Negative binomial: variant of poisson. Used for over-dispersed count data (var > mean)
# zero-inflated regression model: attempt to account for extra zeroes
# OLS regression: log transform it 

#it's not zero inflated, these are true zeroes 
# It's overdispersed 

#for poisson, variance = mean. Check assumptions.  
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 

hist(dmerge$TotalInsects, breaks = 15)
var(dmerge$TotalInsects) #var = 40.24
mean(dmerge$TotalInsects) #mean = 6.16
var(dmerge$TotalInsects)/mean(dmerge$TotalInsects) #Ratio of variance (6.55) is way larger than 2, so we are overdispersed, very overdispersed. We will go with Negative Binomial instead. I considered quasipoisson but the overdispersion is too much. One could also consider zero-inflated negative binomial but we don't have a clear biological reason for 'non visited flowers' that would cause extra zeroes. so we will keep with NB. 

#####################################################
# Preparing the Negative Binomial model 
#####################################################

dmerge = read.csv(glm_abundance_filepath) 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship, plus 1 so we don't get infinite values  
dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)
dmerge$Color = as.factor(dmerge$Color)
dmerge$Flower.shape = as.factor(dmerge$Flower.shape)

clean_dmerge = dmerge %>% 
  dplyr::select(Cultivar, Species, Date, Tag.Number, TotalInsects, TotalInflorescences_AB, Flower.Type, Flower.shape, Inflorescence.Type, Color, False.Sepals, Wind, Cloud, TempF, Month) %>% 
  na.omit() %>% 
  clean_names() 
# note: dplyr::select is needed because MASS package also has a select command and will confuse R since we are loading MASS too 

#####################################
#DATA VISUALIZATION
#####################################

plot(clean_dmerge$species, clean_dmerge$total_inflorescences_ab, 
     ylab = "log # of inflorescences with cleaned data", 
     xlab = "Species", 
     main = "boxplot inflorescences vs spp, cleaned data",
     col = "grey")

plot(dmerge$Species, dmerge$TotalInflorescences_AB, 
     ylab = "log # of inflorescences", 
     xlab = "Species", 
     main = "boxplot infloresences vs spp, full data",
     col = "grey")

# so looks like cleaning hte data isn't costing us much
# looks like h. paniculata have a bit more inflorescences but it's not a particularly clear relatinship 


#-------------------------------------------------------------------

####################################### 
# INTERACTIONS BETWEEN FACTORS 
####################################### 

# -------- flower spp + shape x inflorescnces  -------------# 

# Let's go back to the basics and start with VIF 

mod_test_vif = lm(total_insects ~ flower_type + date + total_inflorescences_ab, data = clean_dmerge) 
# vif doesn't care about the y value, only the predictors as long as they match, and it's cleaner to use a linear model, so we will do this for now though our final models will be negative binomial 

vif(mod_test_vif)
# actually not that bad 

mod_test_vif_nodate = lm(total_insects ~ flower_type + total_inflorescences_ab, data = clean_dmerge) 
vif(mod_test_vif_nodate)
# alright, flower type is not particularly collinear to total inflorescences 
# reducing hte terms to find the problem pairs. A correlation table will also do this but I find it more intuitive to just drop them manually. 

# cleaning our environment a bit 
rm(mod_test_vif)
rm(mod_test_vif_nodate)
rm(model) # cleaning environment a bit 

# Now let's try it by comparing models 

model = glm.nb(total_insects ~ flower_type + total_inflorescences_ab, data = clean_dmerge)
summary(model)

model = glm.nb(total_insects ~ flower_type*total_inflorescences_ab, data = clean_dmerge)

summary(model) 

rm(model)

# huh, ok now the interaction is significant 

# -------- Spp of hydrangea x inflorescnces  -------------# 

# Is hydrangea spp and inflorescence count interacting? 
model = glm.nb(total_insects ~ species*total_inflorescences_ab, data = clean_dmerge)
summary(model) 
# yep, they're interacting  

# -------- Flower shape x inflorescnces  -------------# 

# Is hydrangea flower shape (regardless of spp) and inflorescence count interacting? 
model = glm.nb(total_insects ~ flower_shape*total_inflorescences_ab, data = clean_dmerge)
summary(model) 
# Nope, flower shape doesn't matter, huh 
rm(model) # removing the model from the enviroinment 

###########################################################
#    Graphing interactions 
###########################################################

# inflorescences x  spp and interaction 
# ---------------------------------------------------------

model = glm.nb(total_insects ~ total_inflorescences_ab*species, data = clean_dmerge) 
p = interact_plot(model, 
                  pred = species, 
                  modx = total_inflorescences_ab, 
                  legend.main = "Inflorescence Count Per Plant")


p + 
  labs(title = "Hydrangea paniculata outperformed \n Hydrangea arborescens",
       x = "Species", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
# It's close, but species is not actually interacting with inflorescence count 


#cat plot is for categorical variables. Since inflorescences is a continuous variable, use interact_plot. Cat plot gives error bars; this gives standard deviations. 

# flower shape x  spp and interaction 
# ---------------------------------------------------------

model = glm.nb(total_insects ~ total_inflorescences_ab*flower_shape, data = clean_dmerge) 
p = interact_plot(model, 
                  pred = flower_shape, 
                  modx = total_inflorescences_ab, 
                  legend.main = "Inflorescence Count Per Plant")

p + 
  labs(title = "lacy is better than mop, \n esp among high performing hydrangeas",
       x = "Species", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")

# flower type x  spp and interaction 
# ---------------------------------------------------------

model = glm.nb(total_insects ~ total_inflorescences_ab*flower_type, data = clean_dmerge) 
p = interact_plot(model, 
                  pred = flower_type, 
                  modx = total_inflorescences_ab, 
                  legend.main = "Inflorescence Count Per Plant")

p + 
  labs(title = "high performing lacy hydrangeas were different",
       x = "Species", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
rm(model) # cleaning our environment 


#############################################################
# GENREAL LINEAR MODELS 
# (Negative Binomial)
#############################################################

#Just included all variables without cultivar 

model = glm.nb(total_insects ~ total_inflorescences_ab*flower_type + inflorescence_type + color + false_sepals + wind + cloud + temp_f + month, data = clean_dmerge) 
summary(model)

# Only total inflorescences, flower type, and their interaction were relevant
# Pink, pink-white, and white were relevant, but there's overlap so they don't mean anything biologically 

# model with all variables including cultivar

model = glm.nb(total_insects ~ total_inflorescences_ab*flower_type + inflorescence_type + color + false_sepals + wind + cloud + temp_f + month + cultivar, data = clean_dmerge) 
summary(model)
rm(model) # cleaning environment

# Unsurprisingly it's just garbage, way too many cultivars to extract meaningful results 
# we could include cultivar as a random effect but that's getting really complex and we do talk about cultivar later by graphing a pca plot
# Let's keep things simple and defensible and omit cultivar and be transparent about it 

# Final model 
# (everything from full model minus the factors that did not make sense biologically and the cultivars) 

model = glm.nb(total_insects ~ total_inflorescences_ab*flower_type + wind + cloud + temp_f + month, data = clean_dmerge) 

summary(model)

# Preparing the final model for output in the paper 
-------------------------------------------------------
summary_model = summary(model)             # save summary
coef_table = summary_model$coefficients    # extract coefficients

estimates = coef_table[,"Estimate"]        # get the estimates 
exp_est = exp(estimates)              # take the exp(estimates)

ci_log <- confint(model)               # est. confidence intervals
ci_irr <- exp(ci_log)                  # extract esp(conf int)
ci_irr


#########################################################
# Assumption checking 
#########################################################

# Negative binomial doesn't have the same assumptions of normality as linear models 
# However, we can still check for outliers, leverage, adn similar
# in addition, we are reasonably certain we have accounted for multicollinearity with our VIF and interaction plots from earlier 

plot(model)  
# yep, these look acceptable 

#####################################################
# Post hoc tests 
#####################################################

# Can't do tukey post-hoc directly for  negative binomial 

emm <- emmeans(model, "flower_type")
# Get estimated marginal means from emmeans package 

pairs(emm, adjust = "tukey")
# Pairwise comparisons with Tukey adjustment

# ***  Significant ***
# Lacy H. arborescens <  Lacy H. paniculata**
# Lacy H. paniculata > Mop H. arborescens ***
# Lacy H. paniculata > Mop H. paniculata ** 
# Mop H. arborescens < Mop H. paniculata ***
 
# *** Not significant *** 
# Lacy H. arborescens vs. Mop H. arborescens 
# Lacy H. arborescens vs. Mop H. paniculata 

# Preparing the post-hoc test for the paper 
-------------------------------------------------------

# getting the exp(est) 
pairs_resp <- pairs(emm, type = "response")
pairs_resp 

# getting the confidence intervals 
ci_resp <- confint(pairs_resp)
ci_resp
