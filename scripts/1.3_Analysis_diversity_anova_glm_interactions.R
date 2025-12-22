# ###############################################################################
# # Analysis for Insect Diversity Linearity, Interactions, GLM, ANOVA 
# ###############################################################################

#-------------------------------------------------------------------------------

########################################33
#SETUP 
##########################################
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()
#Load data

dmerge = read.csv("dmergeFINAL.csv")

#load libraries

#install.packages(tidyverse)
#install.packages(vegan)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
library(vegan)

#Create the diversity index 
#We're going to use Shannon diversity index because we aren't particularly concerned about a handful of rare species, we just want an overall picture of diversity 
#Shannon diversity index is based on number of species and evenness. It's am easure of hte diversiety of species in a community. 
#Higher H = higher diversity of species in a community
# Lower H means lower diversity 
# H = 0 means no diversity, the community only has one species 

#Visualized data with boxplots and scatterplots 

dmerge = read.csv("dmergeFINAL.csv")
dmerge$shannon = diversity(dmerge[,29:93], index = "shannon", MARGIN = 1, base = exp(1))
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #make sure that total inflorescences is seen as a number 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log + 1
#dmerge = dmerge[-143,] #there's a single day where shannon diversity index calculated as NA, not sure why, let's just remove it 
dmerge$shannon

#Loading libaries for interactions 
#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)

#citation('interactions')

#############################
#Checking model assumptions
#############################

#checking assumptions -- is insect diversity distributed normally? 

hist(dmerge$shannon) #actually pretty much yes besides a lot of zeroes 
qqnorm(dmerge$shannon) #that ain't good 
#for poisson, variance = mean. Check assumptions.  
var(dmerge$shannon)/mean(dmerge$shannon)  #Ratio of variance (0.512) is less than 1, so we are underdispersed. Kind of unusual for Poisson. Leaning more towards a normal distribution or a quasipoisson model instead. 

#I'm not sure what to do about this. It actually looks normally distributed besides the zeroes. I don't think Poisson would be appropriate because this is not count data? And the zeroes aren't being created by a separate process, so we shouldn't do a zero inflated model. 

#For lack of a better fit, I'm leaning towards a normal GLM (Gaussian) 

#-------------------------------------------------------------------------------

####################################### 
# INTERACTIONS BETWEEN FACTORS 
####################################### 

#Are flower shape and inflorescence count interacting? 
model = glm(shannon ~ Flower.shape*TotalInflorescences_AB, data = dmerge, family = gaussian(link = "identity"))
summary(model) #nope 

#Is species and number of inflorescences interacting? 
model = glm(shannon ~ TotalInflorescences_AB*Species, data = dmerge, family = gaussian(link = "identity"))
summary(model) #NO 

#---------------------- Graphing and modeling interactions 

#first model with species and interaction 
model1 = glm(shannon ~ TotalInflorescences_AB*Species + TempF+ Month, data = dmerge, family = gaussian(link = "identity"))
summary(model1) #inflorescences are significant, month is right on the edge (p = 0.053), species is significant (8.81E-5 Let's keep these though; it matches what we saw in the  plots and what they look like by themselves. 
p = interact_plot(model1, pred = Species, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation")) #cat plot is for categorical variables. Since inflorescences is a continuous variable, use interact_plot. Cat plot gives error bars; this gives standard deviations. 
p + 
  labs(title = "Hydrangea paniculata outperformed \n Hydrangea arborescens",
       x = "Species", 
       y = "Average Insect Diversity Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
# species is not actually interacting with inflorescence count 

#second model with flower shape and interaction
model2 = glm(shannon ~ TotalInflorescences_AB*Flower.shape + TempF+ Month, data = dmerge, family = gaussian(link = "identity"))
summary(model2) 
p = interact_plot(model2, pred = Flower.shape, modx = TotalInflorescences_AB,
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, lacy panicles shapes are the best \n and mop shapes are the worst for pollinators",
       x = "Hydrangea Shapes", 
       y = "Avg Insects diversity Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#no significant interactions 

#-------------------------------------------------------------------------------

##############################
# FINAL MODELS 
##############################

#These are the best ones 

#Full model (not including cultivar, including Flower shape and species) 
model = glm(shannon ~ TotalInflorescences_AB + Flower.shape + Species + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = gaussian(link = "identity"))
summary(model)

#Full model (including cultivar) 
model = glm(shannon ~ TotalInflorescences_AB + Cultivar + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = gaussian(link = "identity"))
summary(model)

#-----------------------ANOVA of flower species and shape (nested) 
#An ANOVA is not necessary for two-factor analysis 

fit = aov(shannon~ Flower.Type, data = dmerge)
summary(fit) #both are highly significant 

#Do we meet requirements for normality? 

##normality of residuls
stdRes <-rstandard(fit)
#qqplot
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2) #not amazing but accceptable
hist(stdRes) #OK, bit tailed 

#post-hoc test
TukeyHSD(fit)
#paniculata is better; lacy is better 


#ANOVA for cultivar 
fit = aov(TotalInsects ~ Cultivar, data = dmerge) 
summary(fit)
TukeyHSD(fit)
