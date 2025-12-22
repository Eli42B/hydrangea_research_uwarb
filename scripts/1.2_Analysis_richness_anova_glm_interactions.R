# ###############################################################################
# # Analysis for Insect Abundance Linearity, Interactions, GLM, ANOVA 
# ###############################################################################

#-------------------------------------------------------------------------------

########################################33
#SETUP 
##########################################
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#organizing data 

dmerge = read.csv("dmergeFINALRichness.csv") #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 

#Loading libaries
#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)
library(ggplot2)


#citation('interactions')
#-------------------------------------------------------------------------------

#Preparing the quasipoisson model 

dmerge = read.csv("dmergeFINALRichness.csv") #reset the data, we were dividing by plant number for the plots, but for this analysis we don't need to and more data is better. Also, the non-integer numbers will throw off the poisson model because it is count based. 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)  #setting up data correctly 

dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship + 1 to fix the zero values and the infinite valuesl that result from zeroes. 

dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)
dmerge$Flower.shape = as.factor(dmerge$Flower.shape)
#-------------------------------------------------------------------------------

####################################### 
# INTERACTIONS BETWEEN FACTORS 
####################################### 

#ARe flower type and inflorescent type interacting? 
model = glm(InsectRichness ~ Flower.Type*Inflorescence.Type, data = dmerge, family = poisson(link = "log"))
summary(model)
#Yes, yes they are 

#Are flower shape and inflorescence count interacting? 
model = glm(InsectRichness ~ Flower.shape*TotalInflorescences_AB, data = dmerge, family = poisson(link = "log"))
summary(model) #nope 

#Is species and number of inflorescences interacting? 
model = glm(InsectRichness ~ TotalInflorescences_AB*Species, data = dmerge, family = poisson(link = "log"))
summary(model) #Yes

#---------------------- Graphing and modeling interactions 

#first model with species and interaction 
model1 = glm(InsectRichness ~ TotalInflorescences_AB*Species + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model1) #inflorescences are significant, month is right on the edge (p = 0.053), species is significant (8.81E-5 Let's keep these though; it matches what we saw in the  plots and what they look like by themselves. 
p = interact_plot(model1, pred = Species, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation")) #cat plot is for categorical variables. Since inflorescences is a continuous variable, use interact_plot. Cat plot gives error bars; this gives standard deviations. 
p + 
  labs(title = "Hydrangea paniculata outperformed \n Hydrangea arborescens",
       x = "Species", 
       y = "Average Insect Richness Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
# It's close, but species is not actually interacting with inflorescence count 

#second model with flower type and interaction
model2 = glm(InsectRichness ~ TotalInflorescences_AB*Flower.shape + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model2) 
p = interact_plot(model2, pred = Flower.shape, modx = TotalInflorescences_AB,
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, lacy panicles shapes are the best \n and mop shapes are the worst for pollinators",
       x = "Hydrangea Shapes", 
       y = "Avg Insects Richness Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#there are significant interactions going on between all 3 types (lacy, lacy panicle, mop, mop like panicle) and inflorescence numbers 

#third model with inflorescent type and interaction  
model3 = glm(InsectRichness ~ TotalInflorescences_AB*Inflorescence.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model3)
p = interact_plot(model3, pred = Inflorescence.Type, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, higher percentages of fertile flowers \n are better for pollinators",
       x = "Categories of Fertile Flower Ratios", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#We need to just rhow out the categories, they don't make biological sense and I'm not confident of how I categorized them, even the literature doesn't strongly agree 

#-------------------------------------------------------------------------------

#############################
# GENREAL LINEAR MODELS 
#############################

#Just included all variables

#Full model (not including cultivar, including Flower Type, not including species) 
model = glm(InsectRichness ~ TotalInflorescences_AB + Flower.shape + Species + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)

#Full model (including cultivar) 
model = glm(InsectRichness ~ TotalInflorescences_AB + Cultivar + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)


#-----------------------ANOVA of flower species and shape (nested) 

#An ANOVA is not necessary for two-factor analysis 

fit = aov(InsectRichness~ Flower.Type, data = dmerge)
summary(fit) #both are highly significant 

#Do we meet requirements for normality? 

#Homogeneity of variences 
resid =  dmerge$TotalInsects - predict.lm(fit)
plot(predict.lm(fit),ylab = "Residuals", xlab = "predicted Y") #actually not too off 

##normality of residuls
stdRes <-rstandard(fit)
#qqplot
qqnorm(stdRes,ylab="Standardized Residuals", xlab="Theoretical Quantiles")
qqline(stdRes, col=2,lwd=2) #not amazing but accceptable
hist(stdRes) #surprisingly good, normal

#post-hoc test
TukeyHSD(fit)
#paniculata is better; lacy is better 

#ANOVA of flower type, not nested 

fit = aov(TotalInsects ~ Flower.Type, data = dmerge)
summary(fit)
TukeyHSD(fit)

#ANOVA for cultivar 
fit = aov(TotalInsects ~ Cultivar, data = dmerge) 
summary(fit)
TukeyHSD(fit)
