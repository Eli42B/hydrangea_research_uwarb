############## Supplementary interaction graphs  ################

#-----------------------------load libraries

#Loading libaries
library(jtools)
library(broom)
library(ggstance)
library(interactions)

#-------------------------------Load data

dmerge = read.csv(dmergeFINAL_filepath) #reset the data, we were dividing by plant number for the plots, but for this analysis we don't need to and more data is better. Also, the non-integer numbers will throw off the poisson model because it is count based. 
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
dmerge$Color = as.factor(dmerge$Color)



#------------------------------------------Graphs

#Note: in original code, I checked assumptions as well, but I will leave these out of the code for final graphs 

########################################
#first model with species and interaction 
########################################

model1 = glm(TotalInsects ~ TotalInflorescences_AB*Species + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model1) #inflorescences are significant, month is right on the edge (p = 0.053), species is significant (8.81E-5 Let's keep these though; it matches what we saw in the  plots and what they look like by themselves. 
#cat_plot(model1, pred = Species, modx = TotalInflorescences_AB)
p = interact_plot(model1, pred = Species, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation")) #cat plot is for categorical variables. Since inflorescences is a continuous variable, use interact_plot. Cat plot gives error bars; this gives standard deviations. 
p + 
  labs(title = "Hydrangea paniculata outperformed \n Hydrangea arborescens",
       x = "Species", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
# It's close, but species is not actually interacting with inflorescence count 

########################################
#second model with flower type and interaction
########################################

model2 = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model2) 
p = interact_plot(model2, pred = Flower.Type, modx = TotalInflorescences_AB,
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, lacy panicles shapes are the best \n and mop shapes are the worst for pollinators",
       x = "Hydrangea Shapes", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#there are significant interactions going on between all 3 types (lacy, lacy panicle, mop, mop like panicle) and inflorescence numbers 
