#################################################
# Generating graphs for comparative curves
#################################################

#-----------------------------load libraries

#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)
#citation('interactions')
library(tidyverse)

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

dmergeRichness = read.csv(glm_richness_filepath) # also getting it for richness 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)  #setting up data correctly 

dmergeRichness$TotalInflorescences_AB = log(dmergeRichness$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship + 1 to fix the zero values and the infinite valuesl that result from zeroes. 

dmergeRichness$Rain = as.factor(dmergeRichness$Rain)
dmergeRichness$Wind = as.numeric(dmergeRichness$Wind)
dmergeRichness$Cloud = as.numeric(dmergeRichness$Cloud)
dmergeRichness$TempF = as.numeric(dmergeRichness$TempF)
dmergeRichness$Month = as.factor(dmergeRichness$Month)
dmergeRichness$Cultivar = as.factor(dmergeRichness$Cultivar)
dmergeRichness$Species = as.factor(dmergeRichness$Species)
dmergeRichness$Flower.Type = as.factor(dmergeRichness$Flower.Type)
dmergeRichness$Color = as.factor(dmergeRichness$Color)

# calculating shannon diversity 
dmerge$shannon = diversity(dmerge[,29:96], index = "shannon", MARGIN = 1, base = exp(1))

dmerge = dmerge[-178,] #there's a single day where shannon diversity index calculated as NA, not sure why, let's just remove it 
dmerge$shannon

#------------------------------Graph for insect abundance 

par(mfrow = c(2, 2))

model1 = glm.nb(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge)
p = interact_plot(model1, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "Insect Abundance Increased as Fertile Flowers Increased, \nBut at Different Rates",
  subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")

#------------------------------Graph for insect richness 

model2 = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmergeRichness, family = quasipoisson(link = "log"))
p = interact_plot(model2, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "Insect Richness Increased as Fertile Flowers Increased, \nBut at Different Rates",
       subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Number of Unique Taxa Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")

#------------------------------Graph for insect diversity  

model3 = glm(shannon ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = gaussian)
p = interact_plot(model3, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "Insect Diversity Increased as Fertile Flowers Increased, \nAt Consistent Rates",
       subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Shannon Diversity Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
