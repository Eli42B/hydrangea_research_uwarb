############## Generating graphs for comparative curves  ################

#Setup
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

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


dmerge = read.csv("dmergeFINAL.csv") #reset the data, we were dividing by plant number for the plots, but for this analysis we don't need to and more data is better. Also, the non-integer numbers will throw off the poisson model because it is count based. 
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

#------------------------------Graph

model2 = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
p = interact_plot(model2, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "All Flower Types Attracted More Insects\nAs Fertile Flowers Increased, but at Different Rates",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")

