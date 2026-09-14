############## Supplementary scatterplots insects vs inflorescences  ################

#-----------------------------load libraries

#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
library(iNEXT)

#-------------------------------Load data

dmerge = read.csv(dmergeFINAL_filepath) 

#Converting data to correct types and taking nat log 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #resetting our data 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log 
dmerge$TotalInflorescences_AB[which(dmerge$TotalInflorescences_AB==-Inf)] = NA #taking the natural log of 0 results in an infinite number, which R throws a fit about, so turn those into NA
dmerge$TotalInflorescences_AB[which(is.nan(dmerge$TotalInflorescences_AB))] = NA 
dmerge = dmerge[dmerge$TotalInflorescences_AB != "NA",]

#------------------------------Graphs 

dev.off()

#Natural log transform for inflorescences looks pretty good too 
plot(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB, xlab = "Natural Log Inflorescence Count",ylab = "Insects Per Day", main = "Plants with More Inflorescences Attracted More Insects Per Day")
lm = abline(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))
text(4,20, "Insects per day = 1.465(logx) - 5.445
     p < 0.001***
     R-squared: 0.176",
     cex = 0.7, pos = 3)

#Scatterplot total insects w/ abiotic factors 

as.numeric(dmerge$Cloud)
as.numeric(dmerge$TempF)

plot(dmerge$TotalInsects ~ dmerge$Cloud, main = "No Noticeable Impact of Cloud Cover On Insects Seen", xlab = "Cloud Cover (Percent)", ylab = "Total Insects Seen Per Day")
plot(dmerge$TotalInsects ~ dmerge$TempF, main = "Temperature (F) Had Effect on Insects Seen Per Day,\n But Was Not Significant", ylab = "Insect Count", xlab = "Temperature (F)") #quadratic interaction peaking in mid 70s 
#we will need to model this quadratically, and then include both the linear and quadratic part of this model in our glm 
#https://www.spsanderson.com/steveondata/posts/2023-11-17/index.html 
quadratic_temp_model <- lm(TotalInsects ~ TempF + I(TempF^2), data = dmerge)
summary(quadratic_temp_model)
#TotalInsects ~ 1.12*TempF + -0.0082(TempF^2) - 31.70
