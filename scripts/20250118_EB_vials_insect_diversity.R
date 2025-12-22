###############################################################################

####-----------SETUP-------------------------------
citation()
###############################################################################

#Getting directories sorted 
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

dmerge$shannon = diversity(dmerge[,30:93], index = "shannon", MARGIN = 1, base = exp(1))


###############################################################################

####-----------AGGREGATION-------------------------------

###############################################################################

#We don't care about aggregation here because we don't need the stacked bargraphs, we just want the model and the scatterplots 


#Aggregating by cultivar 

#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")

#Create a new merged dataframe by cultivar. Don't divide by plant; that doesn't matter in this case because we're looking at an average measure of diversity per day.  
dmerge7 = dmerge %>% 
  group_by(Cultivar)%>% 
  summarise(shannon = mean(shannon, na.rm = TRUE)) %>% 
  select(Cultivar,shannon)
print(dmerge7, n = 25)

###############################################################################

####-----------PLOTS-------------------------------

###############################################################################

#-------------------STACKED BARCHARTS 
#We don't care about stacked barcharts for diversity so we can ignore this 
#Resources

#--------------------------------BOXPLOTS

#note: even though zero data makes the boxplot look  messy, make sure to include it 

#Boxplot comparing hydrangea species using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Species, c("Hydrangea arborescens","Hydrangea paniculata"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = shannon, fill = Species, binwidth = 5.0)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Species", 
       y = "Insect Diversity",
       title = "Hydrangea Species Impact on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing flower type using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Flower.Type, c("Lacy","Mop","Lacy panicle","Mop-like panicle"))
dmerge = dmerge[dmerge$Flower.Type != "#N/A",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = shannon, fill = Flower.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Flower Type", 
       y = "Insect Diversity",
       title = "Hydrangea Flower Type Impact on Pollinator Diversity")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing number of false sepals using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$False.Sepals, c("3","4","5","8"))
dmerge = dmerge[dmerge$False.Sepals != "NA",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = shannon, fill = False.Sepals)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Number of False Sepals", 
       y = "Insect Diversity",
       title = "False Sepals Impact on Pollinators (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 
#inconclusive 


#Boxplot comparing month using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Month, c("Jul","Aug"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = shannon, fill = Month)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Month", 
       y = "Insect Diversity",
       title = "Impact of Month on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing rain using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Rain, c("0","1"))
dmerge = dmerge[dmerge$Rain != "NA",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = shannon, fill = Rain)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Rain: No (0) Yes (1)", 
       y = "Insect Diversity",
       title = "Impact of Rain on Pollinators")
p
# Unsurprisingly, rain had a huge negative impact on pollinators. However, we only did one survey when it was raining, which isn't enough to guarantee this. Because of the low rain sample size, let's just not model it in the glm. 

#-------------------SCATTERPLOTS 

#organizing data 

dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #make sure that total inflorescences is seen as a number 


#Inflorescence count compared to insect abundance 

#linear plot 
plot(dmerge$shannon ~ dmerge$TotalInflorescences_AB)
lm = abline(lm(dmerge$shannon ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$shannon ~ dmerge$TotalInflorescences_AB))
text(80000,30, "Total Insects = 1.031E-5x + 0.8516,
     p < 0.0001***
     R-squared: 0.068",
     cex = 0.5, pos = 3)
#There's a pattern but it's super tiny numbers because it's incrementing by a single inflorescence 

#Natural log transform for inflorescences looks pretty good too 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log + 1  

#dmerge$TotalInflorescences_AB[which(is.nan(dmerge$TotalInflorescences_AB))] = NA 
#dmerge = dmerge[dmerge$TotalInflorescences_AB != "NA",]
plot(dmerge$shannon ~ dmerge$TotalInflorescences_AB, xlab = "Inflorescence Count",ylab = "Daily Insect Diversity (Shannon Index)", main = "Plants with More Inflorescences Were Associated \nwith More Diverse Assemblages of Insects")
lm = abline(lm(dmerge$shannon ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))
text(1.6,1.6, "Insect Diversity = 1.106(x) - 2.387
     p < 0.0001***
     R-squared: 0.195",
     cex = 0.7, pos = 3)
#Inflorescence axis: 1 | 5 |50 | 400 | 3,000 | 22,000 | 150,000 |  

#Scatterplot total insects w/ abiotic factors 

as.numeric(dmerge$Cloud)
as.numeric(dmerge$TempF)

plot(dmerge$shannon ~ dmerge$Cloud, main = "No Noticeable Impact of Cloud Cover On Insect Diversity", xlab = "Cloud Cover (Percent)", ylab = "Insect Diversity")
plot(dmerge$shannon ~ dmerge$TempF, main = "No noticeable Temperature (F) Effect on Insect Diversity", ylab = "Insect Diversity", xlab = "Temperature (F)") #quadratic interaction peaking in mid 70s 

###############################################################################

####-----------ANALYSES-------------------------------

###############################################################################

#Resetting our data 
dmerge = read.csv("dmergeFINAL.csv")
dmerge$shannon = diversity(dmerge[,29:93], index = "shannon", MARGIN = 1, base = exp(1))
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #make sure that total inflorescences is seen as a number 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log + 1
dmerge = dmerge[-143,] #there's a single day where shannon diversity index calculated as NA, not sure why, let's just remove it 
dmerge$shannon

#Loading libaries
#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)

#citation('interactions')

#checking assumptions -- is insect diversity distributed normally? 

hist(dmerge$shannon) #actually pretty much yes besides a lot of zeroes 
qqnorm(dmerge$shannon) #that ain't good 
var(dmerge$shannon)/mean(dmerge$shannon) #won't calculate 

#I'm not sure what to do about this. It actually looks normally distributed besides the zeroes. I don't think Poisson would be appropriate because this is not count data? And the zeroes aren't being created by a separate process, so we shouldn't do a zero inflated model. 

#For lack of a better fit, I'm leaning towards either quasipoisson or a normal glm. 

#for poisson, variance = mean. Check assumptions.  
var(dmerge$shannon)/mean(dmerge$shannon)  #Ratio of variance (0.512) is less than 1, so we are underdispersed. Kind of unusual for Poisson. Leaning more towards a normal distribution or a quasipoisson model instead. 

#Preparing the quasipoisson model 
dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)

#We already know what factors may be interacting becasue we tested for richness and abundance already, so we can skip this.  

#------------------------------- penultimate Models 

#Full model (not including cultivar, including Flower Type, not including species)  - quasipoisson 
model = glm(shannon ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Species has to be in a separate model than flower type becuase they are related -- panicle flower shapes are by definition panicle hydrangeas. And these models require independent variables. Note that we are including an interaction between flower type and inflorescences because preliminary graphs showed an interaction between the two 

#Full model (not including cultivar, including Flower Type, not including species) - Gaussian  
model = glm(shannon ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = gaussian(link = "identity"))
summary(model)
#OK, we take it back, Gaussian is indeed better than Poisson  

#Full model (not including cultivar, including species, not including Flower Type ) 
model = glm(shannon ~ TotalInflorescences_AB + Species + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#The flower type explained things a bit better, not surprising since it includes species in a way and is more detailed, but both are reasonable

#Full model (not including cultivar, including species, not including Flower Type + interaction  ) 
model = glm(shannon ~ TotalInflorescences_AB*Species, data = dmerge, family = gaussian(link = "identity"))
summary(model)

#Full model (not including cultivar, including flower type, not including Flower Type + interaction  ) 
model = glm(shannon ~ TotalInflorescences_AB*Flower.Type, data = dmerge, family = gaussian(link = "identity"))
summary(model)

#Full model (not including cultivar, not including total inflroescences, including Flower Type) 
model = glm(shannon ~ Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#We lose a lot of deviance, we should keep flower type and inflorescences in same model 

#Full model (including cultivar. Drop species and inflorescences since cultivar is related to both of these) 
model = glm(shannon ~ Wind + Rain + Cloud + TempF + Month + Cultivar, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#No cultivars have p < 0.05, just not enough replicates 

#------------------------------- Final Models 
#These are the best ones 

#Full model (not including cultivar, including Flower Type, not including species) 
model = glm(shannon ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = gaussian(link = "identity"))
summary(model)
#Species has to be in a separate model than flower type becuase they are related -- panicle flower shapes are by definition panicle hydrangeas. And these models require independent variables. Note that we are including an interaction between flower type and inflorescences because preliminary graphs showed an interaction between the two 
#Result: only total inflorescences matter, flower type does not 

#Full model (not including cultivar, including species, not including Flower Type ) 
model = glm(shannon ~ TotalInflorescences_AB + Species + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = gaussian(link = "identity"))
summary(model)
#Species is related to flower type so we dropped flower type in this model, but we are not modeling an interaction because prelminary graphs did not show an interaction withs pecies only and inflorescences 
#The flower type explained things a bit better, not surprising since it includes species in a way and is more detailed, but both are reasonable 
#Result, inflorescences and species matter 


##--------------------Species accumulation curve -- that's going to be an insect richness quesiton, so it's not in this R file  

