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

#d_vials<-read.csv("InsectData_Cleaned_1.csv")
#d_vials$Cerambycidae = as.numeric(d_vials$Cerambycidae)
#d_visual<- read.csv("VisualSurveys.csv")
dmerge = read.csv("dmergeFINAL.csv")

#load libraries

#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)

#Generating an aggregated .csv file 

#d_vials2 = d_vials[,4:50]
#d_vials$sum = rowSums(d_vials2)
#write.csv(d_vials,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_vials.csv", row.names = TRUE)
#d_none = d_visual[d_visual$NoVialSample == 1,] #create a separate dataframe of null values 
#dmerge <- merge(d_vials,d_visual,by=c("Date","Tag.Number"))
#OK the  merge worked but it removed all the rows where we collected information but didn't collect a vial
#write.csv(dmerge,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/dmerge.csv", row.names = TRUE)

###############################################################################

####-----------AGGREGATION-------------------------------

###############################################################################

#Aggregating by cultivar 

#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")

#Create a new merged dataframe by cultivar 
dmerge7 = dmerge %>% 
  group_by(Cultivar)%>% 
  summarize(TotalSpiders = sum(Araneae),
            TotalBeetles = sum(across(c(Chauliognathus:Scarabaeoidea))),
            TotalFlies = sum(across(c(Diptera:Ulidiidae))),
            TotalTrueBugs = sum(across(c(Hemiptera:Reduviidae))), 
            TotalHymenoptera = sum(across(c(Hymenoptera:Dolichovespula))),
            TotalButterflies = sum(across(c(Lepidoptera:Vanessa.atalanta)))) 
print(dmerge7, n = 25)

dmerge7 = dmerge7[-1,] #remove the NA row 
print(dmerge7, n = 25)

#OK this is looking good but we need to also divide it by how many plants we have of each cultivar 

#How many plants of each cultivar do we have? (thank you Jude)
tag_test = dmerge %>% 
  group_by(Cultivar) %>%
  summarise(num_plants = n_distinct(Tag.Number)) 
print(tag_test, n = 15)

#create a new aggregated dataframe that divides counts by the number of plants 
dAgg2 = dmerge7 %>% 
  left_join(tag_test,
            by = "Cultivar") %>% 
  mutate(
    RelAbundanceSpiders = TotalSpiders/num_plants,
    RelTotalBeetles = TotalBeetles/num_plants,
    RelTotalFlies = TotalFlies/num_plants,
    RelTotalTrueBugs = TotalTrueBugs/num_plants,
    RelTotalHymenoptera = TotalHymenoptera/num_plants,
    RelTotalButterflies = TotalButterflies/num_plants) %>% 
  select(
    Cultivar, 
    RelTotalBeetles,
    RelTotalFlies,
    RelTotalTrueBugs,
    RelTotalHymenoptera,
    RelTotalButterflies)
print(dAgg2, n = 15, na.print = "")

###############################################################################

####-----------PLOTS-------------------------------

###############################################################################

#-------------------STACKED BARCHARTS 

#Resources

#https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/ 

#Notes 

#To create STACKED barcharts in ggplot2, we need the counts categorized by insect type

#Load libraries
library(ggplot2)

#Getting the data organized for a stacked barchart

#export DAgg2 
#write.csv(dAgg2, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/backup data/DAgg2.csv",row.names = TRUE)
#using DAgg2, created a csv file named DAgg3 inside of 'data in use' folder formatted to work well for a stacked bargraph in ggplot 3 
#It would actually be cleaner to do all our data manipulation and creation of dataframes within R, but I don't know how to offhand, so Excel is faster for now 
DAgg3 = read.csv("DAgg3.csv")

#Stacked Barchart by Bug Type 

ggplot(data = DAgg3,
       aes(x = Cultivar, y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

vials_p1 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p1
vials_p1 + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance by Taxa") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#Descending ggplot2 by hydrangea species 
vials_p2 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p2
vials_p2 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Abundance by Hydrangea  Species") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#Descending ggplot2 by flower type 
vials_p3 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Flower.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip()
vials_p3
vials_p3 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Abundance by Flower Type") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#Descending ggplot2 by inflorescence type 
vials_p4 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Inflorescence.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip()
vials_p4
vials_p4 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Abundance by Inflorescence Type") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#--------------------------------BOXPLOTS

#note: even though zero data makes the boxplot look  messy, make sure to include it 

#organizing data 
dmerge = dmerge[dmerge$Inflorescence.Type != "#N/A",] #remove N/As 

#Boxplot comparing dense, half-open, open, and very dense using non-aggregated data (more accurate). Though, after comparing them, they seem to pretty much say the same thing. 

dmerge$group = factor(dmerge$Inflorescence.Type, c("Open","Half-open","Dense","Very dense"))
p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Inflorescence.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Daily Abundance of Insect Visitors/plant",
       title = "Inflorescence Type Impact on Pollinators",
       subtitle = "Open has highest percentage of fertile florets; very dense has the lowest percentage")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing hydrangea species using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Species, c("Hydrangea arborescens","Hydrangea paniculata"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Species)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Species", 
       y = "Daily Abundance of Insect Visitors per plant",
       title = "Hydrangea Species Impact on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing flower type using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Flower.Type, c("Lacy","Mop","Lacy panicle","Mop-like panicle"))
dmerge = dmerge[dmerge$Flower.Type != "#N/A",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Flower.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Flower Type", 
       y = "Daily Abundance of Insect Visitors per plant",
       title = "Hydrangea Flower Type Impact on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing number of false sepals using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$False.Sepals, c("3","4","5","8"))
dmerge = dmerge[dmerge$False.Sepals != "NA",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = False.Sepals)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Number of False Sepals", 
       y = "Daily Abundance of Insect Visitors per plant",
       title = "False Sepals Impact on Pollinators (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 
#inconclusive 


#Boxplot comparing month using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Month, c("Jul","Aug"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Month)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Month", 
       y = "Daily Abundance of Insect Visitors per plant",
       title = "Impact of Month on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing rain using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Rain, c("0","1"))
dmerge = dmerge[dmerge$Rain != "NA",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Rain)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Rain: No (0) Yes (1)", 
       y = "Insect Visitors per plant per day",
       title = "Impact of Rain on Pollinators")
p
# Unsurprisingly, rain had a huge negative impact on pollinators. However, we only did one survey when it was raining, which isn't enough to guarantee this. Because of the low rain sample size, let's just not model it in the glm. 

#-------------------SCATTERPLOTS 

#organizing data 

dmerge = read.csv("dmergeFINAL.csv") #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #make sure that total inflorescences is seen as a number 


#Inflorescence count compared to insect abundance 

#linear plot 
plot(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB)
lm = abline(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))
text(80000,30, "Total Insects = 0.9312E-5x + 5.198,
     p < 0.001***
     R-squared: 0.084",
     cex = 0.5, pos = 3)

#sqrt plot don't look great if take square root of insects 
dmerge$TotalInsects = sqrt(dmerge$TotalInsects)
plot(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB)
lm = abline(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB), col = "blue") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))


#sqrt plot for sqrt of inflorescences looks a lot better 
dmerge = read.csv("dmergeFINAL.csv") #resetting our data 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #resetting our data 
dmerge$TotalInflorescences_AB = sqrt(dmerge$TotalInflorescences_AB)
plot(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB, xlab = "Sqrt Total Inflorescences",ylab = "Total Insects Per Day")
lm = abline(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))

#Natural log transform for inflorescences looks pretty good too 
dmerge = read.csv("dmergeFINAL.csv") #resetting our data 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #resetting our data 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log 
dmerge$TotalInflorescences_AB[which(dmerge$TotalInflorescences_AB==-Inf)] = NA #taking the natural log of 0 results in an infinite number, which R throws a fit about, so turn those into NA
dmerge$TotalInflorescences_AB[which(is.nan(dmerge$TotalInflorescences_AB))] = NA 
dmerge = dmerge[dmerge$TotalInflorescences_AB != "NA",]
plot(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB, xlab = "Natural Log Inflorescence Count",ylab = "Insects Per Day", main = "Plants with More Inflorescences Attracted More Insects Per Day")
lm = abline(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$TotalInsects ~ dmerge$TotalInflorescences_AB))
text(3.5,20, "Insects per day = 1.465(logx) - 5.445
     p < 0.001***
     R-squared: 0.176",
     cex = 0.7, pos = 3)


#scatterplot avg inflorescences per panicle -- don't bother, it's a pain to get rid of the NAs and iti's going to be correlated with total inflorescences 

#Scatterplot total insects w/ abiotic factors 

as.numeric(dmerge$Cloud)
as.numeric(dmerge$TempF)

plot(dmerge$TotalInsects ~ dmerge$Cloud, main = "No Noticeable Impact of Cloud Cover On Insects Seen", xlab = "Cloud Cover (Percent)", ylab = "Total Insects Seen Per Day")
plot(dmerge$TotalInsects ~ dmerge$TempF, main = "Temperature (F) Effect on Insects Seen Per Day", ylab = "Insect Count", xlab = "Temperature (F)") #quadratic interaction peaking in mid 70s 
#we will need to model this quadratically, and then include both the linear and quadratic part of this model in our glm 
#https://www.spsanderson.com/steveondata/posts/2023-11-17/index.html 
quadratic_temp_model <- lm(TotalInsects ~ TempF + I(TempF^2), data = dmerge)
summary(quadratic_temp_model)
#TotalInsects ~ 1.12*TempF + -0.0082(TempF^2) - 31.70
#I tried to incorporate this into the glm model but it didn't accept it 
linear_temp_model = lm(dmerge$TotalInsects~dmerge$tempF) 
summary(linear_temp_model)


###############################################################################

####-----------ANALYSES-------------------------------

###############################################################################

#organizing data 

dmerge = read.csv("dmergeFINAL.csv") #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
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

citation('interactions')

#checking assumptions -- is insect abundance distributed normally? 

hist(dmerge$TotalInsects, breaks = 15) #oh no 
qqnorm(dmerge$TotalInsects)
var(dmerge$TotalInsects)/mean(dmerge$TotalInsects)

#yep, does not look like a normal distribution. There are a lot of models we can use to sort through this, including: 
# Poisson: often used, esp. for count models 
# Negative binomial: variant of poisson. Used for over-dispersed count data (var > mean)
# zero-inflated regression model: attempt to account for extra zeroes 
# OLS regression: log transform it 

#OK let's try log transforming it 
dmerge$TotalInsects = log10(dmerge$TotalInsects)
dmerge$TotalInsects = replace(dmerge$TotalInsects, dmerge$TotalInsects <0, 0) #replace infinite with 0 
hist(dmerge$TotalInsects, breaks = 15) #oh no 
qqnorm(dmerge$TotalInsects)
var(dmerge$TotalInsects)/mean(dmerge$TotalInsects)
#welp the variance/mean ratio is better but everything else looks almost worse. Huge tail with the zeroes. 

#it's not zero inflated, these are true zeroes 
#Let's go with poisson. It's count data with a lot of zeroes. 

#for poisson, variance = mean. Check assumptions.  
dmerge = read.csv("dmergeFINAL.csv") #resetting our dataframe
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 

hist(dmerge$TotalInsects, breaks = 15)
var(dmerge$TotalInsects) #var = 40.24
mean(dmerge$TotalInsects) #mean = 6.16
var(dmerge$TotalInsects)/mean(dmerge$TotalInsects) #Ratio of variance (6.55) is way larger than 2, so we are overdispersed. Let's try a quasipoisson model instead. 

#Run the quasipoisson model 


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
#OK, we have also some problems -- cultivar, species, and inflorescences are all not independent
#Also, date and cultivar are probably not independent either. Try an interaction factor for those two? 
#Once we divide by cultivar, since it's related to total inflorescences we don't want both in the samse model. We lose a lot of power when we divide by cultivar, so let's test the abiotic factors with TotalInflorescences as the main floral category

#First ------------ checking interactions between factors 

#ARe flower type and inflorescent type interacting? 
model = glm(TotalInsects ~ Flower.Type*Inflorescence.Type, data = dmerge, family = poisson(link = "log"))
summary(model)
#Yes, yes they are 

#Are flower type and species interacting? 
model = glm(TotalInsects ~ Flower.Type*Species, data = dmerge, family = poisson(link = "log"))
summary(model)
interaction.plot(x.factor = dmerge$Flower.Type, 
                 trace.factor = dmerge$Species, 
                 response = dmerge$TotalInsects, 
                 ylab = "Insect Abundance", 
                 xlab = "Flower Type")
#Not interacting as far as I can tell, might not be appropriate because flower types are unique to species. 

#Are flower type and inflorescence count interacting? 

model = glm(TotalInsects ~ Flower.Type*TotalInflorescences_AB, data = dmerge, family = poisson(link = "log"))
summary(model) #yes they are strongly interacting 

#boxplot of inflorescence count ~ Flower Type 
TotalInflorescences_AB = dmerge$TotalInflorescences_AB #define variables
FlowerType = factor(dmerge$Flower.Type, c("Lacy","Mop","Lacy panicle","Mop-like panicle")) ##Set flower type as factor 
plot(FlowerType,TotalInflorescences_AB,
     ylab="# of Inflorescences", xlab= "Flower Type", col="grey")

#Are inflorescence type and total inflorescences interacting? 

model = glm(TotalInsects ~ Inflorescence.Type*TotalInflorescences_AB, data = dmerge, family = poisson(link = "log"))
summary(model) #Not significantly, which is odd. However, recall that inflorescence type is related to flower type, which in turn is related to total inflorescences. 

#boxplot of inflorescence count ~ Inflorescence Type 
TotalInflorescences_AB = dmerge$TotalInflorescences_AB #define variables
InflorescenceType = factor(dmerge$Inflorescence.Type, c("Open","Half-open","Dense","Very dense")) ##Set inflorescence type as factor 
plot(InflorescenceType,TotalInflorescences_AB,
     ylab="# of Inflorescences", xlab= "Inflorescence Type", col="grey")

#Is species and number of inflorescences interacting? 
model = glm(TotalInsects ~ TotalInflorescences_AB*Species, data = dmerge, family = poisson(link = "log"))
summary(model) #Yep 

#boxplot of inflorescence count ~ Species 
TotalInflorescences_AB = dmerge$TotalInflorescences_AB #define variables
Species = factor(dmerge$Species, c("Hydrangea paniculata","Hydrangea arborescens")) ##Set species type as factor 
plot(Species,TotalInflorescences_AB,
     ylab="# of Inflorescences", xlab= "Species", col="grey")


#--------------- Now we can model
#We will look at total infloresceces only.  A key assumption of GLM models is that variables are independent. Flower type and inflorescence type are both related to each other. Flower type is related to total inflorescences. Species is also related to total inflorescences. Therefore we will omit species, flower type, and inflorescent type. 

#Full model (not including cultivar) 
model = glm(TotalInsects ~ TotalInflorescences_AB + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)

#Full model (including cultivar. Drop species and inflorescences since cultivar is related to both of these) 
model = glm(TotalInsects ~ Wind + Rain + Cloud + TempF + Month + Cultivar, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#No cultivars have p < 0.05, just not enough replicates 

#Forwards Regression  

model = glm(TotalInsects ~ TotalInflorescences_AB + Wind + Rain + Cloud + TempF + Month + Cultivar + Species, data = dmerge, family = quasipoisson(link = "log"))
summary(model)

#OK cultivar gets really messy, and it's correlated with total inflorescences, let's do forwards regression

model = glm(TotalInsects ~ TotalInflorescences_AB, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Total Inflorescences p value = 1.3E-5 < 0.001
#inflorescences matter, we can keep for now 

model = glm(TotalInsects ~ TotalInflorescences_AB + Wind, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Wind p value = 0.706, let's drop wind 

model = glm(TotalInsects ~ TotalInflorescences_AB + Rain, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#as expected switching from 0 rain to rain did decrease expected insect abundance, but it wasn't significant, probably because I almost never surveyed in the rain (only one survey). Let's drop rain. 

model = glm(TotalInsects ~ TotalInflorescences_AB + Cloud, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Cloud p value = 0.449, let's drop cloud 

#TotalInsects ~ 1.12*TempF + -0.0082(TempF^2) - 31.70
model = glm(TotalInsects ~ TotalInflorescences_AB + TempF, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#TempF p value is > 0.05, drop it 
#Insects decrease a bit with temp. Note that temp is actually related via quadratic slope, and I haven't been able to incorporate this into the glm 

model = glm(TotalInsects ~ TotalInflorescences_AB  + Month, data = dmerge, family = poisson(link = "log"))
summary(model)
#Month p value = 0.05E-11, p < 0.001, let's keep 

model = glm(TotalInsects ~ TotalInflorescences_AB + Month+ Species, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 

model = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + Month+ Species, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 

model = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + Month+ Species, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 

model = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + Month+ Species, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 
#Flower type and flower type interaction is significant

model = glm(TotalInsects ~ TotalInflorescences_AB*Inflorescence.Type + Month+ Species, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 
#inflorescence type is not significant 







#Final Model 
model = glm(TotalInsects ~ TotalInflorescences_AB*Inflorescence.Type + + Wind + Cloud + TempF + Month+ Species + False.Sepals + Flower.Type, data = dmerge, family = poisson(link = "log"))
summary(model)
#Species p value = 2E-16, p < 0.001, let's keep 
#inflorescence type is not significant 



#first model with species 
model1 = glm(TotalInsects ~ TotalInflorescences_AB*Species + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model1) #inflorescences are significant, month is right on the edge (p = 0.053), species is significant (8.81E-5 Let's keep these though; it matches what we saw in the  plots and what they look like by themselves. 
interact_plot(model1, pred = Species, modx = TotalInflorescences_AB)
#We have categorical predictor variables, so we can use cat_plot to visualize them (note: for continuous variables, use interact_plot())
#Looks like paniculata is better and it has a positive interaction with total inflorescences 

#second model with flower type 
model2 = glm(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model2) 
interact_plot(model2, pred = Flower.Type, modx = TotalInflorescences_AB)
#cool, so mop is definitely the worst, and lacy panicle is probably the best, but lacy, lacy panicle, and mop like panicle all have overlap within the first standard deviation 

#third model with inflorescent type 
model3 = glm(TotalInsects ~ TotalInflorescences_AB*Inflorescence.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model3)
interact_plot(model3, pred = Inflorescence.Type, modx = TotalInflorescences_AB)

#first model with species and interaction 
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


#second model with flower type and interaction
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
#there are significant interactions going on between all 3 types 

#third model with inflorescent type and interaction  
model3 = glm(TotalInsects ~ TotalInflorescences_AB*Inflorescence.Type + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model3)
p = interact_plot(model3, pred = Inflorescence.Type, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, higher percentages of fertile flowers \n are better for pollinators",
       x = "Categories of Fertile Flower Ratios", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#third model with inflorescent type and interaction, flipped axes does not work at all 

#Let's try to model cultivar 

model = glm(TotalInsects ~Cultivar + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
model = glm(TotalInsects ~ Cultivar*TotalInflorescences_AB + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#too many categories, almost nothing except green spire is significant by itself 

##----------------Species accumulation curve rarefaction 

df = dmerge[,28:93]
S = specnumber(df) #observed number of species
raremax = 42 #manually setting it rather than calculating the minimum of rowsums 
raremax = min(rowSums(df))
Srare = rarefy(df, raremax) #It gets super mad if you have zero data in your matrix IE a day with zero species observed. It gets super confused. 
#Error in if (minobs > 1) warning(gettextf("most observed count data have counts 1, but smallest count is %d",  : missing value where TRUE/FALSE needed

#What's happening is htat rarefy wants a matrix, but our dataframe isn't registering as one. You have to coerce it into one
# I really don't know how to fix this 

data.matrix(df,rownames.force = NA)
df2 = data.matrix(df,rownames.force = NA)
S = specnumber(df2)
raremax = min(rowSums(df2))
Srare = rarefy(df2,raremax)

?taxa_are_rows
data(df)
taxa_are_rows(df)
mat = t(otu_table(df))
?otu_table
??phyloseq
install.packages("phyloseq")
library(phyloseq)

#So if you want a rarefaction curve for a single site, use rarefy, not speaccum 
#Example
data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 10, sample = raremax, col = "blue", cex = 0.6)
#Rarecurve draws a rarefactoin curve for each row of hte input data. The rarefaction curves are evaluated using the interval of STEP sample sizes, always including 1 and total sample size. If sample is spsescified, a vertical line is drawn at sample with horizontal lines for hte rarefied species richnesses. 
#rareslope calculates the slope of rarecurve at given sample size 
#use rarefaction functions should be used for observed counts 
