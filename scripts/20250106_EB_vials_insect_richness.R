###############################################################################

####-----------SETUP-------------------------------

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
dAgg4 = read.csv("DAgg4.csv")
dAgg4B = read.csv("DAgg4B.csv")

#load libraries

#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)

#Generating DAgg4 
#Take FINALCombinedDAtasheet_3 
#Convert abundance counts to present/ absence using IF ELSE statement in Excel 
#Count unique counts per cultivar using pivot table 
#Save pivot table as csv 


###############################################################################

####-----------AGGREGATION-------------------------------

###############################################################################

#Aggregating by cultivar 
#dmerge is already aggregated by cultivar 

#OK this is looking good but we need to also divide it by how many plants we have of each cultivar 
# we won't divide by how many plants because we are already probably avoiding double counts by only counting unique species 

###############################################################################

####-----------PLOTS-------------------------------

###############################################################################

#-------------------STACKED BARCHARTS 

#Resources

#https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/ 

#Notes 

#Since the numbers are a lot lower, I have not divided out richness by type of taxa 
#Since we want by cultivar here, I am using the aggregated richness counts from DAgg4 

#Load libraries
library(ggplot2)

#Stacked Barchart, descending, by species
vials_p2 = ggplot(data = dAgg4,
       aes(x = reorder(Cultivar, Insect.Richness), y = Insect.Richness, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p2
vials_p2 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Richness by Hydrangea  Species") + ylab("Number of Insect Taxa") + xlab("Hydrangea Cultivar")

#Stacked Barchart, descending, by species using updated DAgg4B csv file
vials_p2 = ggplot(data = dAgg4B,
                  aes(x = reorder(Cultivar, Insect.Richness), y = Insect.Richness, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p2
vials_p2 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Richness by Hydrangea  Species") + ylab("Number of Insect Taxa") + xlab("Hydrangea Cultivar")

#Descending ggplot2 by flower type 
vials_p3 = ggplot(data = dAgg4,
                  aes(x = reorder(Cultivar, -Insect.Richness), y = Insect.Richness, fill = Flower.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p3
vials_p3 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Richness by Flower Type") + ylab("Number of Insect Taxa") + xlab("Hydrangea Cultivar")

#Descending ggplot2 by inflorescence type 
vials_p4 = ggplot(data = dAgg4,
                  aes(x = reorder(Cultivar, -Insect.Richness), y = Insect.Richness, fill = Inflorescence.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p4
vials_p4 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Richness by Inflorescence Type") + ylab("Number of Insect Taxa") + xlab("Hydrangea Cultivar")

#--------------------------------BOXPLOTS

#setting up the data 
dmerge = read.csv("dmergeFINALrichness.csv")
#note: even though zero data makes the boxplot look  messy, make sure to include it 

#Boxplot comparing dense, half-open, open, and very dense using non-aggregated data (more accurate). Though, after comparing them, they seem to pretty much say the same thing. 

dmerge$group = factor(dmerge$Inflorescence.Type, c("Open","Half-open","Dense","Very dense"))
p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = Inflorescence.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Daily Insect Richness",
       title = "Inflorescence Type Impact on Pollinator Richness",
       subtitle = "No Clear Relationship")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing hydrangea species using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Species, c("Hydrangea arborescens","Hydrangea paniculata"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = Species)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Species", 
       y = "Daily Insect Richness",
       title = "Hydrangea Species Impact on Pollinator Richness")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing flower type using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Flower.Type, c("Lacy","Mop","Lacy panicle","Mop-like panicle"))
dmerge = dmerge[dmerge$Flower.Type != "#N/A",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = Flower.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs( x = "
  
  
  
  
  Hydrangea Flower Shape",
        y = "Daily Insect Richness",
       title = "Hydrangea Flower Shape Impact on Pollinator Richness",
       subtitle = "Lacy shapes were generally better than mops")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing number of false sepals using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$False.Sepals, c("3","4","5","8"))
dmerge = dmerge[dmerge$False.Sepals != "NA",] #remove N/As 

p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = False.Sepals)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Hydrangea Number of False Sepals", 
       y = "Daily Insect Richness",
       title = "False Sepals Impact on Pollinator Richness")
p
p + theme(legend.position = "none") #remove the legend 
#inconclusive 


#Boxplot comparing month using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Month, c("Jul","Aug"))
dmerge = dmerge[dmerge$Species != "#N/A",] #remove N/As 
dmerge = dmerge[dmerge$Species != "NA",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = Month)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Month", 
       y = "Daily Insect Richness",
       title = "Impact of Month on Pollinator Richness",
       subtitle = "Impact was Unclear")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing rain using non-aggregated data (more accurate) 

dmerge$group = factor(dmerge$Rain, c("0","1"))
dmerge = dmerge[dmerge$Rain != "NA",] #remove N/As 
p = ggplot(data = dmerge, 
           aes(x = group, y = InsectRichness, fill = Rain)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Rain: No (0) Yes (1)", 
       y = "Insect Richness per plant per day",
       title = "Impact of Rain on Pollinator Richness")
p
# Guess we don't have one for NA and richness? Weird. Whatevsies. I'd throw it out anyway. 

#-------------------SCATTERPLOTS 

#organizing data 

dmerge = read.csv("dmergeFINALrichness.csv") #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #make sure that total inflorescences is seen as a number 


#Inflorescence count compared to insect richness 

#linear plot 
plot(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB)
lm = abline(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB))
text(80000,30, "Total Insects = 0.9312E-5x + 5.198,
     p < 0.001***
     R-squared: 0.084",
     cex = 0.5, pos = 3)

#sqrt plot don't look great if take square root of insects 
dmerge$InsectRichness = sqrt(dmerge$InsectRichness)
plot(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB)
lm = abline(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB), col = "blue") #Add a linear model and line 
summary(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB))


#sqrt plot for sqrt of inflorescences looks a lot better 
dmerge = read.csv("dmergeFINALrichness.csv") #resetting our data 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #resetting our data 
dmerge$TotalInflorescences_AB = sqrt(dmerge$TotalInflorescences_AB)
plot(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB, xlab = "Sqrt Total Inflorescences",ylab = "Total Insects Per Day")
lm = abline(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB))

#Natural log transform for inflorescences looks pretty good too 
dmerge = read.csv("dmergeFINALrichness.csv") #resetting our data 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #resetting our data 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log 
dmerge$TotalInflorescences_AB[which(dmerge$TotalInflorescences_AB==-Inf)] = NA #taking the natural log of 0 results in an infinite number, which R throws a fit about, so turn those into NA
dmerge$TotalInflorescences_AB[which(is.nan(dmerge$TotalInflorescences_AB))] = NA 
dmerge = dmerge[dmerge$TotalInflorescences_AB != "NA",]
plot(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB, xlab = "Natural Log Inflorescence Count",ylab = "Insect Richness Per Day", main = "Plants with More Inflorescences \n Attracted Higher Number of Unique Species Per Day", sub = "Note: Inflorescences are Increasing Logarithmically")
lm = abline(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB), col = "red") #Add a linear model and line 
summary(lm(dmerge$InsectRichness ~ dmerge$TotalInflorescences_AB))
text(4,7, "Insect Richness per day = 0.633log(x) - 1.49
     p < 0.0001***
     R-squared: 0.170",
     cex = 0.7, pos = 3)

#scatterplot avg inflorescences per panicle -- don't bother, it's a pain to get rid of the NAs and iti's going to be correlated with total inflorescences 

#Scatterplot total insects w/ abiotic factors 

as.numeric(dmerge$Cloud)
as.numeric(dmerge$TempF)

plot(dmerge$InsectRichness ~ dmerge$Cloud, main = "No Noticeable Impact of Cloud Cover On Insect Richness", xlab = "Cloud Cover (Percent)", ylab = "Insects Richness Per Day")
plot(dmerge$InsectRichness ~ dmerge$TempF, main = "Temperature (F) Effect on Daily Insect Richness", ylab = "Daily Insect Richness", xlab = "Temperature (F)") #quadratic interaction peaking in mid 70s 
#we will need to model this quadratically, and then include both the linear and quadratic part of this model in our glm 
#https://www.spsanderson.com/steveondata/posts/2023-11-17/index.html 
quadratic_temp_model <- lm(InsectRichness ~ TempF + I(TempF^2), data = dmerge)
summary(quadratic_temp_model)
#InsectRichness ~ 1.16*TempF + -0.0082(TempF^2) - 37.92
#I tried to incorporate this into the glm model but it didn't accept it 
linear_temp_model = lm(dmerge$InsectRichness~dmerge$TempF) 
summary(linear_temp_model)

#--------------------------------Species Accumulation / Rarefaction 

specaccum = read.csv("spec_accum_vials.csv")

#Graph our data 
plot(specaccum$CumulativeSpp ~specaccum$NumSurveys, main = "Cumulative Species Curve",ylab = "Number of Unique Taxa Found", xlab = "Number of Surveys", col = "lightgrey")

#Fit a line 
fit = lm(specaccum$CumulativeSpp~log(specaccum$NumSurveys)) #fit a line
summary(fit) #extract equation from ilne 
coef(fit) #Equation = -20.81 + 14.7*log(x)
x = c(1:250)
y = -20.81 + 14.7*log(x)
#plot(y~x, xlab = "Number of observations",ylab="total number of species") #plot predicted line 
#plot predicted lines with upper and lower limit
y=predict(fit,newdata=list(x=specaccum$NumSurveys),
          interval="confidence")
x=c(1:287) #we don't want hundreds of repeat times in x, we just want the 5,10, etc. time categories
matlines(x,y,lwd=2) #plotting our predictions 

legend(
  x = "bottomright",
  legend = c("Actual Values","Predicted Values Upper Limit","Predicted Value","Predicted Value Lower Limit"),
  lty = c(3,5,1,3), 
  col = c(1,"green",1,"red"))

text(190,30, "Estimated Number of Predicted Taxa = 14.7*log(N) - 20.81 \nN = # Surveys
     p = 2.2E-16 
     R-squared: 0.9527",
     cex = 0.7, pos = 3)

###############################################################################

####-----------ANALYSES-------------------------------

###############################################################################

#organizing data 

dmerge = read.csv("dmergeFINALrichness.csv") #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
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

#checking assumptions -- is insect abundance distributed normally? 

hist(dmerge$InsectRichness) #pretty tailed
qqnorm(dmerge$InsectRichness) #oh that's actually really good 
var(dmerge$InsectRichness)/mean(dmerge$InsectRichness) #It's overdispersed but not actually by much 

#yep, does not look like a normal distribution. There are a lot of models we can use to sort through this, including: 
# Poisson: often used, esp. for count models 
# Negative binomial: variant of poisson. Used for over-dispersed count data (var > mean)
# zero-inflated regression model: attempt to account for extra zeroes 
# OLS regression: log transform it 

#OK let's try log transforming it 
dmerge$InsectRichness = log10(dmerge$InsectRichness)
dmerge$InsectRichness = replace(dmerge$InsectRichness, dmerge$InsectRichness <0, 0) #replace infinite with 0 
hist(dmerge$InsectRichness) #oh no 
qqnorm(dmerge$InsectRichness)
var(dmerge$InsectRichness)/mean(dmerge$InsectRichness)
#welp the variance/mean ratio is now underdispersed, and everything else looks worse. 
#it's not zero inflated, these are true zeroes 
#Let's go with poisson. It's count data with a lot of zeroes. 

#for poisson, variance = mean. Check assumptions.  
dmerge = read.csv("dmergeFINALrichness.csv") #resetting our dataframe
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log +1 for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 

hist(dmerge$InsectRichness)
var(dmerge$InsectRichness) #var = 7.01
mean(dmerge$InsectRichness) #mean = 3.4
var(dmerge$InsectRichness)/mean(dmerge$InsectRichness) #Ratio of variance (2.06) is  larger than 2, so we are overdispersed, though it's actually fairly close. Let's try a quasipoisson model instead. 

#Run the quasipoisson model 

dmerge = read.csv("dmergeFINALrichness.csv") #reset the data, we were dividing by plant number for the plots, but for this analysis we don't need to and more data is better. Also, the non-integer numbers will throw off the poisson model because it is count based. 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB) #setting up data correctly 
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 


dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
#OK, we have also some problems -- cultivar, species, and inflorescences are all not independent
#Also, date and cultivar are probably not independent either. Try an interaction factor for those two? 
#Once we divide by cultivar, since it's related to total inflorescences we don't want both in the samse model. We lose a lot of power when we divide by cultivar, so let's test the abiotic factors with TotalInflorescences as the main floral category. 

#Check for interactions 
#We know from the insect abundance anlysis that Flower type and inflorescence type are both related to each other. Flower type is related to total inflorescences. Species is also related to total inflorescences. Therefore we will omit species, flower type, and inflorescent type.

#-------------------Model 

#Full model (not including cultivar) 
model = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#only total inflorescences and month matter 

#Full model (including cultivar. Drop species and inflorescence since cultivar is related to both of these) 
model = glm(TotalInsects ~ Wind + Rain + Cloud + TempF + Month + Cultivar, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#No cultivars have p < 0.05, just not enough replicates 

#Simplified model with flower type and interaction
model2 = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model2) 
p = interact_plot(model2, pred = Flower.Type, modx = TotalInflorescences_AB,
                  legend.main = "Inflorescence Count Per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "In general, lacy panicles shapes are the best \n and mop shapes are the worst for pollinators",
       x = "Hydrangea Shapes", 
       y = "Average Insect Richness Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#there are significant interactions going on between 2 groups 

#Simplified model with inflorescent type and interaction  
model3 = glm(InsectRichness ~ TotalInflorescences_AB*Inflorescence.Type + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model3)
p = interact_plot(model3, pred = Inflorescence.Type, modx = TotalInflorescences_AB, 
                  legend.main = "Inflorescence Count per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "No clear relationship between higher percentages \n of fertile flowers and pollinator richness", ,
       x = "Categories of Fertile Flower Ratios", 
       y = "Average Insect Richness Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
#third model with inflorescent type and interaction, flipped axes does not work at all 

#Simplified model with species and interaction 
model3 = glm(InsectRichness ~ TotalInflorescences_AB*Species + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model3)
p = interact_plot(model3, pred = Species, modx = TotalInflorescences_AB, 
                  legend.main = "Total Inflorescences per Plant",
                  modx.labels = c("-1 Standard Deviation", "Mean", "+1 Standard Deviation"))
p + 
  labs(title = "No interaction between plant species and\npollinator richness ", ,
       x = "Species", 
       y = "Average Insect Richness Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")

#Let's try to model cultivar 

model = glm(InsectRichness ~Cultivar + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
model = glm(InsectRichness ~ Cultivar*TotalInflorescences_AB + TempF+ Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#too many categories, almost nothing except silver leaf is significant by itself 

#Overall, patterns with insect richness were almost identical to patterns with insect abundance for vials, with a few exceptions: temperature did not matter in the analyses,and there was no clear relationship between inflorescence type (open to very dense) and pollinator richness

#----------------------Final models 

#Full model (not including cultivar, including Flower Type) 
model = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#only total inflorescences and month matter 

#Full model (not including cultivar, including Flower Type, not including species) 
model = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Species has to be in a separate model than flower type becuase they are related -- panicle flower shapes are by definition panicle hydrangeas. And these models require independent variables. Note that we are including an interaction between flower type and inflorescences because preliminary graphs showed an interaction between the two 

#Full model (not including cultivar, including species, not including Flower Type ) 
model = glm(InsectRichness ~ TotalInflorescences_AB + Species + Wind + Rain + Cloud + TempF + Month, data = dmerge, family = quasipoisson(link = "log"))
summary(model)
#Species is related to flower type so we dropped flower type in this model, but we are not modeling an interaction because prelminary graphs did not show an interaction withs pecies only and inflorescences 
#The flower type explained things a bit better, not surprising since it includes species in a way and is more detailed, but both are reasonable 
