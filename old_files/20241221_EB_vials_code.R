########################################

#Set up working directory
####################################

getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 

###########################################
#Load data
##########################################

#d_vials<-read.csv("InsectData_Cleaned_1.csv")
#d_vials$Cerambycidae = as.numeric(d_vials$Cerambycidae)
#d_visual<- read.csv("VisualSurveys.csv")
dmerge = read.csv("dmergeFINAL.csv")

######################################################
#load libraries
######################################################
#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)

##########################################################################MMergicleaning data -- I ended up cleaning the merge in Excel and saving it as dmergeFINAL.csv 
###############################################################################

#d_vials2 = d_vials[,4:50]
#d_vials$sum = rowSums(d_vials2)
#write.csv(d_vials,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_vials.csv", row.names = TRUE)

#d_none = d_visual[d_visual$NoVialSample == 1,] #create a separate dataframe of null values 

#dmerge <- merge(d_vials,d_visual,by=c("Date","Tag.Number"))
#OK the  merge worked but it removed all the rows where we collected information but didn't collect a vial
#write.csv(dmerge,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/dmerge.csv", row.names = TRUE)

#write.csv(d_none,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_none.csv", row.names = TRUE)


####################################################
#aggregation attempts for insect abundance 
#####################################################

#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")

#### Let's aggregate just by cultivar 

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

#To create STACKED barcharts in ggplot2, we need the counts categorized by insect type

#export DAgg2 


#write.csv(dAgg2, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/backup data/DAgg2.csv",row.names = TRUE)

#using DAgg2, created a csv file named DAgg3 inside of 'data in use' folder formatted to work well for a stacked bargraph in ggplot 3 

#It would actually be cleaner to do all our data manipulation and creation of dataframes within R, but I don't know how to offhand, so Excel is faster for now 


####################################################
#Time for the plots
######################################################
#https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

#BARCHARTS 
#------------------------------------------------------------------------

#Load libraries
library(ggplot2)

#Load data 
DAgg3 = read.csv("DAgg3.csv")

#ggplot stacked and horizontal 
ggplot(data = DAgg3,
       aes(x = Cultivar, y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

#Source: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/ 

#Descending ggplot2 by bug type 
vials_p1 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p1
vials_p1 + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance by Taxa") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar Name")

#Descending ggplot2 by hydrangea species 
vials_p2 = ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal()
vials_p2
vials_p2 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Abundance by Hydrangea  Species") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar Name")

#Descending ggplot2 by flower type 
ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Flower.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

#Descending ggplot2 by inflorescence type 
ggplot(data = DAgg3,
       aes(x = reorder(Cultivar, -RelativeAbundance), y = RelativeAbundance, fill = Inflorescence.Type)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

#BOXPLOTS
#-----------------------------------------------------------------------

#Boxplot comparing dense, half-open, open, and very dense using non-aggregated data (more accurate) 

#note: even though zero data makes the boxplot look  meessy, make sure to include it 

dmerge$group = factor(dmerge$Inflorescence.Type, c("Open","Half-open","Dense","Very dense"))
dmerge = dmerge[dmerge$Inflorescence.Type != "#N/A",] #remove N/As 

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
       y = "Daily Abundance of Insect Visitors per plant",
       title = "Fertile Inflorescence Density Impact on Pollinators (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 

#-------------------------------------------------------------------
#Boxplot comparing hydrangea species using non-aggregated data (more accurate) 

#note: even though zero data makes the boxplot look  messy, make sure to include it 
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
       title = "Hydrangea Species Impact on Pollinators (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 


#-----------------------------------------------------------------------

### MAKING A BOX PLOT WHERE THE Y AXIS REPRESENTS THE TOTAL NUMBER insect that visted a given panicle with the open to very dense characteristics, divided by the  number of panicles on the bush 
#https://www.geeksforgeeks.org/how-to-reorder-boxplots-in-r-with-ggplot2/?ref=ml_lbp

#For standard errors, we need to plotrix package 
#install.packages("plotrix")
#https://www.geeksforgeeks.org/calculate-standard-error-in-r/ 
#I looked into making standard error bars but it looked super terrible 

#reorganize it by open, half open, dense, very dense 
#https://www.geeksforgeeks.org/how-to-reorder-boxplots-in-r-with-ggplot2/?ref=ml_lbp 

DAgg3$group = factor(DAgg3$Inflorescence.Type, c("Open","Half-open","Dense","Very dense"))

p = ggplot(data = DAgg3, 
       aes(x = group, y = RelativeAbundance, fill = Inflorescence.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.5, 
               alpha = 0.7, 
               color = "black") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Relative Abundance of Insect Visitors per Cultivar",
       title = "Hydrangea Panicle Categorization and its Influence on Recorded Visitor Populations (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 

#boxplot by species

#reorganize it by hydrangea species 
#https://www.geeksforgeeks.org/how-to-reorder-boxplots-in-r-with-ggplot2/?ref=ml_lbp 

DAgg3$group = factor(DAgg3$Species, c("Hydrangea arborescens","Hydrangea paniculata"))

p = ggplot(data = DAgg3, 
           aes(x = group, y = RelativeAbundance, fill = Species)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.5, 
               alpha = 0.7, 
               color = "black") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Avg. # Insect Visitors per Cultivar",
       title = "Panicle Species and its Influence on Recorded Visitor Populations (Vacuum data 2024)")
p
p + theme(legend.position = "none") #remove the legend 
