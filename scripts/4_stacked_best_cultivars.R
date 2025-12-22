############## Stacked Barcharts ############################

#Setup
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#-----------------------------load libraries

library(ggplot2)
#install.packages(tidyverse)
#install.packages(vegan)
library(tidyverse)
library(tidytext)
library(lubridate)
library(vegan)

#-------------------------------Load data

DAgg3 = read.csv("DAgg3.csv") #Data for species abundance 
dAgg4 = read.csv("DAgg4.csv") #Data for species richness 
dmerge = read.csv("dmergeFINAL.csv") #Data for shannon diversity 
dSA = read.csv("SA_cultivar.csv") #Surface area by cultivar

#Create the diversity index 
#We're going to use Shannon diversity index because we aren't particularly concerned about a handful of rare species, we just want an overall picture of diversity 
#Shannon diversity index is based on number of species and evenness. It's a measure of the diversity of species in a community. 
#Higher H = higher diversity of species in a community
# Lower H means lower diversity 
# H = 0 means no diversity, the community only has one species 

#Setting up dataframe for shannon index  

dmerge$shannon = diversity(dmerge[,30:93], index = "shannon", MARGIN = 1, base = exp(1)) #Calculate Shannon index 
#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")
#Create a new merged dataframe by cultivar. Don't divide by plant; that doesn't matter in this case because we're looking at an average measure of diversity per day.  
dmerge7 = dmerge %>% 
  group_by(Cultivar, Species, Flower.Type)%>% 
  summarise(shannon = mean(shannon, na.rm = TRUE)) %>% 
  select(Cultivar, Species, Flower.Type, shannon)

dSA = dSA %>% 
  rename(Cultivar = Trademark.Cultivar,
         SA = Surface.area..m.2.)
  
dSAbug = DAgg3 %>% 
  left_join(dSA, by = "Cultivar") %>% 
  select(Cultivar, BugType, RelativeAbundance, SA) %>% 
  drop_na(SA) %>% 
  mutate(abundance_per_SA = RelativeAbundance / SA)

#------------------------------Graphs 

par(mfrow(2,2))

#Insect Abundance x Bug Type 

vials_p1 = ggplot(data = DAgg3,
                  aes(x = reorder(Cultivar, RelativeAbundance), y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p1 + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance by Taxa") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#Insect Abundance x Surface Area x Bug Type

vials_p5 = ggplot(data = dSAbug,
                  aes(x = reorder(Cultivar, abundance_per_SA), y = abundance_per_SA, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p5 + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance per average Cultivar Surface Area,\n by Taxa") + ylab("Insect Visitors (per square meter)") + xlab("Hydrangea Cultivar")

#Insect Abundance x Species + Flower Type  

vials_p3 = ggplot(data = DAgg3,
                  aes(x = reorder(Cultivar, RelativeAbundance), y = RelativeAbundance, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip()+
  theme_minimal(base_size = 14)
vials_p3 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Abundance by Species and Flower Type") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

#Insect Richness x Species + Flower Type

vials_p2 = ggplot(data = dAgg4,
                  aes(x = reorder(Cultivar, Insect.Richness), y = Insect.Richness, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p2 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Richness by Hydrangea Species") + ylab("Number of Insect Taxa") + xlab("Hydrangea Cultivar")

#Insect diversity x Species + Flower Type 

vials_p4 = ggplot(data = dmerge7,
                  aes(x = reorder(Cultivar, shannon), y = shannon, fill = Species)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p4 + scale_fill_discrete(name = "Legend") + ggtitle("Insect Visitor Diversity by Hydrangea Species") + ylab("Shannon Index of Diversity (Larger = More Diverse)") + xlab("Hydrangea Cultivar")

