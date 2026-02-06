############## Stacked Barcharts ############################

#-----------------------------load libraries

library(ggplot2)
library(tidyverse)
library(tidytext)
library(lubridate)
library(vegan)

#-------------------------------Load data

DAgg3 = read.csv(dAgg3_filepath) #Data for species abundance 
dAgg4 = read.csv(dAgg4_filepath) #Data for species richness 
dmerge = read.csv(dmergeFINAL_filepath) #Data for shannon diversity 
dSA = read.csv(dSA_filepath) #Surface area by cultivar

# removing silver leaf 
DAgg3 = DAgg3 %>% 
  filter(Cultivar != "Silver Leaf") 
dAgg4 = dAgg4 %>% 
  filter(Cultivar != "Silver Leaf") 

#Create the diversity index 
#We're going to use Shannon diversity index because we aren't particularly concerned about a handful of rare species, we just want an overall picture of diversity 
#Shannon diversity index is based on number of species and evenness. It's a measure of the diversity of species in a community. 
#Higher H = higher diversity of species in a community
# Lower H means lower diversity 
# H = 0 means no diversity, the community only has one species 

#Setting up dataframe for shannon index  

dmerge$shannon = diversity(dmerge[,29:96], index = "shannon", MARGIN = 1, base = exp(1)) #Calculate Shannon index 
#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")
#Create a new merged dataframe by cultivar. Don't divide by plant; that doesn't matter in this case because we're looking at an average measure of diversity per day.  
dmerge7 = dmerge %>% 
  group_by(Cultivar, Species, Flower.Type) %>% 
  summarise(shannon = mean(shannon, na.rm = TRUE)) 

dSA = dSA %>% 
  rename(Cultivar = Trademark.Cultivar,
         SA = Surface.area..m.2.)

dSAbug = DAgg3 %>% 
  left_join(dSA, by = "Cultivar") %>% 
  drop_na(SA) %>% 
  mutate(abundance_per_SA = RelativeAbundance / SA)

#------------------------------Graphs 

par(mfrow = c(2, 2))  # so they canbe graphed side by side 


#Insect Abundance x Bug Type 

vials_p1 = ggplot(data = DAgg3,
                  aes(x = reorder(Cultivar, RelativeAbundance), y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p1 + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance by Taxa") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")

vials_p1b = ggplot(data = DAgg3,
                  aes(x = reorder(Flower.Type.2, RelativeAbundance), y = RelativeAbundance, fill = BugType)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_size = 14)
vials_p1b + scale_fill_discrete(name = "Legend", labels = c("Beetle","Butterfly","Fly","Bee or Wasp","True Bug")) + ggtitle("Insect Visitor Abundance by Taxa") + ylab("Total Insect Visitors") + xlab("Hydrangea Cultivar")


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

