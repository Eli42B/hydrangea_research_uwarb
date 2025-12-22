###########################################################################
#-------DATA
############################################################################

#Getting directories sorted 
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off() #blanks out graphs 

d = read.csv("BeeMachineAI_Photos_2_working_12_10.csv")

###########################################################################
#-------ADD LIBARRIES 
############################################################################


library(tidyverse)
library(tidytext)

#install.packages("ggbreak")
library(ggbreak)

###########################################################################
#-------AGGREGATING DATA WITH DPLYR 
############################################################################

d_agg_inflorescence = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  mutate(species_number = as.numeric(factor(species1))) %>% 
  drop_na(Trademark.Cultivar,
          AvgInflorescencesPerPanicle) %>%
  group_by(AvgInflorescencesPerPanicle, Date, species1) %>%
  summarize(species_count = n(), .groups = "drop") %>% 
  filter(AvgInflorescencesPerPanicle != "") %>% 
  mutate(Inflorescence_range = cut(AvgInflorescencesPerPanicle, 
                                   breaks = seq(0, 543, by = 10), 
                                   include.lowest = TRUE, 
                                   right = FALSE)) %>% 
  group_by(Inflorescence_range) %>% 
  mutate(n = n(),
         sorting = species_count / n) %>% 
  drop_na(Inflorescence_range) %>% 
  mutate(Month = month.name[as.integer(format(Date, "%m"))],
         Year = year(Date)) %>% 
  filter(Inflorescence_range != "[260,270)")
#---------------------------------------------------------------------

d_per_month2 = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Color,
            EYB_Inflorescence.Type,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F) %>% 
  drop_na(EYB_Inflorescence.Type) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense')))


#----------------------------------------------------------------------

d_distinct_species2 = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Date, species1) %>% 
  reframe(count = n(),
            P_Color = first(Color),
            EYB_Inflorescence.Type,
            .groups = "drop") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Date) %>% 
  reframe(P_Color = first(P_Color),
            count_distinct = n_distinct(species1),
            EYB_Inflorescence.Type,
            .groups = "drop") %>% 
  group_by(Trademark.Cultivar) %>% 
  reframe(average_richness = mean(count_distinct),
            P_Color = first(P_Color),
            EYB_Inflorescence.Type,) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense'))) %>% 
  distinct()

#--------------------------------------------------------------------------

d_inflorescence_type = d %>% 
  group_by(EYB_Inflorescence.Type, species1) %>% 
  summarize(count = n()) %>% 
  filter(EYB_Inflorescence.Type != "",
         count > 5) %>% 
  group_by(EYB_Inflorescence.Type) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense')))

#---------------------------------------------------------------------

d_per_month1 = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Color,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F)

d_distinct_species1 = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Date, species1) %>% 
  summarize(count = n(),
            P_Color = first(Color),
            .groups = "drop") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Date) %>% 
  summarize(P_Color = first(P_Color),
            count_distinct = n_distinct(species1),
            .groups = "drop") %>% 
  group_by(Trademark.Cultivar) %>% 
  summarize(average_richness = mean(count_distinct),
            P_Color = first(P_Color))

#--------------------------------------------------------------------------

d_inflorescence_type = d %>% 
  group_by(EYB_Inflorescence.Type, species1) %>% 
  summarize(count = n()) %>% 
  filter(EYB_Inflorescence.Type != "",
         count > 5) %>% 
  group_by(EYB_Inflorescence.Type) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense')))

#-----------------------------------------------------------------------------

#Create a d_per_month2 dataframe
write.csv(d_per_month2, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/backup data/d_per_month2.csv", row.names = TRUE)
#use vlookup table to add in the data on hydrangea architecture/ flower shape 
#save the edited data as d_per_month3.csv 

d_per_month3 = read.csv("d_per_month3.csv")

###########################################################################
#-------GRAPHING DATA 
############################################################################

##--Bargraphs ------------------------------------------------------------------

### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE (FILLED BY OPENNESS)

ggplot(d_per_month2, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = EYB_Inflorescence.Type)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(limits = c('Open', 
                               'Half-open', 
                               'Dense', 
                               'Very dense'),
                    values = c("white", 
                               "gray67", 
                               "gray33", 
                               "black"))+
  labs(x = "Hydrangea Cultivar",
       y = "Days with at least 1 Observed Insect Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Open inflorescence Types Have Higher Percentages of Fertile Florets",
       fill = "Panicle Inflorescence Type")

### MAKING A PLOT TO VISUALIZE THE AVERAGE NUMBER OF DISTINCT SPECIES IDENTIFIED AT EACH CULTIVAR

ggplot(d_distinct_species2, 
       aes(reorder_within(Trademark.Cultivar, 
                          average_richness,
                          EYB_Inflorescence.Type),
           average_richness))+
  geom_col(aes(fill = EYB_Inflorescence.Type),
           color = "black", 
           width = 1, 
           show.legend = T)+
  scale_fill_manual(limits = c('Open', 
                               'Half-open', 
                               'Dense', 
                               'Very dense'),
                    values = c("white", 
                               "gray67", 
                               "gray33", 
                               "black"))+
  coord_flip()+
  theme_minimal()+
  scale_x_reordered()+
  labs(x = "Hydrangea Cultivar",
       y = "Average Distinct Visiting Species",
       title = "Visitation Richness by Cultivar",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Inflorescence Type")

### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE

ggplot(d_per_month1, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = Color)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(values = c("darkolivegreen1", 
                               "deeppink2", 
                               "lightpink", 
                               "white"))+
  labs(x = "Hydrangea Cultivar",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Color")

### MAKING A PLOT TO VISUALIZE THE AVERAGE NUMBER OF DISTINCT SPECIES IDENTIFIED AT EACH CULTIVAR

ggplot(d_distinct_species1, 
       aes(reorder_within(Trademark.Cultivar, 
                          average_richness,
                          P_Color),
           average_richness))+
  geom_col(aes(fill = P_Color), 
           color = "black", 
           width = 1, 
           show.legend = T)+
  scale_fill_manual(values = c("darkolivegreen1", 
                               "deeppink2", 
                               "lightpink", 
                               "white"))+
  coord_flip()+
  theme_minimal()+
  scale_x_reordered()+
  labs(x = "Hydrangea Cultivar",
       y = "Average Distinct Visiting Species",
       title = "Visitation Richness by Cultivar",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Color")

#----------Boxplots -------------------------------------------------------------

###Boxplot of inflorescence categorization 

### MAKING A BOX PLOT WHERE THE Y AXIS REPRESENTS THE TOTAL NUMBER OF INDIVIDUALS OF A DISTINCT SPECIES THAT VISITED EACH INFLORESCENCE

ggplot(d_inflorescence_type, aes(EYB_Inflorescence.Type, count))+
  geom_boxplot(fill = "lightgray") +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize=0.5, 
               alpha = 0.7,
               color = "black") +
  theme_minimal()+
  labs(x = "Inflorescence Categorization",
       y = "Individuals of a Distinct Visiting Species",
       title = "Hydrangea Panicle Categorization \nand its Influence on Recorded Visitor Populations",
       caption = "Species with fewer than 5 visits excluded")+
  scale_x_discrete(limits = c('Open', 'Half-open', 'Dense', 'Very dense'))

#Boxplot comparing dense, half-open, open, and very dense using monthly aggregated data (probably less accurate than the total individual count), but more comparable to similar boxplots. 

d_per_month3$group = factor(d_per_month3$EYB_Inflorescence.Type, c("Open","Half-open","Dense","Very dense"))
p = ggplot(data = d_per_month3, 
           aes(x = group, y = days, fill = EYB_Inflorescence.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Cumulative Unique Insect Days",
       title = "Hydrangea Inflorescence Categorization \nand its Influence on How Frequently Pollinators Visited Each Month")
p
p + theme(legend.position = "none") #remove the legend 


#Boxplot comparing lacy and mop architecture using monthly aggregated data

d_per_month3$group = factor(d_per_month3$Flower.Type, c("Lacy panicle","Mop-like panicle"))
p = ggplot(data = d_per_month3, 
           aes(x = group, y = days, fill = Flower.Type)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Flower Type Categorization", 
       y = "Cumulative Unique Insect Days",
       title = "Hydrangea Flower Type Categorization \nand its Influence on How Frequently Pollinators Visited Each Month")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing month using monthly aggregated data

d_per_month3$group = factor(d_per_month3$Month, c("July","August","September","October"))
p = ggplot(data = d_per_month3, 
           aes(x = group, y = days, fill = Month)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Month", 
       y = "Cumulative Unique Insect Days",
       title = "How Frequently Pollinators Visited Each Month")
p
p + theme(legend.position = "none") #remove the legend 

#Boxplot comparing color using monthly aggregated data

d_per_month3$group = factor(d_per_month3$Color, c("White","Pink-white","Green-white","pink"))

d_per_month3 = d_per_month3[d_per_month3$Color != "NA",]
p = ggplot(data = d_per_month3, 
           aes(x = group, y = days, fill = Color)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Color", 
       y = "Cumulative Unique Insect Days",
       title = "Hydrangea Color and its Influence \non How Frequently Pollinators Visited Each Month")
p
p + theme(legend.position = "none") #remove the legend 

#we won't worry about rain-- that data was not collected during the visual only survey years. In addition, data was never collected in the rain. 

#-------------------SCATTERPLOTS 

#We can't make equivalent scatterplots because we don't have total inflorescence counts to create continuous data to plot 

#--------------------------------Species Accumulation / Rarefaction 

specaccum = read.csv("spec_accum_photos.csv")

#Graph our data 
plot(specaccum$CumulativeSpp ~specaccum$NumSurveys, main = "Cumulative Species Curve",ylab = "Number of Unique Taxa Found", xlab = "Number of Surveys", col = "lightgrey")

#Fit a line 
fit = lm(specaccum$CumulativeSpp~log(specaccum$NumSurveys)) #fit a line
summary(fit) #extract equation from ilne 
coef(fit) #extract coefficients
x = c(1:87)
y = -34 + 20.10*log(x)
#plot(y~x, xlab = "Number of observations",ylab="total number of species") #plot predicted line 
#plot predicted lines with upper and lower limit
y=predict(fit,newdata=list(x=specaccum$NumSurveys),
          interval="confidence")
x=c(1:87) #we don't want hundreds of repeat times in x, we just want the 5,10, etc. time categories
matlines(x,y,lwd=2) #plotting our predictions 

legend(
  x = "bottomright",
  legend = c("Actual Values","Predicted Values Upper Limit","Predicted Value","Predicted Value Lower Limit"),
  lty = c(3,5,1,3), 
  col = c(1,"green",1,"red"))

text(20,40, "Number of Predicted Taxa = 20.10*log(N) - 34.0 \nN = # Surveys
     p = 2.2E-16 
     R-squared: 0.81",
     cex = 0.7, pos = 3)


###############################################################################

####-----------ANALYSES-------------------------------

###############################################################################

#organizing data 
#Already organized, we will use d_per_month3.csv

#Loading libraries
#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)
citation('ggbreak')

#checking assumptions -- are days distributed normally? 

hist(d_per_month3$days) #wow super not normal at all 
qqnorm(d_per_month3$days) #very bad qq plot 
var(d_per_month3$days)/mean(d_per_month3$days) #It's overdispersed, 2.85 > 2, but could be worse. 
#because of the over dispersal, let's go with quasipoisson 

#Run the quasipoisson model 

#setting up the dataframe 
d_per_month3$Month = as.factor(d_per_month3$Month)
d_per_month3$Cultivar = as.factor(d_per_month3$Trademark.Cultivar)
d_per_month3$Color = as.factor(d_per_month3$Color)
d_per_month3$Inflorescence.Type = as.factor(d_per_month3$EYB_Inflorescence.Type)
d_per_month3$Flower.Type = as.factor(d_per_month3$Flower.Type)

#We don't expect color to really matter, but we'll do forwards regression anyway 

#Forwards regression Month 
model = glm(days ~ Month, data = d_per_month3, family = quasipoisson(link = "log"))
summary(model)
#one month (October) was significant compared to August. We'll plan to keep. 

#Forwards regression Month + Color 
model = glm(days ~ Month + Color, data = d_per_month3, family = quasipoisson(link = "log"))
summary(model)
#color is worthless, we will drop 

#Forwards regression Month + Inflorescence.Type
model = glm(days ~ Month + Inflorescence.Type, data = d_per_month3, family = quasipoisson(link = "log"))
summary(model)
#Inflorescence type Dense is significanly different from open, but doesn't differ significantly from half-open and very dense 

#Forwards regression Month + Inflorescence.Type + Flower.Type
model = glm(days ~ Month + Inflorescence.Type + Flower.Type, data = d_per_month3, family = quasipoisson(link = "log"))
summary(model)
#Flower type mop-like does differ from lacy 

#------------Graphing the analyses 

#primary model w/ Month 
model = glm(days ~ Month + Inflorescence.Type + Flower.Type, data = d_per_month3, family = quasipoisson(link = "log"))
summary(model)
p = cat_plot(model, pred = Inflorescence.Type, modx = Flower.Type,
             legend.main = "Flower Type",
             pred.labels = c("Very Dense","Dense","Half-open","Open"))
p +
  labs(title = "Lacy Panicles Attracted More Pollinators \nThan Mop-like Panicles",
       x = "Inflorescence Type",
       y = "Cumulative Unique Insect Days",
       fill = "Flower Type",
       caption = "Open inflorescence types have a higher percentage of fertile florets than very dense inflorescence types")

