###########################################################################
######DATA

rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
d = read.csv("BeeMachineAI_Photos_3.csv")

###########################################################################
######LIBRARIES

library(tidyverse)
library(tidytext)

#install.packages("ggbreak")
library(ggbreak)

###########################################################################
######DPLYR WORK (elizabeth) to set up csv files for the final graph 

d_agg = d  %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date))   %>% 
  select(Date,species1,Trademark.Cultivar,Month,Year,det_num,Color,Flower.Type) %>%  
  na.omit() #remove NA values. Still has blanks but I give up. 

d_agg = d_agg[d_agg$Trademark.Cultivar != "",] #remove rows without a cultivar name


#write.csv(d_agg,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/temp.csv", row.names = FALSE)
#Export this as a csv and then save it as an excel file to calculate date ranges called 'photo_agg.xlsx' 

##----- Remove in next iteration 
#group by date and cultivar and count the total number of bugs found on that day 
d_agg2 = d_agg  %>% 
  group_by(Trademark.Cultivar,Date)   %>% 
  select(det_num,Color,Flower.Type) %>%
  summarise(TotalBugs = sum(det_num, na.rm = TRUE))

#Add a new column with '1' to represent each day having at least 1 bug present 
BugDay<-rep(1,each=473)
BugDay
d_agg2$BugDay = BugDay
  
#Select only our cultivar, date, and bug days 
d_agg2 = d_agg2  %>% select(Trademark.Cultivar,Date,BugDay)

#Add columns for month 
d_agg3 = d_agg2   %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name))    %>% 
  select(Trademark.Cultivar,Month,BugDay)  

#Summarise by month 
d_agg4 = d_agg3 %>% 
  group_by(Trademark.Cultivar,Month) %>%
  select(BugDay) %>%
  summarise(NumBugDays = sum(BugDay,na.rm = TRUE))

#----- Setting up the stacked barchart 

head(d_agg)

d_barchart_pics = d_agg  %>%
  group_by(Date, species1,Trademark.Cultivar)   %>%
  summarise(det_num = sum(det_num))

#write.csv(d_barchart_pics,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_barchart_pics.csv", row.names = TRUE)
#Export this as a csv and then save it as an excel file to calculate date ranges called 'Percentage_graph.xlsx' 



#######################################################################3
#Dplyr work, Jude 
##########################################################################

rm(list = ls(all = TRUE)) #blanks out your values 
d = read.csv("BeeMachineAI_Photos_3.csv")


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

#write.csv(d_per_month1, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/backup data/d_per_month1.csv", row.names = TRUE) #exporting as csv 

#d_per_month1 = read.csv("d_per_month1.csv") #originally I had to download as csv and then reload so I could add back in the panicle types, but this isn't ncessary anymore since we are updating the entire graph 

#Added categories of hydrangea shapes and number of days per month each cultivar was seen blooming and attracting insects

#Calculating how long each cultivar potentially could be attracting insects and added it to our exported csv file. Wait by definition this is just gonna be times when they actually had stuff. Hmm. 

#summarise by trademark.cultivar only for table of ratios

d_agg5 = read.csv("DAgg5.csv")
d_per_month2 = d_agg5  %>% 
  group_by(Trademark.Cultivar)   %>% 
  select(Trademark.Cultivar,DaysRecordedInsectsPresent,DaysRecordedBlooming)  %>% 
  summarise(DaysRecordedInsectsPresent = sum(DaysRecordedInsectsPresent, na.rm = TRUE),
            DaysRecordedBlooming = sum(DaysRecordedBlooming, na.rm = TRUE))

#write.csv(d_per_month2, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_per_month2.csv", row.names = TRUE) #exporting as csv 

#Write as csv, convert to excel file and add in percentages 

#Extracting a timeline 

d_timeline = d %>% 
  mutate(Date = trimws(Date)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Date,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F)

#write.csv(d_timeline, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_timeline.csv", row.names = TRUE) #exporting as csv 
#Download it as a csv and copy the column with the date ranges into d_per_month4.csv 
#Re-downloading as csv 


#---------------------------------------------------------------------

d_per_spp = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(species1) %>% 
  summarize(days = n_distinct(Date), 
            .groups = "keep") %>% 
  slice_max(days, with_ties = F)

#write.csv(d_per_spp, "~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_per_spp.csv", row.names = TRUE) #exporting as csv 

#Downloaded as a csv file, then added it to the 'Excel files' folder under specaccum.xlsx as a separate workbook 

#----------------------------------------------------------------------

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

#-------------------------------------------------------------------------


###########################################################################
######GGPLOT

### MAKING A PLOT TO MEASURE THE INFLUENCE OF INFLORESCENCE ON VISITATION

ggplot(d_agg_inflorescence, aes(Inflorescence_range, sorting, fill = species1))+
  geom_col(show.legend = F)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10,2))+
  scale_x_discrete()+
  labs(x = "Inflorescence per Panicle",
         y = "Average Species Visitation",
         title = "Influence of Panicle Inflorescence on Insect Visitation")


### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE, COLORED BY COLOR

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

### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE, COLORED BY panicle shape. This no longer works because d_per_month1 is missing 'Flower.type' but no need to fix since we are changing the whole graph 

ggplot(d_per_month1, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = Flower.type)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(values = c("Grey", 
                               "White"))+
  labs(x = "",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle shape",
       fill = "Panicle Shape")

### STACKED BARPLOT,  MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE, COLORED BY panicle shape, AND WITH A TOTAL STACK BASED ON NUMBER OF DAYS EACH CULTIVAR WAS BLOOMING ON AVERAGE PER MONTH ### 

dgraph = read.csv("dphoto_by_month_figure.csv")
#specify order of bars (from top to bottom)

#xtremely ugly but technically what I am trying to do
#It's SUPER hard to reorder it because it's adding up day as a single y value 
#Looked into manually reording it too but again I am not sure this will work because the order will change every month 
#I propose we keep this as is, and then just have a table with a list of hte ratios of blooming 
ggplot(dgraph, 
       aes(
           x = Trademark.Cultivar,
           y = AvgDays, 
           fill = Group))+
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,31),
                     breaks = seq(0,31,1))+
  scale_fill_manual(values = c("White", 
                               "gray",
                               "lightblue",
                               "darkblue"))+
  labs(x = "",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle shape",
       fill = "Panicle Shape")
  
  

  
#Example from Jude   
ggplot(d_per_month1, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = Flower.type)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(values = c("Grey", 
                               "White"))+
  labs(x = "",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle shape",
       fill = "Panicle Shape")



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
       title = "Hydrangea Panicle Categorization and its Influence on Recorded Visitor Populations",
       caption = "Species with fewer than 5 visits excluded")+
  scale_x_discrete(limits = c('Open', 'Half-open', 'Dense', 'Very dense'))

### MAKING A BOX PLOT WHERE THE Y AXIS REPRESENTS THE TOTAL NUMBER OF INDIVIDUALS OF A DISTINCT SPECIES THAT VISITED EACH INFLORESCENCE AND X IS SPECIES 

ggplot(d_inflorescence_type, aes(Species, count))+
  geom_boxplot(fill = "lightgray") +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize=0.5, 
               alpha = 0.7,
               color = "black") +
  theme_minimal()+
  labs(x = "Inflorescence Categorization",
       y = "Individuals of a Distinct Visiting Species",
       title = "Hydrangea Panicle Categorization and its Influence on Recorded Visitor Populations",
       caption = "Species with fewer than 5 visits excluded")+
  scale_x_discrete(limits = c('Open', 'Half-open', 'Dense', 'Very dense'))


### MAKING A TIMELINE ####

#https://stackoverflow.com/questions/44265512/creating-a-timeline-in-r 
#install.packages("vistime")
library(vistime)

#Timeline of actual days that attracted bugs 

#Setting up data 
d2 = read.csv("Timeline_All.csv") #Note, make sure to change the date format in csv to yyyy-mm-dd
d2$StartDate = as.Date(d2$StartDate)
d2$EndDate = as.Date(d2$EndDate)

#Graph
vistime(d2,events = "Trademark.Cultivar", groups = "Species",start = "StartDate", end = "EndDate")

#These are really hard to see; try reducing the number to make them fit better 
d2 = d2  %>%
  slice_max(Days.Blooming, n = 40) #slicing out the top 10 rows with the highest numbers of days blooming and attracting insects
vistime(d2,events = "Trademark.Cultivar", groups = "Species",start = "StartDate", end = "EndDate") #re-running the graph

#Try selecting just the top varieties (in progress)
d2 = read.csv("Timeline_All.csv") #Note, make sure to change the date format in csv to yyyy-mm-dd
d2$StartDate = as.Date(d2$StartDate)
d2$EndDate = as.Date(d2$EndDate)
d2 = d2 %>%
  slice(c(25,9,2,8,11,17,21,26,19,32,34,35,37,40,43,46,51,5,52,22,15,16,18))
vistime(d2,events = "Trademark.Cultivar", groups = "Species",start = "StartDate", end = "EndDate") #re-running the graph


#Timeline of generalized days that attracted bugs 

d3 = read.csv("Timeline_TopVarieties_Average.csv") #For a timeline of averaged bloom time dates of top varieties based on nour data and gardening websites 
vistime(d3,events = "Trademark.Cultivar", groups = "Species",start = "BloomStart", end = "BloomEnd") #re-running the graph 

#------------- Temporary aggregated dataframe

d_per_month1  %>%  summarise(AvgInsectAbundance = sum(days))

d_per_month2 = d_per_month1  %>%  
  group_by(Trademark.Cultivar) %>% 
  select(days) %>% 
  summarise(days = sum(days))


#############################################################
# Stacked Barchart
#############################################################
library(ggplot2)

#Mock Data 

df <- data.frame(TaxaGroup=rep(c("Hymenoptera", "Diptera","Lepidoptera"), each=4),
                  Month=rep(c("July", "Aug", "Sept","Oct"),3),
                  Percent=c(1,2,3,4,5,6,7,8,9,10,11,12))
df

# Stacked barplot with multiple groups
ggplot(data=df, aes(x=Month, y=Percent, fill=TaxaGroup)) +
  geom_bar(stat="identity")
