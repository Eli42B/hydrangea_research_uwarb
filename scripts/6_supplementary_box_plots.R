############## Generating Supplementary Boxplots  ################

#Setup
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#-----------------------------load libraries

library(ggplot2)

#-------------------------------Load data

DAgg3 = read.csv("DAgg3.csv")
dmerge = read.csv("dmergeFINAL.csv")
dmerge = dmerge[dmerge$Inflorescence.Type != "#N/A",] #remove N/As 


#------------------------------Graphs

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
           aes(x = group, y = TotalInsects + 5, fill = Species, binwidth = 5.0)) + 
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

#Boxplot comparing color of flower using non-aggregated data (more accurate). 

dmerge$group = factor(dmerge$Color, c("White","Pink-white","Pink","Green-white"))
p = ggplot(data = dmerge, 
           aes(x = group, y = TotalInsects, fill = Color)) + 
  geom_boxplot(fill = "lightgray") + 
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center', 
               dotsize = 0.05, 
               alpha = 0.7, 
               color = "lightgray") + 
  theme_minimal() + 
  labs(x = "Inflorescence Categorization", 
       y = "Daily Abundance of Insect Visitors/plant",
       title = "Flower Color Impact on Pollinators")
p
p + theme(legend.position = "none") #remove the legend 

