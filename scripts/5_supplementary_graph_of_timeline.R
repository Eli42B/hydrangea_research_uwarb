############## Generating timeline graph  ################

#Setup
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#-----------------------------load libraries
#https://stackoverflow.com/questions/44265512/creating-a-timeline-in-r 

#install.packages(tidyverse)
#library(tidyverse)
#library(tidytext)
#library(lubridate)
library(ggplot2)
#install.packages("iNEXT")
#library(iNEXT)
#citation("iNEXT")
#install.packages("vistime")
library(vistime)


#-------------------------------Load data
#Timeline of actual days that attracted bugs 

#Setting up data 
d2 = read.csv("Timeline_All.csv") #Note, make sure to change the date format in csv to yyyy-mm-dd
d2$StartDate = as.Date(d2$StartDate)
d2$EndDate = as.Date(d2$EndDate)

#------------------------------Graphs 

vistime(d2,events = "Trademark.Cultivar", groups = "Species",start = "StartDate", end = "EndDate")

