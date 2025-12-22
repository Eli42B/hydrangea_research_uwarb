#############################################################
### READING IN DATA

getwd()
setwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")

d = read.csv("BeeMachineAI_Photos_2_working_12_10.csv")

#############################################################
### LIBRARIES
library(tidyverse)

#install.packages("lubridate")
library(lubridate)

#install.packages("writexl")
library(writexl)
#############################################################
### WORKING OUT AGGREGATING
#learn dplyr https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html 
#https://www.geeksforgeeks.org/dplyr-package-in-r-programming/ 


#%>% is the pipe in deplyr and it says, take d dataframe and run it through the function group_by, and run that through the function summarize 

d_cult = d %>% 
  group_by(Trademark.Cultivar) %>% 
  summarize(n = n()) #n counts how often the name showed up 

d_spec = d %>% 
  group_by(species1) %>% 
  summarize(n = n())

test = d_agg %>% 
  group_by(species_number) %>% 
  summarize(n = n())

d_agg = d %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(species_number = as.numeric(factor(species1))) %>% 
  drop_na(Trademark.Cultivar) %>%
  group_by(Trademark.Cultivar, Date, species1) %>%
  summarize(species_count = n(), .groups = "drop") %>% 
  filter(Trademark.Cultivar != "")

#############################################################
### Saving as excel

write_xlsx(d_agg, "aggregated_by_cultivar.csv")



