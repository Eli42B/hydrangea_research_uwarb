###############################################################

####-----------SETUP-------------------------------

###############################################################

#Getting directories sorted 
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#-----------------------------load libraries

#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
#install.packages("iNEXT")
library(iNEXT)
#citation("iNEXT")

#-------------------------------Load data

dmerge = read.csv("specaccum.csv") #loading data for vacuumed bees 
dmerge2 = read.csv("specaccum_2019to2022.csv") #loading data for photo bees 


###############################################################
#----------Graphing the species accumulation curve
###############################################################


#------------------------------Setting up data for 2024 vials 


#Let's graph it with ggiNEXT() with Hill values
##Hill values: (species richness q = 0, Shannon diversity q = 1, Simpson diversity q = 2)

out = iNEXT(dmerge, q=c(0,1,2), datatype = "abundance", endpoint = 5000) #we have 1764 total abundance, so let's see what it looks like if we were to have obtained 3 years of data 
#Sample size based R/E curve (type = 1) 
#Datatype is abundance because our data is based on our bird species abundances 
#endpoint - when do we stop surveying? It defaults to double hte reference sample size, or we can specify an endpoint. In this case, we specified a 500 survey endpoint.


#--------------------- favorite graph for 2024 vials 

#All lines in same graph 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")+
  labs(title = "Species Rarefaction and Extrapolation Curve", 
       caption = "0 = Species richness \n1 = Shannon diversity\n2 = Simpson diversity")


#-----------------------Other graphs for 2024 vials 

#separate graphs 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")

#Separate graphs 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Order.q", color.var = "Both", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")

#Let's graph spp completeness

ggiNEXT(out, type = 2, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE)
ggiNEXT(out, type = 2, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE)
#Sample completeness curve shows when will we hit the  maximum number of species 

#Now let's try type3 graph (diversity estimates)
ggiNEXT(out, type = 3, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE)
ggiNEXT(out, type = 3, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE)
#these show the diversity estimates with confidence intervals as afunction of sample coverage up to the maximum coverage obtained from teh max size 


#------------------------------Setting up data for 2019 to 2022 photos 


#Let's graph it with ggiNEXT() with Hill values
##Hill values: (species richness q = 0, Shannon diversity q = 1, Simpson diversity q = 2)

out = iNEXT(dmerge2, q=c(0,1,2), datatype = "abundance", endpoint = 2200) #we have 729 days for relative total abundance, so let's see what it looks like if we were to have obtained triple the data
#Sample size based R/E curve (type = 1) 
#Datatype is abundance because our data is based on our bird species abundances 
#endpoint - when do we stop surveying? It defaults to double hte reference sample size, or we can specify an endpoint. In this case, we specified a 2200 survey endpoint.


#--------------------- favorite graph for 2019 to 2022 photos  

#All lines in same graph 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")+
  labs(title = "Species Rarefaction and Extrapolation Curve\nPhoto Data 2019-2022", 
       caption = "0 = Species richness \n1 = Shannon diversity\n2 = Simpson diversity",
        y = "Relative Species Diversity",
        x = "Relative Taxa Abundance\n(Count of Number of Days Taxa was Present)")


#-----------------------Other graphs for 2019 to 2022 photos 

#separate graphs 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")

#Separate graphs 
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Order.q", color.var = "Both", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")

#Let's graph spp completeness

ggiNEXT(out, type = 2, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE)
ggiNEXT(out, type = 2, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE)
#Sample completeness curve shows when will we hit the  maximum number of species 

#Now let's try type3 graph (diversity estimates)
ggiNEXT(out, type = 3, se = TRUE, facet.var = "Order.q", color.var = "None", grey = FALSE)
ggiNEXT(out, type = 3, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE)
#these show the diversity estimates with confidence intervals as afunction of sample coverage up to the maximum coverage obtained from teh max size 

