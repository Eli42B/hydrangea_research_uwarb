############## Generating rarefaction graphs for species accumulation curve ################

#-----------------------------load libraries

library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
library(iNEXT)

#-------------------------------Load data

dmerge = read.csv(specaccum_vaccum_filepath) #loading data for vacuumed bees 
dmerge2 = read.csv(specaccum_photo_filepath) #loading data for photo bees

#------------------------------Graphs 

#Let's graph it with ggiNEXT() with Hill values
##Hill values: (species richness q = 0, Shannon diversity q = 1, Simpson diversity q = 2)

out = iNEXT(dmerge, q=c(0,1,2), datatype = "abundance", endpoint = 5000) #we have 1764 total abundance, so let's see what it looks like if we were to have obtained 3 years of data 
#Sample size based R/E curve (type = 1) 
#Datatype is abundance because our data is based on our bird species abundances 
#endpoint - when do we stop surveying? It defaults to double hte reference sample size, or we can specify an endpoint. In this case, we specified a 500 survey endpoint.

p = ggiNEXT(out, type = 1, se = TRUE, facet.var = "Assemblage", color.var = "None", grey = FALSE) + 
  theme_bw(base_size = 18) + 
  theme(legend.position = "right")+
  labs(title = "Species Rarefaction and Extrapolation Curve \n Researchers Collected Sufficient Data", caption = "0 = Species richness \n1 = Shannon diversity\n2 = Simpson diversity")+
  ylab("Species Richness")
p

############################################################
#Graph the actual data in front of the rarefaction plot 
############################################################

d = read.csv(specaccum_vials_filepath, header = TRUE) 

d1 = as.data.frame(cbind(d$CumulativeNumInsects,d$CumulativeSpp)) 

p + geom_point(data = d1, aes(x = V1, y = V2), color = "Black", size = 0.75)

