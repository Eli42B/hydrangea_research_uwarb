#################################################
# Generating graphs for comparative curves
#################################################

#-----------------------------load libraries

#install.packages("jtools")
library(jtools)
#install.packages("broom")
library(broom)
#install.packages("ggstance")
library(ggstance)
#install.packages("interactions")
library(interactions)
#citation('interactions')
library(tidyverse)
library(cowplot)            # for graphing ggplots side by side 
 

#-------------------------------Load data

# --- dmerge data for abundance # -----

dmerge = read.csv(dmergeFINAL_filepath) #reset the data, we were dividing by plant number for the plots, but for this analysis we don't need to and more data is better. Also, the non-integer numbers will throw off the poisson model because it is count based. 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)  #setting up data correctly 

dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship + 1 to fix the zero values and the infinite valuesl that result from zeroes. 

dmerge$Rain = as.factor(dmerge$Rain)
dmerge$Wind = as.numeric(dmerge$Wind)
dmerge$Cloud = as.numeric(dmerge$Cloud)
dmerge$TempF = as.numeric(dmerge$TempF)
dmerge$Month = as.factor(dmerge$Month)
dmerge$Cultivar = as.factor(dmerge$Cultivar)
dmerge$Species = as.factor(dmerge$Species)
dmerge$Flower.Type = as.factor(dmerge$Flower.Type)
dmerge$Color = as.factor(dmerge$Color)

# removing silver leaf 
dmerge = dmerge %>% 
  filter(Cultivar != "Silver Leaf")

# --- dmerge data for richness # -----

dmergeRichness = read.csv(glm_richness_filepath) # also getting it for richness 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)  #setting up data correctly 

dmergeRichness$TotalInflorescences_AB = log(dmergeRichness$TotalInflorescences_AB + 1) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship + 1 to fix the zero values and the infinite valuesl that result from zeroes. 

dmergeRichness$Rain = as.factor(dmergeRichness$Rain)
dmergeRichness$Wind = as.numeric(dmergeRichness$Wind)
dmergeRichness$Cloud = as.numeric(dmergeRichness$Cloud)
dmergeRichness$TempF = as.numeric(dmergeRichness$TempF)
dmergeRichness$Month = as.factor(dmergeRichness$Month)
dmergeRichness$Cultivar = as.factor(dmergeRichness$Cultivar)
dmergeRichness$Species = as.factor(dmergeRichness$Species)
dmergeRichness$Flower.Type = as.factor(dmergeRichness$Flower.Type)
dmergeRichness$Color = as.factor(dmergeRichness$Color)

# calculating shannon diversity 
dmerge$shannon = diversity(dmerge[,29:96], index = "shannon", MARGIN = 1, base = exp(1))

#dmerge = dmerge[-178,] #there's a single day where shannon diversity index calculated as NA, not sure why, let's just remove it 
dmerge$shannon

# removing silver leaf 
dmergeRichness = dmergeRichness %>% 
  filter(Cultivar != "Silver Leaf")


#------------------------------Graph for insect abundance 

model1 = glm.nb(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge)
p_abundance = interact_plot(
  model1, 
  pred = TotalInflorescences_AB, 
  modx = Flower.Type, 
  interval = TRUE,      # draw confidence interval 
  int.width = 0.95,     # 95% confidence interval 
  vary.lty = TRUE)      # so the dots and dashes stay 

# ------------------------------- Attempting using copilot to make graph prettier 

type_cols <- c(
  "Mop"              = "#08306B",
  "Lacy"             = "#08306B",
  "Mop-like panicle" = "#8B0000",
  "Lacy panicle"     = "#8B0000"
)

type_ltys <- c(
  "Mop"              = "solid",
  "Lacy"             = "dotted",
  "Mop-like panicle" = "solid",
  "Lacy panicle"     = "dotted"
)

p_abundance = p_abundance + 
  
  # adding labels to x and y axes 
  labs(
    x = "log(Number of Fertile Flowers + 1)", 
    y = "Average Insects Per Plant Per Day", 
    fill = "Total Inflorescences per Plant") + 
  
  # making text bigger 
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold"), 
    panel.border = element_blank(), 
    # removing the weird black box around the chart 
    legend.position = "bottom"
  ) + 

  # adding x and y black axis lines 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.1) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.1) + 
  
  # applying line styling 
  # everything breaks here, we get a bunch of warning messages 
  scale_color_manual(values = type_cols) +
  scale_linetype_manual(values = type_ltys) + 
  
  # making the CIs grey and subtle 
  scale_fill_manual(values = rep("grey70", 4), guide = "none") + 
  guides(fill = "none") 
  
p_abundance

#------------------------------Graph for insect richness 

model2 = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmergeRichness, family = quasipoisson(link = "log"))

p_richness = interact_plot(
  model2, 
  pred = TotalInflorescences_AB, 
  modx = Flower.Type, 
  interval = TRUE,      # draw confidence interval 
  int.width = 0.95,     # 95% confidence interval 
  vary.lty = TRUE)      # so the dots and dashes stay 


type_cols <- c(
  "Mop"              = "#08306B",
  "Lacy"             = "#08306B",
  "Mop-like panicle" = "#8B0000",
  "Lacy panicle"     = "#8B0000"
)

type_ltys <- c(
  "Mop"              = "solid",
  "Lacy"             = "dotted",
  "Mop-like panicle" = "solid",
  "Lacy panicle"     = "dotted"
)

p_richness = p_richness + 
  
  # adding labels to x and y axes 
  labs(
    x = "log(Number of Fertile Flowers + 1)", 
    y = "Average # of Unique Taxa Per Plant Per Day", 
    fill = "Total Inflorescences per Plant") + 
  
  # making text bigger 
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold"), 
    panel.border = element_blank() # removing the weird black box around the chart 
  ) + 
  
  # adding x and y black axis lines 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.1) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.1) + 
  
  # applying line styling 
  scale_color_manual(values = type_cols) +
  scale_linetype_manual(values = type_ltys) + 
  
  # making the CIs grey and subtle 
  scale_fill_manual(values = rep("grey70", 4), guide = "none") + 
  guides(fill = "none") 

p_richness


###############################################################################
# Not using this but saving just in case 
###############################################################################

#------------------------------Graph for insect diversity  

model3 = glm(shannon ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = gaussian)
p = interact_plot(model3, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "Insect Diversity Increased as Fertile Flowers Increased, \nAt Consistent Rates",
       subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Shannon Diversity Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
