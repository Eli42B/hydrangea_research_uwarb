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
p_abundance = interact_plot(model1, pred = TotalInflorescences_AB, modx = Flower.Type)
p_abundance = p_abundance + 
  labs(
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Insects Per Plant Per Day", 
       fill = "Total Inflorescences per Plant") 

# ------------------------------- Attempting using copilot to make graph prettier 

type_cols <- c(
  "mop"              = "#08306B",
  "lacy"             = "#9CCBFF",
  "mop-like panicle" = "#8B0000",
  "lacy panicle"     = "#F4A3A3"
)

type_ltys <- c(
  "mop"              = "solid",
  "lacy"             = "dotted",
  "mop-like panicle" = "solid",
  "lacy panicle"     = "dotted"
)


model1 = glm.nb(TotalInsects ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge)
p_abundance = interact_plot(model1, pred = TotalInflorescences_AB, modx = Flower.Type)

p_abundance = p_abundance + 
  labs(
    x = "log(Number of Fertile Flowers + 1)", 
    y = "Average Insects Per Plant Per Day", 
    fill = "Total Inflorescences per Plant") 
p_abundance <- p_abundance +
  # ✅ If you want bold zero-lines inside the panel:
  geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
  
  # ✅ Apply your line styling
  scale_color_manual(values = type_cols) +
  scale_linetype_manual(values = type_ltys) +
  
  # ✅ Make text bigger + make plot feel “anchored”
  theme_bw(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18, face = "bold"),
    axis.title   = element_text(size = 16),
    axis.text    = element_text(size = 14),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 13),
    
    # “Not floating” look:
    axis.line = element_line(color = "black", linewidth = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  )

p_abundance

#------------------------------Graph for insect richness 

model2 = glm(InsectRichness ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmergeRichness, family = quasipoisson(link = "log"))
p_richness = interact_plot(model2, pred = TotalInflorescences_AB, modx = Flower.Type)
p_richness = p_richness + 
  labs(title = "Insect Richness Increased as Fertile Flowers Increased, \nBut at Different Rates",
       subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Number of Unique Taxa Per Plant Per Day", 
       fill = "Total Inflorescences per Plant") 
# plotting side by side 


library(cowplot)
plot_grid(p_abundance, p_richness, nrow = 1, ncol = 2, labels = "AUTO")

# mfrow won't work because it's base R and we are using ggplot2 

#------------------------------Graph for insect diversity  

model3 = glm(shannon ~ TotalInflorescences_AB*Flower.Type + TempF+ Month, data = dmerge, family = gaussian)
p = interact_plot(model3, pred = TotalInflorescences_AB, modx = Flower.Type)
p + 
  labs(title = "Insect Diversity Increased as Fertile Flowers Increased, \nAt Consistent Rates",
       subtitle = "Plotted by Flower Type",
       x = "log(Number of Fertile Flowers + 1)", 
       y = "Average Shannon Diversity Per Plant Per Day", 
       fill = "Total Inflorescences per Plant")
