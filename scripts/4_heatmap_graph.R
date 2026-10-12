
##########################################################
# heatmap 
##########################################################

# Loading libraries 

library(dplyr)
library(forcats)

# Loading data 
df1 = read.csv(heatmap_lowest_taxa_path)
df2 = read.csv(heatmap_morphospecies_path)

# lowest taxonomic level 
# -------------------------------------------------------

cm_df <- as.data.frame(
  table(
    Human = df1$human_species1,
    AI = df1$species1
  )
)

cm_df <- cm_df %>%
  mutate(
    Human = fct_reorder(Human, Freq, .fun = sum),
    AI = fct_reorder(AI, Freq, .fun = sum)
  )

ggplot(cm_df,
       aes(AI, Human, fill = Freq)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(
      angle = 75,
      vjust = 1,
      hjust = 1
    )
  )

# morphospecies 
# -------------------------------------------------------

cm_df <- as.data.frame(
  table(
    Human = df2$human_species1,
    AI = df2$species1
  )
)

cm_df <- cm_df %>%
  mutate(
    Human = fct_reorder(Human, Freq, .fun = sum),
    AI = fct_reorder(AI, Freq, .fun = sum)
  )

ggplot(cm_df,
       aes(AI, Human, fill = Freq)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(
      angle = 75,
      vjust = 1,
      hjust = 1
    )
  )


########################################################################3
########################################################################

# Garbageland

##########################################################################
##########################################################################

# attempted confusion matrix, realized didn't have 1:1 mapping of labels 

####################################################
# Confusion matrix 


# to compare predicted vs expected values
# for Bee Machine AI vs. human entomologist 
####################################################

# Libraries 
library(caret)

# Load data 


# Confusion matrix 1 (lowest taxaonomic level) 
df1$species1 = as.factor(df1$species1)
df1$human_species1 = as.factor(df1$human_species1)

confusionMatrix(data = df1$species1, reference = df1$human_species1)


#Creates vectors having data points
expected_value <- factor(c(1,0,1,0,1,1,1,0,0,1))
predicted_value <- factor(c(1,0,0,1,1,1,0,0,0,1))

#Creating confusion matrix (analysis)
example <- confusionMatrix(data=predicted_value, reference = expected_value) 
example # display results (analysis) 

# Getting just the matrix 
table(expected_value,predicted_value)

# Confusion Matrixes w/ gmodels 
# ---------------------------------------------------

#install required packages
#install.packages('gmodels')
#import required library 
library(gmodels)

#Computes the crosstable calculations
CrossTable(expected_value,predicted_value)






