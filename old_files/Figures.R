#############################################################
### READING IN DATA

d = read.csv("BeeMachineAI_Photos_2_working.csv")
d_agg_cult = read.csv("aggregated_by_cultivar.csv")
#############################################################
### LIBRARIES
library(tidyverse)

install.packages("writexl")
library(writexl)
#############################################################
### FIGURES
d_agg_cult = d_agg_cult %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))]) %>% 
  left_join(tag_test, 
            by = "Trademark.Cultivar") %>% 
  mutate(abundance_per = (species_count / num_plants))
  

  

tag_test = d %>% 
  group_by(Trademark.Cultivar) %>%
  summarise(num_plants = n_distinct(Tag.Number)) 

##### Y AXIS IS SPECIES PER INDIVIDUAL PLANT



ggplot(d_agg_cult, aes(x = Trademark.Cultivar, y = abundance_per, fill = species1)) +
  geom_col(show.legend = F) +
  facet_wrap(facets = vars(Month), scales = "free") +  # Facet by month
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text.x = element_text(size = 12)  # Adjust facet title size
  ) +
  labs(x = "Cultivar", y = "Species Visitation", fill = "species")

