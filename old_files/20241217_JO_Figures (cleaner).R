###########################################################################
######DATA

d = read.csv("BeeMachineAI_Photos_2_working_12_10.csv")

d_agg_cult = read.csv("aggregated_by_cultivar.csv")

###########################################################################
######LIBRARIES

library(tidyverse)
library(tidytext)

###########################################################################
######DPLYR WORK

tag_test = d %>% 
  group_by(Trademark.Cultivar) %>%
  summarise(num_plants = n_distinct(Tag.Number)) 


d_agg_cult = d_agg_cult %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Year = year(Date)) %>% 
  left_join(tag_test, 
            by = "Trademark.Cultivar") %>% 
  mutate(abundance_per = (species_count / num_plants)) %>% 
  group_by(Trademark.Cultivar, 
           Month, 
           Year) %>% 
  mutate(sum_per = sum(abundance_per)) %>% 
  group_by(Trademark.Cultivar, 
           Month) %>% 
  reframe(n = n(),
          Month,
          Year,
          species1,
          Date,
          abundance_per,
          sum_per,
          sorting = sum_per/n)

##########################################################################
#######GGPLOT

abundance_per_plant = ggplot(d_agg_cult, 
       aes(x = reorder_within(Trademark.Cultivar, 
                              sum_per, 
                              Month), 
           y = sorting, 
           fill = species1)) +
  geom_col(show.legend = F) +
  facet_wrap(~Month, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(size = 12)) +
  labs(x = "Hydrangea Cultivar", 
       y = "Species Visitation (number)", 
       fill = "species",
       title = "Species Visitation to UW-Madison Arboretum Hydrangea",
       subtitle = "Spanning 2019-2021, bar fill indicates distinct visitor species")+
  scale_x_reordered()

ggsave(filename = "Abundance per Plant.png",
       plot = abundance_per_plant,
       device = png,
       width = 8,
       height = 6,
       units = "in")

