###########################################################################
######DATA

d = read.csv("BeeMachineAI_Photos_2_working.csv")

###########################################################################
######LIBRARIES

library(tidyverse)
library(tidytext)

install.packages("ggbreak")
library(ggbreak)

###########################################################################
######DPLYR WORK

d_agg_inflorescence = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  mutate(species_number = as.numeric(factor(species1))) %>% 
  drop_na(Trademark.Cultivar,
          AvgInflorescencesPerPanicle) %>%
  group_by(AvgInflorescencesPerPanicle, Date, species1) %>%
  summarize(species_count = n(), .groups = "drop") %>% 
  filter(AvgInflorescencesPerPanicle != "") %>% 
  mutate(Inflorescence_range = cut(AvgInflorescencesPerPanicle, 
                                   breaks = seq(0, 543, by = 10), 
                                   include.lowest = TRUE, 
                                   right = FALSE)) %>% 
  group_by(Inflorescence_range) %>% 
  mutate(n = n(),
         sorting = species_count / n) %>% 
  drop_na(Inflorescence_range) %>% 
  mutate(Month = month.name[as.integer(format(Date, "%m"))],
         Year = year(Date)) %>% 
  filter(Inflorescence_range != "[260,270)")
#---------------------------------------------------------------------

d_per_month1 = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Color,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F)
#----------------------------------------------------------------------

  d_distinct_species1 = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Date, species1) %>% 
  summarize(count = n(),
            P_Color = first(Color),
            .groups = "drop") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Date) %>% 
  summarize(P_Color = first(P_Color),
            count_distinct = n_distinct(species1),
            .groups = "drop") %>% 
  group_by(Trademark.Cultivar) %>% 
  summarize(average_richness = mean(count_distinct),
            P_Color = first(P_Color))

#--------------------------------------------------------------------------

d_inflorescence_type = d %>% 
  group_by(EYB_Inflorescence.Type, species1) %>% 
  summarize(count = n()) %>% 
  filter(EYB_Inflorescence.Type != "",
         count > 5) %>% 
  group_by(EYB_Inflorescence.Type) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense')))

  
  

###########################################################################
######GGPLOT

### MAKING A PLOT TO MEASURE THE INFLUENCE OF INFLORESCENCE ON VISITATION

ggplot(d_agg_inflorescence, aes(Inflorescence_range, sorting, fill = species1))+
  geom_col(show.legend = F)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10,2))+
  scale_x_discrete()+
  labs(x = "Inflorescence per Panicle",
         y = "Average Species Visitation",
         title = "Influence of Panicle Inflorescence on Insect Visitation")


### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE

ggplot(d_per_month1, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = Color)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(values = c("darkolivegreen1", 
                               "deeppink2", 
                               "lightpink", 
                               "white"))+
  labs(x = "Hydrangea Cultivar",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Color")

### MAKING A PLOT TO VISUALIZE THE AVERAGE NUMBER OF DISTINCT SPECIES IDENTIFIED AT EACH CULTIVAR

ggplot(d_distinct_species1, 
       aes(reorder_within(Trademark.Cultivar, 
                          average_richness,
                          P_Color),
           average_richness))+
  geom_col(aes(fill = P_Color), 
           color = "black", 
           width = 1, 
           show.legend = T)+
  scale_fill_manual(values = c("darkolivegreen1", 
                               "deeppink2", 
                               "lightpink", 
                               "white"))+
  coord_flip()+
  theme_minimal()+
  scale_x_reordered()+
  labs(x = "Hydrangea Cultivar",
       y = "Average Distinct Visiting Species",
       title = "Visitation Richness by Cultivar",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Color")

### MAKING A BOX PLOT WHERE THE Y AXIS REPRESENTS THE TOTAL NUMBER OF INDIVIDUALS OF A DISTINCT SPECIES THAT VISITED EACH INFLORESCENCE

ggplot(d_inflorescence_type, aes(EYB_Inflorescence.Type, count))+
  geom_boxplot(fill = "lightgray") +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize=0.5, 
               alpha = 0.7,
               color = "black") +
  theme_minimal()+
  labs(x = "Inflorescence Categorization",
       y = "Individuals of a Distinct Visiting Species",
       title = "Hydrangea Panicle Categorization and its Influence on Recorded Visitor Populations",
       caption = "Species with fewer than 5 visits excluded")+
  scale_x_discrete(limits = c('Open', 'Half-open', 'Dense', 'Very dense'))



