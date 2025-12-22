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

d_per_month2 = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Color,
            EYB_Inflorescence.Type,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F) %>% 
  drop_na(EYB_Inflorescence.Type) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense')))


#----------------------------------------------------------------------

d_distinct_species2 = d %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Date, species1) %>% 
  reframe(count = n(),
            P_Color = first(Color),
            EYB_Inflorescence.Type,
            .groups = "drop") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Date) %>% 
  reframe(P_Color = first(P_Color),
            count_distinct = n_distinct(species1),
            EYB_Inflorescence.Type,
            .groups = "drop") %>% 
  group_by(Trademark.Cultivar) %>% 
  reframe(average_richness = mean(count_distinct),
            P_Color = first(P_Color),
            EYB_Inflorescence.Type,) %>% 
  arrange(match(EYB_Inflorescence.Type, c('Open', 'Half-open', 'Dense', 'Very dense'))) %>% 
  distinct()

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


### MAKING A PLOT TO VISUALIZE THE DAYS PER MONTH THAT EACH CULTIVAR WAS VISITED AT LEAST ONCE (FILLED BY OPENNESS)

ggplot(d_per_month2, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = EYB_Inflorescence.Type)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,3))+
  scale_fill_manual(limits = c('Open', 
                               'Half-open', 
                               'Dense', 
                               'Very dense'),
                    values = c("white", 
                               "gray67", 
                               "gray33", 
                               "black"))+
  labs(x = "Hydrangea Cultivar",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Inflorescence Type")

### MAKING A PLOT TO VISUALIZE THE AVERAGE NUMBER OF DISTINCT SPECIES IDENTIFIED AT EACH CULTIVAR

ggplot(d_distinct_species2, 
       aes(reorder_within(Trademark.Cultivar, 
                          average_richness,
                          EYB_Inflorescence.Type),
           average_richness))+
  geom_col(aes(fill = EYB_Inflorescence.Type),
           color = "black", 
           width = 1, 
           show.legend = T)+
  scale_fill_manual(limits = c('Open', 
                               'Half-open', 
                               'Dense', 
                               'Very dense'),
                    values = c("white", 
                               "gray67", 
                               "gray33", 
                               "black"))+
  coord_flip()+
  theme_minimal()+
  scale_x_reordered()+
  labs(x = "Hydrangea Cultivar",
       y = "Average Distinct Visiting Species",
       title = "Visitation Richness by Cultivar",
       subtitle = "Bar colors correspond to panicle coloration",
       fill = "Panicle Inflorescence Type")



