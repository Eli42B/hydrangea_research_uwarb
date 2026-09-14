############## graphs for panicle ydrangea cultivars daily visitation graph  ################

#################################
#Setup
#################################

d = read.csv(beemachine_photos_filepath)

#################################
#dplyr
#################################

d_per_month1 = d %>% 
  mutate(Color = trimws(Color)) %>% 
  mutate(Date = ymd(Date),
         Month = month.name[as.integer(format(Date, "%m"))],
         Month = factor(Month, levels = month.name),
         Year = year(Date)) %>% 
  group_by(Trademark.Cultivar, Month, species1) %>% 
  summarize(days = n_distinct(Date), 
            Color,Flower.Type,
            .groups = "keep") %>% 
  filter(Trademark.Cultivar != "") %>% 
  group_by(Trademark.Cultivar, Month) %>% 
  slice_max(days, with_ties = F)

#################################
#Original Graph
#################################

ggplot(d_per_month1, aes(reorder_within(Trademark.Cultivar, days, Month), days, fill = Flower.Type)) +
  geom_col(color = "black", width = 1, show.legend = T)+
  coord_flip()+
  facet_wrap(~Month, scales = "free")+
  theme_minimal()+
  scale_x_reordered()+
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(0,4,1))+
  scale_fill_manual(values = c("Grey", 
                               "White"))+
  labs(x = "",
       y = "Days with at least 1 Observed Visitor",
       title = "Hydrangea Cultivars Daily Visitation",
       subtitle = "Bar colors correspond to panicle shape",
       fill = "Panicle Shape")

##########################
#Stacked Bargraph
##########################

library(ggplot2)

p = ggplot(data = d_per_month1, aes(x = Month, y = days, fill = Trademark.Cultivar)) + 
  geom_bar(stat = "identity")
p + ggtitle("Cultivar Popularity by Month") + ylab("Number of Weeks with Insects Reported") + xlab("Month")

#Alternate title
p + ggtitle("Researchers Observed Insects More \n Frequently, On More Hydrangeas, \n in Later Months") + ylab("Number of Weeks with Insects Reported") + xlab("Month")

