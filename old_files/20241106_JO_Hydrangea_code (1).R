# Set up working directory

rm(list = ls(all = T)) ##blanks out your values
#getwd()
#file.choose() #Find filepath

setwd("C:/Users/judeo/OneDrive/Desktop/Bio 152 IP/data")

#getwd()

#read in data from your working directory
datasheet = read.csv("Datasheet_cleaned_1.csv")
  
plot(d)


# read in data from your clipboard
read.excel = function(header = T,...) {
  read.table("clipboard",sep = "\t", header = header,...)
}
datasheet = read.excel()

datasheet_modified = datasheet %>% 
  group_by(Date) %>% 
  summarize (n=n(),
             Diptera = sum(across(c(Anthomyiidae:Ulidiidae))),
             Hymenoptera = sum(across(c(Hymenoptera:Polistes))),
             Mean_Diptera = Diptera / n,
             Mean_Hymenoptera = Hymenoptera / n,
             Relative_Diptera = Diptera/Hymenoptera)
  
datasheet_richness_dipt = datasheet %>% 
  summarize(across(Dendryphantini..jumping.spiders.:Limniphilidae, sum)) %>% 
  select(Diptera:Syrphidae..total., Tachinidae, Tipulidae) %>%
  rename(Syrphidae = Syrphidae..total.) %>% 
  stack()



library(tidyverse)

##TOTAL COUNTED
total_counted = ggplot(datasheet_modified, aes(x = Date))+
  geom_point(aes(y = Diptera,
                 color = "firebrick3"))+
  geom_line(aes(y = Diptera, 
                group = 1,
                color = "firebrick3")) +
  geom_point(aes(y = Hymenoptera,
                 color = "steelblue"))+
  geom_line(aes(y = Hymenoptera,
                color = "steelblue", 
                group = 1)) +
  labs(title = "Hymenoptera and Diptera Visitation at UW-Madison Arboretum Panicled Hydrangea",
       x = "Date",
       y = "Total Observed",
       color = "Order") +
  scale_x_discrete(name = "Date",
                   breaks = c("2024-07-03", "2024-07-29", "2024-08-21"),
                   labels = c("2024-07-03", "2024-07-29", "2024-08-21")) +
  scale_color_manual(labels=c("Hymenoptera", "Diptera"), 
                     values=c("steelblue", "firebrick3")) +
  theme_bw()


##MEAN ABUNDANCIES
mean_abundance = ggplot(datasheet_modified, aes(x = Date))+
  geom_point(aes(y = Mean_Diptera,
                 color = "firebrick3"))+
  geom_line(aes(y = Mean_Diptera, 
                group = 1,
                color = "firebrick3")) +
  geom_point(aes(y = Mean_Hymenoptera,
                 color = "steelblue"))+
  geom_line(aes(y = Mean_Hymenoptera,
                color = "steelblue", 
                group = 1)) +
  labs(title = "Mean Hymenoptera and Diptera Visitation at UW-Madison Arboretum Panicled Hydrangea",
       x = "Date",
       y = "Mean Abundance",
       color = "Order") +
  scale_x_discrete(name = "Date",
                   breaks = c("2024-07-03", "2024-07-29", "2024-08-21"),
                   labels = c("2024-07-03", "2024-07-29", "2024-08-21")) +
  scale_color_manual(labels=c("Hymenoptera", "Diptera"), 
                     values=c("steelblue", "firebrick3")) +
  theme_bw()



##DIPTERA WITH RESPECT TO HYMENOPTERA
diptera_relative = ggplot(datasheet_modified, aes(Date, Relative_Diptera)) +
  geom_point(group = 1) +
  geom_line(group = 1) +
  labs(title = "Diptera relative to Hymenoptera at UW-Madison Arboretum Panicled Hydrangea",
       x = "Date",
       y = "Diptera:Hymenoptera") +
  scale_x_discrete(name = "Date",
                   breaks = c("2024-07-03", "2024-07-29", "2024-08-21"),
                   labels = c("2024-07-03", "2024-07-29", "2024-08-21")) +
  theme_bw()


##RICHNESS
Diptera_Richness = ggplot(datasheet_richness_dipt, aes(x = "", y = values, fill = ind)) +
  geom_bar(stat = "identity", 
           width = 1,
           color = "white") +
  coord_polar("y", 
              start = 0) +
  theme_void() +
  labs(title = "Species Richness of Diptera at UW-Madison Arboretum Panicled Hydrangea",
       fill = "Family") +
  scale_fill_manual("Family",
                    values = c("Syrphidae" = "black"))



ggsave(filename = "Total_Abundance.png",
       plot = total_counted,
       device = png,
       width = 8,
       height = 6,
       units = "in")

ggsave(filename = "Mean_Abundance.png",
       plot = mean_abundance,
       device = png,
       width = 8,
       height = 6,
       units = "in")

ggsave(filename = "Relative_Diptera.png",
       plot = diptera_relative,
       device = png,
       width = 8,
       height = 6,
       units = "in")

ggsave(filename = "Diptera_Richness.png",
       plot = Diptera_Richness,
       device = png,
       width = 8,
       height = 6,
       units = "in")
