########################################3

#Set up working directory
####################################

getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 

###########################################
#Load data
##########################################

#d_vials<-read.csv("InsectData_Cleaned_1.csv")
#d_vials$Cerambycidae = as.numeric(d_vials$Cerambycidae)
#d_visual<- read.csv("VisualSurveys.csv")
dmerge = read.csv("dmergeFINAL.csv")

######################################################
#load libraries
######################################################
#install.packages(tidyverse)
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)

##########################################################################MMergicleaning data -- I ended up cleaning the merge in Excel and saving it as dmergeFINAL.csv 
#########################################################################

#d_vials2 = d_vials[,4:50]
#d_vials$sum = rowSums(d_vials2)
#write.csv(d_vials,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_vials.csv", row.names = TRUE)

#d_none = d_visual[d_visual$NoVialSample == 1,] #create a separate dataframe of null values 

#dmerge <- merge(d_vials,d_visual,by=c("Date","Tag.Number"))
#OK the  merge worked but it removed all the rows where we collected information but didn't collect a vial
#write.csv(dmerge,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/dmerge.csv", row.names = TRUE)

#write.csv(d_none,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/d_none.csv", row.names = TRUE)


####################################################
#aggregation attempts
#####################################################

#convert date into a date format 
dmerge$Date = as.Date(dmerge$Date,origin = "1900-01-01")

#sum by date and cultivar for total abundance of all spp, and total abundance by order 
dmerge6 = dmerge %>% 
  group_by(Date, Cultivar)%>% 
  summarize(TotalSpiders = sum(Araneae),
            TotalBeetles = sum(across(c(Chauliognathus:Scarabaeoidea))),
            TotalFlies = sum(across(c(Diptera:Ulidiidae))),
            TotalTrueBugs = sum(across(c(Hemiptera:Reduviidae))), 
            TotalHymenoptera = sum(across(c(Hymenoptera:Dolichovespula))),
            TotalButterflies = sum(across(c(Lepidoptera:Vanessa.atalanta)))) 
print(dmerge6, n = 15)

#OK this is looking good but we need to also divide it by how many plants we have of each cultivar 

#How many plants of each cultivar do we have? (thank you Jude)
tag_test = dmerge %>% 
  group_by(Cultivar) %>%
  summarise(num_plants = n_distinct(Tag.Number)) 
print(tag_test, n = 15)

#trying to add month back in 
#how to add month back in? 
#Code assumes yyyy-mm-dd format so you need to get the year since the data is mm-dd-yyyy

d_agg_cult = dmerge6 %>% 
  left_join(tag_test,
            by = "Cultivar") %>% 
  mutate(month = year(Date),na.rm = TRUE, format = "%mm%-%dd-%yy") %>% 
  mutate(month = month.abb[month]) %>%
  select(Date, Cultivar, month,TotalSpiders,TotalBeetles,TotalFlies,TotalTrueBugs, TotalHymenoptera, TotalHymenoptera)
print(d_agg_cult, n = 15)

#add a column that equals the number of plants per cultivar with left join, and then use mutate to make an abundance column that divides by the number of plants, and then add a column that equals month
d_agg_cult = dmerge6 %>% 
  left_join(tag_test,
            by = "Cultivar") %>% 
  mutate(Month = year(Date),na.rm = TRUE, format = "%mm%-%dd-%yy") %>%
  mutate(Month = month.abb[Month]) %>%
  mutate(
    RelAbundanceSpiders = TotalSpiders/num_plants,
    RelTotalBeetles = TotalBeetles/num_plants,
    RelTotalFlies = TotalFlies/num_plants,
    RelTotalTrueBugs = TotalTrueBugs/num_plants,
    RelTotalHymenoptera = TotalHymenoptera/num_plants,
    RelTotalButterflies = TotalButterflies/num_plants) %>% 
  select(
    Date, 
    Month,
    Cultivar, 
    RelTotalBeetles,
    RelTotalFlies,
    RelTotalTrueBugs,
    RelTotalHymenoptera,
    RelTotalButterflies)
print(d_agg_cult, n = 15, na.print = "")

#this is kind of ugly code and I think that we could put some of these pipes inside of one big one but that is too hard and I'm tired 
#OK here is aggregated by date and cultivar 

#### Let's aggregate just by cultivar 

#sum by date and cultivar for total abundance of all spp, and total abundance by order 
dmerge7 = dmerge %>% 
  group_by(Cultivar)%>% 
  summarize(TotalSpiders = sum(Araneae),
            TotalBeetles = sum(across(c(Chauliognathus:Scarabaeoidea))),
            TotalFlies = sum(across(c(Diptera:Ulidiidae))),
            TotalTrueBugs = sum(across(c(Hemiptera:Reduviidae))), 
            TotalHymenoptera = sum(across(c(Hymenoptera:Dolichovespula))),
            TotalButterflies = sum(across(c(Lepidoptera:Vanessa.atalanta)))) 
print(dmerge7, n = 25)

dmerge7 = dmerge7[-1,] #remove the NA row 
print(dmerge7, n = 25)

#OK this is looking good but we need to also divide it by how many plants we have of each cultivar 

#How many plants of each cultivar do we have? (thank you Jude)
tag_test = dmerge %>% 
  group_by(Cultivar) %>%
  summarise(num_plants = n_distinct(Tag.Number)) 
print(tag_test, n = 15)

dAgg2 = dmerge7 %>% 
  left_join(tag_test,
            by = "Cultivar") %>% 
  mutate(
    RelAbundanceSpiders = TotalSpiders/num_plants,
    RelTotalBeetles = TotalBeetles/num_plants,
    RelTotalFlies = TotalFlies/num_plants,
    RelTotalTrueBugs = TotalTrueBugs/num_plants,
    RelTotalHymenoptera = TotalHymenoptera/num_plants,
    RelTotalButterflies = TotalButterflies/num_plants) %>% 
  select(
    Cultivar, 
    RelTotalBeetles,
    RelTotalFlies,
    RelTotalTrueBugs,
    RelTotalHymenoptera,
    RelTotalButterflies)
print(dAgg2, n = 15, na.print = "")

#To create stackedk barcharts in ggplot2, we need the counts categorized by insect type


####################################################
#Time for the plots
######################################################
#https://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

#Load libraries
library(ggplot2)

#Plot by cultivar and relative abundance without date 
barplot(dAgg2$RelTotalAbundance)
barplot(height = dAgg2$RelTotalAbundance,
        main = "Relative Abundance of Insects by Cultivar",
        ylab = "Relative Insect Abundance",
        xlab = "Cultivars",
        names.arg = dAgg2$Cultivar,
        border = "pink",
        col = "purple")

#Basic Barplot in ggplot2 
ggplot(dAgg2, 
       aes(y = RelTotalAbundance,x = Cultivar)) + 
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip()

#Descending ggplot2 
ggplot(dAgg2,
       aes(x = reorder(Cultivar,-RelTotalAbundance), y = RelTotalAbundance, fill = Cultivar)) + 
  geom_bar(stat = "identity") + 
  labs(title = "2024 Relative Insect Abundance",
       x = "Cultivar", y = "Relative Total Insect Abundance") +
  coord_flip()

#OK the descending graph for hymenoptera looks good but something VERY wrong is going on with the total relative abundance and I have no idea what. Fix it later. 

#Arrange by descending order 
dAgg2_desc = dAgg2[order(-dAgg2$RelTotalAbundance),]
barplot(dAgg2_desc$RelTotalAbundance,
        main = "Relative Abundance of Insects by Cultivar",
        ylab = "Relative Insect Abundance",
        xlab = "Cultivars",
        names.arg = dAgg2$Cultivar,
        border = "pink",
        col = "purple")

#very simple barplot x = cultivar y = relative total abundance 
p = ggplot(data = dAgg2_desc,
           aes(x = Cultivar, y = RelTotalAbundance)) +
  geom_bar(stat = "identity", fill = "purple")
p
#horizontal bar plot 
p + coord_flip()

#create bar graphs by cultivar and relative abundance 
#create stacked bar graphs by date 


#very simple barplot x = cultivar y = relative total abundance 
p = ggplot(data = d_agg_cult,aes(x = Cultivar, y = RelTotalAbundance)) +
  geom_bar(stat = "identity", fill = "purple")
p
#horizontal bar plot 
p + coord_flip()

#descending order 
d_agg_cult = d_agg_cult[order(-d_agg_cult$RelTotalAbundance),]
p = ggplot(data = d_agg_cult,aes(x = Cultivar, y = RelTotalAbundance)) +
  geom_bar(stat = "identity", fill = "purple")
p

IB_asc <- IB[order(IB$Users),]
barplot(IB_asc$Users, 
        main = "2018 Internet Browser Users (in millions)",
        xlab = "Internet Browser", 
        ylab = "Users", 
        names.arg = IB_asc$Browser)

#------------------------------------------
#Jude's code 
abundance_per_plant = ggplot(d_agg_cult, 
                             aes(x = reorder_within(Trademark.Cultivar, 
                                                    sum_per, 
                                                    Month), 
                                 y = sorting, 
                                 fill = species1)) +
  geom_col(show.legend = F) +
  facet_wrap(~Month, scales = "free") +
  theme_minimal() +
  coord_flip()+
  #theme(
  # axis.text.x = element_text(angle = 45, hjust = 1),
  # strip.text.x = element_text(size = 12)) +
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




################################################
#Garbagelandia? 
################################################

#The current dmerge contains double rows whenever we had both vials and visual collected. Sum them. 

#let's look at our data 
dmerge2 = dmerge %>%
  group_by(Date,Tag.Number) %>% 
  select(Date, Tag.Number, Cultivar, Araneae)

#OK now let's sum by date, tag number, and cultivar 
dmerge3 = dmerge %>% 
  group_by(Date,Tag.Number, Cultivar) %>% 
  summarise(Araneae = sum(Araneae, na.rm = TRUE))
print(dmerge3, n = 20)

#now sum by date and cultivar for one critter 
dmerge4 = dmerge %>% 
  group_by(Date, Cultivar) %>% 
  summarise(Araneae = sum(Araneae, na.rm = TRUE))
print(dmerge4, n = 15)

#sum by date and cultivar for multiple critters 
dmerge5 = dmerge %>% 
  group_by(Date, Cultivar) %>% 
  summarise(
    Araneae = sum(Araneae, na.rm = TRUE),
    Chauliognathus = sum(Chauliognathus, na.rm = TRUE), 
    Syrphidae_total = sum(Syrphidae_total, na.rm = TRUE))
print(dmerge5, n = 15)

dmerge5 = dmerge %>% 
  group_by(Date, Cultivar)%>% 
  summarize(TotalAbundance = sum(across(c(Araneae:Miridae))))
print(dmerge5,n=15)


#we need to figure out how to sum abundance and richness in R rather than Excel. I'll do in Excel for now for speed 

#For now, we'll export d3 into excel 
write.csv(dmerge,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/dmerge_temporary.csv", row.names = TRUE)
#then in Excel I added the summed abundances by row. Note: We need to do richness separately. 
#After I did those changes in Excel, re-read it into R 

dmerge = read.csv("dmerge_temporary.csv")
dmerge$abundance_vacuum_and_visual #checking that it loaded in properly 

#x is independent, y is dependent. x should be floral resources, y should be insect abundance

plot(dmerge$abundance_vacuum_and_visual~dmerge$TotalInflorescences_AB)
#ok orders of magnitude are making this really hard 
plot(dmerge$abundance_vacuum_and_visual~sqrt(dmerge$TotalInflorescences_AB))
plot(dmerge$abundance_vacuum_and_visual~log10(dmerge$TotalInflorescences_AB)) #how many powers of ten are required to reach this number of inflorescneces? In other words, inflorescence as an order of magnitude

#calculate average abundance of insect visitors in Excel 
#re-read this new sheet back into R 

d4 = read.csv("d4temporary.csv")
d4$average_abundance
#remove the na values 
listA = c(d4$average_abundance) #make a list with a NA value
is.na(listA) #does our list have an NA value? 
sum(listA, na.rm = TRUE) #let's say we want to sum our list. We tell R to remove the NA value for a single command. 
listA = listA[!is.na(listA)] #or we can make a new listA without NA values
d4$[d4$average_abundance !==NA]
sum(listA) 
sum(d4$average_abundance, na.rm = TRUE)
d4 = d4$average_abundance
d4 = d4[!is.na(d4$average_abundance)]
listA = listA[!is.na(listA)] #or we can make a new listA without NA values 



##################### t test for whether one month is better than another 

#sum abundances for all critters 
d_mod = d %>%   #using ggplot to call multiple lines of commands
  group_by(Date) %>%  
  summarize (n=n(),insect_sum=sum(Dendryphantini..jumping.spiders.:Limniphilidae)) #sum across all columns of bugs 

#is it normal? Let's check if we can do this test 
plot(d$insect_sum~d$Date)
plot(d$total.visitors~d$jday)
hist(d$total.visitors,breaks=300)
abline(lm(d$total.visitors~d$jday))
qqnorm(residuals(lm(d$total.visitors~d$jday)))
qqline(residuals(lm(d$total.visitors~d$jday)))
plot(residuals(lm(d$total.visitors~d$jday)))
abline(h=0)
hist(residuals(lm(d$total.visitors~d$jday)))
summary(lm(d$total.visitors~d$jday))

#If not, if we take square root or ln is it normally distributed? 
#do the same checks

#-----------use t-tests to see if two sets are different 
y1<-c(1,28,78,1,58,2,1,0,0,1,0,24,0,1,0,0)
y2<-c(0,83,284,0,99,2,4,1,5,0,9,217,0,0,33,3)
t.test(y1,y2,paired=FALSE) #this is unnpaired because whether something showed up one month shouldn't change whether something showed up the next month

# a large t-score tells us that the groups are different, a small t-score means they are similar
#A low p-value (0.05 or less) is good 
#Different sample sizes are okay. The Welch approximation t-test is designed to do the same thing as an independent samples t-test, but without relying on the assumption that the variances are equal. It is readily available in most standard statistical software. In R, it's actually the default for the t.test function (the argument var.equal controls whether a IST or Welch approximation is performed). 

#conduct a power test 
sd(y1-y2)
mean(y1-y2)
power.t.test(n = NULL,delta = 34.0625, sd = 66.85553,sig.level = 0.05,power = .8)
#delta is difference of means, sd is difference of sds
#power is how often we are okay with it making a Type II error / the closer to 1.0 it is, the better the test is at detecting a false null hypothesis. 0.8 is about as generous as we can be and still be research-grade.  
# so we need 61.4 samples to make this worth anything 

#t-test checking between halictus and braconidae 

y1 = d$Lasioglossum
y2 = d$Braconidae

#normality 
plot(d$Lasioglossum~d$jday)
plot(d$Lasioglossum~d$jday)
hist(d$Lasioglossum,breaks=300)
abline(lm(d$Lasioglossum~d$jday))
qqnorm(residuals(lm(d$Lasioglossum~d$jday)))
qqline(residuals(lm(d$Lasioglossum~d$jday)))
plot(residuals(lm(d$Lasioglossum~d$jday)))
abline(h=0)
hist(residuals(lm(d$Lasioglossum~d$jday)))

#transform the data if needed 
d$Lasioglossum = log(d$Lasioglossum) 
#negative values and looks worse, we'll keep the original 

#t-test
#I do not think this is sufficiently normal 

#####################################################
#graphs

d_mod = d_vials %>%   #using ggplot to call multiple lines of commands
  group_by(Date) %>%  
  summarize (n=n(),lasioglossum_sum=sum(Lasioglossum)) #sum across lasioglossum 

ggplot(d_mod, aes(Date,lasioglossum_sum))+ #plot lasioglossum sum across date 
  geom_point() +
  geom_line(group=1)+
  geom_smooth(se = F, method = lm) #not sure the smoothing is working, that's hte last line

ggplot(d_mod, aes(Date,lasioglossum_sum))+
  geom_point() +
  geom_line(group=1)
