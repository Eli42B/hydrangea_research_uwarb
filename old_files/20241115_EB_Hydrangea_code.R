########################################3

#Set up working directory
####################################

getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
rm(list = ls(all = TRUE)) #blanks out your values 

d = read.csv("InsectData_Cleaned_1.csv")
d_vials<-read.csv("InsectData_Cleaned_1.csv")
d_visual<- read.csv("VisualSurveys.csv")


#rm(list = ls(all = TRUE)) #blanks out your values 

#read in data from your clipboard
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
d = read.excel()

d = d[1:200,] #remove NAs 
######################################################
#load libraries
######################################################
install.packages()
install.packages(tidyverse)
library(tidyverse)

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


#####################################################################################
# data from abiotic stuf and observed bees not vacuumed 
#####################################################################################

rm(list = ls(all = TRUE)) #blanks out your values 

d_vials<-read.csv("InsectData_Cleaned_1.csv")
d_visual<- read.csv("VisualSurveys.csv")
dmerge = merge(d_vials, d_visual, by.x=c("Date", "Tag.Number"), by.y=c("Date", "Tag.Number"))
#OK the  merge worked but it removed all the rows where we collected information but didn't collect a vial
#whatever good enough for now we will look through it later 
#Jude that's your next project 

#how does insect abundance correspond to inflorescence abundance? 
################################################

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


