########################################3

#Set up working directory
####################################

#getwd()
setwd("C:/Users/Elizabeth/Documents/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data_in_use")

rm(list = ls(all = TRUE)) #blanks out your values 

d<-read.csv("InsectData_Cleaned_1.csv")
plot(d)

rm(list = ls(all = TRUE)) #blanks out your values 

#read in data from your clipboard
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
d = read.excel()

######################################################
#load libraries
######################################################
#install.packages(tidyverse)





#####################################################
#graphs

library(tidyverse)

d_mod = d %>% 
  group_by(Date) %>% 
  summarize (n=n(),lasioglossum_sum=sum(Lasioglossum))

ggplot(d_mod, aes(Date,lasioglossum_sum))+
      geom_point() +
      geom_line(group=1)+
         geom_smooth(se = F, method = lm) #not sure the smoothing is working, that's hte last line

ggplot(d_mod, aes(Date,lasioglossum_sum))+
  geom_point() +
  geom_line(group=1)

###################### group by category 

summarize (n = n(),
           diptera_sum = sum(across(c(Anth))))
d$colSum = colSums(c[,4:76])
library(tidyverse)
ggplot(d_mod, aes(x = Date))+
  geom_point(aes(y = diptera))

d$Date2 = mdy(d$Date) then #fix your dates by creating a new column Date2 and telling it to create dates based on the pattern 'month day year'. Or 'day month year etc. depending on what your datea looks like 


##################### random stuff 

#is it normal? 

#check if we can do this test 
plot(d$Thomisidae~d$Date)


plot(d$total.visitors~d$jday)
hist(d$total.visitors,breaks=300)
abline(lm(d$total.visitors~d$jday))
qqnorm(residuals(lm(d$total.visitors~d$jday)))
qqline(residuals(lm(d$total.visitors~d$jday)))
plot(residuals(lm(d$total.visitors~d$jday)))
abline(h=0)
hist(residuals(lm(d$total.visitors~d$jday)))
summary(lm(d$total.visitors~d$jday))

plot()




