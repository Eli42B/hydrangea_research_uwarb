#Set up working directory 

rm(list = ls(all = TRUE)) #blanks out your values 
#getwd()
#file.choose() #find the dang filepath

setwd("C:/Users/Elizabeth/Documents/School/2024 Semester 1/Jude Ogden/Hydrangea_code")

d<-read.csv("InsectData_Cleaned_1.csv")
plot(d)

rm(list = ls(all = TRUE)) #blanks out your values 
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
d = read.excel()
