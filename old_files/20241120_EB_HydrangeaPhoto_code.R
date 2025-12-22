#Upload data
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")

rm(list = ls(all = TRUE))
d<-read.csv("InsectPhotos.csv")

### Clean data 

d<-d[d$det_num=="1",] #filter to only get detection number = 1, 
d$combination = d$det_confidence*d$prob1 #create a column that multiplies detection confidence and probability 

#Write an updated csv into the folder
write.csv(d,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/HydrangeaPhotoDataEdited.csv",row.names = TRUE)

#### Then I tested out this new dataframe by checking identifications 

############## Read in new dataframe
d = read.csv("HydrangeaPhotoDataEdited3.csv")

d_butterflies = d[d$species1=="Lepidoptera",] #take out the butterflies
d_confidence= d[d$det_confidence >= 0.7,] #filter by det_confidence > 0.7
d2 = rbind(d_butterflies,d_confidence)

#Write an updated csv into the folder
write.csv(d,"~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use/HydrangeaPhotoDataEdited4.csv",row.names = TRUE)

