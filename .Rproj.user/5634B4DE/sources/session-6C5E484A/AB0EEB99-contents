#Uploading recordings that are in other WT projects
library(wildRtrax)
library(lubridate)
library(tidyverse)
#This is utter bullshit. This task thing is absolute garbage.
#1. Make a list of all the files/recordings we want in our project----
files<-wt_audio_scanner(path="W:/BayneLabWorkSpace/Woodpeckers/PIWO_Density",file_type = 'both')
files<-files%>%select(location,recording_date_time)
files<-files%>%mutate(recordingDate=recording_date_time)
files$recording_date_time<-NULL

#2. Remove leading ABMI- and 0's from locations----
reallocs<-gsub("ABMI-","",files$location)
reallocs<-sub("0+","",reallocs)
files$location<-reallocs
#3. Create unique ID----
files$ID<-paste(files$location,files$recording_date_time,sep="")

#4. Download Current WT tasks and format correctly----
original_tasks<-read.csv("W:/BayneLabWorkSpace/Woodpeckers/PIWO_Density/Zeller PIWO Density_Tasks_20230222.csv",fileEncoding="UTF-8-BOM")

original_tasks$recordingDate<-str_sub(original_tasks$recordingDate, 2, nchar(original_tasks$recordingDate))#removes the space before the date
original_tasks$recordingDate<-ymd_hms(original_tasks$recordingDate) #date format
original_tasks$ID<-paste(original_tasks$location,original_tasks$recordingDate,sep="")

#3. Join the dataframes----
output<-bind_rows(files,original_tasks)

#4. Remove duplicated files----
output<-output[!duplicated(output$ID),]
output$method<-"None"
output$taskLength<-"60"
output[output==""]<-NA
#5. Save CSV----

write.csv(output,"W:/BayneLabWorkSpace/Woodpeckers/PIWO_Density/updatedtasks.csv", row.names = FALSE)

#6. Remove random error rows----
#-1 from what WT tells you

output<-output[-252,]
output<-output[-251,]
output<-output[-174,]
output<-output[-156,]
output<-output[-155,]
output<-output[-154,]

output<-output[-249,]
output<-output[-241,]
output<-output[-238,]
output<-output[-221,]
output<-output[-216,]
output<-output[-209,]
output<-output[-204,]
output<-output[-178,]
output<-output[-177,]
output<-output[-176,]
output<-output[-175,]
output<-output[-136,]
output<-output[-135,]
output<-output[-133,]
output<-output[-132,]
output<-output[-131,]
output<-output[-126,]
output<-output[-121,]
output<-output[-120,]
output<-output[-119,]
output<-output[-117,]
output<-output[-66,]
output<-output[-18,]

output<-output[-201,]
output<-output[-64,]
