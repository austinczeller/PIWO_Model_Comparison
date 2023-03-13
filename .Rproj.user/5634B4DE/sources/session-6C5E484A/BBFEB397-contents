#Moving for Wildtrax
library(wildRtrax)
library(tidyverse)
library(lubridate)
#1. Setting path variables----
BUpubilic_prefix<- "P:/"
org<-"ABMI"
project<-"ABMI-EH"
visit<-"01"
numberofrecordings<-25
file_destination<-"W:/BayneLabWorkSpace/Woodpeckers/PIWO_Density/EveningOut"

#2. Location data----
locations<-read.csv("data/sitesforupload.csv")%>%filter(YEAR%in%c(2015,2016,2017,2018))
#Data folder structure is different past 2018
locs<-c(locations$location)
noquad<-c(str_sub(locations$location,end=-4))
noquad<-str_pad(noquad, 4, pad = "0")
quad<-c(locations$quadrant)
years<-c(locations$YEAR)
renamed_paths<-c()
#3. Mover Loop----
for(i in 1:nrow(locations)){
  location<-locs[i]
  sitet<-noquad[i]
  year<-years[i]
  quadt<-quad[i]
  PATH<-paste(BUpubilic_prefix,org,"/","ARU/",project,"/",year,"/",visit,"/",
              "ABMI-",sitet,"/",
              "ABMI-",sitet,"-",quadt,"/",sep="") 
  
  #There are discrepancies between ABMI and BU file path patterns, so make sure PATH matches your organization's pattern. 
  renamed_paths<-append(renamed_paths,PATH)
  
  files<-tibble("x"=list.files(path=renamed_paths[i],pattern=".wa",recursive=TRUE))
  
  #Gathering metadata for filtering functions. For pure random sampling do not run filtering functions.
  #Functions can be altered to get desired filters. Default is only recordings from 4am-10am.
  files<-wt_audio_scanner(PATH,file_type = "both") #Here you can filter for 
  
  files<-files%>%mutate(hour=hour(files$recording_date_time))
  files<-files%>%filter(hour%in%c(5:9))%>%filter(julian%in%95:110)
  
  
  
  #Randomly sampling from file list
  sample<-sample_n(files,numberofrecordings,replace=TRUE)
  #Mover script in R
  
  file.copy(sample$file_path,file_destination)
}
print("Transfer Complete")