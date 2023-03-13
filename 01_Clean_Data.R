#Cleaning data 
library(tidyverse)
#1. Load data----
df<-read.csv("data/SpeciesRawDownload/ABMI_Zeller_PIWO_Density_basic_summary.csv")
hab<-read.csv("data/ABMIpoints_Beaudoin.hardbuffers.150.565.noABMIgrid-sf-terra.csv")
#2. Assign Habitat Variables----
df$decid<-hab$SpeciesGroups_Broadleaf_Spp_v1.565m[match(df$location,hab$location)]
df$age<-hab$Structure_Stand_Age_v1.565m[match(df$location,hab$location)]
df$deadwood<-hab$Structure_Biomass_TotalDead_v1.565m[match(df$location,hab$location)]

#3. Condense for Occupancy and Intensity of use----
occ <- df %>% 
  group_by(location) %>% 
  summarize(PIWO = ifelse(any(grepl("PIWO", species_code)), 1, 0),
            age=age,
            decid=decid,
            deadwood=deadwood)%>%unique()
intens<-df%>%
  group_by(location) %>% 
  summarize(PIWO_count = sum(grepl("PIWO", species_code)),
            age=age,
            decid=decid,
            deadwood=deadwood)%>%unique()
#4. Explore data----
obs<-df%>%select(location,recording_date,age,decid,deadwood)%>%unique()
obs<-obs%>%group_by(location)%>%summarize(n=length(location),age=age,decid=decid,deadwood=deadwood)%>%unique()
hist(obs$age,breaks= 20)
hist(obs$decid)
hist(obs$deadwood)
ggplot(obs)+geom_bar(aes(x=n))



#5. Write as csv----
write.csv(occ,"data/occupancy.csv")
write.csv(intens,"data/intensity.csv")
