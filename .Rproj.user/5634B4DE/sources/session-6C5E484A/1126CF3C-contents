#Clean Data for unmarked dataframe
library(lubridate)
library(tidyverse)
library(unmarked)
#1. Load data----
data <- read.csv("data/SpeciesRawDownload/ABMI_Zeller_PIWO_Density_basic_summary.csv")


#Site covariates
stand_covariates <- read.csv("data/ABMIpoints_Beaudoin.hardbuffers.150.565.noABMIgrid-sf-terra.csv")
siteCovs <- stand_covariates%>%
  select(location, YEAR, longitude, latitude,Structure_Stand_Age_v1.565m, 
         SpeciesGroups_Broadleaf_Spp_v1.565m,Structure_Biomass_TotalDead_v1.565m)%>%
  group_by(location)%>%
  summarize(YEAR=min(YEAR),longitude, latitude,Structure_Stand_Age_v1.565m, 
            SpeciesGroups_Broadleaf_Spp_v1.565m,Structure_Biomass_TotalDead_v1.565m)%>%unique()

siteCovs<-siteCovs%>%filter(location%in%data$location)
data<-data%>%filter(location%in%siteCovs$location)%>%filter(!status=="New")



#2. Assign Habitat Variables----
# Filter for only the columns i want 


#Only keep the rows with sites that are used in analysis

siteCovs <- na.omit(siteCovs)%>%
  group_by(location) %>%
  ungroup()
#I filtered out the 2020 data because there there is two entries for some sites (one from 2017 and one from 2020)


#2. Scale/standardize your variables before you add site cover data to unmarked dataframe----
# Stand age within 565m buffer radius of point 
siteCovs$Structure_Stand_Age_v1.565m <-ifelse(siteCovs$Structure_Stand_Age_v1.565m==0,1,siteCovs$Structure_Stand_Age_v1.565m)
siteCovs$Structure_Stand_Age_v1.565m <-siteCovs$Structure_Stand_Age_v1.565m/max(siteCovs$Structure_Stand_Age_v1.565m)
range(siteCovs$Structure_Stand_Age_v1.565m) # just check to make sure range is 0-1

# Decid %
siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m <- ifelse(siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m==0,1,siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m)
siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m <- siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m/max(siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m)
range(siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m)

# Deadwood
siteCovs$Structure_Biomass_TotalDead_v1.565m <- ifelse(siteCovs$Structure_Biomass_TotalDead_v1.565m==0,1,siteCovs$Structure_Biomass_TotalDead_v1.565m)
siteCovs$Structure_Biomass_TotalDead_v1.565m <- siteCovs$Structure_Biomass_TotalDead_v1.565m/max(siteCovs$Structure_Biomass_TotalDead_v1.565m)
range(siteCovs$Structure_Biomass_TotalDead_v1.565m)


#3. Join site Covs with data----
# remove locatios from data that there are no site covariates for by merging based on location names 

data$decid<-siteCovs$SpeciesGroups_Broadleaf_Spp_v1.565m[match(data$location,siteCovs$location)]
data$age<-siteCovs$Structure_Stand_Age_v1.565m[match(data$location,siteCovs$location)]
data$deadwood<-siteCovs$Structure_Biomass_TotalDead_v1.565m[match(data$location,siteCovs$location)]


#4. Wrangle observations and observation covariates for the species of interest----
visits <- data %>%
  dplyr::filter(species_code=="PIWO") %>%
  dplyr::select(location, recording_date) %>%
  unique() %>%
  mutate(occur=1) %>%
  right_join(data %>%
               dplyr::select(location, recording_date, observer, method) %>%
               unique(),
             by=c("location", "recording_date")) %>%
  mutate(occur = ifelse(is.na(occur), 0, 1),
         recording_date = as.POSIXct(recording_date),
         year = format(recording_date, format = "%Y"),
         doy = as.numeric(format(recording_date, format = "%j")),
         hr = as.numeric(format(recording_date, format = "%H"))) %>%
  group_by(location) %>%
  arrange(recording_date) %>%
  mutate(visit = row_number()) %>%
  ungroup()

#5. Create location X recording dataframe of observations----
#(1 for detected, 0 for undetected)
visits$occur <- as.integer(visits$occur)
str(visits$occur)
y <- visits %>%
  dplyr::select(location, visit, occur) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = occur) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()


#6. Create location X recording dataframes for observation covariates----
#(doy = day of year, hr = hour of day, method = processing method, observer = observer ID)
visits$doy <- as.integer(visits$doy)
str(visits$doy)
doy <- visits %>%
  dplyr::select(location, visit, doy) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = doy) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

doy2 <- visits %>%
  mutate(doy2 = doy^2) %>%
  dplyr::select(location, visit, doy2) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = doy2) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

visits$hr <- as.integer(visits$hr)
hr <- visits %>%
  dplyr::select(location, visit, hr) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = hr) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

hr2 <- visits %>%
  mutate(hr2 = hr^2) %>%
  dplyr::select(location, visit, hr2) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = hr2) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

method <- visits %>%
  dplyr::select(location, visit, method) %>%
  mutate(method = as.factor(method)) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = method) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

observer <- visits %>%
  dplyr::select(location, visit, observer) %>%
  mutate(observer = as.factor(observer)) %>%
  pivot_wider(id_cols = location, names_from = visit, values_from = observer) %>%
  arrange(location) %>%
  dplyr::select(-location) %>%
  data.frame()

#7. Create a list of the observation covariates----
obsCovs <- list(doy=doy, doy2=doy2, hr=hr, hr2 = hr2, method=method, observer=observer)


#8. Put together as an unmarked object for single species occupancy models----
# Run this line if you dont have site covariates 
#umf <- unmarkedFrameOccu(y=y, siteCovs=NULL, obsCovs=obsCovs)

#This one if you have covs 
umf <- unmarkedFrameOccu(y=y, siteCovs=siteCovs, obsCovs=obsCovs)
