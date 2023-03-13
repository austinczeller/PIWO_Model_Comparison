#Site Selection
library(tidyverse)
#1. Tidying data----
tasks<-read.csv("data/SpeciesRawDownload/ABMI_Zeller_PIWO_Density_recording_task_report.csv")
             #,read.csv("data/SpeciesRawDownload/ABMI_Zeller-Woodpecker_Visual_Scanning_Project-ABMI_2021_recording_task_report.csv"))

hab<-read.csv("data/ABMIpoints_Beaudoin.hardbuffers.150.565.noABMIgrid-sf-terra.csv")

sites<-tasks%>%select(location,latitude,longitude)%>%unique()

n<-tasks%>%group_by(location)%>%count()
sites$n<-n$n[match(sites$location,n$location)]

#2. Add Environmental Covariates----
# % decid
sites$decid<-hab$SpeciesGroups_Broadleaf_Spp_v1.565m[match(sites$location,hab$location)]

# stand age
sites$age<-hab$Structure_Stand_Age_v1.565m[match(sites$location,hab$location)]

# dead wood
sites$dwood<-hab$Structure_Biomass_TotalDead_v1.565m[match(sites$location,hab$location)]

#3. Visualize Sampling----
#prev sampling
#decid
ggplot(sites)+geom_histogram(aes(x=decid))
#age
ggplot(sites)+geom_histogram(aes(x=age),binwidth = 10)
#deadwood
ggplot(sites)+geom_histogram(aes(x=dwood))

ggplot(hab)+geom_histogram(aes(x=Structure_Stand_Age_v1.565m))

#4. Identify sites we need to sample----
#decid 40-100%
#age >50 <100
#dwood looks pretty good, maybe >20
l<-c(sites$location)
current_sample<-sites
population<-data.frame(location=hab$location,
                       decid=hab$SpeciesGroups_Broadleaf_Spp_v1.565m,
                       age=hab$Structure_Stand_Age_v1.565m,
                       dwood=hab$Structure_Biomass_TotalDead_v1.565m,
                       year=hab$YEAR,
                       quadrant=hab$quadrant)
# Define function to even out a variable within bins of 10%
even_sample_within_bins <- function(var, n, priority) {
  # Calculate number of bins and bin width
  n_bins <- floor(100 / n)
  bin_width <- 100 / n_bins
  
  # Calculate bin indices for each observation
  bin_idx <- floor((var - 1e-6) / bin_width) + 1
  
  # Calculate the count of observations in each bin
  bin_counts <- table(bin_idx)
  
  # Calculate the target count of observations in each bin
  target_counts <- rep(n, n_bins)
  
  # Prioritize evenly sampling the "priority" variable
  if (priority %in% names(bin_counts)) {
    target_counts[names(target_counts) == priority] <-
      n + bin_counts[names(bin_counts) == priority] %% n
  }
  
  # Calculate the indices of observations to keep in each bin
  keep_indices <- lapply(1:n_bins, function(i) {
    sample(which(bin_idx == i), target_counts[i], replace = FALSE)
  })
  
  # Return the indices of observations to keep
  unlist(keep_indices)
}

# Even out the sampling distribution for the decid variable
decid_keep <- even_sample_within_bins(
  var = population$decid,
  n = length(current_sample$decid) / 10,
  priority = "decid"
)

# Even out the sampling distribution for the age variable
age_keep <- even_sample_within_bins(
  var = population$age,
  n = length(current_sample$age) / 10,
  priority = "decid"
)


# Get the new samples for each variable
new_decid <- population$decid[decid_keep]
new_age <- population$age[age_keep]


# Combine the new samples into a new data frame
new_sample <- data.frame(decid = new_decid, age = new_age)
s<-c(new_sample$decid)

# Identify the additional sites to be uploaded to WildTrax
sitesforupload<-population%>%filter(decid%in%s)%>%unique()

#Visualize the sample
ggplot(sitesforupload)+geom_histogram(aes(decid))
ggplot(sitesforupload)+geom_histogram(aes(age))
ggplot(sitesforupload)+geom_histogram(aes(dwood))

#5. Export site list----
write.csv(sitesforupload,"data/sitesforupload.csv")



#6. Misc. site selection----
#young forests with okay decid
a<-hab%>%filter(Structure_Stand_Age_v1.565m<50)%>%filter(!location%in%c(sites$location))%>%
  filter(SpeciesGroups_Broadleaf_Spp_v1.565m>30)
write.csv(a,"W:/BayneLabWorkSpace/Woodpeckers/DoneSoon/twentyyoungfrst.csv")
mean(a$Structure_Stand_Age_v1.565m)
mean(a$SpeciesGroups_Broadleaf_Spp_v1.565m)

#old forests at least 4% decid
b<-hab%>%filter(Structure_Stand_Age_v1.565m>100)%>%filter(SpeciesGroups_Broadleaf_Spp_v1.565m>4)
write.csv(b1,"W:/BayneLabWorkSpace/Woodpeckers/DoneSoon/oldboys.csv")
mean(b$Structure_Stand_Age_v1.565m)
mean(b$SpeciesGroups_Broadleaf_Spp_v1.565m)
