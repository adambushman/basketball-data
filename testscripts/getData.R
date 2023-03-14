library(AirSensor) 
library(PWFSLSmoke) 
library(AirMonitorPlots)



#Set up your archive directory where going to store your files
archiveDir <- ("~/Weber State Lectures/Fall Semester 2021 Air Quality Math 4400/RcodeWSU4400")
setArchiveBaseUrl("http://air-sensor-data.mazamascience.com/PurpleAir/v1/")
dir.create(archiveDir, recursive = TRUE)

# Specify time range - Let's look at last month
startdate <- "2019-09-01"
enddate <- "2021-09-01"
timezone <- "Mountain" #olson timezone


###---LOADING THE DATA FOR THE COUNTRY AND FILTERING BY STATE ----#####
# Load the default 'pas' (today for the entire US, currently ~18000 sensors)
pas <- pas_load()
# Look at it
pas_leaflet(pas)


#LOS ANGELES (1,198 observations)
la <- pas_load()%>%
  pas_filterNear(latitude = 34.0522, longitude = -118.2437, radius = "25 Km")

#OAKLAND
oak <- pas_load()%>%
  pas_filterNear(latitude = 37.8044, longitude = -122.2712, radius = "8 Km")


setwd("~/R")

write.csv(la, file="LA_10-15", row.names = TRUE)
write.csv(oak, file="OAK_10-15", row.names = TRUE)
