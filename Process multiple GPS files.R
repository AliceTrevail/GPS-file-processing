# Packages & functions

source('F:/Data/R functions/Visualise_track_data.R', echo=TRUE)
source('F:/Data/R functions/Track_calculations.R', echo=TRUE)
source('F:/Data/R functions/Trip_metrics.R', echo=TRUE)
source('F:/Data/R functions/Number_trips_Allbirds.R', echo=TRUE)

#****************************************************************************

#Import file containing Bird ring numbers and Allocated bird ID numbers for analysis

BirdIDs <- read.csv("F:/Data/Data processing details/GPS data cleaning.csv", as.is = T)

#****************************************************************************

# Import from folder 1.
# Remove unneccessary columns from GPS logger (Course, Type, Distance, Essential & Speed)
# Add columns for colony, species
# Add columns for Year & Birdring from file name
# Add column for bird ID from BirdIDs file
# Add columns for Speed, Angle, Straightness, ColonyDistance & Colony or Trip
# Allocate trip IDs
# Save to folder 2.

path <- "F:/Data/GPS_Kittiwake_rathlin/1.Rathlin_ Before & after deployment points removed/2017/"

files <- list.files(path=path, pattern="*.csv")
for(file in files) {
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep=""), as.is = T))
}



mylist <- ls(pattern = "_2017")

lapply(mylist,      # apply to my list
       function(x){     # create new function
         df <- get(x)   # extract data frame from list
         df$Course <- NULL
         df$Type <- NULL
         df$Distance <- NULL
         df$Essential <- NULL
         df$Speed <- NULL
         df$Colony <- "Puffin"
         df$Species <- "Kittiwake"
         df$Year <- substr(x, 14, 17)
         df$Birdring <- substr(x, 1, 7)
         df$BirdID <- BirdIDs[BirdIDs$Bird.file.number == substr(x, 1, 7), "Allocated.bird.ID.number"]
         df$Speed <- speed(df)   
         df$Angle <- turning.angle(df)
         df$S <- Straightness(df)
         df$ColonyDist <- Dist.from.colony(df)
         df$ColonyorTrip <- ifelse(df$ColonyDist > 0.3, "trip","colony")
         df <- Give.trip.IDs(df)
         assign(paste0(x, "_2"), df, .GlobalEnv)  # assign data frame to environment with new name
         return(NULL)
       })

##### if repeat bird from previous year ####
mylist <- ls(pattern = "EJ")

lapply(mylist,      # apply to my list
       function(x){     # create new function
         df <- get(x)   # extract data frame from list
         df$Course <- NULL
         df$Type <- NULL
         df$Distance <- NULL
         df$Essential <- NULL
         df$Speed <- NULL
         df$Colony <- "Puffin"
         df$Species <- "Kittiwake"
         df$Year <- substr(x, 14, 17)
         df$Birdring <- substr(x, 1, 7)
         df$BirdID <- BirdIDs[BirdIDs$Bird.file.number == substr(x, 1, 7), "Allocated.bird.ID.number"]
         df$Speed <- speed(df)   
         df$Angle <- turning.angle(df)
         df$S <- Straightness(df)
         df$ColonyDist <- Dist.from.colony(df)
         df$ColonyorTrip <- ifelse(df$ColonyDist > 0.3, "trip","colony")
         df <- Give.trip.IDs.oldbird(df, firstnumber = "21")
         assign(paste0(x, "_2"), df, .GlobalEnv)  # assign data frame to environment with new name
         return(NULL)
       })

###########################################

mylist2 <- ls(pattern = "_2017_2")

setwd("F:/Data/GPS_Kittiwake_rathlin/2.Rathlin_ Speed, colonydist, colonyortrip, tripID added/2017")

lapply(mylist2,
       function(x){
         df <- get(x)
         write.csv(df, file = paste0(x, ".csv"), row.names = F)
       })


#****************************************************************************

# Import from folder 2.

path <- "F:/Data/GPS_Kittiwake_rathlin/2.Rathlin_ Speed, colonydist, colonyortrip, tripID added/2017/"
files <- list.files(path=path, pattern="*.csv")
for(file in files) {
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep=""), as.is = T))
}


individuals <- lapply(ls(pattern = "_2"), get)

alltracks <- rbind.fill(individuals)

trips <- Define.trips(alltracks, 14)
trips[trips$forage == "forage",]

#trips$forage[trips$TripID == "2017008.01"] <- "non-forage"
#trips$forage[trips$TripID == "2017008.02"] <- "non-forage"
#trips$forage[trips$TripID == "2017008.04"] <- "non-forage"
#trips$forage[trips$TripID == "2017019.29"] <- "non-forage"


alltracks$forage <- trips[match(alltracks$TripID, trips$TripID), "forage"]

alltracks$FTripID <- ifelse(alltracks$forage == "forage", alltracks$TripID, "NA")

Ki_Rathlin_2017_ALLbirds <- assign.pointID(alltracks, 2017, "03720")

setwd("F:/Data/GPS_Kittiwake_rathlin/3.ALLbirds_ALLpoints files")
write.csv(Ki_Rathlin_2017_ALLbirds, "Ki_Rathlin_2017_ALLbirds.csv", row.names = F)

map.rathlin.google(alltracks[alltracks$forage == "forage",], map.zoom = 8)
