
#**********************************************
# set working directory  ####
setwd("F:/R GPS code testing")


#**********************************************
# source functions ###
# run files containing functions, in this case located within working directory:
# if not in working directory, just add full file path
source('Functions/Visualise_track_data.R', echo=TRUE)
source('Functions/Track_calculations.R', echo=TRUE)
source('Functions/Trip_metrics.R', echo=TRUE)



#**********************************************
# Import GPS file ###

# import .csv file of individual bird GPS data, again in this case located within working directory:
# if not in working directory, just add full file path
bird <- read.csv("Testkittiwake.csv")
head(bird)

#*** In this example, I have already added the Year, Species, Colony and BirdID in excel

#**********************************************
# Visualise data ###
  
# for entire csv file, plot distance from first GPS location against time
# this can help to identify erroneous points & get an idea of how many trips taken
plot.trip.distance(bird)

# plot over google maps.
# first define colony (longitude, latitude) to position centre of map: 
skomer.is <- c(lon = -5.275, lat = 51.7375)
# plot on google maps
map.colony.google(bird, colony = skomer.is)
# zoom in this function has to take an integer value with max 12. Default set to 9, but can zoom in as follows:
map.colony.google(bird, colony = skomer.is, map.zoom = 10)



#**********************************************
# Process data ###

# add column for speed
bird$Speed <- speed(bird)
head(bird)
# the first row will have a speed of NA, can set to zero:
bird$Speed[is.na(bird$Speed)] <- 0
head(bird)

#add colum for the distance of each row from the colony:
bird$ColonyDist <- Dist.from.colony(bird)
head(bird)

# for each row, define if at the colony or out on a trip, with a buffer of 100m in this case.
# change buffer in "bird$ColonyDist > 0.1", here the units are km. So, 0.1 = 100m; 0.3 = 300m etc.
bird$ColonyorTrip <- ifelse(bird$ColonyDist > 0.1, "trip","colony")
head(bird)
plot.trip.distance(bird[bird$ColonyorTrip == "trip",])

# assign trip IDs
bird.trips <- Give.trip.IDs(bird)
head(bird.trips)



#**********************************************
# Create summary trip metric file ###
trips <- Create.trip.metric.file(bird.trips)
trips


#**********************************************
# Save new data file

write.csv(bird.trips, file="kittiwake_processed.csv", row.names=FALSE)
write.csv(trips, file="Trip_summary.csv", row.names=FALSE)