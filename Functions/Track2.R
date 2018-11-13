if(!"sp" %in% installed.packages())
  install.packages("sp")
library(sp)

if(!"trip" %in% installed.packages())
  install.packages("trip")
library(trip)

if(!"maptools" %in% installed.packages())
  install.packages("maptools")
library(maptools)

if(!"plyr" %in% installed.packages())
  install.packages("plyr")
library(plyr)


########################################################################
################ Functions #############################################


########### Speed ###################

# calculates speed for point as speed from last GPS fix
# first speed allocated zero


between.point.distance <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           #creates two column matrix of lat and long for trip trackDistance function
  between.point.distances <- trackDistance(trip.matrix,     #calculate distance between each GPS point, into vector
                                           longlat = TRUE)
}


speed <- function(x){
  x$PointDist <- c(0,(between.point.distance(x)))           #dist in km
  x$DateTime <- as.POSIXct(paste(x$Date, x$Time),           #add column of date and time recognisable by r from existing date and time columns
                           tz="",                           #current time zone
                           format="%d/%m/%Y %H:%M:%S")      #for date format Year/Month/Date and time format Hours:Minutes:Seconds, units = mins
  x$TimeElapsed <- 0                                        #create empty column to fill with for loop
  for (i in 2:NROW(x)){
    x$TimeElapsed[i] <- difftime(x$DateTime[i], 
                                 x$DateTime[i-1], 
                                 units = "secs")            #time diff in mins
  }
  
  (x$PointDist * 1000)/x$TimeElapsed                        #speed = dist/time. in m/s
}


########### Distance from colony (km) ############

# calculates distance from GPS coordinates in first row of dataframe
# when pre- & post- deployment points removed, this is effectively
# distance from the colony


Dist.from.colony <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           #creates two column matrix of lat and long for trip trackDistance function
  distances <- spDistsN1(trip.matrix, trip.matrix[1,],      #calculate distances between GPS points and initial GPS point
                         longlat = TRUE)                    #in km
}



########### Turning angles ##############

# Calculates turning angle from compass bearing of travel between two adjacent points

turning.angle <- function(x){
  x$Angle <- NA                               # new column to fill with for loop
  for (i in 2:(NROW(x)-1)){                  # for second to penultimate row:
    a <- bearing(x[i-1, c("Longitude","Latitude")],x[i, c("Longitude","Latitude")])   # a = bearing from previous location to point of interest
    b <- bearing(x[i, c("Longitude","Latitude")],x[i+1, c("Longitude","Latitude")])   # b = bearing from point of interest to next location
    
    c <- abs(b - a)   # c = positive value of b-a
    
    x$Angle[i] <- ifelse(c<= 180, c, 360-c)   # if c is greater than 180, subtract c from 360 and assign to angle column
  }
  x$Angle
}


########## Straightness ##############

# create column with straightness across 5 rows

Straightness <- function(x){
  
  x$Straightness <- NA
  
  for (i in 2:(NROW(x)-1)){
    start <- x[i-1, c("Longitude","Latitude")]
    end <- x[i+1, c("Longitude","Latitude")]
    D <- dist(rbind(start, end)) # D = Euclidean dist between start & end points
    
    trip.matrix <- data.matrix(x[c((i-1):(i+1)),c("Longitude","Latitude")], 
                               rownames.force = NA)
    L <- sum(trackDistance(trip.matrix,longlat = FALSE)) # L = total path length travelled
    
    x$Straightness[i] <- D/L
  }
  
  x$Straightness
  
}

############# Assign trip Ids

# Automatically assign trip ID for an individual bird file

Give.trip.IDs <- function(x){
  # 1. Create new column where if colony or trip same as above = TRUE, if different = FALSE
  # This means that rows that are the start of a trip can be identified
  x$Same <- NA
  
  for (i in 2:NROW(x)){
    x$Same[i] <- ifelse((x[i,"ColonyorTrip"] == x[i-1, "ColonyorTrip"]), "TRUE", "FALSE")
  }
  
  # 2. Create subset that is only trip points
  trip.matrix <- subset(x, ColonyorTrip == "trip")
  
  # 3. Create subset that is only the first points of each trip
  new.trips <- subset(trip.matrix, Same == FALSE)
  
  # 4. Asign trip IDs
  # First trip ID = BirdID .01
  # Rest are sequential
  new.trips$TripID <- NA
  new.trips$TripID[1] <- paste0(new.trips[1,"BirdID"], ".01")
  if (NROW(new.trips) > 1) {
    for (i in 2:NROW(new.trips)){
      new.trips$TripID[i] <- (as.numeric(new.trips$TripID[i-1]) + 0.01)
    } 
  }
  
  # 5. Merge data frames to include trip IDs
  # Fill NAs with previous value of trip ID
  trip.matrix2 <- merge(trip.matrix, new.trips, all.x = TRUE)
  trip.matrix2$TripID <- na.locf(trip.matrix2$TripID)
  
  # 6. Merge data frames to include colony points again
  trip.matrix3 <- merge(x, trip.matrix2,  all.x = TRUE)
  trip.matrix3$Same <- NULL
  
  
  # 7. Give last point at colony before trip the same trip ID
  for (i in 1:(NROW(trip.matrix3)-1)){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i+1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i+1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  # 8. Give first point at colony after trip the same trip ID
  for (i in 2:(NROW(trip.matrix3))){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i-1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i-1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  trip.matrix3
}


Give.trip.IDs.oldbird <- function(x, firstnumber){
  # 1. Create new column where if colony or trip same as above = TRUE, if different = FALSE
  # This means that rows that are the start of a trip can be identified
  x$Same <- NA
  
  for (i in 2:NROW(x)){
    x$Same[i] <- ifelse((x[i,"ColonyorTrip"] == x[i-1, "ColonyorTrip"]), "TRUE", "FALSE")
  }
  
  # 2. Create subset that is only trip points
  trip.matrix <- subset(x, ColonyorTrip == "trip")
  
  # 3. Create subset that is only the first points of each trip
  new.trips <- subset(trip.matrix, Same == FALSE)
  
  # 4. Asign trip IDs
  # First trip ID = BirdID .01
  # If number of trips > 1; Rest are sequential
  new.trips$TripID <- NA
  new.trips$TripID[1] <- paste0(new.trips[1,"BirdID"], ".", firstnumber)
  if (NROW(new.trips) > 1) {
    for (i in 2:NROW(new.trips)){
      new.trips$TripID[i] <- (as.numeric(new.trips$TripID[i-1]) + 0.01)
    } 
  }
  
  # 5. Merge data frames to include trip IDs
  # Fill NAs with previous value of trip ID
  trip.matrix2 <- merge(trip.matrix, new.trips, all.x = TRUE)
  trip.matrix2$TripID <- na.locf(trip.matrix2$TripID)
  
  # 6. Merge data frames to include colony points again
  trip.matrix3 <- merge(x, trip.matrix2,  all.x = TRUE)
  trip.matrix3$Same <- NULL
  
  
  # 7. Give last point at colony before trip the same trip ID
  for (i in 1:(NROW(trip.matrix3)-1)){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i+1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i+1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  # 8. Give first point at colony after trip the same trip ID
  for (i in 2:(NROW(trip.matrix3))){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i-1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i-1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  trip.matrix3
}


########### assign point IDs ################

assign.pointID <- function(x, Year, firstnumber){                    # run function on data frame and year
  x <- x[with(x, order(BirdID, Date)),]                 # sort data frame by individual and date.
  x$PointID <- NA                                       # create new column of NAs
  x$PointID[1] <- paste0(Year, firstnumber)                 # first row point ID = 'Year00001'
  for (i in 2:NROW(x)){                                 # run loop for second to last row of data frame
    x$PointID[i] <- (as.numeric(x$PointID[i-1]) + 1)    # give point ID as previous row + 1
  }                                                     # end loop
  x                                                     # return data frame
}                                                       # end function
