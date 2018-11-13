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

between.point.distance <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           
  between.point.distances <- trackDistance(trip.matrix,     
                                           longlat = TRUE)
}


speed <- function(x){
  x$PointDist <- c(0,(between.point.distance(x)))           
  x$DateTime <- as.POSIXct(paste(x$Date, x$Time),           
                           tz="",                           
                           format="%d/%m/%Y %H:%M:%S")      
  x$TimeElapsed <- 0                                        
  for (i in 2:NROW(x)){
    x$TimeElapsed[i] <- difftime(x$DateTime[i], 
                                 x$DateTime[i-1], 
                                 units = "secs")            
  }
  
  (x$PointDist * 1000)/x$TimeElapsed                        
}


Dist.from.colony <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           
  distances <- spDistsN1(trip.matrix, trip.matrix[1,],      
                         longlat = TRUE)                    
}


turning.angle <- function(x){
  x$Angle <- NA                               
  for (i in 2:(NROW(x)-1)){                  
    a <- bearing(x[i-1, c("Longitude","Latitude")],x[i, c("Longitude","Latitude")])  
    b <- bearing(x[i, c("Longitude","Latitude")],x[i+1, c("Longitude","Latitude")])  
    
    c <- abs(b - a)  
    
    x$Angle[i] <- ifelse(c<= 180, c, 360-c) 
  }
  x$Angle
}


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

Give.trip.IDs <- function(x){
  x$Same <- NA
  
  for (i in 2:NROW(x)){
    x$Same[i] <- ifelse((x[i,"ColonyorTrip"] == x[i-1, "ColonyorTrip"]), "TRUE", "FALSE")
  }
  
  trip.matrix <- subset(x, ColonyorTrip == "trip")
  

  new.trips <- subset(trip.matrix, Same == FALSE)
  
  new.trips$TripID <- NA
  new.trips$TripID[1] <- paste0(new.trips[1,"BirdID"], ".01")
  if (NROW(new.trips) > 1) {
    for (i in 2:NROW(new.trips)){
      new.trips$TripID[i] <- (as.numeric(new.trips$TripID[i-1]) + 0.01)
    } 
  }
  
  trip.matrix2 <- merge(trip.matrix, new.trips, all.x = TRUE)
  trip.matrix2$TripID <- na.locf(trip.matrix2$TripID)
  
  trip.matrix3 <- merge(x, trip.matrix2,  all.x = TRUE)
  trip.matrix3$Same <- NULL
  
  
  for (i in 1:(NROW(trip.matrix3)-1)){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i+1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i+1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  for (i in 2:(NROW(trip.matrix3))){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i-1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i-1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  trip.matrix3
}


Give.trip.IDs.oldbird <- function(x, firstnumber){
  x$Same <- NA
  
  for (i in 2:NROW(x)){
    x$Same[i] <- ifelse((x[i,"ColonyorTrip"] == x[i-1, "ColonyorTrip"]), "TRUE", "FALSE")
  }
  
  trip.matrix <- subset(x, ColonyorTrip == "trip")
  
  new.trips <- subset(trip.matrix, Same == FALSE)
  
  new.trips$TripID <- NA
  new.trips$TripID[1] <- paste0(new.trips[1,"BirdID"], ".", firstnumber)
  if (NROW(new.trips) > 1) {
    for (i in 2:NROW(new.trips)){
      new.trips$TripID[i] <- (as.numeric(new.trips$TripID[i-1]) + 0.01)
    } 
  }
  
  trip.matrix2 <- merge(trip.matrix, new.trips, all.x = TRUE)
  trip.matrix2$TripID <- na.locf(trip.matrix2$TripID)
  
  trip.matrix3 <- merge(x, trip.matrix2,  all.x = TRUE)
  trip.matrix3$Same <- NULL
  
  
  for (i in 1:(NROW(trip.matrix3)-1)){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i+1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i+1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  for (i in 2:(NROW(trip.matrix3))){
    trip.matrix3$TripID[i] <- ifelse(((trip.matrix3[i,"ColonyorTrip"] == "colony") &
                                        (trip.matrix3[i-1,"ColonyorTrip"] == "trip")),
                                     trip.matrix3[i-1, "TripID"], trip.matrix3[i, "TripID"])
  }
  
  trip.matrix3
}


assign.pointID <- function(x, Year, firstnumber){      
  x <- x[with(x, order(BirdID, Date)),]                 
  x$PointID <- NA                                       
  x$PointID[1] <- paste0(Year, firstnumber)            
  for (i in 2:NROW(x)){                                 
    x$PointID[i] <- (as.numeric(x$PointID[i-1]) + 1)    
  }                                                     
  x                                                     
}                                                       
