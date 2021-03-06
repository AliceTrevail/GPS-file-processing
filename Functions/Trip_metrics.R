# From GPS files create file of trip metrics:
#     Total trip distance
#     Bearing from puffin island to farthest point along trip
#     First data point
#     Outbound straightness: straightness from colony to furthest point
#     Homeward straightness: straightness from furthest point to colony
#     Create trip metric file



###################################################################################
########################### Packages #############################################

if(!"sp" %in% installed.packages())
  install.packages("sp")
library(sp)

if(!"trip" %in% installed.packages())
  install.packages("trip")
library(trip)

if(!"geosphere" %in% installed.packages())
  install.packages("geosphere")
library(geosphere)

if(!"zoo" %in% installed.packages())
  install.packages("zoo")
library(zoo)



###################################################################################
########################### Functions #############################################


##### Total trip distance #####

total.trip.distance <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           #creates two column matrix of lat and long for trip trackDistance function
  between.point.distances <- trackDistance(trip.matrix,     #calculate distance between each GPS point, into vector
                                           longlat = TRUE)  #using Great Circle Distance method, if longlat = FALSE : Euclidean distance
  total.distances <- sum(between.point.distances)           #total distance = sum of all distances between individual GPS points
}


##### Total trip duration ######

total.trip.duration <- function(x){
  x$DateTime <- as.POSIXct(paste(x$Date, x$Time),           #add column of date and time recognisable by r from existing date and time columns
                           tz="",                           #current time zone
                           format="%d/%m/%Y %H:%M:%S")      #for date format Year/Month/Date and time format Hours:Minutes:Seconds
  time.start <- x[1,"DateTime"]                             #return start time from first row
  time.end <- tail(x[,"DateTime"],1)                        #return last time
  time.difference <- difftime(time.end,time.start,          #calculate time elapsed between start and end
                              units = ("hours"))            #in hours
}

##### Bearing from puffin island to farthest point along trip #####

farthest.bearing <- function(x){
  farthest.point <- x[which.max(x$ColonyDist),c("Longitude","Latitude")]  #extract long and lat for maximum distance from colony
  Puffin.island <- c(lon = -4.0296, lat = 53.3185)                        #set coordinates of puffin island
  # calculate angle from puffin island to furthest point using geosphere package
  # if 180-360, returns an anti-clockwise angle from N, so ifelse part to correct
  angle <- ifelse(bearing(Puffin.island,farthest.point)<0, 360+bearing(Puffin.island,farthest.point), bearing(Puffin.island,farthest.point))
}

#### first data point
first <- function(x){
  ifelse(all(is.na(x)), return("NA"), # if column only NAs, print "NA"
         (head(na.trim(x), n=1L)))    # otherwise, return first value of vector that isn't NA
}

#### last data point
last <- function(x){
  ifelse(all(is.na(x)), return("NA"), # if column only NAs, print "NA"
         (tail(na.trim(x), n=1L)))    # otherwise, return last value of vector that isn't NA
}


#### Outbound straightness: straightness from colony to furthest point

Straightness.out <- function(x){
  # D = Euclidean dist between start & end points
  start <- x[1, c("Longitude","Latitude")]
  end <- x[which.max(x[,"ColonyDist"]), c("Longitude","Latitude")]
  D <- dist(rbind(start[,c("Longitude","Latitude")], end[,c("Longitude","Latitude")]))
  
  # L = total path length travelled
  end.no <- which.max(x[,"ColonyDist"])
  trip.matrix <- data.matrix(x[c(1:end.no),c("Longitude","Latitude")], 
                             rownames.force = NA)
  L <- if (nrow(trip.matrix) <2){return("NA")} else {sum(trackDistance(trip.matrix,longlat = FALSE))}
  
  # Straightness = D/L
  S <- if (L == "NA"){print("NA")} else {D/L}
  S
}


#### Homeward straightness: straightness from furthest point to colony

Straightness.home <- function(x){
  # D = Euclidean dist between start & end points
  start <- x[which.max(x[,"ColonyDist"]), c("Longitude","Latitude")]
  end <- x[NROW(x), c("Longitude","Latitude")]
  D <- dist(rbind(start, end))
  
  # L = total path length travelled
  start.no <- which.max(x[,"ColonyDist"])
  trip.matrix <- data.matrix(x[c(start.no:NROW(x)),c("Longitude","Latitude")], 
                             rownames.force = NA)
  L <- if (nrow(trip.matrix) <2){return("NA")} else {sum(trackDistance(trip.matrix,longlat = FALSE))}
  
  
  # Straightness = D/L
  S <- if (L == "NA"){print("NA")} else {D/L}
  S
}

### Proportion of time flying

Prop.flying <- function(x){
  flight <- x[complete.cases(x$Speed),"Speed"] > 1      #generates logical vector where speed > 1 is TRUE
  proportion <- (sum(flight)/NROW(complete.cases(x$Speed)))*100
  return(proportion)
}


##### Create trip metric file #####
# one row per defined trip

Create.trip.metric.file <- function(x){
  x <- x[order(x$TripID),]
  Trip_metrics <- unique(x[c("Year",        # Creates new matrix for each unique row, ie. each unique trip
                             "Species",     # Includes columns year, species, colony, birdID and TripID and Date from original file
                             "Colony",
                             "BirdID",
                             "TripID"
  )])
  Trip_metrics <- Trip_metrics[complete.cases(Trip_metrics),]   #removes na's
  metric.file <- data.frame(Trip_metrics)                       #convert to data frame in order to add columns
  metric.file$TripDate <- tapply(x$Date, x$TripID, first)   #new column = trip start date
  
  metric.file$noPoints <- tapply(x$TripID, x$TripID, length)
  
  metric.file$DepTime <- tapply(x$Time, x$TripID, first)   #new column = trip departure time
  metric.file$TripDuration <- by(x, x$TripID, total.trip.duration)   #new column = trip duration by applying function to entire dataframe by group = tripID
  metric.file$TotalDistance <- by(x, x$TripID, total.trip.distance)  #new column = total trip distance
  metric.file$MaxDistance <- tapply(x$ColonyDist, x$TripID, max)     #new column = max distance from colony for each trip by returning maximum distance from colony for each group=tripID
  metric.file$AngleToMaxDist <- by(x, x$TripID, farthest.bearing)   #new column = bearing from puffin island to point farthest from the colony
   
  return(metric.file)   #return new data frame in order to save it to the environment
}


##### Define trips #####
# one row per defined trip

Define.trips <- function(x, minutes){
  Trip_metrics <- unique(x[c("BirdID",
                             "TripID"
  )])
  Trip_metrics <- Trip_metrics[complete.cases(Trip_metrics),]   #removes na's
  metric.file <- data.frame(Trip_metrics)                       #convert to data frame in order to add columns
  metric.file$TripDurationhrs <- by(x, x$TripID, total.trip.duration)   #new column = trip duration by applying function to entire dataframe by group = tripID
  metric.file$TripDurationmins <- 60*(metric.file$TripDurationhrs)   #new column = trip duration by applying function to entire dataframe by group = tripID
  metric.file$TotalDistance <- by(x, x$TripID, total.trip.distance)  #new column = total trip distance
  metric.file$MaxDistance <- tapply(x$ColonyDist, x$TripID, max)     #new column = max distance from colony for each trip by returning maximum distance from colony for each group=tripID
  metric.file$AvSpeed <- tapply(x$Speed, x$TripID, mean, na.rm=TRUE)
  metric.file$FlightPercent <- by(x, x$TripID, Prop.flying)
  metric.file$AvAlt <- tapply(x$Altitude, x$TripID, mean)
  metric.file$forage <- ifelse(metric.file$TripDurationmins >= minutes, "forage", "non-forage")
  metric.file$forage <- ifelse(metric.file$FlightPercent == 0, "non-forage", metric.file$forage)
  metric.file$forage <- ifelse(metric.file$AvAlt > 100, "non-forage", metric.file$forage)
  return(metric.file)   #return new data frame in order to save it to the environment
}



