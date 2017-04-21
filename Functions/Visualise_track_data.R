# For stage 1 of GPS data processing, visualise track data by plotting 
# distance from colony & mapping around Puffin island

########################################################################
################ Packages ##############################################

if(!"sp" %in% installed.packages())
  install.packages("sp")
library(sp)

if(!"trip" %in% installed.packages())
  install.packages("trip")
library(trip)

if(!"ggmap" %in% installed.packages())
  install.packages("ggmap")
library(ggmap)

########################################################################
################ Functions #############################################


########### Plot distances from colony (km) ############

# creates a simple graph of distance from GPS co-ordinates 
# in the first row of the dataframe against time.

# when pre- & post- deployment points removed this is 
# effectively distance from colony


plot.trip.distance <- function(x){
  trip.matrix <- data.matrix(x[,c("Longitude","Latitude")], 
                             rownames.force = NA)           #creates two column matrix of lat and long for trip trackDistance function
  distances <- spDistsN1(trip.matrix, trip.matrix[1,],      #calculate distances between GPS points and initial GPS point
                         longlat = TRUE)                    #using Great Circle Distance method 
  x$DateTime <- as.POSIXct(paste(x$Date, x$Time),           #add column of date and time recognisable by r from existing date and time columns
                           tz="",                           #current time zone
                           format="%d/%m/%Y %H:%M:%S")      #for date format Year/Month/Date and time format Hours:Minutes:Seconds
  plot(x$DateTime, distances, xaxt = "n",pch=20,
       xlab="Time of day", ylab="Distance from colony")
  
  axis.POSIXct(1,x$DateTime,format="%H:%M",                 #change x axis units to time
               at = seq(from = round(x$DateTime[1], "hours"),
                        to = x$DateTime[1] + ceiling(difftime(tail(x$DateTime, 1), head(x$DateTime, 1), 
                                                              units = "hours")),
                        by = "6 hour"),
               las=2)
}


########## Map around colony: googlemaps ##########

# Imports map for specified area from google maps and overlays
# GPS co-ordinates as individual points

# modify function 

map.colony.google <- function(x, colony, map.zoom = 9){
  Colony.map <- get_map(location=colony, source="google", 
                        maptype="satellite", crop=FALSE,
                        zoom = map.zoom)  # zoom must be integer number: 
  # 9 will show all of the longest tracks. location=Puffin.island.mapcentre (offset)
  # 12 is close zoom of colony to inspect short trips. location=Puffin.island
  
  ggmap(Colony.map)+                                          #plot basic map
    geom_point(aes(x = Longitude, y = Latitude), data = x,    #add tracking data points
               alpha = .5, color="darkred", size = 1.2)
}


