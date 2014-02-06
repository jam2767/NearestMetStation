# Load some required packages:
loaded <- require("plotrix")
if(!loaded){
  print("trying to install plotrix")
  install.packages("plotrix")
  loaded <- require("plotrix")
  if(loaded){
    print("plotrix installed and loaded")
    library(plotrix)
  } 
  else {
    stop("Could not install plotrix. You need to figure out how to install that manually before this function will work!")
  }    
}

loaded <- require("Imap")
if(!loaded){
  print("trying to install Imap")
  install.packages("Imap")
  loaded <- require("Imap")
  if(loaded){
    print("Imap installed and loaded")
    library(Imap)
  } 
  else {
    stop("Could not install Imap. You need to figure out how to install that manually before this function will work!")
  }    
}

## load point data file
#dat <- read.csv("data_file_011314_edit3_point.csv")
dat <- read.csv("data_file_011314_point.csv")
names <- colnames(dat)
lat.unique <- unique(x=dat$Latitude)
lon.unique <- unique(x=dat$Longitude)

location <- data.frame(lat = dat$Latitude,lon = dat$Longitude)

loc.unique <- unique(location)
loc.unique <- na.omit(loc.unique)
loc.index <- as.character(row.names(loc.unique))
unique_phen_sites <- dat[loc.index,]                ## unique sites

# The data in unique_phen_sites is all factor/categorical data. Convert at least lat/lon to numeric:
unique_phen_sites <- transform(unique_phen_sites, 
                               Latitude = as.numeric(levels(Latitude))[Latitude],
                               Longitude = as.numeric(levels(Longitude))[Longitude])
                               

stations_gsod <- read.csv("ish-history.csv",na.strings = c("-99999","-999999"))
stations_gsod <- na.omit(stations_gsod)
stations_gsod$LAT<- stations_gsod$LAT/1000
stations_gsod$LON<- stations_gsod$LON/1000

if(!file.exists("stations_ghcn_trimmed.csv")) {
  source("get_ghcn_data.R")  
}
stations_ghcn_trimmed <- read.csv("stations_ghcn_trimmed.csv")

# Join the two datasets:
gsod_ghcn_data <- as.data.frame(cbind(rep("gsod",nrow(stations_gsod)),
                                      1:nrow(stations_gsod),
                                      as.numeric(stations_gsod$LAT), 
                                      as.numeric(stations_gsod$LON),
                                      as.numeric(substr(stations_gsod$BEGIN,1,4)),
                                      as.numeric(substr(stations_gsod$END,1,4)),
                                      rep("NA",nrow(stations_gsod))))
colnames(gsod_ghcn_data) <- c("dataset","orig.row.num","LAT","LON","BEGIN.YR","END.YR","ELEMENT")

# Add ghcn data to gsod_ghcn_data
ghcn_data_to_bind = as.data.frame(cbind(rep("ghcn",nrow(stations_ghcn_trimmed)),
                                        1:nrow(stations_ghcn_trimmed),
                                        as.numeric(stations_ghcn_trimmed$LATITUDE), 
                                        as.numeric(stations_ghcn_trimmed$LONGITUDE),
                                        as.numeric(stations_ghcn_trimmed$FIRSTYEAR),
                                        as.numeric(stations_ghcn_trimmed$LASTYEAR),
                                        as.character(stations_ghcn_trimmed$ELEMENT)))
colnames(ghcn_data_to_bind) <- c("dataset","orig.row.num","LAT","LON","BEGIN.YR","END.YR","ELEMENT")

# Concatenating GSOD and GHCN data frames
gsod_ghcn_data <- rbind(gsod_ghcn_data,ghcn_data_to_bind)

# Okay, for some reason, none of these are the right classes of data, so let's fix that:
gsod_ghcn_data <- transform(gsod_ghcn_data, orig.row.num = as.integer(levels(orig.row.num))[orig.row.num],
                            LAT = as.numeric(levels(LAT))[LAT],
                            LON = as.numeric(levels(LON))[LON],
                            BEGIN.YR = as.integer(levels(BEGIN.YR))[BEGIN.YR],
                            END.YR = as.integer(levels(END.YR))[END.YR])

# Er... this is weird... type in summary(gsod_ghcn_data) and you'll notice that
# the minimum longitude is -802.33, and the minumum BEGIN.YR is 1763... suprising. Anyway...

# Loop over stations -- everthing, man
for(ST in 1:nrow(unique_phen_sites)){ # for 1:number of phenology sites

  print(sprintf("Processing station %i of %i...",ST,nrow(unique_phen_sites)))
  
  # latitude and longitude of the phenology site:
  phen_lat <- unique_phen_sites$Latitude[ST]
  phen_lon <- unique_phen_sites$Longitude[ST]
      
  nearby_data <- subset(gsod_ghcn_data, (abs(gsod_ghcn_data$LAT - phen_lat) < 2) 
                       & (abs(gsod_ghcn_data$LON - phen_lon) < 2))
  
  distance <- rep(NA,nrow(nearby_data))
  
  # For each potential nearest met station...
  for(MET_ST in 1:nrow(nearby_data)){
    lat_MET_ST <- nearby_data$LAT[MET_ST]
    lon_MET_ST <- nearby_data$LON[MET_ST]
    
    # ...calculate the distances to the nearby met stations:
    distance[MET_ST] <- gdist(lon.1=lon_MET_ST,lat.1=lat_MET_ST,
                               lon.2=phen_lon,lat.2=phen_lat, units = "km")
  }
  
  # Let's put them in order of increasing distance:
  nearby_data <- cbind(nearby_data, distance)  
  nearby_data <- na.omit(nearby_data)
  nearby_data <- nearby_data[with(nearby_data,order(distance)),] # Closest is in row 1
  
  # Now find the closest one that has at least XX% data.
  # We'll use a little while loop. Keep looping until either a station is found or we're out of stations.
  required_data_completeness <- 0.7 # 70%
  no_nearby_stns <- FALSE
  BREAK <- FALSE
  
  # If there are no stations in nearby_data, then skip the loop entirely
  if(nrow(nearby_data) == 0) {
    BREAK <- TRUE
    no_nearby_stns <- TRUE
  }

  while (~BREAK) {
    
    # Get the station id for the closest met station:
    stn_id <- 
    # Download the data for the closest met station (the station in row 1 of nearby_data)
    download_met_data(nearby_data$dataset[1],stn_id)
    
    # If the station doesn't have enough data, remove it from nearby_data:
    
    
    # Otherwise, exit the loop:
    
    
    
    # If there are no stations left in nearby_data, then exit the loop
    if(nrow(nearby_data) == 0) {
      BREAK <- TRUE
      no_nearby_stns <- TRUE
    }
    
    
    
  }
  
  
    
}




for(ST in 1:nrow(unique_phen_sites)){ # for 1:number of phenology sites
  
  print(sprintf("Processing station %i of %i...",ST,length(loc.index)))
  BREAK = 0
  count = 1
  
  while (BREAK == 0) {

    sorted.distance = sort(distance[ST,],decreasing = FALSE, index.return = TRUE)
    
    min.dist.index = sorted.distance$ix[count]
    min.dist = sorted.distance$x[count]
    
    year.start.ISD = as.numeric(substr(stations_gsod$BEGIN[min.dist.index],1,4))
    year.end.ISD = as.numeric(substr(stations_gsod$END[min.dist.index],1,4))
    
    year.start.pheno = as.numeric(unique_phen_sites$Year_Sampled_Start[ST])
    year.end.pheno = as.numeric(unique_phen_sites$Year_Sampled_End[ST])
    
    if(is.na(year.end.pheno)){
      if(year.end.ISD < year.start.pheno){
        count = count + 1
        print(count)
      } else if(year.start.ISD > year.start.pheno){
        count = count + 1
        print(count)
      } else if(year.start.pheno >= year.start.ISD & year.start.pheno <= year.end.ISD){
        phen.station.id = numeric()
        phen.station.id = as.numeric(row.names(loc.unique[ST,]))
        isd.station.usaf = stations_gsod$USAF[min.dist.index]
        isd.station.wban = stations_gsod$WBAN[min.dist.index]
        
        phenology.isd.stations[ST,1] = phen.station.id
        phenology.isd.stations[ST,2] = isd.station.usaf
        phenology.isd.stations[ST,3] = isd.station.wban
        phenology.isd.stations[ST,4] = min.dist.index 
        phenology.isd.stations[ST,5] = min.dist 
        phenology.isd.stations[ST,6] =year.start.ISD 
        phenology.isd.stations[ST,7] =year.end.ISD 
        phenology.isd.stations[ST,8] =year.start.pheno
        phenology.isd.stations[ST,9] =year.end.pheno
        
        
        BREAK = 1
      }   
      else {
        if (year.start.ISD <= year.start.pheno & year.end.ISD >= year.end.pheno){
          phen.station.id = numeric()
          phen.station.id = as.numeric(row.names(loc.unique[ST,]))
          isd.station.usaf = stations_gsod$USAF[min.dist.index]
          isd.station.wban = stations_gsod$WBAN[min.dist.index]
          
          phenology.isd.stations[ST,1] = phen.station.id
          phenology.isd.stations[ST,2] = isd.station.usaf
          phenology.isd.stations[ST,3] = isd.station.wban
          phenology.isd.stations[ST,4] = min.dist.index 
          phenology.isd.stations[ST,5] = min.dist 
          phenology.isd.stations[ST,6] =year.start.ISD 
          phenology.isd.stations[ST,7] =year.end.ISD 
          phenology.isd.stations[ST,8] =year.start.pheno
          phenology.isd.stations[ST,9] =year.end.pheno
          
          BREAK = 1
          
        } else{
          count = count + 1
        }
        
      } 
    }
  }




############################################# Old stuff below


# distance = matrix(0,length(loc.index),nrow(stations_gsod))
# for(ST in 1:length(loc.index)){
#   lati = unique_phen_sites$Latitude[ST]  
#   loni = unique_phen_sites$Longitude[ST]   
#   print(ST)
#   for(ISD in 1:nrow(stations_gsod)){
#     lat_ISD = stations_gsod$LAT[ISD]
#     lon_ISD = stations_gsod$LON[ISD]
#     distance[ST,ISD] = gdist(lon.1=loni,lat.1=lati,
#                              lon.2=lon_ISD,lat.2=lat_ISD, units = "km")
#     
#   }
#   
# }


#write.csv(x=distance,file="distance.csv")
distance = read.csv("distance.csv")
distance = as.matrix(distance)
## start and end date for station record and pheno data. Find min distance
## between stations within required period of record
phenology.isd.stations = matrix(0,nrow(loc.unique),9)
for(ST in 1:length(loc.index)){
  print(ST)
  BREAK = 0
  count = 1
  while (BREAK == 0) {
    
    sorted.distance = sort(distance[ST,],decreasing = FALSE, index.return = TRUE)
    
    min.dist.index = sorted.distance$ix[count]
    min.dist = sorted.distance$x[count]
    
    year.start.ISD = as.numeric(substr(stations_gsod$BEGIN[min.dist.index],1,4))
    year.end.ISD = as.numeric(substr(stations_gsod$END[min.dist.index],1,4))
    
    year.start.pheno = as.numeric(unique_phen_sites$Year_Sampled_Start[ST])
    year.end.pheno = as.numeric(unique_phen_sites$Year_Sampled_End[ST])
    
    if(is.na(year.end.pheno)){
      if(year.end.ISD < year.start.pheno){
        count = count + 1
        print(count)
      } else if(year.start.ISD > year.start.pheno){
        count = count + 1
        print(count)
      } else if(year.start.pheno >= year.start.ISD & year.start.pheno <= year.end.ISD){
        phen.station.id = numeric()
        phen.station.id = as.numeric(row.names(loc.unique[ST,]))
        isd.station.usaf = stations_gsod$USAF[min.dist.index]
        isd.station.wban = stations_gsod$WBAN[min.dist.index]
        
        phenology.isd.stations[ST,1] = phen.station.id
        phenology.isd.stations[ST,2] = isd.station.usaf
        phenology.isd.stations[ST,3] = isd.station.wban
        phenology.isd.stations[ST,4] = min.dist.index 
        phenology.isd.stations[ST,5] = min.dist 
        phenology.isd.stations[ST,6] =year.start.ISD 
        phenology.isd.stations[ST,7] =year.end.ISD 
        phenology.isd.stations[ST,8] =year.start.pheno
        phenology.isd.stations[ST,9] =year.end.pheno
        
        
        BREAK = 1
      }   
      else {
      if (year.start.ISD <= year.start.pheno & year.end.ISD >= year.end.pheno){
        phen.station.id = numeric()
        phen.station.id = as.numeric(row.names(loc.unique[ST,]))
        isd.station.usaf = stations_gsod$USAF[min.dist.index]
        isd.station.wban = stations_gsod$WBAN[min.dist.index]
        
        phenology.isd.stations[ST,1] = phen.station.id
        phenology.isd.stations[ST,2] = isd.station.usaf
        phenology.isd.stations[ST,3] = isd.station.wban
        phenology.isd.stations[ST,4] = min.dist.index 
        phenology.isd.stations[ST,5] = min.dist 
        phenology.isd.stations[ST,6] =year.start.ISD 
        phenology.isd.stations[ST,7] =year.end.ISD 
        phenology.isd.stations[ST,8] =year.start.pheno
        phenology.isd.stations[ST,9] =year.end.pheno
        
        BREAK = 1
        
      } else{
        count = count + 1
      }
      
    } 
  }
}

colnames(phenology.isd.stations) <- c("ID","USAF","WBAN","DIndex","MinDist",
                                      "Start_Station","End_Station","Start_Pheno",
                                      "End_Pheno")

write.csv(x=phenology.isd.stations,file="phenology_nearest_station.csv",row.names=FALSE)
                                                                                                                                          