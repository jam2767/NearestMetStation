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
dat <- read.csv("data_file_011314_edit3_point.csv")
#dat <- read.csv("data_file_011314_point.csv")
names <- colnames(dat)
lat.unique <- unique(x=dat$Latitude)
lon.unique <- unique(x=dat$Longitude)

location <- data.frame(lat = dat$Latitude,lon = dat$Longitude)

loc.unique <- unique(location)
loc.unique <- na.omit(loc.unique)
loc.index <- as.character(row.names(loc.unique))
unique_loc_meta <- dat[loc.index,]                ## unique sites

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
                                      as.numeric(substr(stations_gsod$END,1,4))))
colnames(gsod_ghcn_data) <- c("dataset","orig.row.num","LAT","LON","BEGIN.YR","END.YR")

# Add ghcn data to gsod_ghcn_data












as.numeric(substr(stations_gsod$BEGIN[min.dist.index],1,4))
GSOD/GHCN, LAT LON START END ORIG_ROW_NUM

# Loop over stations -- everthing, man
for(ST in 1:length(loc.index)){
  print(sprintf("Processing station %i of %i...",ST,length(loc.index)))
  BREAK = 0
  count = 1
  while (BREAK == 0) {
    
    sorted.distance = sort(distance[ST,],decreasing = FALSE, index.return = TRUE)
    
    min.dist.index = sorted.distance$ix[count]
    min.dist = sorted.distance$x[count]
    
    year.start.ISD = as.numeric(substr(stations_gsod$BEGIN[min.dist.index],1,4))
    year.end.ISD = as.numeric(substr(stations_gsod$END[min.dist.index],1,4))
    
    year.start.pheno = as.numeric(unique_loc_meta$Year_Sampled_Start[ST])
    year.end.pheno = as.numeric(unique_loc_meta$Year_Sampled_End[ST])
    
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




############################################# Old stuff


# distance = matrix(0,length(loc.index),nrow(stations_gsod))
# for(ST in 1:length(loc.index)){
#   lati = unique_loc_meta$Latitude[ST]  
#   loni = unique_loc_meta$Longitude[ST]   
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
    
    year.start.pheno = as.numeric(unique_loc_meta$Year_Sampled_Start[ST])
    year.end.pheno = as.numeric(unique_loc_meta$Year_Sampled_End[ST])
    
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
                                                                                                                                          