# Aggregate the met data!

# Load some required packages:
loaded <- require("geosphere")
if(!loaded){
  print("trying to install package geosphere")
  install.packages("geosphere")
  loaded <- require("geosphere")
  if(loaded){
    print("geosphere installed and loaded")
    library(geosphere)
  } 
  else {
    stop("Could not install geosphere. You need to figure out how to install that manually before this function will work!")
  }    
}


# LOAD MASTER DATA FILE
master_data <- read.csv("data_file_point.csv")
# Replace little "na"s with NA:
small_na = (master_data=="na")
master_data[small_na] = NA

master_data$Site_ID = NA
master_data$Met_Station_ID = NA

# LOAD UNIQUE DATA FILE (POINT)
unique_data <- read.csv("PointMetData.csv")
unique_data$Site_ID = NA

# Add site ids and met stn ids:
for (ROW in 1:nrow(unique_data)){
  
  # Find the row(s) in master data that corresponds to row in unique
  unique_lat = as.numeric(unique_data$Latitude[ROW])
  unique_lon = as.numeric(unique_data$Longitude[ROW])
  
  # Match the lat/lon values (might be som rounding errors, so give a tolerance)
  matching_sites = ( abs(master_data$Latitude-unique_lat) < 0.01 & 
                       abs(master_data$Longitude-unique_lon) < 0.01 )
  
  # Assign site ID, 1000 + ROW 
  master_data$Site_ID[matching_sites] = ROW + 1000
  unique_data$Site_ID[ROW] = ROW + 1000
  
  # Put Met Station ID in master data frame
  master_data$Met_Station_ID[matching_sites] = as.character(unique_data$Met_Station_ID[ROW])
  
}

# Okay, now I think we should reshape the master data file so that each
# measurement has its own row as well as a new column for the type of
# measurement, one of {LeafFall50, LeafFall80, LeafFall100, LAI_zero}
#
# Let's do it!

# First, find all of the records that have LeafFall50:
has_LF50 <- !is.na(master_data$LeafFall50)
LF50_data <- subset(master_data[has_LF50,])
LF50_data$pheno_method <- "LeafFall50"
LF50_data$pheno_day <- LF50_data$LeafFall50

# Now for LeafFall80:
has_LF80 <- !is.na(master_data$LeafFall80)
LF80_data <- subset(master_data[has_LF80,])
LF80_data$pheno_method <- "LeafFall80"
LF80_data$pheno_day <- LF80_data$LeafFall80

# Now for LeafFall100:
has_LF100 <- !is.na(master_data$LeafFall100)
LF100_data <- subset(master_data[has_LF100,])
LF100_data$pheno_method <- "LeafFall100"
LF100_data$pheno_day <- LF100_data$LeafFall100

# Now for LAI_zero:
has_LAI_zero <- !is.na(master_data$LAI_zero)
LAI_zero_data <- subset(master_data[has_LAI_zero,])
LAI_zero_data$pheno_method <- "LAI_zero"
LAI_zero_data$pheno_day <- LAI_zero_data$LAI_zero

# Join them together into one data frame, but now where each row is a single
# observation (instead of having both LeafFall50 AND LeafFall80, for example)
response_var_data <- rbind(LF50_data,LF80_data,LF100_data,LAI_zero_data)

# We'll delete the response variable columns we aren't using:
response_var_data <- response_var_data[,-(23:67)]

# And we'll add in the met data columns that we need (let's just add a bunch for now):
response_var_data$Precip_total_1to30days <- NA
response_var_data$Precip_total_31to60days <- NA
response_var_data$Precip_total_61to90days <- NA

response_var_data$Tmin_mean_1to30days <- NA
response_var_data$Tmin_mean_31to60days <- NA
response_var_data$Tmin_mean_61to90days <- NA

response_var_data$Tmin_median_1to30days <- NA
response_var_data$Tmin_median_31to60days <- NA
response_var_data$Tmin_median_61to90days <- NA

response_var_data$Tmax_mean_1to30days <- NA
response_var_data$Tmax_mean_31to60days <- NA
response_var_data$Tmax_mean_61to90days <- NA

response_var_data$Tmax_median_1to30days <- NA
response_var_data$Tmax_median_31to60days <- NA
response_var_data$Tmax_median_61to90days <- NA

response_var_data$Tmean_1to30days <- NA
response_var_data$Tmean_31to60days <- NA
response_var_data$Tmean_61to90days <- NA

response_var_data$GDD_from_0C_30days <- NA
response_var_data$GDD_from_0C_60days <- NA
response_var_data$GDD_from_0C_90days <- NA

response_var_data$CDD_from_0C_30days <- NA
response_var_data$CDD_from_0C_60days <- NA
response_var_data$CDD_from_0C_90days <- NA

response_var_data$GDD_from_10C_30days <- NA
response_var_data$GDD_from_10C_60days <- NA
response_var_data$GDD_from_10C_90days <- NA

response_var_data$CDD_from_10C_30days <- NA
response_var_data$CDD_from_10C_60days <- NA
response_var_data$CDD_from_10C_90days <- NA

response_var_data$photoperiod_mean_30days <- NA
response_var_data$photoperiod_mean_60days <- NA
response_var_data$photoperiod_mean_90days <- NA


# GREAT!  Newly re-formatted data frame!

# Now, we'll get the aggregated (1-30, 31-60, 61-90 day) met data for each. For
# now, if the pheno_date is < {30,60,90}, then NA
for(ROW in 1:nrow(response_var_data)) {
  
  # Find met station ID
  met_stat_id = response_var_data$Met_Station_ID[ROW]  
  
  has_met_lat_lon <- (met_stat_id != "NoDATA") & !is.na(met_stat_id)
  if ( has_met_lat_lon ) {
    # We have a met station within 2 degrees, and so can input data:
    
    # Find the year
    start_year_phen =  as.numeric(response_var_data$Year_Sampled_Start[ROW])
    end_year_phen = as.numeric(as.character(response_var_data$Year_Sampled_End[ROW]))
    
    if (is.na(end_year_phen)){
      end_year_phen = start_year_phen
    }
    
    # load met data
    met_filename = paste('Formatted_Met_Data_Point/',met_stat_id,'.csv',sep='')
    met_data = read.csv(met_filename)
    
    # Make a quick day-of-year vector to go along with the met data:
    DOY <- as.numeric(strftime(met_data$Date, format = "%j"))
    
    # And this is the day of leaf-fall by whichever metric:
    pheno_day <- response_var_data$pheno_day[ROW]
    
    # Fill in the aggregated met data!
    
    # We can easily fill in values for 0-29 days prior if pheno_day >= 30
    if(pheno_day >= 30) {
      mask_1_30 <- (DOY <= pheno_day) & (DOY > pheno_day - 30)
      response_var_data$Precip_total_1to30days[ROW] <- 30*mean(met_data$ppt_mm[mask_1_30],na.rm=TRUE)
      response_var_data$Tmin_mean_1to30days[ROW] <- mean(met_data$tmin_C[mask_1_30], na.rm=TRUE)
      response_var_data$Tmin_median_1to30days[ROW] <- median(met_data$tmin_C[mask_1_30], na.rm=TRUE)
      response_var_data$Tmax_mean_1to30days[ROW] <- mean(met_data$tmax_C[mask_1_30], na.rm=TRUE)
      response_var_data$Tmax_median_1to30days[ROW] <- median(met_data$tmax_C[mask_1_30], na.rm=TRUE)
      # We need a mean daily temperature series for Growing/Chilling Degree Days:
      mean_temp <- 0.5*(met_data$tmin_C[mask_1_30]+met_data$tmax_C[mask_1_30])
      response_var_data$Tmean_1to30days[ROW] <- mean(mean_temp, na.rm=TRUE)      
      response_var_data$GDD_from_0C_30days[ROW] <- 30*mean(mean_temp[mean_temp>0], na.rm=TRUE)
      response_var_data$CDD_from_0C_30days[ROW] <- 30*mean(mean_temp[mean_temp<0], na.rm=TRUE)
      response_var_data$GDD_from_10C_30days[ROW] <- 30*mean(mean_temp[mean_temp>10], na.rm=TRUE)
      response_var_data$CDD_from_10C_30days[ROW] <- 30*mean(mean_temp[mean_temp<10], na.rm=TRUE)
      response_var_data$photoperiod_mean_30days[ROW] <- mean(daylength(lat=response_var_data$Latitude[ROW],
                                                                       doy=as.Date(met_data$Date[mask_1_30])))
    }
    
    if(pheno_day >= 60) {
      mask_31_60 <- (DOY <= pheno_day - 30) & (DOY > pheno_day - 60)
      response_var_data$Precip_total_31to60days[ROW] <- 30*mean(met_data$ppt_mm[mask_31_60],na.rm=TRUE)
      response_var_data$Tmin_mean_31to60days[ROW] <- mean(met_data$tmin_C[mask_31_60], na.rm=TRUE)
      response_var_data$Tmin_median_31to60days[ROW] <- median(met_data$tmin_C[mask_31_60], na.rm=TRUE)
      response_var_data$Tmax_mean_31to60days[ROW] <- mean(met_data$tmax_C[mask_31_60], na.rm=TRUE)
      response_var_data$Tmax_median_31to60days[ROW] <- median(met_data$tmax_C[mask_31_60], na.rm=TRUE)
      response_var_data$Tmean_31to60days[ROW] <- mean(0.5*(met_data$tmin_C[mask_31_60]+met_data$tmax_C[mask_31_60]), 
                                                      na.rm=TRUE)
      # We need a mean daily temperature series for days 1-60 for Growing/Chilling Degree Days:
      mean_temp <- 0.5*(met_data$tmin_C[mask_31_60 | mask_1_30]+met_data$tmax_C[mask_31_60 | mask_1_30])
      response_var_data$GDD_from_0C_60days[ROW] <- 30*mean(mean_temp[mean_temp>0], na.rm=TRUE)
      response_var_data$CDD_from_0C_60days[ROW] <- 30*mean(mean_temp[mean_temp<0], na.rm=TRUE)
      response_var_data$GDD_from_10C_60days[ROW] <- 30*mean(mean_temp[mean_temp>10], na.rm=TRUE)
      response_var_data$CDD_from_10C_60days[ROW] <- 30*mean(mean_temp[mean_temp<10], na.rm=TRUE)
      # The 60-day photoperiod is the mean over all days 1-60
      response_var_data$photoperiod_mean_60days[ROW] <- mean(daylength(lat=response_var_data$Latitude[ROW],
                                                                       doy=as.Date(met_data$Date[mask_1_30 | mask_31_60])))
      
    }
    
    if(pheno_day >= 90) {
      mask_61_90 <- (DOY <= pheno_day - 60) & (DOY > pheno_day - 90)
      response_var_data$Precip_total_61to90days[ROW] <- 30*mean(met_data$ppt_mm[mask_61_90],na.rm=TRUE)
      response_var_data$Tmin_mean_61to90days[ROW] <- mean(met_data$tmin_C[mask_61_90], na.rm=TRUE)
      response_var_data$Tmin_median_61to90days[ROW] <- median(met_data$tmin_C[mask_61_90], na.rm=TRUE)
      response_var_data$Tmax_mean_61to90days[ROW] <- mean(met_data$tmax_C[mask_61_90], na.rm=TRUE)
      response_var_data$Tmax_median_61to90days[ROW] <- median(met_data$tmax_C[mask_61_90], na.rm=TRUE)
      response_var_data$Tmean_61to90days[ROW] <- mean(0.5*(met_data$tmin_C[mask_61_90]+met_data$tmax_C[mask_61_90]), 
                                                      na.rm=TRUE)
      # We need a mean daily temperature series for days 1-90 for Growing/Chilling Degree Days:
      mean_temp <- 0.5*(met_data$tmin_C[mask_61_90 | mask_31_60 | mask_1_30]
                        + met_data$tmax_C[mask_61_90 | mask_31_60 | mask_1_30])
      response_var_data$GDD_from_0C_90days[ROW] <- 30*mean(mean_temp[mean_temp>0], na.rm=TRUE)
      response_var_data$CDD_from_0C_90days[ROW] <- 30*mean(mean_temp[mean_temp<0], na.rm=TRUE)
      response_var_data$GDD_from_10C_90days[ROW] <- 30*mean(mean_temp[mean_temp>10], na.rm=TRUE)
      response_var_data$CDD_from_10C_90days[ROW] <- 30*mean(mean_temp[mean_temp<10], na.rm=TRUE)
      # The 90-day photoperiod is the mean over all days 1-90
      response_var_data$photoperiod_mean_90days[ROW] <- mean(daylength(lat=response_var_data$Latitude[ROW],
                                                                       doy=as.Date(met_data$Date[mask_1_30 | mask_31_60 | mask_61_90])))
      
    }  
  } # end if has_met_lat_lon  
} # end for loop over ROW

# Save new master plus met csv:


