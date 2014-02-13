# Aggregate the met data

# LOAD MASTER DATA FILE
master_data <- read.csv("data_file_013014_point_edit2.csv")
# Replace little "na"s with NA:
small_na = (master_data=="na")
master_data[small_na] = NA

master_data$Site_ID = NA
master_data$Met_Station_ID = NA

# LOAD UNIQUE DATA FILE (POINT)
unique_data <- read.csv("PointMetData.csv")
unique_data$Site_ID = NA

for (ROW in 1:nrow(unique_data)){
  
  # Find the row(s) in master data that corresponds to row in unique
  unique_site = as.character(unique_data$Site[ROW])
  unique_lat = as.numeric(unique_data$Latitude[ROW])
  unique_lon = as.numeric(unique_data$Longitude[ROW])
  
  site = (as.character(master_data$Site) == unique_site & master_data$Latitude == unique_lat & 
     master_data$Longitude == unique_lon)
  
  # Assign site ID, 1000 + ROW 
  master_data$Site_ID[site] = ROW + 1000
  unique_data$Site_ID[ROW] = ROW + 1000
  
  # Put Met Station ID in master data frame
  master_data$Met_Station_ID[site] = as.character(unique_data$Met_Station_ID[ROW])

}

# Okay, noy I think we should reshape the master data file so that each
# measurement has its own row as well as a new column for the type of
# measurement, one of {LeafFall50, LeafFall80, LeafFall100, LAI_zero}
#
# Let's kick this pig!

# First, find all of the records that have LeafFall50:
has_LF50 <- !is.na(master_data$LeafFall50)
LF50_data <- subset(master_data[has_LF50,])

# Now for LeafFall80:
has_LF80 <- !is.na(master_data$LeafFall80)
LF80_data <- subset(master_data[has_LF80,])

# Now for LeafFall100:
has_LF100 <- !is.na(master_data$LeafFall100)
LF100_data <- subset(master_data[has_LF100,])

# Now for LAI_zero:
has_LAI_zero <- !is.na(master_data$LAI_zero)
LAI_zero_data <- subset(master_data[has_LAI_zero,])


for ROW in 1:nrow(master_data) {

  # Find met station ID
  met_stat_id = master_data$Met_Station_ID[ROW]
  
  # Angela -- I think we should for now just use the columns LeafFall50, 
  # LeafFall80, LeafFall100, and LAI_zero. If we should include other values
  # they should be put into those columns (by someone else)
  
  
  # Find DOY of phenology 
  all_doy_phen = master_data[ROW,23:64]
  mean_doy_phen = mean(as.numeric(as.matrix(all_doy_phen)),na.rm = TRUE)
  
  # Find the year
  start_year_phen =  as.numeric(master_data$Year_Sampled_Start[ROW])
  end_year_phen = as.numeric(as.character(master_data$Year_Sampled_End[ROW]))
  
  if (is.na(end_year_phen)){
    end_year_phen = start_year_phen
  }
  
  # load met data
  met_filename = paste('Formatted_Met_Data_Point/',met_stat_id,'.csv',sep='')
  met_data = read.csv(met_filename)
  
  # Aggregate met data for each year, if multiple years, take the average.
  for (YR = start_year_phen:end_year_phen){
    
    
    
    
  }
   
  # write 30, 60, 90 month PP
  # write 30, 60, 90 temp data

# write photoperiod

}

# Save new master plus met csv