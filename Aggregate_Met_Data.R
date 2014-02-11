# Aggregate the met data

# LOAD MASTER DATA FILE
master_data <- read.csv("data_file_013014_point_edit2.csv")
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


for ROW in 1:nrow(master_data) {

  # Find met station ID
  met_stat_id = master_data$Met_Station_ID[ROW]
  
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