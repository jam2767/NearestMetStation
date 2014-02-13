# Aggregate the met data

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


# We'll delete the response variable columns we aren't using:
response_var_data <- response_var_data[,-(23:67)]

# GREAT!  Newly re-formatted data frame!

# Now, we'll get the aggregated (1-30, 31-60, 61-90 day) met data for each. For
# now, if the pheno_date is < {30,60,90}, then NA
for ROW in 1:nrow(response_var_data) {
  
  # Find met station ID
  met_stat_id = response_var_data$Met_Station_ID[ROW]  
  
  if (met_stat_id == "NoDATA") {
    # We couldn't find a met station within 2 degrees for some reason...
    
    # Set met data values to NA:
    
  } else{
    
    # Find the year
    start_year_phen =  as.numeric(response_var_data$Year_Sampled_Start[ROW])
    end_year_phen = as.numeric(as.character(response_var_data$Year_Sampled_End[ROW]))
    
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
    
  } # end else  
} # end for loop over ROW

# Save new master plus met csv