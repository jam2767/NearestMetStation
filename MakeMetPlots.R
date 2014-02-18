# Make some quick figures from Master+Met data and do some simple analysis.

# Load some required packages:
loaded <- require("maps")
if(!loaded){
  print("trying to install package maps")
  install.packages("maps")
  loaded <- require("maps")
  if(loaded){
    print("maps installed and loaded")
    library(maps)
  } 
  else {
    stop("Could not install maps You need to figure out how to install that manually before this function will work!")
  }    
}

loaded <- require("mapdata")
if(!loaded){
  print("trying to install package mapdata")
  install.packages("mapdata")
  loaded <- require("mapdata")
  if(loaded){
    print("mapdata installed and loaded")
    library(mapdata)
  } 
  else {
    stop("Could not install mapdata You need to figure out how to install that manually before this function will work!")
  }    
}

# Open a pdf socket for plot output:
pdf("MetPlots.pdf", paper="USr")

# Load the data:
master_plus_met <- read.csv("Master_Plus_Met.csv")

# Make a histogram of distances from met station to pheno site:
lon_lat <- data.frame(master_plus_met$Longitude,master_plus_met$Latitude)
unique_lon_lat <- unique(lon_lat)

unique_indices <- as.integer(row.names(unique_lon_lat))

unique_distances <- master_plus_met$Met_Station_Distance_km[unique_indices]
hist(unique_distances)

# Make a map of distances to pheno-site
map("worldHires")
points(unique_lon_lat[unique_distances<10,],pch='.',col="red")
points(unique_lon_lat[unique_distances<25 & unique_distances >= 10,],pch='.',col="orange")
points(unique_lon_lat[unique_distances<50 & unique_distances >= 25,],pch='.',col="yellow")
points(unique_lon_lat[unique_distances<100 & unique_distances >= 50,],pch='.',col="green")
points(unique_lon_lat[unique_distances<150 & unique_distances >= 100,],pch='.',col="blue")


color_list <- c("lightslateblue","green4","maroon3")
# Plot pheno-day versus a bunch of met variables:
relevant_data <- master_plus_met[c(6,7,8,29,30:62)]

par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Latitude,relevant_data$pheno_day,ylab="Pheno Day",xlab="Latitude")

par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Latitude[relevant_data$Latitude > 25],
     relevant_data$pheno_day[relevant_data$Latitude > 25],ylab="Pheno Day",xlab="Latitude")

#boxplot(relevant_data$Biome,relevant_data$pheno_day,xlab="Pheno Day",ylab="Latitude")

par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Precip_total_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="Precip [mm]",col=color_list[1],pch=20)
points(relevant_data$Precip_total_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Precip_total_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)

par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmin_mean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_min [C]",col=color_list[1],pch=20)
points(relevant_data$Tmin_mean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmin_mean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)



par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmax_mean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_max [C]",col=color_list[1],pch=20)
points(relevant_data$Tmax_mean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmax_mean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)

par(mar=c(5.1,8.1,4.1,2.1))
plot(relevant_data$Tmean_1to30days,relevant_data$pheno_day, 
     ylab="Pheno Day",xlab="mean T_mean [C]",col=color_list[1],pch=20)
points(relevant_data$Tmean_31to60days,relevant_data$pheno_day,col=color_list[2],pch=20)
points(relevant_data$Tmean_61to90days,relevant_data$pheno_day,col=color_list[3],pch=20)
legend(x="bottomright",c("1-30 days","31-60 days","61-90 days"),
       col=color_list,pch=20)



# Run a few regressions

# Plot pheno-day vs. precip AND pheno-day versus expected pheno-day from precip-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. Tmin AND pheno-day versus expected pheno-day from Tmin-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. Tmax AND pheno-day versus expected pheno-day from Tmax-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. Tmean AND pheno-day versus expected pheno-day from Tmean-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. GDD_0 AND pheno-day versus expected pheno-day from GDD_0-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. GDD_10 AND pheno-day versus expected pheno-day from GDD_10-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. CDD_0 AND pheno-day versus expected pheno-day from CDD_0-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. CDD_10 AND pheno-day versus expected pheno-day from CDD_10-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day vs. photoperiod AND pheno-day versus expected pheno-day from photoperiod-only linear effect
# (4 plots: unregressed, 30, 60, 90)

# Plot pheno-day versus modeled/expected pheno-day from all met variables.
# (And determine portion of variance explained by meteorology)

# Plot pheno-day versus modeled/expected pheno-day from latitude variables.
# (And determine portion of variance explained by latitude)


dev.off()