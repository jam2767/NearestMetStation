# If not already downloaded, get the file ghcnd-stations.txt
if (!file.exists("ghcnd-inventory.txt")) {
  # File doesn't exist, need to download it:
  download.file(url="ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 
                destfile="ghcnd-inventory.txt")
}

## Read the GHCN station list file:
#  ------------------------------
# Variable   Columns   Type
# ------------------------------
#   ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEMENT      32-35   Character
# FIRSTYEAR    37-40   Integer
# LASTYEAR     42-45   Integer
# ------------------------------

string_lengths <- c(11,-1,8,-1,9,-1,4,-1,4,-1,4)
columns <- c("ID", "LATITUDE", "LONGITUDE","ELEMENT", "FIRSTYEAR","LASTYEAR")
stations_ghcn = read.fwf(file="ghcnd-inventory.txt", widths = string_lengths,
                         comment.char="", col.names=columns)

# Extract the variables (TMAX, TMIN, PRCP) that we need:
stations_ghcn <- subset(stations_ghcn,(stations_ghcn$ELEMENT=="TMAX") | 
                          (stations_ghcn$ELEMENT=="TMIN") | (stations_ghcn$ELEMENT=="PRCP"))

# For GHCN data, if the station doesn't have all three variables (at 
# least at some point in time), delete the station:
all.station.ids <- unique(stations_ghcn$ID)

met.variable.categories <- rep(NA,nrow(stations_ghcn))
met.variable.categories[stations_ghcn$ELEMENT=="TMAX"] = 2
met.variable.categories[stations_ghcn$ELEMENT=="TMIN"] = 3
met.variable.categories[stations_ghcn$ELEMENT=="PRCP"] = 5

rows.to.be.removed <- c()
counter = 0
for (stn.id in all.station.ids) {
  idx <- which(stations_ghcn$ID == stn.id)
  product <- prod(met.variable.categories[idx])
  if( (product %% 30) > 0 ) {
    rows.to.be.removed <- c(rows.to.be.removed,idx)
  }
  counter = counter+1
  if ( (counter %% 1000) == 0) {
    print(counter)
  }
}

stations_ghcn_trimmed <- stations_ghcn[-rows.to.be.removed,]

write.csv(file="stations_ghcn_trimmed.csv",stations_ghcn_trimmed)