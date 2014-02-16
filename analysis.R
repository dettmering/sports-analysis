# Analysis pipeline for my mountainbiking data
# Read csv from Google MyTracks into 'a' variable, skip first 3 lines
# e.g., a <- read.csv("~/Dropbox/Matsch.csv", skip = 3)

# Rename columns

colnames(a) <- c('Segment', 'Point', 'Lat', 'Lon', 'Elevation.m', 'Heading.deg', 'Precision.m', 'Speed.m.s', 'Time', 'Power.W', 'Cadence.rpm', 'Pulse.per.min')

# Conversion geodetic to cartesian
# Source DOI: 10.1371/journal.pcbi.1003446

convertCartesian <- function(lat, lon, lat0, lon0) {
  lat <- lat * pi / 180
  lon <- lon * pi / 180
  lat0 <- lat0 * pi / 180
  lon0 <- lon0 * pi / 180
  
  R <- 6378137.0 # Radius of earth (m)
  f <- 1 / 298.257223563 # Flatness of earth
  
  C <- sqrt(1 - (f * (2 - f) * (sin(lat0))^2))
  R1 <- R / C
  R2 <- R * (1 - f * (2 - f)) / C^3
  
  x <- (lon - lon0) * cos(lat0) * R1
  y <- (lat - lat0) * R2

  return(c(x, y))
}

# Calculate Euclidean distance

euclidDist <- function(px, py, pz, qx, qy, qz) {
  # If no elevation data are available, ignore elevation.
  
  if (is.na(pz)) {
    pz <- 0
    qz <- 0
  }
  if (is.na(qz)) {
    pz <- 0
    qz <- 0
  }
  
  distance <- sqrt((px - qx)^2 + (py - qy)^2 + (pz - qz)^2)
  
  return(distance)
}

# Convert geodetic to cartesian coordinates

x <- 0

for (x in 1:length(a$Lat)) {
  a[x, 'x'] <- convertCartesian(a[x, 'Lat'], a[x, 'Lon'], a[1, 'Lat'], a[1, 'Lon'])[1]
  a[x, 'y'] <- convertCartesian(a[x, 'Lat'], a[x, 'Lon'], a[1, 'Lat'], a[1, 'Lon'])[2]
}

# Calculate distance

a$Distance.m <- 0
distance <- 0

for (x in 1:length(a$x)) {
  a[(x + 1), 'Distance.m'] <- euclidDist(a[x,'x'], a[x,'y'], a[x,'Elevation.m'], a[(x + 1),'x'], a[(x + 1),'y'], a[(x + 1),'Elevation.m'])
  distance <- distance + a[x, 'Distance.m']
  a[(x + 1), 'Cumulative.Distance.m'] <- distance
}

# Convert speed to km/h

a$Speed.kmh <- a$Speed.m.s * 3600 / 1000