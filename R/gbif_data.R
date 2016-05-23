#Load packages
require("data.table")
require("dismo")
require("doMC")
require("maptools")
require("reshape2")
require("rgeos")
require("rgbif")

#Specify boundary for species records query
boundary <- readShapePoly("../GIS/CAL_NAD83LL_ADMIN_STATE_SIMPLE.shp") #California shapefile for query boundary (projected in NAD83 (EPSG:4269))

#Create WKT string
wkt <- writeWKT(boundary, byid = FALSE)

#For complex shapes - simplify for query using convex hull
lonlat<- boundary@polygons[[1]]@Polygons[[1]]@coords ## extract the polygon coordinates
temp <- chull(lonlat) ## extract the convex hull of the polygon to reduce the length of the WKT string
lonlat <- lonlat[c(temp,temp[1]),]  ## overwrite the coordinates
wkt <- paste("POLYGON((",paste(apply(lonlat,1,function(z) paste(z,collapse=" ")),collapse=","),"))",sep="") ## create WKT string
rm(lonlat,temp)

#Specify target species
sp.target <- "Odocoileus hemionus"

#Specify start year
yr.start <- 2000

#Specify end year
yr.end <- 2015

#Download target species data - add fields as required - note additional fields may be added (?occ_search and http://www.gbif.org/developer/occurrence#parameters)
sp.data <- occ_search(scientificName = paste(sp.target), hasCoordinate = TRUE, basisOfRecord = "HUMAN_OBSERVATION", geometry = wkt, year = paste(yr.start,",",yr.end,sep=""), hasGeospatialIssue = FALSE, limit = 1000, start = 0, fields=c('scientificName','decimalLatitude','decimalLongitude','year','month','day'), return = "data")
sp.data <- as.data.table(sp.data)

#Download background species data - setup parallel data retrieval workers (split over multiple years to reduce server load and address server allowances (i.e. multiple queries originating from identical IP address)) - capped at 200,000 we set the limit to 10,000 x 15 years = 150,000
registerDoMC(detectCores() - 1)

raw.sp.data <- foreach(i = c(yr.start:yr.end), .packages = c("rgbif", "doMC"), combine = rbind) %dopar% {
  temp <- occ_search(scientificName = NULL, hasCoordinate = TRUE, basisOfRecord = "HUMAN_OBSERVATION", geometry = wkt, year = i, hasGeospatialIssue = FALSE, limit = 10000, start = 0, fields=c('scientificName','classKey','decimalLatitude','decimalLongitude','year','month','day'), return = "data")
  temp
}

save(raw.sp.data, file = "raw_sp_data_gbif") #Store a copy since the query can be quite time consuming (10+ minutes)...
#load("raw_sp_data_gbif") #Load saved copy of data - alternative method is to save R workspace...

#Combine all background data into single table
all.sp.data <- data.table()
for (i in 1:length(raw.sp.data)){
  all.sp.data <- rbind(all.sp.data,raw.sp.data[[i]])
}

#Sort and group records by time and location of observations to determine sites with multiple species surveys
setkey(all.sp.data,year,month,day,decimalLongitude,decimalLatitude,scientificName,classKey)

sites.spp <- all.sp.data[,c("MULT" = length(unique(scientificName))>1 & length(unique(classKey))>1, "TOTAL" = .N, "SPP" = list(paste(unique(scientificName),collapse=", "))), by="decimalLongitude,decimalLatitude,year,month,day"]

sites.tax <- sites.spp[MULT == TRUE]

#Construct species datsets using reported target species locations as presences (1's) and sites where multiple species were observed excluding target species as background (0's)
sites.1 <- sp.data[, .N, by="decimalLongitude,decimalLatitude"]
sites.0 <- sites.tax[!like(SPP,sp.target)]
x1 <- cbind(sites.1[,.("LON"=decimalLongitude,"LAT"=decimalLatitude)],"OCC"=rep(1,nrow(sites.1)))
x0 <- cbind(sites.0[,.("LON"=decimalLongitude,"LAT"=decimalLatitude)],"OCC"=rep(0,nrow(sites.0)))
sp.model.data <- rbind(x1,x0)
write.table(sp.model.data, file = "sp_model_data_ll.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Optional conversion to projected coordinates for use in SDMs which sample from environmental covariate grids
coord.sys <- CRS("+init=epsg:3157") #Selected California NAD83 Zone 10 UTM

ll <- SpatialPoints(sp.model.data[,.(LON,LAT)], proj4string=CRS("+init=epsg:4269"))
UTM <- data.frame(spTransform(ll, coord.sys))
names(UTM) <- c('X','Y')
sp.model.data <- as.data.table(cbind(UTM,"OCC"=sp.model.data[,OCC]))
write.table(sp.model.data, file = "Data/cal_speciesdata.csv", row.names=FALSE, col.names=TRUE, sep=",")
