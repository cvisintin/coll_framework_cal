#Load packages
require(data.table)
require(doMC)
require(maptools)
require(rgeos)
require(rgbif)
require(raster)
require(RPostgreSQL)
require(fields)
require(dismo)

# thin.algorithm <- function (rec.df.orig, thin.par, reps) # function slightly adapted from https://github.com/cran/spThin/blob/master/R/thin.algorithm.R
# {
#   reduced.rec.dfs <- list()
#   for (i in 1:reps) {
#     rec.df <- rec.df.orig
#     DistMat <- rdist(x1 = rec.df)
#     diag(DistMat) <- NA
#     while (min(DistMat, na.rm = TRUE) < thin.par & nrow(rec.df) > 
#            1) {
#       CloseRecs <- which(DistMat < thin.par, arr.ind = TRUE)[, 
#                                                              1]
#       RemoveRec <- as.numeric(names(which(table(CloseRecs) == 
#                                             max(table(CloseRecs)))))
#       if (length(RemoveRec) > 1) {
#         RemoveRec <- sample(RemoveRec, 1)
#       }
#       rec.df <- rec.df[-RemoveRec, ]
#       DistMat <- DistMat[-RemoveRec, -RemoveRec]
#       if (length(DistMat) == 1) {
#         break
#       }
#     }
#     colnames(rec.df) <- c("x", "y")
#     reduced.rec.dfs[[i]] <- rec.df
#   }
#   locs.thinned <- reduced.rec.dfs
#   y.x.thin.count <- unlist(lapply(locs.thinned, nrow))
#   max.thin.recs <- max(y.x.thin.count)
#   df.temp <- locs.thinned[[which(y.x.thin.count == max.thin.recs)[1]]]
#   colnames(df.temp) <- colnames(rec.df.orig)
#   
#   return(df.temp)
# }

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

grid.files <- list.files(path='data/grids/cal/envi/500') #Create vector of filenames

grid.names <- substring(unlist(strsplit(grid.files,"\\_500."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

cal.rst.study <- raster("data/grids/cal/CAL_NAD8310_GRID_STUDY_500.tif")
clip.study <- extent(445000,1165000,3962000,4329000) #Define clipping extent of maps

X <- Y <- cal.rst.study
Y[] <- yFromCell(cal.rst.study, 1:ncell(cal.rst.study))/mean(extent(cal.rst.study)[3:4])
Y <- crop(Y,clip.study) * cal.rst.study
X[] <- xFromCell(cal.rst.study, 1:ncell(cal.rst.study))/mean(extent(cal.rst.study)[1:2])
X <- crop(X,clip.study) * cal.rst.study

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/cal/envi/500/",grid.files[i]))
  #temp <- crop(temp, clip.state)
  #assign(grid.names[i],temp * cal.rst.state)
  temp <- crop(temp, clip.study)
  assign(grid.names[i],temp * cal.rst.study)
}
vars <- stack(c(mget(grid.names),"X"=X,"Y"=Y)) #Combine all maps to single stack
save(vars,file="data/cal_study_vars_500")

# Query database study boundary for species records
wkt <- as.character(dbGetQuery(con,"
  SELECT
    ST_AsText(ST_ConvexHull(ST_Transform(geom,4326)))
  FROM
    gis_california.cal_nad8310_admin_study_area
"))

#Specify target species
sp.target <- "Odocoileus hemionus"

#Specify start year
yr.start <- 2006

#Specify end year
yr.end <- 2016

#Download target species data - add fields as required - note additional fields may be added (?occ_search and http://www.gbif.org/developer/occurrence#parameters)
sp.data <- occ_search(scientificName = paste(sp.target), hasCoordinate = TRUE, basisOfRecord = "HUMAN_OBSERVATION", geometry = wkt, year = paste(yr.start,",",yr.end,sep=""), hasGeospatialIssue = FALSE, limit = 5000, start = 0, fields=c('scientificName','decimalLatitude','decimalLongitude','year','month','day'), return = "data")
sp.data <- as.data.table(sp.data)

########### Alternative background sampling protocol - Construct species datsets using reported target species locations as presences (1's) and sites where multiple species were observed excluding target species as background (0's)
############## 
#Download background species data - setup parallel data retrieval workers (split over multiple years to reduce server load and address server allowances (i.e. multiple queries originating from identical IP address)) - capped at 200,000 we set the limit to 10,000 x 15 years = 150,000
# registerDoMC(detectCores() - 1)
# 
# raw.sp.data <- foreach(i = c(yr.start:yr.end), .packages = c("rgbif", "doMC"), combine = rbind) %dopar% {
#   temp <- occ_search(scientificName = NULL, hasCoordinate = TRUE, basisOfRecord = "HUMAN_OBSERVATION", geometry = wkt, year = i, hasGeospatialIssue = FALSE, limit = 10000, start = 0, fields=c('scientificName','classKey','decimalLatitude','decimalLongitude','year','month','day'), return = "data")
#   temp
# }
# save(raw.sp.data, file = "data/raw_sp_data_gbif") #Store a copy since the query can be quite time consuming (10+ minutes)...
#load("raw_sp_data_gbif") #Load saved copy of data - alternative method is to save R workspace...

#Combine all background data into single table
# all.sp.data <- data.table()
# for (i in 1:length(raw.sp.data)){
#   all.sp.data <- rbind(all.sp.data,raw.sp.data[[i]])
# }

#Sort and group records by time and location of observations to determine sites with multiple species surveys
#setkey(all.sp.data,year,month,day,decimalLongitude,decimalLatitude,scientificName,classKey)

#sites.spp <- all.sp.data[,c("MULT" = length(unique(scientificName))>1 & length(unique(classKey))>1, "TOTAL" = .N, "SPP" = list(paste(unique(scientificName),collapse=", "))), by="decimalLongitude,decimalLatitude,year,month,day"]

#sites.tax <- sites.spp[MULT == TRUE]

#sites.0 <- sites.tax[!like(SPP,sp.target)]
#x0 <- cbind(sites.0[,.("LON"=decimalLongitude,"LAT"=decimalLatitude)],"OCC"=rep(0,nrow(sites.0)))

sites.1 <- sp.data[, .N, by="decimalLongitude,decimalLatitude"]

x1 <- cbind(sites.1[,.("LON"=decimalLongitude,"LAT"=decimalLatitude)],"OCC"=rep(1,nrow(sites.1)))

#sp.model.data <- rbind(x1,x0)
#write.table(sp.model.data, file = "sp_model_data_ll.csv", row.names=FALSE, col.names=TRUE, sep=",")

#Optional conversion to projected coordinates for use in SDMs which sample from environmental covariate grids
coord.sys <- CRS("+init=epsg:3157") #Selected California NAD83 Zone 10 UTM

ll <- SpatialPoints(x1[,.(LON,LAT)], proj4string=CRS("+init=epsg:4269"))
UTM <- data.frame(spTransform(ll, coord.sys))
names(UTM) <- c('X','Y')

#data1.1 <- thin.algorithm(UTM, 1000, 50)
data1.1 <- gridSample(UTM, X)

data1.1 <- as.data.table(cbind(data1.1,"OCC"=1))

data0 <- read.csv("data/cal_study_bg_data_pts.csv")

deer.data <- rbind(data1.1,data0)
colnames(deer.data)[1:2] <- c("XCOORD","YCOORD")

#Sample covariate grid values at all egk coordinates
samples.df <- extract(vars,deer.data[,.(XCOORD,YCOORD)])

#Build modelling dataset
final.data <- cbind(deer.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.table(final.data, file = "data/cal_model_data_sdm_500.csv", row.names=FALSE, col.names=TRUE, sep=",")
