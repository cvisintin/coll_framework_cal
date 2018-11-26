require(maptools)
require(scales)
require(rgdal)
require(rgeos)
require(raster)
require(fields)
require(sp)
require(data.table)
require(dismo)
require(ALA4R)

load(file = "data/vic_study_vars_500")

vic.rst <- raster("data/grids/vic/VIC_GDA9455_GRID_STATE_500.tif")

# sr <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# wkt <- writeWKT(as(extent(projectRaster(vic.rst, crs = sr)), 'SpatialPolygons'))
# 
# #Specify target species
# sp.target <- "Cervus"
# 
# #Specify start year
# yr.start <- 2000
# 
# #Specify end year
# yr.end <- 2016
# 
# #Download target species data - add fields as required - note additional fields may be added (?occ_search and http://www.gbif.org/developer/occurrence#parameters)
# fields <- ala_fields("occurrence_stored",as_is=TRUE)
# 
# species <- specieslist(sp.target, wkt)
# 
# sp.data <- occurrences(taxon = paste(sp.target),
#                        wkt = wkt,
#                        fields = c('species','year','latitude','longitude'),
#                        download_reason_id = 10)[['data']][,1:3]
# 
# sp.data <- as.data.table(sp.data)
# 
# sites.1 <- sp.data[species != "", .N, by="longitude,latitude"]
# 
# x1 <- cbind(sites.1[,.("LON"=longitude,"LAT"=latitude)],"OCC"=rep(1,nrow(sites.1)))
# 
# #Optional conversion to projected coordinates for use in SDMs which sample from environmental covariate grids
# coord.sys <- CRS("+init=epsg:28355")
# 
# ll <- SpatialPoints(x1[,.(LON,LAT)], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# UTM <- data.frame(spTransform(ll, coord.sys))
# names(UTM) <- c('X','Y')
# data.ala <- gridSample(UTM, vic.rst, n=1)
# data.ala <- as.data.table(cbind(data.ala,"OCC"=1))


data1 <- cbind(gridSample(read.csv("data/VIC_GDA9455_FAUNA_VBA_DEER.csv")[, 4:5], vic.rst, n=1), "OCC"=1)
data0 <- read.csv("data/vic_bg_data_pts.csv")

deer.data <- rbind(data1, data0)
colnames(deer.data)[1:2] <- c("XCOORD", "YCOORD")

#Sample covariate grid values at all deer coordinates
samples.df <- extract(vars, deer.data[ , 1:2])

#Build modelling dataset
final.data <- cbind(deer.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.csv(final.data, "data/vic_model_data_sdm_deer_500.csv", row.names=FALSE) #change path to suit

#Deer modelling

model.data <- read.csv("data/vic_model_data_sdm_deer_500.csv", header=T, sep=",")

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

cor(model.data[, 4:10])

sdm.colors = colorRampPalette(c("white","darkred")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

deer.brt = gbm.step(data = model.data, gbm.x = c(4:10), gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model

summary(deer.brt)

save(deer.brt,file="output/vic_deer_brt_500")
load(file="output/vic_deer_brt_500")

#Report reduction in deviance on null model (percent of error in data explained by model)
(brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep=""))

#Report discrimination performance of model in area under receiver operator characteristic curve
(brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep=""))

brt.preds <- predict(vars, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")
plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

vars_noXY <- vars
vars_noXY[["X"]][!is.na(vars_noXY[["X"]])] <- 1
vars_noXY[["Y"]][!is.na(vars_noXY[["Y"]])] <- 1
brt.preds <- predict(vars_noXY, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

writeRaster(brt.preds, filename="output/deer_preds_brt_500.tif", format="GTiff", overwrite=TRUE, NAflag=-9999, datatype='FLT4S') #Write out prediction map in tif format

brt.preds <- raster("output/deer_preds_brt_500.tif")
system("raster2pgsql -d -C -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_cal/output/deer_preds_brt_500.tif gis_victoria.vic_gda9455_grid_deer_preds_brt_500 | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")
