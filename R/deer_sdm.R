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

setwd("C:/Users/cwdavies/Desktop/deer_roadrisk/R")

thin.algorithm <- function (rec.df.orig, thin.par, reps) # function slightly adapted from https://github.com/cran/spThin/blob/master/R/thin.algorithm.R
{
  reduced.rec.dfs <- list()
  for (Rep in 1:reps) {
    rec.df <- rec.df.orig
    DistMat <- rdist(x1 = rec.df)
    diag(DistMat) <- NA
    while (min(DistMat, na.rm = TRUE) < thin.par & nrow(rec.df) > 
           1) {
      CloseRecs <- which(DistMat < thin.par, arr.ind = TRUE)[, 
                                                             1]
      RemoveRec <- as.numeric(names(which(table(CloseRecs) == 
                                            max(table(CloseRecs)))))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      rec.df <- rec.df[-RemoveRec, ]
      DistMat <- DistMat[-RemoveRec, -RemoveRec]
      if (length(DistMat) == 1) {
        break
      }
    }
    colnames(rec.df) <- c("x", "y")
    reduced.rec.dfs[[Rep]] <- rec.df
  }
  locs.thinned <- reduced.rec.dfs
  y.x.thin.count <- unlist(lapply(locs.thinned, nrow))
  max.thin.recs <- max(y.x.thin.count)
  df.temp <- locs.thinned[[which(y.x.thin.count == max.thin.recs)[1]]]
  colnames(df.temp) <- colnames(rec.df.orig)
  
  return(df.temp)
}

grid.files <- list.files(path='../data/grids/envi') #Create vector of filenames - change to your directory with raster covariates

grid.names <- substring(unlist(strsplit(grid.files,"\\_500."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

vic.rst <- raster("../data/grids/VIC_GDA9455_GRID_STATE_500.tif")

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

X <- Y <- vic.rst
Y[] <- yFromCell(vic.rst, 1:ncell(vic.rst))/1000000
Y <- crop(Y,clip) * vic.rst
X[] <- xFromCell(vic.rst, 1:ncell(vic.rst))/1000000
X <- crop(X,clip) * vic.rst

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("../data/grids/envi/",grid.files[i]))
  temp <- projectRaster(temp, vic.rst, res=500)
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * vic.rst)
}
vars <- stack(c(mget(grid.names),"X"=X,"Y"=Y)) #Combine all maps to single stack
save(vars,file="../data/vic_study_vars")

# sr <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# wkt <- writeWKT(as(extent(projectRaster(vic.rst, crs = sr)), 'SpatialPolygons'))
# 
# #Specify target species
# sp.target <- "Cervus unicolor"
# 
# #Specify start year
# yr.start <- 2000
# 
# #Specify end year
# yr.end <- 2017
# 
# #Download target species data - add fields as required - note additional fields may be added (?occ_search and http://www.gbif.org/developer/occurrence#parameters)
# fields <- ala_fields("occurrence_stored",as_is=TRUE)
# 
# species <- specieslist(sp.target, wkt)
# 
# sp.data <- occurrences(taxon = paste(sp.target),
#                        wkt = wkt,
#                        fq = "",
#                        fields = c('species','latitude','longitude'),
#                        download_reason_id = 10)[['data']][,1:2]
# 
# sp.data <- as.data.table(sp.data)
# 
# sites.1 <- sp.data[, .N, by="longitude,latitude"]
# 
# x1 <- cbind(sites.1[,.("LON"=longitude,"LAT"=latitude)],"OCC"=rep(1,nrow(sites.1)))
# 
# #Optional conversion to projected coordinates for use in SDMs which sample from environmental covariate grids
# coord.sys <- CRS("+init=epsg:28355")
# 
# ll <- SpatialPoints(x1[,.(LON,LAT)], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# UTM <- data.frame(spTransform(ll, coord.sys))
# names(UTM) <- c('X','Y')

data <- read.csv("../data/VIC_GDA9455_FAUNA_VBA_DEER.csv")

data1.1 <- thin.algorithm(data[, 4:5], 500, 50)
data1.1 <- as.data.table(cbind(data1.1,"OCC"=1))

data0 <- read.csv("../data/vic_bg_data_pts.csv")

deer.data <- rbind(data1.1,data0)
colnames(deer.data)[1:2] <- c("XCOORD","YCOORD")

#Sample covariate grid values at all deer coordinates
samples.df <- extract(vars,deer.data[,.(XCOORD,YCOORD)])

#Build modelling dataset
final.data <- cbind(deer.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.csv(final.data, "../data/vic_model_data_sdm.csv", row.names=FALSE) #change path to suit

#Deer modelling

model.data <- read.csv("../data/vic_model_data_sdm.csv", header=T, sep=",")

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])


sdm.colors = colorRampPalette(c("white","darkred")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

deer.brt.all = gbm.step(data = model.data, gbm.x = 4:31, gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model
save(deer.brt.all,file="../output/vic_brt_all")
load(file="../output/vic_brt_all")
summary(deer.brt.all)

# var     rel.inf
# X                     X 13.33192876
# Y                     Y  5.73691844
# PRECSEAS       PRECSEAS  5.57518013
# GREEN             GREEN  5.06483042
# TREEDENS       TREEDENS  5.03837985
# MNTEMPWQ       MNTEMPWQ  4.55075367
# SLOPE             SLOPE  4.27355538
# MNDIRANGE     MNDIRANGE  4.14612666 X
# PRECDM           PRECDM  4.10768673
# HYDROLOGY     HYDROLOGY  4.02228353
# DISTMAJRIV   DISTMAJRIV  3.89832520
# PRECWAQ         PRECWAQ  3.79936011 X
# MNTEMPDQ       MNTEMPDQ  3.60785410
# ELEV               ELEV  3.42358181
# PRECCQ           PRECCQ  3.05687625 X
# ANPREC           ANPREC  2.91972208 X
# ISOTHERM       ISOTHERM  2.84706357 X
# MINTEMPCM     MINTEMPCM  2.66010799 X
# TEMPSEAS       TEMPSEAS  2.65707280 X
# MAXTEMPWM     MAXTEMPWM  2.58542070 X
# PRECWQ           PRECWQ  2.35627580 X
# TEMPANRANGE TEMPANRANGE  2.13398875 X
# PRECWM           PRECWM  1.93912372 X
# PRECDQ           PRECDQ  1.72771996 X
# MNTEMPCQ       MNTEMPCQ  1.56523530 X
# MNTEMPWAQ     MNTEMPWAQ  1.52008118 X
# MNTEMPAN       MNTEMPAN  1.44262362 X
# LIGHT             LIGHT  0.01192352

cor(model.data[, 4:31])

deer.brt = gbm.step(data = model.data, gbm.x = c(5:8, 10, 18, 20, 22, 26, 29:31), gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model
save(deer.brt,file="../output/vic_brt")
load(file="../output/vic_brt")
summary(deer.brt)


#Report reduction in deviance on null model (percent of error in data explained by model)
brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

brt.preds <- predict(vars, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

vars_noXY <- vars
vars_noXY[["X"]][!is.na(vars_noXY[["X"]])] <- 1
vars_noXY[["Y"]][!is.na(vars_noXY[["Y"]])] <- 1
brt.preds <- predict(vars_noXY, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

writeRaster(brt.preds, filename="../output/deer_preds_brt.tif", format="GTiff", overwrite=TRUE, NAflag=-9999, datatype='FLT4S') #Write out prediction map in tif format

brt.preds <- raster("output/deer_preds_brt2.tif")
system("raster2pgsql -d -C -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_cal/output/deer_preds_brt2.tif gis_victoria.vic_gda9455_grid_deer_preds_brt_500 | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")
