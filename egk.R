#Load in required packages
require(dismo)
require(gbm)
require(ggplot2)
require(maptools)
require(ncf)
require(raster)
require(rgdal)
require(rgeos)
require(scales)
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

#Clean residual xml files created by QGIS
xml <- dir(path="../GIS/Vic",  pattern=".aux.xml")
file.remove(xml)
rm(xml)

#Read in list of environmental covariate grids (projected to GDA94(CSRS) / MGA zone 55 (EPSG:28355))
grid.files <- list.files("../GIS/Vic/")

#Extract names of files
grid.names <- unlist(strsplit(grid.files,"\\."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)]

#Read in Victoria boundary data in GDA94(CSRS) / MGA zone 55 (EPSG:28355) projection
victoria <- readShapePoly("../GIS/VIC_GDA9455_ADMIN_STATE.shp")

#Build empty raster object with resolution of 1000 metres and extents to cover Victoria
r <- raster(ncol=822, nrow=563, xmn=-58000, xmx=764000, ymn=5661000, ymx=6224000)

#Rasterize spatial polygon to produce terrestrial clipping boundary
vic.rst <- rasterize(victoria, r, 'UFI')

#Set clipping boundary extents
clip <- extent(-58000, 764000, 5661000, 6224000)

#Loop through all grids and clip to proper extents
for (i in 1:length(grid.files)) {
  temp <- raster(paste("../GIS/Vic/",grid.files[i], sep=""))
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * vic.rst)
}

#Combine all covariate grids to raster stack
vars <- stack(mget(grid.names))

#Read in egk occurrence data (presence and psuedo-background coded 1 and 0 and spatial coordinates in NAD83(CSRS) / UTM zone 10N (EPSG:3157) projection)
#egk.data <- read.csv("Data/vic_speciesdata2.csv", header=T, sep=",")
#egk.data <- read.csv("../Data/sp_model_data_utm.csv", header=T, sep=",")

data0 <- read.csv("/home/casey/Research/Projects/SDMs/Data/egk_vba_data_0.csv")
#data1 <- read.csv("/home/casey/Research/Projects/SDMs/Data/egk_vba_data_1.csv")
#data1$RECORD_ID <- NULL

data1 <- dbGetQuery(con,paste0("
      SELECT DISTINCT
        ST_X(pts.geom) AS X, ST_Y(pts.geom) AS Y, CAST(1 AS INTEGER) AS OCC
      FROM
        gis_victoria.vic_gda9455_fauna_vba AS pts, gis_victoria.vic_gda9455_admin_state AS poly
      WHERE
        ST_contains(poly.geom, pts.geom)
      AND
        pts.start_year >= '2000'
      AND
        sci_name = 'Macropus giganteus'
      GROUP BY
        pts.geom;
  "))

colnames(data1) <- toupper(colnames(data1))

egk.data <- rbind(data1,data0)

#Extract egk data coordinates
egk.coord <- egk.data[,1:2]

#Sample covariate grid values at all egk coordinates
samples.df <- extract(vars,egk.coord)

#Build modelling dataset
model.data <- cbind(egk.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
model.data <- na.omit(model.data)
rownames(model.data) <- NULL

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Set random seed to make results of gradient boosted regressions identical for each run (reproducible)
set.seed(123)

#Construct and run boosted regression tree model
egk.brt = gbm.step(data = model.data, gbm.x = 4:11, gbm.y = 3, family = "bernoulli", tree.complexity = 7, learning.rate = 0.005, bag.fraction = 0.5)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((egk.brt[["self.statistics"]][["mean.null"]] - egk.brt[["cv.statistics"]][["deviance.mean"]])/egk.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((egk.brt[["cv.statistics"]][["deviance.se"]]/egk.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(egk.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(egk.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

#Optional procedure to step through model covariates and select most parsimonious combination of variables
#egk.brt.simp = gbm.simplify(egk.brt)

#Make predictions with model fit based on covariate values in maps
brt.preds <- predict(vars, egk.brt, n.trees=egk.brt$gbm.call$best.trees, type="response")

#Write out prediction map (relative likelihood of occurrence) in TIFF format
writeRaster(brt.preds, filename="Pred/EGK.tif", format="GTiff", overwrite=TRUE)

#Use system to translate and uplaod ascii grid to postgis database
system("raster2pgsql -I -M -s 28355 -t auto /home/casey/Research/Projects/Deer_Collisions/Model/Pred/EGK.tif gis_victoria.egk | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

#Plot prediction map using red to white color scheme
plot(brt.preds, col=sdm.colors(100))

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(egk.brt), increment=1000, resamp=0, latlon=FALSE)
cor.df <- data.frame(x=as.numeric(names(cor$correlation[2:21])), y=cor$correlation[2:21])

#Plot first 20 kms in 1 km bins
ggplot(cor.df,aes(x=x,y=y)) + 
  geom_line(colour=c("grey70"),size=1) + 
  ylab("Moran's I") + 
  xlab("Distance (km)") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(text = element_text(size = 20)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_continuous(breaks=seq(1, 20, 1))

#Read in road segment data, sample values of species occurrence, and write values to csv file
vic.roads <- read.csv("Data/vic_trafficdata.csv", header=T, sep=",")
roads.coord <- vic.roads[,10:11]
samples.df <- extract(brt.preds,roads.coord)
pred.data <- data.frame(cbind(vic.roads$UID,samples.df))
names(pred.data) <- c("UID","EGK")
write.csv(pred.data, file = "Pred/vic_egk_preds.csv", row.names=FALSE)


# SELECT DISTINCT r.id AS id, g.egk2 AS egk
# FROM (
#   SELECT id, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
#   FROM gis_victoria.vic_gda9455_roads_state) AS r, gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
# WHERE ST_Intersects(r.geom,g.geom)
