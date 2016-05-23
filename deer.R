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

#Clean residual xml files created by QGIS
xml <- dir(path="../GIS/Cal",  pattern=".aux.xml")
file.remove(xml)
rm(xml)

#Read in list of environmental covariate grids (projected to in NAD83(CSRS) / UTM zone 10N (EPSG:3157))
grid.files <- list.files("../GIS/Cal/")

#Extract names of files
grid.names <- unlist(strsplit(grid.files,"\\."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)]

#Read in California boundary data in NAD83(CSRS) / UTM zone 10N (EPSG:3157) projection
california <- readShapePoly("../GIS/CAL_NAD8310_ADMIN_STATE_SIMPLE.shp")

#Set optional buffer to increase raster grid coverage
#california <- gBuffer(california, width=1000)

#Build empty raster object with resolution of 1000 metres and extents to cover California
r <- raster(ncol=938, nrow=1040, xmn=380000, xmx=1318000, ymn=3614000, ymx=4654000)

#Rasterize spatial polygon to produce terrestrial clipping boundary
cal.rst <- rasterize(california, r, 'UFI')

#Set clipping boundary extents
clip <- extent(380000, 1318000, 3614000, 4654000)

#Loop through all grids and clip to proper extents
for (i in 1:length(grid.files)) {
  temp <- raster(paste("../GIS/Cal/",grid.files[i], sep=""))
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * cal.rst)
}

#Combine all covariate grids to raster stack
vars <- stack(mget(grid.names))

#Read in deer occurrence data (presence and psuedo-background coded 1 and 0 and spatial coordinates in NAD83(CSRS) / UTM zone 10N (EPSG:3157) projection)
#deer.data <- read.csv("Data/cal_speciesdata.csv", header=T, sep=",")
#deer.data <- read.csv("../Data/sp_model_data_utm.csv", header=T, sep=",")

data0 <- read.csv("/home/casey/Research/Projects/Deer_Collisions/Model/Data/deer0.csv")
data1 <- read.csv("/home/casey/Research/Projects/Deer_Collisions/Model/Data/deer1.csv")
deer.data <- rbind(data1, data0)

#Extract deer data coordinates
deer.coord <- deer.data[,1:2]

#Sample covariate grid values at all deer coordinates
samples.df <- extract(vars,deer.coord)

#Build modelling dataset
model.data <- cbind(deer.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
model.data <- na.omit(model.data)
rownames(model.data) <- NULL

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Set random seed to make results of gradient boosted regressions identical for each run (reproducible)
set.seed(123)

#Construct and run boosted regression tree model
deer.brt = gbm.step(data = model.data, gbm.x = 4:11, gbm.y = 3, family = "bernoulli", tree.complexity = 7, learning.rate = 0.005, bag.fraction = 0.5)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

#Optional procedure to step through model covariates and select most parsimonious combination of variables
#deer.brt.simp = gbm.simplify(deer.brt)

#Make predictions with model fit based on covariate values in maps
brt.preds <- predict(vars, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

#Write out prediction map (relative likelihood of occurrence) in TIFF format
writeRaster(brt.preds, filename="Pred/DEER.tif", format="GTiff", overwrite=TRUE)

#Plot prediction map using red to white color scheme
plot(brt.preds, col=sdm.colors(100))

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(deer.brt), increment=1000, resamp=0, latlon=FALSE)
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
cal.roads <- read.csv("Data/cal_trafficdata.csv", header=T, sep=",")
roads.coord <- cal.roads[,10:11]
samples.df <- extract(brt.preds,roads.coord)
pred.data <- data.frame(cbind(cal.roads$UID,samples.df))
names(pred.data) <- c("UID","DEER")
write.csv(pred.data, file = "Pred/cal_deer_preds.csv", row.names=FALSE)
