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

grid.files <- list.files(path='data/grids/cal/envi') #Create vector of filenames
grid.names <- substring(unlist(strsplit(grid.files,"\\_1000."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

cal.rst.study <- raster("data/grids/cal/CAL_NAD8310_GRID_STUDY_1000.tif")
#cal.rst.state <- raster("data/grids/cal/CAL_NAD8310_GRID_STATE_1000.tif")
clip.study <- extent(445000,1165000,3962000,4329000) #Define clipping extent of maps
#clip.state <- extent(374000,1318000,3613000,4654000)

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/cal/envi/",grid.files[i]))
  #temp <- crop(temp, clip.state)
  #assign(grid.names[i],temp * cal.rst.state)
  temp <- crop(temp, clip.study)
  assign(grid.names[i],temp * cal.rst.study)
}
vars <- stack(mget(grid.names)) #Combine all maps to single stack

#Read in modelling dataset
model.data <- read.csv("data/cal_model_data_sdm.csv", header=T, sep=",")

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Set random seed to make results of gradient boosted regressions identical for each run (reproducible)
set.seed(123)

#Construct and run boosted regression tree model
deer.brt = gbm.step(data = model.data, gbm.x = 4:10, gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5)
save(deer.brt,file="output/cal_brt")

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
writeRaster(brt.preds, filename="output/deer_preds_brt.tif", format="GTiff", overwrite=TRUE)

#Plot prediction map using red to white color scheme
plot(brt.preds, col=sdm.colors(100))

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(deer.brt), increment=1000, resamp=0, latlon=FALSE)
cal.cor.df <- data.frame(x=as.numeric(names(cor$correlation[2:20])), y=cor$correlation[2:20])

save(cal.cor.df,file="output/cal_brt_cor")

#Plot first 20 kms in 1 km bins
ggplot(cal.cor.df,aes(x=x,y=y)) + 
  geom_line(colour=c("grey70"),size=1) + 
  ylab("Moran's I") + 
  xlab("Distance (km)") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(text = element_text(size = 20)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_continuous(breaks=seq(1, 20, 1))
