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

load(file="output/cal_brt2")

summary(deer.brt)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

#Make predictions with model fit based on covariate values in maps
load("data/vic_study_vars")
brt.preds <- predict(vars, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

#Write out prediction map (relative likelihood of occurrence) in TIFF format
writeRaster(brt.preds, filename="output/deer_preds_brt_vic.tif", format="GTiff", overwrite=TRUE)

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Plot prediction map using red to white color scheme
plot(brt.preds, col=sdm.colors(100))

#Use system commands to translate and upload grid to postgis database
system("raster2pgsql -d -C -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_cal/output/deer_preds_brt_vic.tif gis_victoria.vic_gda9455_grid_deer_preds_brt | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")


