require(maptools)
require(scales)
require(rgdal)
require(raster)
require(gbm)
require(dismo)
require(data.table)
require(ncf)
require(foreach)

model.data <- read.csv("data/vic_model_data_sdm.csv", header=T, sep=",")

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

sdm.colors = colorRampPalette(c("white","red")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

kang.brt = gbm.step(data = model.data, gbm.x = 5:11, gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5, prev.stratify = FALSE) #Create boosted regression tree model
save(kang.brt,file="output/vic_brt")
summary(kang.brt)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((kang.brt[["self.statistics"]][["mean.null"]] - kang.brt[["cv.statistics"]][["deviance.mean"]])/kang.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((kang.brt[["cv.statistics"]][["deviance.se"]]/kang.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(kang.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(kang.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

load("data/vic_study_vars")
brt.preds <- predict(vars, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps

writeRaster(brt.preds, filename="/home/casey/Research/Github/coll_framework_cal/output/egk_preds_brt.tif", format="GTiff", overwrite=TRUE, NAflag=-9999, datatype='FLT4S') #Write out prediction map in tif format

#Use system commands to translate and uplaod grid to postgis database
#system("raster2pgsql -d -C -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_cal/output/egk_preds_brt.tif gis_victoria.vic_gda9455_grid_egk_preds_brt | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(kang.brt), increment=1000, resamp=0, latlon=FALSE)
vic.cor.df <- data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20])

save(vic.cor.df,file="output/vic_brt_cor")