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

#Read in modelling dataset
model.data <- read.csv("data/cal_model_data_sdm_500.csv", header=T, sep=",")

#model.data$TREEDENS <- model.data$TREEDENS / 100

apply(model.data, 2, range)

cor(model.data[, 4:12])

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Set random seed to make results of gradient boosted regressions identical for each run (reproducible)
set.seed(123)

#Construct and run boosted regression tree model
deer.brt = gbm.step(data = model.data, gbm.x = c(4:6,9:12), gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.05, bag.fraction = 0.5)
save(deer.brt,file="output/cal_brt_500")
summary(deer.brt)

gbm.plot(deer.brt)

#Report reduction in deviance on null model (percent of error explained by model)
(brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep=""))

#Report discrimination performance of model in area under receiver operator characteristic curve
(brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep=""))

#Optional procedure to step through model covariates and select most parsimonious combination of variables
#deer.brt.simp = gbm.simplify(deer.brt)

#Make predictions with model fit based on covariate values in maps
load("data/cal_study_vars_500")

#Replace X and Y values with ones...
vars[["X"]][!is.na(values(vars[["X"]]))] <- 1
vars[["Y"]][!is.na(values(vars[["Y"]]))] <- 1

brt.preds <- predict(vars, deer.brt, n.trees=deer.brt$gbm.call$best.trees, type="response")

#Write out prediction map (relative likelihood of occurrence) in TIFF format
writeRaster(brt.preds, filename="output/deer_preds_brt_500.tif", format="GTiff", overwrite=TRUE)

#Use system commands to translate and upload grid to postgis database
#system("raster2pgsql -d -C -I -M -s 3157 -t auto /home/casey/Research/Github/coll_framework_cal/output/deer_preds_brt_500.tif gis_california.cal_nad8310_grid_deer_preds_brt_500 | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

#Plot prediction map using red to white color scheme
plot(brt.preds, col=sdm.colors(100))
points(model.data[model.data$OCC == 1, 1:2], pch = ".")

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(deer.brt), increment=500, resamp=0, latlon=FALSE)
cal.cor.df <- data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20])

save(cal.cor.df,file="output/cal_brt_cor")