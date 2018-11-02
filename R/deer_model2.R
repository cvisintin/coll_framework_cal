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
model.data <- read.csv("data/cal_model_data_sdm.csv", header=T, sep=",")

model.data$TREEDENS <- model.data$TREEDENS / 100

apply(model.data, 2, range)

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

#Define color scheme for plotting
sdm.colors = colorRampPalette(c("white","red"))

#Set random seed to make results of gradient boosted regressions identical for each run (reproducible)
set.seed(123)

#Construct and run boosted regression tree model
deer.brt = gbm.step(data = model.data, gbm.x = 4:10, gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5)
save(deer.brt,file="output/cal_brt2")
summary(deer.brt)

gbm.plot(deer.brt)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((deer.brt[["self.statistics"]][["mean.null"]] - deer.brt[["cv.statistics"]][["deviance.mean"]])/deer.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((deer.brt[["cv.statistics"]][["deviance.se"]]/deer.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(deer.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(deer.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

