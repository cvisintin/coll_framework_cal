require(randomForest)
require(data.table)
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab


model.data <- as.data.table(read.delim("data/cal_model_data_traffic.csv", header=T, sep=","))  #Read in traffic volume data for road segments

model.data[!is.na(aadt),.N]

model.data[!is.na(speedlmt),.N]

cor(na.omit(model.data[,.(popdens,kmtohwy,kmtodev,rddens,rdclass)]))

set.seed(123)
volume.rf <- randomForest(formula = log(aadt) ~ kmtodev + kmtohwy + popdens + rdclass + rddens, data = model.data[!is.na(model.data$aadt),], mtry=2, importance = TRUE, sampsize = 1000)  #Fit random forest model

volume.rf$importance

vol.preds <- predict(volume.rf, model.data, type="response")

vol.preds.df <- cbind("uid"=model.data$uid,"tvol"=exp(vol.preds)) #Combine predictions with unique IDs for all road segments
write.csv(vol.preds.df, file = "output/cal_tvol_preds_rf.csv", row.names=FALSE)

vol.preds.dt <- as.data.table(vol.preds.df)
setkey(vol.preds.dt,uid)
perf.vol <- merge(vol.preds.dt,model.data[!is.na(model.data$aadt),])
plot(perf.vol$aadt,perf.vol$tvol)
abline(a=0,b=.5, lty=2)

dbWriteTable(con, c("gis_california", "cal_nogeom_roads_volpreds"), value = vol.preds.dt, row.names=FALSE, overwrite=TRUE)


set.seed(123)
speed.rf <- randomForest(formula = speedlmt ~ rdclass + rddens, data = model.data[!is.na(model.data$speedlmt),], mtry=2, importance = TRUE, sampsize = 1000)  #Fit random forest model

speed.rf$importance

speed.preds <- predict(speed.rf, model.data, type="response")

speed.preds.df <- cbind("uid"=model.data$uid,"tspd"=speed.preds)  #Combine predictions with unique IDs for all road segments
write.csv(speed.preds.df, file = "output/cal_tspd_preds_rf.csv", row.names=FALSE)

speed.preds.dt <- as.data.table(speed.preds.df)
setkey(speed.preds.dt,uid)
perf.spd <- merge(speed.preds.dt,model.data[!is.na(model.data$speedlmt),])
plot(perf.spd$speedlmt,perf.spd$tspd)
abline(a=0,b=.5, lty=2)

dbWriteTable(con, c("gis_california", "cal_nogeom_roads_speedpreds"), value = speed.preds.dt, row.names=FALSE, overwrite=TRUE)