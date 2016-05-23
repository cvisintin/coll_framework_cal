library("h2o")

localH2O = h2o.init(nthreads = -1) 

traffic.hex <-  h2o.uploadFile(localH2O, path = "Data/cal_trafficdata.csv")
summary(traffic.hex)

traffic.hex[,8] <- as.factor(traffic.hex[,8])


volume.train = traffic.hex[!is.na(traffic.hex$AADT),]
volume.train$L_AADT <- log(volume.train$AADT)

system.time(
volume.h2orf <- h2o.randomForest(
                x=c("INCOMEPP","KMTODEV","KMTOHWY","POPDENS","RDCLASS","RDDENS"),
                y="L_AADT",
                training_frame=volume.train,
                validation_frame=NULL,
                mtries = -1,
                sample_rate = 0.67,
                build_tree_one_node = FALSE,
                ntrees = 500,
                binomial_double_trees = FALSE,
                balance_classes = FALSE,
                seed=123)
)

system.time(
volume.preds <- h2o.predict(volume.h2orf, traffic.hex)
)

vol.preds <- cbind(as.data.frame(traffic.hex[["UID", exact = TRUE]]),as.data.frame(exp(volume.preds[["predict", exact = TRUE]])))

names(vol.preds) <- c("UID","TVOL")

write.csv(vol.preds, file = "Pred/cal_vol_preds.csv", row.names=FALSE)


speed.train = traffic.hex[!is.na(traffic.hex$SPEEDLMT),]

speed.h2orf <- h2o.randomForest(
  x=c("RDCLASS","RDDENS"),
  y="SPEEDLMT",
  training_frame=speed.train,
  validation_frame=NULL,
  mtries = -1,
  sample_rate = 0.67,
  build_tree_one_node = FALSE,
  ntrees = 500,
  binomial_double_trees = FALSE,
  balance_classes = FALSE,
  seed=123)

speed.preds <- h2o.predict(speed.h2orf, traffic.hex)

spd.preds <- cbind(as.data.frame(traffic.hex[["UID", exact = TRUE]]),as.data.frame(speed.preds[["predict", exact = TRUE]]))

names(spd.preds) <- c("UID","TSPD")

write.csv(spd.preds, file = "Pred/cal_speed_preds.csv", row.names=FALSE)
