library("data.table")

#Define function for receiver operator characteristic (ROC)
"roc" <- function (obsdat, preddat){
    if (length(obsdat) != length(preddat)) 
      stop("obs and preds must be equal lengths")
    n.x <- length(obsdat[obsdat == 0])
    n.y <- length(obsdat[obsdat == 1])
    xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
    rnk <- rank(xy)
    roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
    return(round(roc, 4))
  }

egk.data <- as.data.table(read.delim("Pred/vic_egk_preds.csv", header=T, sep=","))  #Read in egk occurrence data
egk.data$EGK[egk.data$EGK == 0] <- .0001
setkey(egk.data,UID)

tvol.data <- as.data.table(read.delim("Pred/vic_vol_preds.csv", header=T, sep=","))  #Read in traffic volume data
setkey(tvol.data,UID)

tspd.data <- as.data.table(read.delim("Pred/vic_speed_preds.csv", header=T, sep=","))  #Read in traffic speed data
setkey(tspd.data,UID)

coll.data <- as.data.table(read.delim("Data/vic_collisiondata.csv", header=T, sep=","))  #Read in collision data
setkey(coll.data,UID)

#ncoll.data <- as.data.table(read.delim("Data/vic_collisioncountdata.csv", header=T, sep=",")) ####Poisson
#setkey(ncoll.data,UID) ####Poisson

x.data <- Reduce(merge,list(egk.data,tvol.data,tspd.data))

data1 <- merge(coll.data,x.data, by="UID")
#ndata <- merge(ncoll.data,x.data, by="UID") ####Poisson

#set.seed(123)
#data0 <- cbind(x.data[sample(seq(1:nrow(x.data)),3*nrow(data1)),],"COLL"=rep(0,3*nrow(data1)))
data0 <- cbind(x.data,"COLL"=rep(0,nrow(x.data)))
setcolorder(data0, colnames(data1))

model.data <- rbind(data1,data0)
model.data <- na.omit(model.data)

#model.data <- na.omit(ndata) ####Poisson

#Calculate natural logarithm of each covariate to test multiplicative effect of linear relationship
model.data$log.EGK <- log(model.data$EGK)
model.data$log.TVOL <- log(model.data$TVOL)
model.data$log.TSPD <- log(model.data$TSPD)

#Center logged covariates by subtracting means
model.data$c.log.EGK <- model.data$log.EGK - mean(model.data$log.EGK)
model.data$c.log.TVOL <- model.data$log.TVOL - mean(model.data$log.TVOL)
model.data$c.log.TSPD <- model.data$log.TSPD - mean(model.data$log.TSPD)

coll.glm <- glm(formula = COLL ~ c.log.EGK + c.log.TVOL + I(c.log.TVOL^2) + c.log.TSPD, family=binomial, data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

predpr <- predict(coll.glm,type="response")
roc(model.data$COLL,predpr)

indep.data <- na.omit(x.data)  #Remove any records with missing information in covariate data

#Calculate natural logarithm of each covariate
indep.data$log.EGK <- log(indep.data$EGK)
indep.data$log.TVOL <- log(indep.data$TVOL)
indep.data$log.TSPD <- log(indep.data$TSPD)

#Center logged covariates by subtracting means to match covariates used in regression mode
indep.data$c.log.EGK <- indep.data$log.EGK - mean(indep.data$log.EGK)
indep.data$c.log.TVOL <- indep.data$log.TVOL - mean(indep.data$log.TVOL)
indep.data$c.log.TSPD <- indep.data$log.TSPD - mean(indep.data$log.TSPD)

glm.preds <- predict(coll.glm, indep.data, type="response")  #Predict collision probability to all road segments using model fit

coll.preds <- data.table(UID=indep.data$UID,COLL=glm.preds)  #Combine predictions with unique IDs for all road segments

write.csv(coll.preds, file = "Pred/vic_coll_preds.csv", row.names=FALSE)  #Write out predictions for all road segments

#Poisson Modelling:

ncoll.glm <- glm(formula = NCOLL ~ c.log.EGK + c.log.TVOL + I(c.log.TVOL^2) + c.log.TSPD, family=poisson, data = model.data) ####Poisson

summary(ncoll.glm) ####Poisson

paste("% Deviance Explained: ",round(((ncoll.glm$null.deviance - ncoll.glm$deviance)/ncoll.glm$null.deviance)*100,2),sep="") ####Poisson

glm.preds <- predict(ncoll.glm, indep.data, type="response") ####Poisson
