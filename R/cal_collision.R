library("data.table")
library("ncf")
library("spdep")

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

road.data <- as.data.table(read.delim("Data/cal_road_coords.csv", header=T, sep=","))  #Read in deer occurrence data
setkey(road.data,UID)

deer.data <- as.data.table(read.delim("Pred/cal_deer_preds.csv", header=T, sep=","))  #Read in deer occurrence data
deer.data$DEER[deer.data$DEER == 0] <- .0001
setkey(deer.data,UID)

tvol.data <- as.data.table(read.delim("Pred/cal_vol_preds.csv", header=T, sep=","))  #Read in traffic volume data
setkey(tvol.data,UID)

tspd.data <- as.data.table(read.delim("Pred/cal_speed_preds.csv", header=T, sep=","))  #Read in traffic speed data
setkey(tspd.data,UID)

#coll.data <- as.data.table(read.delim("Data/cal_collisiondata.csv", header=T, sep=","))  #Read in collision data
coll.data <- as.data.table(read.delim("Data/cal_collisiondata-cros.csv", header=T, sep=","))
setkey(coll.data,UID)

x.data <- na.omit(Reduce(merge,list(deer.data,tvol.data,tspd.data)))

data1 <- merge(coll.data,x.data, by="UID")

set.seed(123)
data0 <- cbind(x.data[sample(seq(1:nrow(x.data)),3*nrow(data1)),],"COLL"=rep(0,3*nrow(data1)))
setcolorder(data0, colnames(data1))

model.data <- rbind(data1,data0)
model.data <- na.omit(model.data)

model.data.coords <- merge(model.data,road.data, by="UID")

#Calculate natural logarithm of each covariate to test multiplicative effect of linear relationship
model.data$log.DEER <- log(model.data$DEER)
model.data$log.TVOL <- log(model.data$TVOL)
model.data$log.TSPD <- log(model.data$TSPD)

#Center logged covariates by subtracting means
model.data$c.log.DEER <- model.data$log.DEER - mean(model.data$log.DEER)
model.data$c.log.TVOL <- model.data$log.TVOL - mean(model.data$log.TVOL)
model.data$c.log.TSPD <- model.data$log.TSPD - mean(model.data$log.TSPD)

coll.glm <- glm(formula = COLL ~ c.log.DEER + c.log.TVOL + I(c.log.TVOL^2) + c.log.TSPD, family=binomial, data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

predpr <- predict(coll.glm,type="response")
roc(model.data$COLL,predpr)

indep.data <- na.omit(x.data)  #Remove any records with missing information in covariate data

#Calculate natural logarithm of each covariate
indep.data$log.DEER <- log(indep.data$DEER)
indep.data$log.TVOL <- log(indep.data$TVOL)
indep.data$log.TSPD <- log(indep.data$TSPD)

#Center logged covariates by subtracting means to match covariates used in regression mode
indep.data$c.log.DEER <- indep.data$log.DEER - mean(indep.data$log.DEER)
indep.data$c.log.TVOL <- indep.data$log.TVOL - mean(indep.data$log.TVOL)
indep.data$c.log.TSPD <- indep.data$log.TSPD - mean(indep.data$log.TSPD)

glm.preds <- predict(coll.glm, indep.data, type="response")  #Predict collision probability to all road segments using model fit

coll.preds <- data.table(UID=indep.data$UID,COLL=glm.preds)  #Combine predictions with unique IDs for all road segments

write.csv(coll.preds, file = "Pred/cal_coll_preds.csv", row.names=FALSE)  #Write out predictions for all road segments



##########################Spatial Autocorrelation###########################
data(hopkins)
hopkins_part <- hopkins[21:36,36:21]
hopkins_part[which(hopkins_part > 0, arr.ind=TRUE)] <- 1
hopkins.rook.nb <- cell2nb(16, 16, type="rook")
glmbase <- glm(c(hopkins_part) ~ 1, family="binomial")
set.seed(123)
MEbinom1 <- ME(c(hopkins_part) ~ 1, family="binomial",
               listw=nb2listw(hopkins.rook.nb, style="B"), alpha=0.2, verbose=TRUE)
glmME <- glm(c(hopkins_part) ~ 1 + fitted(MEbinom1), family="binomial")
anova(glmME, test="Chisq")
anova(glmbase, glmME, test="Chisq")




model.data.nb <- dnearneigh(coordinates(model.data.coords[,.(X,Y)]), 0, 10000)
set.seed(123)
MEbinom <- ME(model.data$COLL ~ model.data$c.log.DEER + model.data$c.log.TVOL + I(model.data$c.log.TVOL^2) + model.data$c.log.TSPD, family="binomial", listw=nb2listw(model.data.nb, style="B", zero.policy=TRUE), alpha=0.2, verbose=TRUE)
coll.glmME <- glm(model.data$COLL ~ c.log.DEER + c.log.TVOL + I(c.log.TVOL^2) + c.log.TSPD + fitted(MEbinom), family="binomial", data = model.data)


cor <- correlog(model.data.coords[,X], model.data.coords[,Y], resid(coll.glm), increment=1000, resamp=0, latlon=FALSE)
cor_df <- data.frame(x=as.numeric(names(cor$correlation[2:21])), y=cor$correlation[2:21])


cor2 <- correlog(model.data.coords[,X], model.data.coords[,Y], model.data.coords[,COLL], increment=1000, resamp=0, latlon=FALSE)
cor_df2 <- data.frame(x=as.numeric(names(cor$correlation[2:21])), y=cor$correlation[2:21])

shapes <- unlist(lapply(c("1", "2", "3", "4", "5", "6", "7"), utf8ToInt))

ggplot(cor_df,aes(x=x,y=y)) + geom_line(colour=c("grey70"),size=.75) + geom_point(size=2.5) + ylab("Moran's I") + xlab("Distance (km)") + theme_bw() + theme(legend.key = element_blank()) + geom_hline(aes(yintercept=0), linetype=2) + scale_x_continuous(breaks=seq(1, 20, 1))

ggplot(auto.coll,aes(x=x,y=y,group=col,shape=col)) + geom_line(colour=c("grey70"),size=.75) + geom_point(size=2.5) + ylab("Moran's I") + xlab("Distance (km)") + labs(shape = "Species") + theme_bw() + theme(legend.key = element_blank()) + scale_colour_manual(values=plotPal) + scale_shape_manual(values=shapes) + geom_hline(aes(yintercept=0), linetype=2) + scale_x_continuous(breaks=seq(1, 20, 1))



TVOL.brks <- seq(min(x.data$TVOL), max(x.data$TVOL), length=(max(x.data$TVOL)-min(x.data$TVOL))/200)
TSPD.brks <- seq(min(x.data$TSPD), max(x.data$TSPD), length=(max(x.data$TSPD)-min(x.data$TSPD))/10)
DEER.brks <- seq(min(x.data$DEER), max(x.data$DEER), length=(max(x.data$DEER)-min(x.data$DEER))/.1)

deer.data[,bin:=findInterval(DEER, DEER.brks)]
deer.data2 <- deer.data[, list(mean(DEER), .N), by=bin]

tvol.data[,bin:=findInterval(TVOL, TVOL.brks)]
tvol.data2 <- tvol.data[, list(mean(TVOL), .N), by=bin]
