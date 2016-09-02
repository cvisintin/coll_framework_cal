require(RPostgreSQL)
require(data.table)
require(raster)
require(boot)
require(doMC)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

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

roads <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid as uid, ST_X(r.geom) AS x, ST_Y(r.geom) AS y
  FROM
	  (SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_california.cal_nad8310_roads_study) AS r
  "))
setkey(roads,uid)

tvol.preds <- as.data.table(read.csv("output/cal_tvol_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

tspd.preds <- as.data.table(read.csv("output/cal_tspd_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

sdm.preds <- raster("output/deer_preds_brt.tif")

cov.data$deer <- raster::extract(sdm.preds,cov.data[,.(x,y)])

coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_california.cal_nad8310_roads_study as r,
      (SELECT
        id, geom
      FROM
        gis_california.cal_nad8310_fauna_cros
      WHERE
        scientific = 'Odocoileus hemionus'
      AND
        odatetime >= '2010-01-01 00:00') AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(coll,uid)

data1 <- merge(cov.data, coll)

set.seed(123)

##########Added code for 1000 simulations and model averaging#############
rns <- sample(1:10000, 1000, replace=F)

models1000 <- foreach(i = 1:length(rns), .combine=rbind, .export="roc") %dopar% {
  set.seed(rns[i])
  data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
  model.data <- rbind(data1,data0)
  model.data <- na.omit(model.data)
  coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
  coefs <- as.numeric(coll.glm$coefficients)
  devred <- round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2)
  rocvalue <- roc(model.data$coll,coll.glm$fitted.values)
  c(coefs, devred, rocvalue)
}

invcloglog <- function (x) {1-exp(-exp(x))}

occ <- NULL

occ <- foreach(i = 1:length(rns), .combine=rbind, .export="roc") %dopar% {
  set.seed(rns[i])
  data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
  model.data <- rbind(data1,data0)
  model.data <- na.omit(model.data)
  coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
  data.frame(x=model.data[,deer], y=invcloglog(cbind(1,log(model.data[,deer]),mean(log(model.data[,tvol])),mean((log(model.data[,tvol]))*(log(model.data[,tvol]))),mean(log(model.data[,tspd]))) %*% coef(coll.glm)), col=rep(i, each=length(model.data[,deer])))
}

tiff('figs/occ.tif', pointsize = 8, compression = "lzw", res=300, width = 1100, height = 900)
ggplot(occ,aes(x=x,y=y,group=col)) +
  geom_line(size=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme(legend.position="none") +
  #labs(color = "Species") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  #scale_colour_manual(values=plotPal) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) #+
#guides(colour=FALSE)
dev.off()



#########################################################################

data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))

model.data <- rbind(data1,data0)
model.data <- na.omit(model.data)

coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/cal_coll_coef.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm)[2:4,2]/sum(anova(coll.glm)[2:4,2]), format='f',digits=4),"output/cal_coll_anova.csv",row.names=FALSE)

save(coll.glm,file="output/cal_coll_glm")

save(model.data,file="output/cal_coll_model_data")

coll.preds <- predict(coll.glm, cov.data, type="response")

coll.preds.df <- as.data.table(cbind("uid"=cov.data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/cal_coll_preds_glm.csv", row.names=FALSE)

dbWriteTable(con, c("gis_california", "cal_nogeom_roads_deercollrisk"), value = coll.preds.df, row.names=FALSE)

# coll.ind <- as.data.table(dbGetQuery(con,"
#   SELECT DISTINCT ON (p.id)
#     r.uid AS uid, CAST(1 AS INTEGER) AS coll
# 	FROM
#     gis_victoria.vic_gda9455_roads_state as r,
#       (SELECT
#         id, geom
#       FROM
#         gis_victoria.vic_gda9455_fauna_wv
#       WHERE
#         species = 'Kangaroo -  Eastern Grey'
#       AND
#         cause = 'hit by vehicle'
#       AND
#         year >= 2013
#       AND
#         year != 2014) AS p
#   WHERE ST_DWithin(p.geom,r.geom,100)
#   ORDER BY p.id, ST_Distance(p.geom,r.geom)
#   ")) #~1 second query
# setkey(coll.ind,uid)
# 
# data1v <- merge(cov.data, coll.ind)
# 
# set.seed(123)
# data0v <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1v)),],"coll"=rep(0,2*nrow(data1v)))
# 
# val.data <- rbind(data1v,data0v)
# val.data <- na.omit(val.data)

# #Calculate natural logarithm of each covariate
# val.data$log.egk <- log(val.data$egk)
# val.data$log.tvol <- log(val.data$tvol)
# val.data$log.tspd <- log(val.data$tspd)
# 
# #Center logged covariates by subtracting means to match covariates used in regression model
# val.data$c.log.egk <- val.data$log.egk - mean(val.data$log.egk)
# val.data$c.log.tvol <- val.data$log.tvol - mean(val.data$log.tvol)
# val.data$c.log.tspd <- val.data$log.tspd - mean(val.data$log.tspd)

# val.pred.glm <- predict(coll.glm, val.data, type="response")  #Make predictions with regression model fit
# 
# roc.val <- roc(val.data$coll, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value