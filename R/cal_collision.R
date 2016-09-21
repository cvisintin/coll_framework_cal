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
      (SELECT DISTINCT ON (geom)
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

models1000 <- foreach(i = 1:length(rns)) %do% {
  set.seed(rns[i])
  data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
  model.data <- rbind(data1,data0)
  model.data <- na.omit(model.data)
  coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
  list("coefs"=coef(summary(coll.glm))[, "Estimate"],
       "coefs_se"=coef(summary(coll.glm))[, "Std. Error"],
       "zvalue"=coef(summary(coll.glm))[, "z value"],
       "coefs_prz"=coef(summary(coll.glm))[, "Pr(>|z|)"],
       "devred"=round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),
       "rocvalue"=roc(model.data$coll,coll.glm$fitted.values),
       "data"=model.data,
       "nbg"=length(model.data$coll[model.data$coll==0])
  )
}
save(models1000,file="output/cal_coll_glm_1000")

mean(sapply(models1000, function(x){x[["nbg"]]}))
sd(sapply(models1000, function(x){x[["nbg"]]}))

mean(sapply(models1000, function(x){x[["devred"]]}))
sd(sapply(models1000, function(x){x[["devred"]]}))

mean(sapply(models1000, function(x){x[["rocvalue"]]}))
sd(sapply(models1000, function(x){x[["rocvalue"]]}))

summary1000 <- matrix(NA, nrow=5, ncol=4)

for(i in 1:nrow(summary1000)){
  summary1000[i,1] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["coefs"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs"]][i]})),2), ")")
  summary1000[i,2] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["coefs_se"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs_se"]][i]})),2), ")")
  summary1000[i,3] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["zvalue"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["zvalue"]][i]})),2), ")")
  summary1000[i,4] <- paste0(signif(mean(sapply(models1000, function(x){x[["coefs_prz"]][i]})),3))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs_prz"]][i]})),2), ")")
}
write.csv(summary1000, file = "output/cal_coll_glm_fit.csv", row.names=FALSE)

preds1000 <- matrix(0, nrow=length(cov.data$uid), ncol=length(rns)+1)
preds1000[,1] <- cov.data$uid
for(i in 1:length(rns)){
  set.seed(rns[i])
  data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
  model.data <- rbind(data1,data0)
  model.data <- na.omit(model.data)
  coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
  preds1000[,i+1] <- predict(coll.glm, cov.data, type="response")
}

coll.preds.df <- data.frame("uid"=cov.data$uid,"collrisk"=rowMeans(preds1000[,2:1001], na.rm = FALSE))

#write.csv(coll.preds.df, file = "output/cal_coll_preds_glm.csv", row.names=FALSE)

dbWriteTable(con, c("gis_california", "cal_nogeom_roads_deercollrisk_m"), value = coll.preds.df, row.names=FALSE, overwrite=TRUE)

#########################################################################

# data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
# 
# model.data <- rbind(data1,data0)
# model.data <- na.omit(model.data)
# 
# coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)  #Fit regression model
# 
# summary(coll.glm)  #Examine fit of regression model
# 
# paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance
# 
# write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/cal_coll_coef.csv",row.names=FALSE)
# 
# write.csv(formatC(anova(coll.glm)[2:4,2]/sum(anova(coll.glm)[2:4,2]), format='f',digits=4),"output/cal_coll_anova.csv",row.names=FALSE)
# 
# save(coll.glm,file="output/cal_coll_glm")
# 
# save(model.data,file="output/cal_coll_model_data")
# 
# coll.preds <- predict(coll.glm, cov.data, type="response")
# 
# coll.preds.df <- as.data.table(cbind("uid"=cov.data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
# coll.preds.df <- na.omit(coll.preds.df)
# 
# write.csv(coll.preds.df, file = "output/cal_coll_preds_glm.csv", row.names=FALSE)