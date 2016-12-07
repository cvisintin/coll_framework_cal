require(RPostgreSQL)
require(data.table)
require(raster)
require(boot)
require(doMC)
require(fields)
require(caret)

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

cov.data$coll <- as.integer(0)

coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_california.cal_nad8310_roads_study as r,
      (SELECT DISTINCT ON (geom)
        id, geom
      FROM
        gis_california.cal_nad8310_fauna_cros_deer
      WHERE
        confidence != 'Best Guess'
      AND
        odatetime >= '2006-06-01 00:00') AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(coll,uid)

data1 <- coll

data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
data <- data[!duplicated(data[,.(x,y)]),]

# ##########Added code for 1000 simulations and model averaging#############
# set.seed(123)
# rns <- sample(1:10000, 1000, replace=F)
# 
# models1000 <- foreach(i = 1:length(rns)) %do% {
#   set.seed(rns[i])
#   data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
#   model.data <- rbind(data1,data0)
#   model.data <- na.omit(model.data)
#   coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
#   list("coefs"=coef(summary(coll.glm))[, "Estimate"],
#        "coefs_se"=coef(summary(coll.glm))[, "Std. Error"],
#        "zvalue"=coef(summary(coll.glm))[, "z value"],
#        "coefs_prz"=coef(summary(coll.glm))[, "Pr(>|z|)"],
#        "devred"=round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),
#        "rocvalue"=roc(model.data$coll,coll.glm$fitted.values),
#        "data"=model.data,
#        "nbg"=length(model.data$coll[model.data$coll==0])
#   )
# }
# save(models1000,file="output/cal_coll_glm_1000")
# 
# mean(sapply(models1000, function(x){x[["nbg"]]}))
# sd(sapply(models1000, function(x){x[["nbg"]]}))
# 
# mean(sapply(models1000, function(x){x[["devred"]]}))
# sd(sapply(models1000, function(x){x[["devred"]]}))
# 
# mean(sapply(models1000, function(x){x[["rocvalue"]]}))
# sd(sapply(models1000, function(x){x[["rocvalue"]]}))
# 
# summary1000 <- matrix(NA, nrow=5, ncol=4)
# 
# for(i in 1:nrow(summary1000)){
#   summary1000[i,1] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["coefs"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs"]][i]})),2), ")")
#   summary1000[i,2] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["coefs_se"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs_se"]][i]})),2), ")")
#   summary1000[i,3] <- paste0(formatC(signif(mean(sapply(models1000, function(x){x[["zvalue"]][i]})),digits=3), digits=3, format="fg", flag="#"))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["zvalue"]][i]})),2), ")")
#   summary1000[i,4] <- paste0(signif(mean(sapply(models1000, function(x){x[["coefs_prz"]][i]})),3))#, " (s.d. ", round(sd(sapply(models1000, function(x){x[["coefs_prz"]][i]})),2), ")")
# }
# write.csv(summary1000, file = "output/cal_coll_glm_fit.csv", row.names=FALSE)
# 
# preds1000 <- matrix(0, nrow=length(cov.data$uid), ncol=length(rns)+1)
# preds1000[,1] <- cov.data$uid
# for(i in 1:length(rns)){
#   set.seed(rns[i])
#   data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
#   model.data <- rbind(data1,data0)
#   model.data <- na.omit(model.data)
#   coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
#   preds1000[,i+1] <- predict(coll.glm, cov.data, type="response")
# }
# 
# coll.preds.df <- data.frame("uid"=cov.data$uid,"collrisk"=rowMeans(preds1000[,2:1001], na.rm = FALSE))
# 
# #write.csv(coll.preds.df, file = "output/cal_coll_preds_glm.csv", row.names=FALSE)
# 
# dbWriteTable(con, c("gis_california", "cal_nogeom_roads_deercollrisk_m"), value = coll.preds.df, row.names=FALSE, overwrite=TRUE)
# 
#########################################################################

coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/cal_coll_coef.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm)[2:5,2]/sum(anova(coll.glm)[2:5,2]), format='f',digits=4),"output/cal_coll_anova.csv",row.names=FALSE)

write.csv(varImp(coll.glm, scale=FALSE),"output/cal_coll_varimp.csv",row.names=FALSE)

save(coll.glm,file="output/cal_coll_glm")

save(data,file="output/cal_coll_model_data")

coll.preds <- predict(coll.glm, cov.data, type="response")

coll.preds.df <- as.data.table(cbind("uid"=cov.data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/cal_coll_preds_glm.csv", row.names=FALSE)


prob <- predict(coll.glm, type = 'response')
n <- 5000

registerDoMC(cores=detectCores()-1)

system.time(
coll.resid <- foreach(i = 1:nrow(data), .combine=c) %dopar% {
  simulations.c = c()
  while (length(simulations.c) < 10000 &
         all(simulations.c != coll.glm$y[i])) {
    set.seed(123+i)
    simulations.c = c(simulations.c, rbinom(n, 1, prob[i]))
  }
  if (!any(simulations.c == coll.glm$y[i]))
    warning(sprintf('datapoint %i had no valid samples', i))
  
  # add jitter
  set.seed(123+i)
  fuzzy.y <- coll.glm$y[i] + runif(1, -0.5, 0.5)
  set.seed(123+i)
  fuzzy.simulations <- simulations.c + runif(n, -0.5, 0.5)
  
  # make sure ecdf doesn't go to 1 or 0
  sim.limits <- range(sort(unique(fuzzy.simulations))[-c(1,length(unique(fuzzy.simulations)))])
  fuzzy.y <- pmin(pmax(fuzzy.y, sim.limits[1]), sim.limits[2])
  
  ecdf(fuzzy.simulations)(fuzzy.y)
}
) ###644 second runtime

save(coll.resid,file="output/cal_coll_resid")

coll.resid.norm <- qnorm(coll.resid)

spc <- cbind(data$x, data$y, coll.resid.norm)

cal.cor.df.1000 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=1000, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(cal.cor.df.1000,file="output/vic_coll_cor_1000")

cal.cor.df.250 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=250, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(cal.cor.df.250,file="output/vic_coll_cor_250")

#################################Validation#################################

val.coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
  FROM
    gis_california.cal_nad8310_roads_study as r,
    (SELECT DISTINCT ON (geom)
      id, ST_Transform(geom,3157) AS geom
    FROM
      gis_california.cal_wgs86ll_fauna_chp
    WHERE
		  species = 'Mule (or Black tailed) Deer'
    AND
      animal_out <> 'Alive / No Injury') AS p
    WHERE
      ST_DWithin(p.geom,r.geom,100)
    ORDER BY
      p.id, ST_Distance(p.geom,r.geom)
"))
setkey(val.coll,uid)

val.data1 <- val.coll

val.data <- copy(cov.data)
val.data[val.data1, coll := i.coll]
val.data <- na.omit(val.data)
val.data <- val.data[!duplicated(val.data[,.(x,y)]),]

val.pred.glm <- predict(coll.glm, val.data, type="link")  #Make predictions with regression model fit on link scale

#summary(glm(val.data$coll~1, family=binomial(link = "cloglog")))
#summary(glm(val.data$coll~1, offset=val.pred.glm, family=binomial(link = "cloglog")))

summary(glm(val.data$coll ~ val.pred.glm, family = binomial(link = "cloglog")))  #slope is close to ine therefore model is well calibrated to external data after accounting for multiplicative differences

exp(0.34639) #collisions are more abundant in validation set

summary(glm(val.data$coll~val.pred.glm, offset=val.pred.glm, family=binomial(link = "cloglog"))) #slope is not significantly different from 1 (difference of slopes = 0)

#p <- predict(glm(val.data$coll~1, offset=val.pred.glm, family=binomial(link = "cloglog")), type="response")
#p2 <-  predict(coll.glm, val.data, type="response")
#p3 <- predict(glm(val.data$coll~val.pred.glm, family=binomial(link = "cloglog")), type="response")
#roc.val <- roc(val.data$coll, predict(coll.glm, val.data, type="response"))  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value

#0.95746    0.05074  18.870   <2e-16 ***

#require(rms)
#val.prob(p, val.data$coll, logistic.cal=FALSE)

#glm(val.data$coll ~ qlogis(p), family = binomial)

#require(survival)
#coxph(Surv(rep(0,nrow(data)),rep(1,nrow(data)),data$coll)~log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), data=data)
