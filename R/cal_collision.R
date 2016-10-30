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
        gis_california.cal_nad8310_fauna_cros
      WHERE
        scientific = 'Odocoileus hemionus'
      AND
        odatetime >= '2010-01-01 00:00') AS p
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

# INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 909090, 'sr-org', 7759, '', '"+proj=tmerc +lat_0=0 +lon_0=145 +k=1 +x_0=500000 +y_0=10000000 +ellps=WGS84
# +towgs84=-117.808,-51.536,137.784,0.303,0.446,0.234,-0.29 +units=m +no_defs"')

# UPDATE spatial_ref_sys SET proj4text = '+proj=longlat +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat,null +no_defs' WHERE srid = 909090;

# CREATE TABLE gis_victoria.vic_gda9455_fauna_egkcoll_crashstats(
#   id SERIAL PRIMARY KEY,
#   cs_id VARCHAR(20),
#   year INTEGER,
#   month INTEGER,
#   day INTEGER,
#   hour CHAR(8)
# );
# SELECT AddGeometryColumn ('gis_victoria','vic_gda9455_fauna_egkcoll_crashstats','geom',28355,'POINT',2);
# INSERT INTO gis_victoria.vic_gda9455_fauna_egkcoll_crashstats(cs_id, year, month, day, hour, geom)
# (SELECT
# accident.accident_no AS cs_id,
# extract(year from date(accident.accidentdate)) AS year,
# extract(month from date(accident.accidentdate)) AS month,
# extract(day from date(accident.accidentdate)) AS day,
# replace(accident.accidenttime,'.',':') AS hour,
# ST_Transform(ST_SetSRID(ST_MakePoint(node.long, node.lat),4326),28355) AS geom
# FROM
# crashstats.accident
# INNER JOIN
# crashstats.node
# ON
# accident.accident_no=node.accident_no
# INNER JOIN
# crashstats.subdca
# ON
# accident.accident_no=subdca.accident_no
# WHERE
# subdca.sub_dca_code_desc LIKE '%Kangaroo%'
# AND
# RIGHT(accident.accidentdate,4) >= '2010');
# CREATE INDEX vic_gda9455_fauna_egkcoll_crashstats_geom_idx ON gis_victoria.vic_gda9455_fauna_egkcoll_crashstats USING gist (geom);

val.coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state as r,
      (SELECT DISTINCT ON (geom)
        id, geom
      FROM
        gis_victoria.vic_gda9455_fauna_egkcoll_crashstats
      WHERE
        year <= 2014) AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(val.coll,uid)

val.data1 <- val.coll

val.data <- copy(cov.data)
val.data[val.data1, coll := i.coll]
val.data <- na.omit(val.data)
val.data <- val.data[!duplicated(val.data[,.(x,y)]),]

val.pred.glm <- predict(coll.glm, data, type="link")  #Make predictions with regression model fit

roc.val <- roc(val.data$coll, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value
