require(RPostgreSQL)
require(data.table)
require(raster)
require(boot)
require(doMC)
require(fields)
#require(spatstat)
require(maptools)
require(ncf)
#require(caret)

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

# roads <- as.data.table(dbGetQuery(con,"
#   SELECT
#     r.uid AS uid, r.length AS length, ST_X(r.geom) AS x, ST_Y(r.geom) AS y
#   FROM
# 	  (SELECT
#       uid, ST_Length(geom)/1000 AS length, ST_LineInterpolatePoint(geom, 0.5) AS geom
# 		FROM
#       gis_victoria.vic_gda9455_roads_state_orig_500) AS r
#   "))
# setkey(roads,uid)

roads <- as.data.table(dbGetQuery(con,"
SELECT r.uid AS uid, ST_Length(r.geom)/1000 AS length, sum((st_length(st_intersection(r.geom,g.geom))/st_length(r.geom)) * (g).val) AS egk
  FROM gis_victoria.vic_gda9455_roads_state_orig_500 AS r, 
  (SELECT (ST_PixelAsPolygons(rast)).val AS val, (ST_PixelAsPolygons(rast)).geom AS geom
  FROM gis_victoria.vic_gda9455_grid_egk_preds_brt_500) AS g
  WHERE ST_Intersects(r.geom,g.geom)
  GROUP BY r.uid
  "))
setkey(roads,uid)


tvol.preds <- as.data.table(read.csv("output/vic_tvol_preds_rf_500.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

tspd.preds <- as.data.table(read.csv("output/vic_tspd_preds_rf_500.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

#sdm.preds <- raster("output/egk_preds_brt.tif")

#cov.data$egk <- raster::extract(sdm.preds,cov.data[,.(x,y)])

cov.data$coll <- as.integer(0)

coll_a <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state_orig_500 as r,
      (SELECT DISTINCT ON (geom)
        id, geom
      FROM
        gis_victoria.vic_gda9455_fauna_wv
      WHERE
        species = 'Kangaroo -  Eastern Grey'
      AND
        cause = 'hit by vehicle') AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(coll_a,uid)

coll_b <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state_orig_500 as r,
      (SELECT DISTINCT ON (geom)
        id, geom
      FROM
        gis_victoria.vic_gda9455_fauna_wv_2015_egkcoll
      WHERE
        (year >= 2014 AND month >= 6 AND day >2)) AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(coll_b,uid)

data1 <- rbind(coll_a,coll_b)

data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
#data <- data[!duplicated(data[,.(x,y)]),]

coll.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), offset=log(length*6), family=binomial(link = "cloglog"), data = data)  #Fit regression model, offset accounts for road length and years of data

summary(coll.glm)  #Examine fit of regression model

paste0("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2))  #Report reduction in deviance

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/vic_coll_coef_500.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm)[2:5,2]/sum(anova(coll.glm)[2:5,2]), format='f',digits=4),"output/vic_coll_anova_500.csv",row.names=FALSE)

write.csv(varImp(coll.glm, scale=FALSE),"output/vic_coll_varimp_500.csv",row.names=FALSE)

save(coll.glm,file="output/vic_coll_glm_500")

save(data,file="output/vic_coll_model_data_500")

coll.preds <- predict(coll.glm, cov.data, type="response") #Predict with offset to get expected collisions on each segment per six years

range(na.omit(coll.preds/(cov.data$length*6))) #expected collisions per kilometer per year

sum(na.omit(coll.preds))/6 #total expected collisions per year

coll.preds.df <- as.data.table(cbind("uid"=cov.data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/vic_coll_preds_glm_500.csv", row.names=FALSE)

dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_500"), value = coll.preds.df, row.names=FALSE, overwrite=TRUE)

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
) ###753 second runtime

save(coll.resid,file="output/vic_coll_resid_500")

coll.resid.norm <- qnorm(coll.resid)

spc <- cbind(data$x, data$y, coll.resid.norm)

vic.cor.df.500 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=500, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(vic.cor.df.500,file="output/vic_coll_cor_500")

vic.cor.df.125 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=125, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(vic.cor.df.125,file="output/vic_coll_cor_125")

################################# Validation #################################

val.coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state_orig_500 as r,
      (SELECT DISTINCT ON (geom)
        id, geom
      FROM
        gis_victoria.vic_gda9455_fauna_egkcoll_crashstats) AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(val.coll,uid)

val.data1 <- val.coll

val.data <- copy(cov.data)
val.data[val.data1, coll := i.coll]
val.data <- na.omit(val.data)
#val.data <- val.data[!duplicated(val.data[,.(x,y)]),]

val.pred.glm <- predict(coll.glm, val.data, type="link")  #Make predictions with regression model fit on link scale

summary(glm(val.data$coll ~ val.pred.glm, family = binomial(link = "cloglog")))  #slope is close to one therefore model is well calibrated to external data after accounting for multiplicative differences

exp(-1.90119) #collisions are more rare in validation set

summary(glm(val.data$coll~val.pred.glm, offset=val.pred.glm, family=binomial(link = "cloglog"))) #slope is not significantly different from 1 (difference of slopes = 0)

roc(val.data$coll, predict(coll.glm, val.data, type="response"))  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value

###################### Get expected number of collisions for the top twenty road segments ###############
top.segments <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid, r.road_name AS name, p.collrisk/((ST_Length(r.geom)/1000)*6) AS collrisk, ST_AsText(ST_LineInterpolatePoint(ST_LineMerge(r.geom),0.5)) AS xy_coordinates
  FROM
	gis_victoria.vic_gda9455_roads_state_orig_500 AS r,
	gis_victoria.vic_nogeom_roads_egkcollrisk_500 AS p
  WHERE
	r.uid = p.uid
  ORDER BY collrisk DESC
  LIMIT 20
  "))
top.segments$xy_coordinates <- gsub("POINT\\(", "", top.segments$xy_coordinates)
top.segments$xy_coordinates <- gsub("\\)", "", top.segments$xy_coordinates)
top.segments$xy_coordinates <- gsub(" ", ", ", top.segments$xy_coordinates)

top.segments$name <- paste0(toupper(substr(top.segments$name, 1, 1)), tolower(substring(top.segments$name, 2)))

write.csv(top.segments, file = "output/vic_coll_segments.csv", row.names=FALSE)


# SELECT
# r.uid, r.road_name AS name, p.collrisk/((ST_Length(r.geom)/1000)*6) AS collrisk, ST_AsText(ST_LineInterpolatePoint(ST_LineMerge(r.geom),0.5)) AS xy_coordinates, r.geom
# FROM
# gis_victoria.vic_gda9455_roads_state AS r,
# gis_victoria.vic_nogeom_roads_egkcollrisk AS p,
# gis_victoria.vic_gda9455_admin_sla AS s
# WHERE
# r.uid = p.uid
# AND
# ST_Intersects(s.geom, r.geom)
# AND
# s.sla_name11 LIKE 'Ballarat%'
# ORDER BY collrisk DESC