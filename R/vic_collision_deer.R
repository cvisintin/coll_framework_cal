require(data.table)
require(raster)
require(boot)
require(doMC)
require(fields)
require(maptools)
require(ncf)
require(dplyr)
require(RPostgreSQL)

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

"dev" <- function (model){
  round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2)
}

# drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
# con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
# 
# roads <- as.data.table(dbGetQuery(con,"
# SELECT a.uid, a.length, a.deer, b.tvol, c.tspd
# FROM
# (SELECT r.uid AS uid, ST_Length(r.geom)/1000 AS length, sum((st_length(st_intersection(r.geom,g.geom))/st_length(r.geom)) * (g).val) AS deer
#   FROM gis_victoria.vic_gda9455_roads_state_orig_500 AS r,
#   (SELECT (ST_PixelAsPolygons(rast)).val AS val, (ST_PixelAsPolygons(rast)).geom AS geom
#   FROM gis_victoria.vic_gda9455_grid_deer_preds_brt_500) AS g
#   WHERE ST_Intersects(r.geom,g.geom)
#   GROUP BY r.uid) AS a, gis_victoria.vic_nogeom_roads_volpreds_500 AS b, gis_victoria.vic_nogeom_roads_speedpreds_500 AS c
# WHERE
# a.uid = b.uid
# AND
# a.uid = c.uid
#   "))
# setkey(roads,uid)
# 
# roads$coll <- as.integer(0)
# 
# deercoll <- as.data.table(dbGetQuery(con,"
#                                    SELECT DISTINCT ON (p.id)
#                                    r.uid AS uid, CAST(1 AS INTEGER) AS coll
#                                    FROM
#                                    gis_victoria.vic_gda9455_roads_state_orig_500 as r,
#                                    (SELECT DISTINCT ON (geom)
#                                    id, geom
#                                    FROM
#                                    gis_victoria.vic_gda9455_fauna_deercoll_vicroads) AS p
#                                    WHERE ST_DWithin(p.geom,r.geom, 30)
#                                    ORDER BY p.id, ST_Distance(p.geom,r.geom)
#                                    "))
# setkey(deercoll,uid)
# 
# data <- copy(roads)
# data[deercoll, coll := i.coll]
# data <- na.omit(data)
# 
#write.csv(data, "data/deer_coll_rds_vic.csv", row.names = FALSE)

data <- read.csv("data/deer_coll_rds_vic.csv")

#linMap <- function(x, a, b) approxfun(range(x), c(a, b))(x)

load("output/deer_coll_glm_500")

summary(coll.glm)  #Examine fit of regression model

paste0("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2))  #Report reduction in deviance

coll.preds <- predict(coll.glm, data, type="response") #Predict with offset to get expected collisions on each segment

coll.preds.df <- as.data.table(cbind("uid"=data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/vic_coll_preds_glm_deer.csv", row.names = FALSE)

################################# Validation #################################

val.pred.glm <- predict(coll.glm, data, type="link")  #Make predictions with regression model fit on link scale

summary(glm(data$coll ~ val.pred.glm, family = binomial(link = "cloglog")))  #slope is close to one and significant therefore model is well calibrated to external data

summary(glm(data$coll ~ val.pred.glm, offset=val.pred.glm, family=binomial(link = "cloglog"))) #slope is not significantly different from 1 (difference of slopes close to 0)

roc(data$coll, predict(coll.glm, data, type="response"))  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value

################################# Sensitivity Analysis #################################

n <- 100 #simulations to run

roc_dist <- foreach(i = seq_len(n), .combine = c) %do% {
  data_s <- transform(data, coll = sample(coll))
  roc(data_s$coll, predict(coll.glm, data_s, type="response"))
}

roc_dist_rs <- sapply(roc_dist, FUN = function(x) if(x < 0.5) {1-x} else {x})

plot(density(roc_dist_rs), xlim = c(0.5,1))
abline(v = roc(data$coll, predict(coll.glm, data, type="response")), col="darkred")

################################# Aggregated Validation #################################

# drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
# con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
# 
# dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_deercollrisk_500"), value = coll.preds.df, row.names=FALSE, overwrite=TRUE)
# 
# wv.data <- as.data.table(read.csv("data/WV_deer.csv")) #Load aggregated independent data for validation
# wv.data$RESCUE_SUBURB[wv.data$RESCUE_SUBURB == "WARBURTON EAST"] <- "WARBURTON"
# data.towns <- wv.data[,.N,by="RESCUE_SUBURB"]
# setnames(data.towns,c("towns","ncoll"))
# setkey(data.towns,towns)
# 
# #Predictions to roads in town boundaries
# preds_towns <- as.data.table(dbGetQuery(con,"
#                                           SELECT
#                                           r.uid AS uid, p.locality AS towns, ST_Length(ST_Intersection(r.geom, p.geom))/1000 AS length, r.collrisk AS collrisk
#                                           FROM
#                                           (SELECT
#                                           x.uid AS uid, x.geom AS geom, y.collrisk AS collrisk
#                                           FROM
#                                           gis_victoria.vic_gda9455_roads_state_orig_500 AS x, gis_victoria.vic_nogeom_roads_deercollrisk_500 AS y
#                                           WHERE
#                                           x.uid = y.uid) AS r, gis_victoria.vic_gda9455_admin_localities AS p
#                                           WHERE
#                                           ST_Contains(p.geom, r.geom);
#                                           "))
# setkey(preds_towns,towns)
# 
# range(preds_towns$collrisk)
# range(exp(preds_towns$collrisk))
# 
# write.csv(preds_towns[,sum(exp(collrisk)),by="towns"], "data/collpreds_towns.csv")
# 
# val.data.towns <- merge(preds_towns[,sum(exp(collrisk)),by="towns"], data.towns, by="towns", all.x=TRUE)
# val.data.towns$ncoll[is.na(val.data.towns$ncoll)] <- 0
# colnames(val.data.towns)[2] <- "expcoll"
# 
# data.towns[!val.data.towns,]
# 
# val.data.wv <- na.omit(val.data.towns)
# 
# write.csv(val.data.wv, "data/val_data_towns.csv", row.names = FALSE)

val.data.wv <- read.csv("data/val_data_towns.csv")

summary(glm(formula = ncoll ~ log(expcoll), data = val.data.wv, family = poisson)) #slope is close to one and significant therefore model is well calibrated to external data

dev(glm(formula=ncoll ~ log(expcoll), data=val.data.wv, family=poisson)) #percent of deviance (error in data) explained by the model fit
