require(RPostgreSQL)
require(data.table)
require(raster)
require(boot)
require(doMC)
require(fields)
require(spatstat)
require(maptools)
require(ncf)
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
      gis_victoria.vic_gda9455_roads_state) AS r
  "))
setkey(roads,uid)

tvol.preds <- as.data.table(read.csv("output/vic_tvol_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

tspd.preds <- as.data.table(read.csv("output/vic_tspd_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

sdm.preds <- raster("output/egk_preds_brt.tif")

cov.data$egk <- raster::extract(sdm.preds,cov.data[,.(x,y)])

cov.data$coll <- as.integer(0)

# coll <- as.data.table(dbGetQuery(con,"
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
#         cause = 'hit by vehicle') AS p
#   WHERE ST_DWithin(p.geom,r.geom,100)
#   ORDER BY p.id, ST_Distance(p.geom,r.geom)
#   "))
# setkey(coll,uid)

coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state as r,
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
setkey(coll,uid)

# coll <- as.data.table(dbGetQuery(con,"
#   SELECT DISTINCT ON (p.id)
#     r.uid AS uid, CAST(1 AS INTEGER) AS coll
# 	FROM
#     gis_victoria.vic_gda9455_roads_state as r,
#       (SELECT DISTINCT ON (grid_location)
#         g.id AS id, g.geom AS geom
#       FROM
#         (SELECT id, geom, st_snapToGrid(geom, 5, 5) AS grid_location
#         FROM
#           gis_victoria.vic_gda9455_fauna_wv
#         WHERE
#           species = 'Kangaroo -  Eastern Grey'
#         AND
#           cause = 'hit by vehicle'
#         ORDER BY grid_location) as g) AS p
#   WHERE ST_DWithin(p.geom,r.geom,100)
#   ORDER BY p.id, ST_Distance(p.geom,r.geom)
#   "))
# setkey(coll,uid)

# create table dups as
# select t1.*, st_centroid(st_union(t1.geom, t2.geom)) as cent_geom
# from (SELECT
#       id, geom
#       FROM
#       gis_victoria.vic_gda9455_fauna_wv
#       WHERE
#       species = 'Kangaroo -  Eastern Grey'
#       AND
#       cause = 'hit by vehicle') as t1 join (SELECT
#                                             id, geom
#                                             FROM
#                                             gis_victoria.vic_gda9455_fauna_wv
#                                             WHERE
#                                             species = 'Kangaroo -  Eastern Grey'
#                                             AND
#                                             cause = 'hit by vehicle') as t2 on st_dwithin(t1.geom, t2.geom, 1000)
# where t1.id != t2.id;
# 
# --table of no duplicates
# create table no_dups as select x.*
#   from (SELECT
#         id, geom
#         FROM
#         gis_victoria.vic_gda9455_fauna_wv
#         WHERE
#         species = 'Kangaroo -  Eastern Grey'
#         AND
#         cause = 'hit by vehicle') as x left join dups on x.id = dups.id
# where dups.id is null;
# 
# --insert distinct duplicates
# insert into no_dups
# select distinct on (cent_geom) id, geom
# from dups;
# 
# select * from no_dups
# order by id;

# coll <- as.data.table(dbGetQuery(con,"
# SELECT
#   y.uid AS uid, CAST(1 AS INTEGER) AS coll
# FROM
#   (SELECT DISTINCT ON (p.id)
#     p.geom AS geom, r.uid AS uid
#   FROM
#     gis_victoria.vic_gda9455_roads_state as r,
#     (SELECT
#       id, geom
#     FROM
#       gis_victoria.vic_gda9455_fauna_wv
#     WHERE
#       species = 'Kangaroo -  Eastern Grey'
#     AND
#       cause = 'hit by vehicle') AS p
#   WHERE
#     ST_DWithin(p.geom,r.geom,100)
#   ORDER BY
#     p.id, ST_Distance(p.geom,r.geom)) AS y,
#   gis_victoria.vic_gda9455_admin_state_1kmgrid AS x
# WHERE
#   ST_Intersects(y.geom,x.geom)
# GROUP BY x.id, y.geom, y.uid
#   "))
# setkey(coll,uid)

data1 <- coll

data <- copy(cov.data)
data[data1, coll := i.coll]
data <- na.omit(data)
data <- data[!duplicated(data[,.(x,y)]),]

# vic.rst <- raster("data/grids/vic/VIC_GDA9455_GRID_STATE_1000.tif")
# 
# vic.mat <- as.matrix(vic.rst)
# vic.mat <- vic.mat[nrow(vic.mat):1, ]
# vic.mat[!is.finite(vic.mat)] <- 0
# vic.mat2 <- matrix(FALSE,nrow=563,ncol=822)
# vic.mat2[] <- as.logical(vic.mat)
# 
# vic.win <- owin(xrange=c(-58,764), yrange=c(5661,6224), units=c("kilometer","kilometers"), mask=vic.mat2)
# 
# vic.ppp <- ppp(data[,x]/1000,data[,y]/1000, window=vic.win, marks=data[,coll])
# 
# #K <- Kest(vic.ppp)
# 
# vic.idw <- idw(vic.ppp, power=1)
# rast_idw <- raster(as.SpatialGridDataFrame.im(vic.idw))
# rast_idw <- setExtent(rast_idw, extent(-58, 764, 5661, 6224))
# proj4string(rast_idw) <- CRS("+init=epsg:28355")
# AC <- extract(rast_idw, cbind(data[,x]/1000, data[,y]/1000))
# 
# model.data <- cbind(data,AC)

#s <- surf.ls(2, data[,.("x"=x/1000,"y"=y/1000,"z"=coll)])
#s <- surf.gls(2, expcov, data[,.("x"=x/1000,"y"=y/1000,"z"=coll)], 1, d=1)
#trsurf <- trmat(s, -58, 764, 5661, 6224, 1)

##########Added code for 1000 simulations and model averaging#############
# set.seed(123)
# rns <- sample(1:10000, 1000, replace=F)
# 
# models1000 <- foreach(i = 1:length(rns)) %do% {
#   set.seed(rns[i])
#   data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
#   model.data <- rbind(data1,data0)
#   model.data <- na.omit(model.data)
#   coll.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
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
# save(models1000,file="output/vic_coll_glm_1000")
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
# write.csv(summary1000, file = "output/vic_coll_glm_fit.csv", row.names=FALSE)
# 
# preds1000 <- matrix(0, nrow=length(cov.data$uid), ncol=length(rns)+1)
# preds1000[,1] <- cov.data$uid
# for(i in 1:length(rns)){
#   set.seed(rns[i])
#   data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))
#   model.data <- rbind(data1,data0)
#   model.data <- na.omit(model.data)
#   coll.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = model.data)
#   preds1000[,i+1] <- predict(coll.glm, cov.data, type="response")
# }
# 
# coll.preds.df <- data.frame("uid"=cov.data$uid,"collrisk"=rowMeans(preds1000[,2:1001], na.rm = FALSE))
# 
# #write.csv(coll.preds.df, file = "output/vic_coll_preds_glm.csv", row.names=FALSE)
# 
# dbWriteTable(con, c("gis_victoria", "vic_nogeom_roads_egkcollrisk_m"), value = coll.preds.df, row.names=FALSE, overwrite=TRUE)

#########################################################################

coll.glm <- glm(formula = coll ~ log(egk) + log(tvol) + I(log(tvol)^2) + log(tspd), family=binomial(link = "cloglog"), data = data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

# crds <- as.matrix(data[,.("x"=x/1000,"y"=y/1000)])
# k1 <- knn2nb(knearneigh(crds, 1, longlat=FALSE, RANN=TRUE))
# #k1d <- nbdists(k1, crds, longlat=FALSE)
# #max(unlist(k1d))
# 
# sdist <- 600
# all.dists <- foreach(i = 1:10000, .combine=c) %do% {
#   crd <- data[i,c(x,y)]
#   crds <- as.matrix(data[x<crd[1]+sdist & x>crd[1]-sdist & y<crd[2]+sdist & y>crd[2]-sdist,.(x,y)])
#   unlist(nbdists(knn2nb(knearneigh(crds, 1, longlat=FALSE)), crds, longlat=FALSE))
# }
# 
# system.time(dnb <- dnearneigh(crds, 0, 1000, longlat=FALSE))
# 
# out <- RANN::nn2(crds, k=2, treetype="bd", searchtype="radius", radius=21000)$nn.idx[, -1, drop = FALSE]
# dimnames(out) <- NULL
# res <- list(nn = out, np = nrow(crds), k = 1, dimension = ncol(crds), x = crds)
# class(res) <- "knn"
# attr(res, "call") <- match.call()
# 
# #system.time(lw <- nb2listw(knn2nb(res), zero.policy=TRUE))
# #system.time(mt <- moran.test(resid(coll.glm), lw, zero.policy=TRUE))
# 
# sp.cor <- sp.correlogram(knn2nb(res), resid(coll.glm), order=21, method="I", randomisation=FALSE)
# 
# #cor <- correlog(data[,.(x,y)], resid(coll.glm), nbclass=21, method="Moran")
# #cor <- eco.correlog(resid(coll.glm), data[,.(x,y)], int=1000, smax=20000, nclass=20, method="I")
# #vic.cor.df <- data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20])

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/vic_coll_coef.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm)[2:5,2]/sum(anova(coll.glm)[2:5,2]), format='f',digits=4),"output/vic_coll_anova.csv",row.names=FALSE)

write.csv(varImp(coll.glm, scale=FALSE),"output/vic_coll_varimp.csv",row.names=FALSE)

save(coll.glm,file="output/vic_coll_glm")

save(data,file="output/vic_coll_model_data")

coll.preds <- predict(coll.glm, cov.data, type="response")

coll.preds.df <- as.data.table(cbind("uid"=cov.data$uid,"collrisk"=coll.preds)) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/vic_coll_preds_glm.csv", row.names=FALSE)


######################Stan###################
# require(rstan)
# set.seed(123)
# 
# #N <- nrow(data)
# N <- 10000
# id <- sample(data$uid,10000)
# y <- sample(data$coll,10000)
# x1 <- sample(data$egk,10000)
# x2 <- sample(data$tvol,10000)
# x3 <- sample(data$tspd,10000)
# 
# scode <- "
#   data{
#     int<lower=1> N;
#     int<lower=0,upper=N> id[N];
#     int<lower=0,upper=1> y[N];
#     real x1[N];
#     real x2[N];
#     real x3[N];
#   }
# 
#   transformed data{
#     real x1l[N];
#     real x2l[N];
#     real x2l2[N];
#     real x3l[N];
# 
#     for (n in 1:N) {
#       x1l[n] = log(x1[n]);
#       x2l[n] = log(x2[n]);
#       x2l2[n] = log(x2[n])*log(x2[n]);
#       x3l[n] = log(x3[n]);
#     }
#   }
# 
#   parameters{
#     vector[N] a;
#     vector[4] b;
#     real<lower=0,upper=100> sigma_a;
#     real mu_a;
#   }
# 
#   transformed parameters {
#     vector[N] p;
#     vector[N] lp;
# 
#     for (i in 1:N)
#       lp[i] = a + b[1] * x1l[i] + b[2] * x2l[i] + b[3] * x2l2[i] + b[4] * x3l[i] + a[id[i]];
#       p[i] = inv_cloglog(lp[i])
#   } 
# 
#   model {
#     mu_a ~ normal(0, 1);
#     a ~ normal (mu_a, sigma_a);
#     b ~ normal (0, 100);
#     y ~ bernoulli_logit(p);
#   }
# "
# 
# coll_model_fit <- stan(model_code = scode, iter = 500, chains = 3, cores = 3, seed=123, verbose = FALSE, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))
# precis(coll_model_fit)

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

save(coll.resid,file="output/vic_coll_resid")

coll.resid.norm <- qnorm(coll.resid)

spc <- cbind(data$x, data$y, coll.resid.norm)

vic.cor.df.1000 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=1000, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(vic.cor.df.1000,file="output/vic_coll_cor_1000")

vic.cor.df.250 <- foreach(i = 1:20, .combine=rbind) %dopar% {
  set.seed(123+i)
  spc.r <- spc[sample(nrow(spc), 5000),]
  cor <- correlog(spc.r[,1], spc.r[,2], spc.r[,3], increment=250, resamp=0, latlon=FALSE)
  data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20], n=i)
}
save(vic.cor.df.250,file="output/vic_coll_cor_250")

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

val.pred.glm <- predict(coll.glm, data, type="response")  #Make predictions with regression model fit

roc.val <- roc(val.data$coll, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value
