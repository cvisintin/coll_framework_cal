library(rstan)
library(rethinking)
library(R2jags)
library(dplyr)
library(data.table)
library(RPostgreSQL)

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

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

roads <- as.data.table(dbGetQuery(con,"
SELECT r.uid AS uid, ST_X(ST_LineInterpolatePoint(r.geom, 0.5)) AS X, ST_Y(ST_LineInterpolatePoint(r.geom, 0.5)) AS Y, ST_Length(r.geom)/1000 AS length, sum((st_length(st_intersection(r.geom,g.geom))/st_length(r.geom)) * (g).val) AS deer
  FROM gis_california.cal_nad8310_roads_study_500 AS r, 
  (SELECT (ST_PixelAsPolygons(rast)).val AS val, (ST_PixelAsPolygons(rast)).geom AS geom
  FROM gis_california.cal_nad8310_grid_deer_preds_brt_500) AS g
  WHERE ST_Intersects(r.geom,g.geom)
  GROUP BY r.uid
  "))
setkey(roads,uid)

tvol.preds <- as.data.table(read.csv("output/cal_tvol_preds_rf_500.csv"))

tspd.preds <- as.data.table(read.csv("output/cal_tspd_preds_rf_500.csv"))

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

cov.data$coll <- as.integer(0)

coll <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, CAST(COUNT(p.geom) AS INTEGER) AS coll
	FROM
    gis_california.cal_nad8310_roads_study_500 as r,
      (SELECT
        id, geom
      FROM
        gis_california.cal_nad8310_fauna_cros_deer
      WHERE
        confidence != 'Best Guess'
      AND
        odatetime >= '2006-06-01 00:00') AS p
  WHERE ST_DWithin(p.geom, r.geom, 10)
  GROUP BY r.uid
  "))
setkey(coll,uid)

data1 <- coll

model.data <- copy(cov.data)
model.data[data1, coll := i.coll]
model.data <- na.omit(model.data)

model.data$tspd <- model.data$tspd * 1.60934

cor(model.data)

###########GLM Model############
coll.glm <- glm(formula = coll ~ log(deer) + log(tvol) + I(log(tvol)^2) + log(tspd), offset=log(length*10), family=poisson, data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

###########JAGS Model############
ndata <- nrow(model.data)
ncoll <- model.data$coll
deer <- log(model.data$deer)
tvol <- log(model.data$tvol)
tvol2 <- log(model.data$tvol)^2
tspd <- log(model.data$tspd)
xcoord <- model.data$X/1000000
ycoord <- model.data$Y/1000000
length <- log(model.data$length * 10)

model <- function() {
  for (i in 1:ndata)
  {
    ncoll[i] ~ dpois(ncoll[i])
    log(ncoll[i]) <- a + b[1] * deer[i] + b[2] * tvol[i] + b[3] * tvol2[i] + b[4] * tspd[i] + length[i]
  }
  
  # description of the assumed distribution of the coefficients:
  a ~ dnorm(0, 1)
  b[1] ~ dnorm(0, 1)
  b[2] ~ dnorm(0, 1)
  b[3] ~ dnorm(0, 1)
  b[4] ~ dnorm(0, 1)
  
}

jags.data <- list("ndata", "ncoll", "deer", "tvol", "tvol2", "tspd", "length")

inits <- function() list(a = 0.5, b = c(0,0,0,0))

parameters <- c("a", "b")

out <- jags.parallel(data = jags.data, inits = inits, parameters.to.save = parameters, model.file = model, n.chains=3, n.iter=1000)

traceplot(out)

###########STAN Model############

scode <- "
  data{
    int<lower=1> ndata;
    vector[ndata] ncoll;
    vector[ndata] deer;
    vector[ndata] tvol;
    vector[ndata] tvol2;
    vector[ndata] tspd;
    vector[ndata] length;
  }
  parameters{
    real a;
    real b1;
    real b2;
    real b3;
    real b4;
  }
  model{
    b4 ~ normal( 0 , 1 );
    b3 ~ normal( 0 , 1 );
    b2 ~ normal( 0 , 1 );
    b1 ~ normal( 0 , 1 );
    a ~ normal( 0 , 1 );
    
    ncoll ~ poisson_log(a + b1 * deer + b2 * tvol + b3 * tvol2 + b4 * tspd + length);
  }
  generated quantities{
    int y_rep[ndata]; 
    for (n in 1:ndata) 
      y_rep[n] = poisson_log_rng(a + b1 * deer[n] + b2 * tvol[n] + b3 * tvol2[n] + b4 * tspd[n] + length[n]);
  }
"
coll_model_fit <- stan(model_code = scode,
                       init = function() list(a = 0.5, b1 = 0, b2 = 0, b3 = 0, b4 = 0),
                       iter = 200,
                       chains = 3,
                       cores = 3,
                       seed=123)
precis(coll_model_fit)
traceplot(As.mcmc.list(coll_model_fit,c("a", "b1", "b2", "b3", "b4")))

set.seed(123) 
coll.model.map.null <- map(
  alist(
    y ~ dbinom(1,p), 
    logit(p) <- a,
    a ~ dnorm(0,1)
  ),
  #start=list(a=1),
  data=list(y=model.data$COLL)
)
precis(coll.model.map.null)


set.seed(123) 
coll.model.map <- map(
  alist(
    y ~ dbinom(1,p), 
    p <- 1 - exp(-exp(a + b1*log(x1) + b2*log(x2) + b3*log(x3))),
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1)
  ),
  start=list(a=0.5,b1=0,b2=0,b3=0),
  data=list(y=model.data$COLL,x1=model.data$EGK,x2=model.data$TVOL,x3=model.data$TSPD)
)
precis(coll.model.map)

set.seed(123) 
coll.model.map2 <- map(
  alist(
    y ~ dbinom(1,p), 
    logit(p) <- a + b1*x1 + b2*x2 + b3*x3,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data$COLL,x1=model.data$log.EGK,x2=model.data$log.TVOL,x3=model.data$log.TSPD)
)
precis(coll.model.map2)




coll.model.stan <- map2stan(
  alist(
    y ~ dbinom(1, p), 
    p <- 1 - exp(-exp(a + b1*log(x1) + b2*log(x2) + b3*log(x3))),
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1)
  ),
  start=list(a=0.5,b1=0,b2=0,b3=0),
  data=list(y=model.data$COLL,x1=model.data$EGK,x2=model.data$TVOL,x3=model.data$TSPD),
  iter=5000,
  warmup=1000,
  chains=3,
  cores=3
)
precis(coll.model.stan)


dmat <- as.matrix(dist(cbind(model.data$X,model.data$Y))/1000000)
model.data$ID <- 1:nrow(model.data)

coll.model.stan2 <- map2stan(
  alist(
    y ~ dbinom(1,p),
    p <- 1 - exp(-exp(a + g[id] + b1*x1 + b2*x2 + b3*x3)),
    g[id] ~ GPL2(dmat, etasq, rhosq, 0.01),
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    etasq ~ dcauchy(0,1),
    rhosq ~ dcauchy(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data$COLL,x1=model.data$log.EGK,x2=model.data$log.TVOL,x3=model.data$log.TSPD,id=model.data$ID,dmat=dmat),
  iter=10,
  warmup=2,
  chains=1,
  cores=1
)
precis(coll.model.stan2)

stancode(coll.model.stan2)

coll.model.stan3 <- map2stan(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl1*x2 + bl2*x3,
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl1 ~ dnorm(0,1),
    bl2 ~dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data$COLL,x1=model.data$c.log.EGK,x2=model.data$c.log.TVOL,x3=model.data$c.log.TSPD),
  iter=5000,
  warmup=1000,
  chains=3,
  cores=3
)
precis(coll.model.stan3)


compare(coll.model.stan, coll.model.stan2)

