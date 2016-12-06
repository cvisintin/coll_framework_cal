require(maptools)
require(scales)
require(rgdal)
require(raster)
require(gbm)
require(dismo)
require(data.table)
require(ncf)
require(foreach)

# grid.files <- list.files(path='data/grids/vic/envi') #Create vector of filenames
# 
# grid.names <- substring(unlist(strsplit(grid.files,"\\_1000."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names
# 
# vic.rst <- raster("data/grids/vic/VIC_GDA9455_GRID_STATE_1000.tif")
# 
# clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps
# 
# #Read in grids, crop, and multiply with template to create consistent covariate maps
# for (i in 1:length(grid.files)) {
#   temp <- raster(paste0("data/grids/vic/envi/",grid.files[i]))
#   temp <- crop(temp, clip)
#   assign(grid.names[i],temp * vic.rst)
# }
# vars <- stack(c(mget(grid.names),"X"=X,"Y"=Y)) #Combine all maps to single stack

model.data <- read.csv("data/vic_model_data_sdm.csv", header=T, sep=",")

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

sdm.colors = colorRampPalette(c("white","red")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

kang.brt = gbm.step(data = model.data, gbm.x = 4:12, gbm.y = 3, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, bag.fraction = 0.5) #Create boosted regression tree model
save(kang.brt,file="output/vic_brt")
summary(kang.brt)

#Report reduction in deviance on null model (percent of error explained by model)
brt.devexp <- paste(round(((kang.brt[["self.statistics"]][["mean.null"]] - kang.brt[["cv.statistics"]][["deviance.mean"]])/kang.brt[["self.statistics"]][["mean.null"]])*100,2)," +/- ",round((kang.brt[["cv.statistics"]][["deviance.se"]]/kang.brt[["self.statistics"]][["mean.null"]])*100,2),sep="")
brt.devexp

#Report discrimination performance of model in area under receiver operator characteristic curve
brt.roc <- paste(round(kang.brt[["cv.statistics"]][["discrimination.mean"]],2)," +/- ",round(kang.brt[["cv.statistics"]][["discrimination.se"]],2),sep="")
brt.roc

load("data/vic_study_vars")
brt.preds <- predict(vars, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps

writeRaster(brt.preds, filename="/home/casey/Research/Github/coll_framework_cal/output/egk_preds_brt.tif", format="GTiff", overwrite=TRUE, NAflag=-9999, datatype='FLT4S') #Write out prediction map in tif format

#Use system commands to translate and uplaod grid to postgis database
#system("raster2pgsql -d -C -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_cal/output/egk_preds_brt.tif gis_victoria.vic_gda9455_grid_egk_preds_brt | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

#Calculate spatial autocorrelation in model residuals and create dataframe
cor <- correlog(model.data[,1], model.data[,2], resid(kang.brt), increment=1000, resamp=0, latlon=FALSE)
vic.cor.df <- data.frame(x=as.numeric(names(cor$correlation[1:20])), y=cor$correlation[1:20])

save(vic.cor.df,file="output/vic_brt_cor")

########################DWPR######################

p.wt <- rep(1.0e-6, length(model.data$OCC))

p.wt[model.data$OCC == 0] <- 237629/sum(model.data$OCC == 0)

X.des <- model.matrix(~ ELEV + GREEN + LIGHT + MNTEMPWQ + PRECDM + SLOPE + TREEDENS + X + Y, model.data)
kang.dwpr <- glm(model.data$OCC/p.wt ~ X.des-1, family=poisson, weights=p.wt)

kang.dwpr <- glm(model.data$OCC/p.wt ~ ELEV + GREEN + LIGHT + MNTEMPWQ + PRECDM + SLOPE + TREEDENS + X + Y, family=poisson, weights=p.wt, data=model.data)

require(ppmlasso)

form = ~ poly(ELEV,GREEN,LIGHT,MNTEMPWQ,PRECDM,SLOPE,TREEDENS,degree=2,raw=TRUE)
scales = c(.001, .002, .004, .008, .016)

findres(scales, formula = form, sp.xy = model.data[model.data$OCC==1,], env.grid = quad)

# cov$XCOORD <- cov$X
# cov$YCOORD <- cov$Y
# cov$X <- cov$Y <- 0
# 
# X <- vars[["X"]]
# Y <- vars[["Y"]]
# X[na.omit(X)] <- 0
# Y[na.omit(Y)] <- 0

vars.pred <- stack(vars[[1:7]],X,Y)

names(vars.pred) <- names(coef(kang.dwpr))[-1]

dwpr.preds <- predict(vars, kang.dwpr, type="response") #Make predictions with model fit based on covariate values in maps

writeRaster(dwpr.preds, filename="/home/casey/Research/Github/coll_framework_cal/output/egk_preds_dwpr.tif", format="GTiff", overwrite=TRUE, NAflag=-9999, datatype='FLT4S') #Write out prediction map in tif format

plot(dwpr.preds, col=colorRampPalette(c("white","red"))(100)) #Plot prediction map using red to white color scheme

require(spatstat)

cov <- foreach(i = 1:9, .combine=cbind) %do% {
  #assign(names(vars)[i],as(vars[[i]],'vector'))
  df <- data.frame(as(vars[[i]],'vector'))
  colnames(df) <- paste(names(vars)[i])
  df
}

quad <- na.omit(cov)

# X <- unique(quad$X*1000)
# Y <- unique(quad$Y*1000)
ux = sort(unique(quad$X*1000))
uy = sort(unique(quad$Y*1000))
nx = length(ux)
ny = length(uy)
col.ref = match(quad$X*1000, ux)
row.ref = match(quad$Y*1000, uy)
all.vec = rep(NA, max(row.ref)*max(col.ref))
vec.ref = (col.ref - 1)*max(row.ref) + row.ref
all.vec[vec.ref] = 1
vic.mask = matrix(all.vec, max(row.ref), max(col.ref), dimnames = list(uy, ux))
vic.win = as.owin(im(vic.mask, xcol = ux, yrow = uy))
ppp.dat = ppp(model.data$X[model.data$OCC==1]*1000, model.data$Y[model.data$OCC==1]*1000, window = vic.win, check = FALSE)
quads = ppp(x=quad$X*1000, y=quad$Y*1000, window = vic.win)

Q = quadscheme(data = ppp.dat, dummy = quads, method = "grid", ntile = c(nx, ny), npix = c(nx, ny))

X.des = cbind(poly(quad$ELEV, quad$GREEN, quad$LIGHT, quad$MNTEMPWQ, quad$PRECDM, quad$SLOPE, quad$TREEDENS, degree = 2,raw = TRUE))

int.list = list()

for (i in 1:dim(X.des)[2]) {
  all.vec = rep(NA, max(row.ref)*max(col.ref))
  vec.ref = (col.ref - 1)*max(row.ref) + row.ref
  all.vec[vec.ref] = X.des[,i]
  int.list[[i]] = im(matrix(all.vec, max(row.ref), max(col.ref), dimnames = list(uy, ux)), xcol = ux, yrow = uy)
}

names(int.list) = paste("V", 1:dim(X.des)[2], sep = "")
pred.list = int.list

int.form = as.formula(paste("~", paste(names(int.list), collapse = "+")))
ft.int = ppm(Q, trend = as.formula(int.form), covariates = int.list)

pred.int = predict(ft.int, covariates = pred.list, ngrid = c(ny, nx))

plot(pred.int)
plot(leverage(ft.int))
plot(influence(ft.int))

plot(parres(ft.int, "V1"))
plot(parres(ft.int, "V2"))
plot(parres(ft.int, "V3"))
plot(parres(ft.int, "V4"))
plot(parres(ft.int, "V5"))
plot(parres(ft.int, "V6"))
plot(parres(ft.int, "V7"))

sp.xy = unique(data.frame(X=model.data$X[model.data$OCC==1], Y=model.data$Y[model.data$OCC==1]))

#ppm.form = ~ poly(ELEV, GREEN, LIGHT, MNTEMPWQ, PRECDM, SLOPE, TREEDENS, degree = 2,raw = TRUE)
#ppm.fit = ppmlasso(ppm.form, sp.xy = sp.xy, env.grid = quad, sp.scale = 1, criterion = "nlgcv")

victoria <- readShapePoly("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA9455_ADMIN_STATE.shp")

polygon <- data.frame(x=rev(victoria@polygons[[1]]@Polygons[[1]]@coords[,1]/1000),y=rev(victoria@polygons[[1]]@Polygons[[1]]@coords[,2]/1000))
sd <- spatstat::ppp(sp.xy$X*1000, sp.xy$Y*1000, poly=polygon)
minimum.contrast(sd, model = "exponential", method = "g", intens = density(sd), transform = log)
chooseCellwidth(sd, cwinit=1)
Cellwidth <- 1
covar=SpatialPixelsDataFrame(cbind(quad$X*1000,quad$Y*1000),quad[,1:7])
polyolay <- getpolyol(data = sd, pixelcovariates = covar, cellwidth = Cellwidth)
covar@data=guessinterp(covar@data)

kang.graf <- graf(model.data$OCC, model.data[,4:10])

graf.preds <- predict(kang.graf, na.omit(cov[,1:7]), type="response") #Make predictions with model fit based on covariate values in maps

