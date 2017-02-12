require(maptools)
require(scales)
require(rgdal)
require(raster)
require(RPostgreSQL)
require(fields)
require(sp)
require(data.table)
require(dismo)

thin.algorithm <- function (rec.df.orig, thin.par, reps) 
{
  reduced.rec.dfs <- list()
  for (Rep in 1:reps) {
    rec.df <- rec.df.orig
    DistMat <- rdist(x1 = rec.df)
    diag(DistMat) <- NA
    while (min(DistMat, na.rm = TRUE) < thin.par & nrow(rec.df) > 
           1) {
      CloseRecs <- which(DistMat < thin.par, arr.ind = TRUE)[, 
                                                             1]
      RemoveRec <- as.numeric(names(which(table(CloseRecs) == 
                                            max(table(CloseRecs)))))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      rec.df <- rec.df[-RemoveRec, ]
      DistMat <- DistMat[-RemoveRec, -RemoveRec]
      if (length(DistMat) == 1) {
        break
      }
    }
    colnames(rec.df) <- c("x", "y")
    reduced.rec.dfs[[Rep]] <- rec.df
  }
  locs.thinned <- reduced.rec.dfs
  y.x.thin.count <- unlist(lapply(locs.thinned, nrow))
  max.thin.recs <- max(y.x.thin.count)
  df.temp <- locs.thinned[[which(y.x.thin.count == max.thin.recs)[1]]]
  colnames(df.temp) <- colnames(rec.df.orig)
  
  return(df.temp)
}

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

grid.files <- list.files(path='data/grids/vic/envi') #Create vector of filenames

grid.names <- substring(unlist(strsplit(grid.files,"\\_1000."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

vic.rst <- raster("data/grids/vic/VIC_GDA9455_GRID_STATE_1000.tif")

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

X <- Y <- vic.rst
Y[] <- yFromCell(vic.rst, 1:ncell(vic.rst))/1000000
Y <- crop(Y,clip) * vic.rst
X[] <- xFromCell(vic.rst, 1:ncell(vic.rst))/1000000
X <- crop(X,clip) * vic.rst

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/vic/envi/",grid.files[i]))
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * vic.rst)
}
vars <- stack(c(mget(grid.names),"X"=X,"Y"=Y)) #Combine all maps to single stack
save(vars,file="data/vic_study_vars")

data0 <- read.csv("data/vic_bg_data_pts.csv")

data1 <- dbGetQuery(con,paste0("
      SELECT DISTINCT ON (pts.geom)
        ST_X(pts.geom) AS X, ST_Y(pts.geom) AS Y
      FROM
        gis_victoria.vic_gda9455_fauna_vba AS pts, gis_victoria.vic_gda9455_admin_state AS poly
      WHERE
        ST_contains(poly.geom, pts.geom)
      AND
        pts.start_year >= '2000'
      AND
        sci_name = 'Macropus giganteus';
  "))

colnames(data1) <- toupper(colnames(data1))

data1.1 <- thin.algorithm(data1, 1000, 50)
data1.1 <- as.data.table(cbind(data1.1,"OCC"=1))

##############################Grouping for cross-validation folds################################
k <- kmeans(matrix(c(data1$X,data1$Y), ncol=2), 10)

cen <- aggregate(matrix(c(data1$X,data1$Y), ncol=2),by=list(k$cluster),FUN=mean)
cen.sp <- SpatialPointsDataFrame(as.matrix(cen[,2:3]), data.frame("FOLD"=cen[,1]), proj4string=CRS("+init=epsg:28355"))

v <- voronoi(cen.sp, clip)
xy0 <- SpatialPointsDataFrame(matrix(c(read.csv("data/vic_bg_data_pts.csv")$X,read.csv("data/vic_bg_data_pts.csv")$Y), ncol=2), data.frame(ID=seq(1:nrow(data0))), proj4string=CRS("+init=epsg:28355"))

data1.1 <- as.data.table(cbind("X"=data1$X, "Y"=data1$Y, "OCC"=1, "FOLD"=k$cluster))
data0 <- as.data.table(cbind(read.csv("data/vic_bg_data_pts.csv"), over(xy0 , v)))

egk.data <- rbind(data1.1,data0)
plot(egk.data$X,egk.data$Y, col=egk.data$FOLD)


xy <- SpatialPointsDataFrame(matrix(c(data1$X,data1$Y), ncol=2), data.frame(ID=seq(1:nrow(data1))), proj4string=CRS("+init=epsg:28355"))

xy0 <- SpatialPointsDataFrame(matrix(c(data0$X,data0$Y), ncol=2), data.frame(ID=seq(1:nrow(data0))), proj4string=CRS("+init=epsg:28355"))

hc <- hclust(dist(data.frame(rownames=rownames(xy@data), x=coordinates(xy)[,1], y=coordinates(xy)[,2])), method="complete")

#plot(hc, hang=-1)

hc.d <- cutree(hc, h=200000)

xy@data <- data.frame(xy@data, fold=hc.d)

data1.1 <- as.data.table(cbind(xy@coords, "OCC"=1, xy@data$fold))
colnames(data1.1) <- c("X", "Y", "OCC", "ID")

xy.dt <- as.data.table(cbind(xy@data,xy@coords))

cen.points <- xy.dt[, .(x=mean(coords.x1),y=mean(coords.x2)), by=fold]

xy.cen <- SpatialPointsDataFrame(matrix(c(cen.points$x,cen.points$y), ncol=2), data.frame(ID=cen.points$fold), proj4string=CRS("+init=epsg:28355"))

v <- voronoi(xy.cen, clip)

#plot(v, col=factor(v@data$ID))

data0 <- as.data.table(cbind(data0, over(xy0 , v)))

egk.data <- rbind(data1.1,data0)

plot(egk.data$X,egk.data$Y, col=egk.data$ID)

##########################################################################################

data1.1 <- as.data.table(cbind(data1,"OCC"=1))

egk.data <- rbind(data1.1,data0)
colnames(egk.data)[1:2] <- c("XCOORD","YCOORD")

#egk.data <- rbind(data1,data0)

#Sample covariate grid values at all egk coordinates
samples.df <- extract(vars,egk.data[,.(XCOORD,YCOORD)])

#Build modelling dataset
final.data <- cbind(egk.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.csv(final.data, "data/vic_model_data_sdm.csv", row.names=FALSE)
