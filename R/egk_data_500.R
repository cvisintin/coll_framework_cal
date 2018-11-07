require(maptools)
require(scales)
require(rgdal)
require(raster)
require(RPostgreSQL)
require(fields)
require(sp)
require(data.table)
require(dismo)
require(foreach)
require(doMC)

# thin.algorithm <- function (rec.df.orig, thin.par, reps) # function slightly adapted from https://github.com/cran/spThin/blob/master/R/thin.algorithm.R
# {
#   reduced.rec.dfs <- list()
#   for (Rep in 1:reps) {
#     rec.df <- rec.df.orig
#     DistMat <- rdist(x1 = rec.df)
#     diag(DistMat) <- NA
#     while (min(DistMat, na.rm = TRUE) < thin.par & nrow(rec.df) > 
#            1) {
#       CloseRecs <- which(DistMat < thin.par, arr.ind = TRUE)[, 
#                                                              1]
#       RemoveRec <- as.numeric(names(which(table(CloseRecs) == 
#                                             max(table(CloseRecs)))))
#       if (length(RemoveRec) > 1) {
#         RemoveRec <- sample(RemoveRec, 1)
#       }
#       rec.df <- rec.df[-RemoveRec, ]
#       DistMat <- DistMat[-RemoveRec, -RemoveRec]
#       if (length(DistMat) == 1) {
#         break
#       }
#     }
#     colnames(rec.df) <- c("x", "y")
#     reduced.rec.dfs[[Rep]] <- rec.df
#   }
#   locs.thinned <- reduced.rec.dfs
#   y.x.thin.count <- unlist(lapply(locs.thinned, nrow))
#   max.thin.recs <- max(y.x.thin.count)
#   df.temp <- locs.thinned[[which(y.x.thin.count == max.thin.recs)[1]]]
#   colnames(df.temp) <- colnames(rec.df.orig)
#   
#   return(df.temp)
# }

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

grid.files <- list.files(path='data/grids/vic/envi/500') #Create vector of filenames

grid.names <- substring(unlist(strsplit(grid.files,"\\_500."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

vic.rst <- raster("data/grids/VIC_GDA9455_GRID_STATE_500.tif")

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

X <- Y <- vic.rst.study
Y[] <- yFromCell(vic.rst.study, 1:ncell(vic.rst.study))/mean(extent(vic.rst.study)[3:4])
Y <- crop(Y,clip.study) * vic.rst.study
X[] <- xFromCell(vic.rst.study, 1:ncell(vic.rst.study))/mean(extent(vic.rst.study)[1:2])
X <- crop(X,clip.study) * vic.rst.study

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/vic/envi/500/",grid.files[i]))
  #temp <- crop(temp, clip.state)
  #assign(grid.names[i],temp * vic.rst.state)
  temp <- crop(temp, clip.study)
  assign(grid.names[i],temp * vic.rst.study)
}
vars <- stack(c(mget(grid.names),"X"=X,"Y"=Y)) #Combine all maps to single stack
save(vars,file="data/vic_study_vars_500")

corLocal(MNTEMPWQ, PRECDM, method='spearman')

data1 <- dbGetQuery(con,paste0("
      SELECT DISTINCT ON (pts.geom)
        ST_X(pts.geom) AS X, ST_Y(pts.geom) AS Y
      FROM
        gis_victoria.vic_gda9455_fauna_vba_2018 AS pts, gis_victoria.vic_gda9455_admin_state AS poly
      WHERE
        ST_contains(poly.geom, pts.geom)
      AND
        pts.start_year >= '2006'
      AND
        pts.start_year <= '2016'
      AND
        sci_name = 'Macropus giganteus';
  "))

colnames(data1) <- toupper(colnames(data1))

#data1.1 <- thin.algorithm(data1, 500, 50)
data1.1 <- gridSample(data1, X, n=1)

data1.1 <- as.data.table(cbind(data1.1,"OCC"=1))

data0 <- read.csv("data/vic_bg_data_pts.csv")

egk.data <- rbind(data1.1,data0)
colnames(egk.data)[1:2] <- c("XCOORD","YCOORD")

#Sample covariate grid values at all egk coordinates
samples.df <- extract(vars,egk.data[,.(XCOORD,YCOORD)])

#Build modelling dataset
final.data <- cbind(egk.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.csv(final.data, "data/vic_model_data_sdm2.csv", row.names=FALSE)
