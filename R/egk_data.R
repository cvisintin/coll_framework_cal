require(maptools)
require(scales)
require(rgdal)
require(raster)
require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

grid.files <- list.files(path='data/grids/vic/envi') #Create vector of filenames

grid.names <- substring(unlist(strsplit(grid.files,"\\_1000."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

vic.rst <- raster("data/grids/vic/VIC_GDA9455_GRID_STATE_1000.tif")

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/vic/envi/",grid.files[i]))
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * vic.rst)
}
vars <- stack(mget(grid.names)) #Combine all maps to single stack

data0 <- read.csv("data/vic_bg_data_pts.csv")

data1 <- dbGetQuery(con,paste0("
      SELECT DISTINCT ON (pts.geom)
        ST_X(pts.geom) AS X, ST_Y(pts.geom) AS Y, CAST(1 AS INTEGER) AS OCC
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

egk.data <- rbind(data1,data0)

#Sample covariate grid values at all egk coordinates
samples.df <- extract(vars,egk.data[,1:2])

#Build modelling dataset
final.data <- cbind(egk.data,samples.df)

#Remove any records with missing information - occurs where sampling detected NAs in grids
final.data <- na.omit(final.data)

write.csv(final.data, "data/vic_model_data_sdm.csv", row.names=FALSE)
