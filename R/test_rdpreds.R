library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab


roads.preds <- dbGetQuery(con,paste0("
SELECT r.uid, sum((st_length(st_intersection(r.geom,g.geom))/st_length(r.geom)) * (g).val)
  FROM test.roads_gda9455 AS r, 
  (SELECT (ST_PixelAsPolygons(rast)).val AS val, (ST_PixelAsPolygons(rast)).geom AS geom
  FROM test.egk_gda9455) AS g
  WHERE ST_Intersects(r.geom,g.geom)
  GROUP BY r.uid
    "))
# 
# SELECT r.uid, sum((st_length(st_intersection(r.geom,g.geom))/st_length(r.geom)) * (g).val)
# FROM gis_victoria.vic_gda9455_roads_state_orig_500 AS r, 
# (SELECT (ST_PixelAsPolygons(rast)).val AS val, (ST_PixelAsPolygons(rast)).geom AS geom
#  FROM gis_victoria.vic_gda9455_grid_egk_preds_brt_500) AS g
# WHERE ST_Intersects(r.geom,g.geom)
# GROUP BY r.uid