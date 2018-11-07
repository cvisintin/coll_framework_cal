require(RPostgreSQL)
require(data.table)
require(doMC)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

dbGetQuery(con,"VACUUM ANALYZE gis_california.cal_nad8310_roads_study_500")

rds.count <- dbGetQuery(con,"
  SELECT
    Max(uid)
	FROM
      gis_california.cal_nad8310_roads_study_500
  ")$max

chunks <- split(seq_len(rds.count), ceiling(seq_along(c(1:rds.count))/100000))


RDCLASS <- as.data.table(dbGetQuery(con,"
  SELECT
    uid, rdclass
  FROM
	  gis_california.cal_nad8310_roads_study_500
  "))
setkey(RDCLASS,uid)


registerDoMC(cores=detectCores()-1)
system.time(
RDDENS <- as.data.table(foreach(i = 1:length(chunks), .packages="RPostgreSQL", .combine=rbind) %dopar% {
  drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
  con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
  temp <- dbGetQuery(con,paste0("
  SELECT
    p.uid AS uid, (Sum(ST_Length(ST_Intersection(ST_Buffer(ST_LineInterpolatePoint(p.geom, 0.5), 564.333), r.geom)))/1000) AS rddens
  FROM
	  gis_california.cal_nad8310_roads_study_500 AS r,
	  (SELECT uid, geom FROM gis_california.cal_nad8310_roads_study_500 WHERE uid BETWEEN ",range(chunks[i])[1]," AND ",range(chunks[i])[2],") AS p
  WHERE
    ST_DWithin(p.geom, r.geom, 564.333)
  GROUP BY
    p.uid
  "))
  temp
})#~1400 second query
)
setkey(RDDENS,uid)


registerDoMC(cores=detectCores()-1)
KMTOHWY <- as.data.table(foreach(i = 1:length(chunks), .packages="RPostgreSQL", .combine=rbind) %dopar% {
  drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
  con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
  temp <- dbGetQuery(con,paste0("
    SELECT DISTINCT ON (r.uid)
      r.uid AS uid, ST_Distance(r.geom,p.geom)/1000 as kmtohwy
    FROM
      (SELECT
        ST_Union(geom) AS geom
      FROM
        gis_california.cal_nad8310_roads_study_500
      WHERE
        rdclass = 1 OR rdclass = 2) AS p,
      (SELECT
        uid, ST_LineInterpolatePoint(geom, 0.5) AS geom
      FROM
        gis_california.cal_nad8310_roads_study_500) AS r
      WHERE r.uid BETWEEN ",range(chunks[i])[1]," AND ",range(chunks[i])[2]
  ))
  temp
})#~900 second query
setkey(KMTOHWY,uid)


KMTODEV <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, p.kmtodev as kmtodev
	FROM gis_california.cal_nad8310_admin_study_kmtodev AS p,
		(SELECT
      uid, ST_LineInterpolatePoint(geom, 0.5) AS geom
		FROM
      gis_california.cal_nad8310_roads_study_500) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~100 second query
setkey(KMTODEV,uid)


POPDENS <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, p.popdens as popdens
	FROM gis_california.cal_nad8310_demo_study_popdens AS p,
		(SELECT
      uid, ST_LineInterpolatePoint(geom, 0.5) AS geom
		FROM
      gis_california.cal_nad8310_roads_study_500) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~90 second query
setkey(POPDENS,uid)


SPEEDLMT <- as.data.table(dbGetQuery(con,"
  SELECT
      r.uid as uid, mode() WITHIN GROUP (ORDER BY p.maxspeed) AS speedlmt
		FROM
      gis_california.cal_nad8310_roads_study_500 as r, gis_california.cal_nad8310_roads_study_speeds AS p
    WHERE ST_DWithin(p.geom, r.geom, 5)
    GROUP BY r.uid
  ")) #~10 second query
setkey(SPEEDLMT,uid)


AADT <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, avg(p.aadt) as aadt
	FROM
    gis_california.cal_nad8310_roads_study_500 as r, gis_california.cal_nad8310_roads_study_aadt AS p
  WHERE ST_DWithin(p.geom, r.geom, 5)
  GROUP BY r.uid
  ")) #~1 second query
setkey(AADT,uid)


merged.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(AADT,SPEEDLMT,RDCLASS,RDDENS,KMTOHWY,KMTODEV,POPDENS))

merged.data[is.na(popdens),popdens:=0]

write.csv(merged.data, "data/cal_model_data_traffic_500.csv", row.names=FALSE)
