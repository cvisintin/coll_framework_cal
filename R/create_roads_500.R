require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

# Combine and split roads for Victoria
dbSendQuery(con,"
CREATE TABLE gis_victoria.vic_gda9455_roads_state_orig_500 AS
SELECT ROW_NUMBER() OVER (ORDER BY 1) AS uid, c.rdname, c.rdclass, ST_LineSubstring(c.geom, 500.00*n/c.length,
	CASE
		WHEN 500.00*(n+1) < c.length THEN 500.00*(n+1)/c.length
		ELSE 1
	END) AS geom
FROM
	(SELECT b.rdname, b.rdclass, ST_Length((ST_Dump(b.geom)).geom) As length, (ST_Dump(b.geom)).geom AS geom
	 FROM
		(SELECT a.rdname, a.rdclass, ST_LineMerge(ST_Union(a.geom)) AS geom
		 FROM
			(SELECT r.ezirdnmlbl AS rdname, r.class_code AS rdclass, ST_Intersection(p.geom, r.geom) AS geom
			 FROM gis_victoria.vic_gda9455_roads_state_orig AS r, gis_victoria.vic_gda9455_admin_state AS p
			 WHERE ST_Intersects(r.geom, p.geom)
             AND road_seal = '1') AS a
         GROUP BY rdname, rdclass) AS b) AS c
CROSS JOIN generate_series(0,100) AS n
WHERE n*500.00/c.length < 1;

ALTER TABLE gis_victoria.vic_gda9455_roads_state_orig_500 ADD PRIMARY KEY (uid);

SELECT Populate_Geometry_Columns('gis_victoria.vic_gda9455_roads_state_orig_500'::regclass);

CREATE INDEX vic_gda9455_roads_state_orig_500_gix ON gis_victoria.vic_gda9455_roads_state_orig_500 USING GIST (geom);
  ")

# Combine and split roads for California
dbSendQuery(con,"
CREATE TABLE gis_california.cal_nad8310_roads_study_500 AS
SELECT ROW_NUMBER() OVER (ORDER BY 1) AS uid, c.rdname, c.rdclass, ST_LineSubstring(c.geom, 500.00*n/c.length,
	CASE
		WHEN 500.00*(n+1) < c.length THEN 500.00*(n+1)/c.length
		ELSE 1
	END) AS geom
FROM
	(SELECT b.rdname, b.rdclass, ST_Length((ST_Dump(b.geom)).geom) As length, (ST_Dump(b.geom)).geom AS geom
	 FROM
		(SELECT a.rdname, a.rdclass, ST_LineMerge(ST_Union(a.geom)) AS geom
		 FROM
			(SELECT r.fullname AS rdname, r.fc_draft AS rdclass, CASE 
                                                                    WHEN ST_CoveredBy(r.geom, p.geom) 
                                                                    THEN r.geom 
                                                                    ELSE ST_Intersection(r.geom, p.geom)
                                                                    END AS geom
			 FROM gis_california.cal_nad8310_roads_orig AS r, gis_california.cal_nad8310_admin_study_area AS p
			 WHERE ST_Intersects(r.geom, p.geom)) AS a
         GROUP BY rdname, rdclass) AS b) AS c
CROSS JOIN generate_series(0,100) AS n
WHERE n*500.00/c.length < 1;

ALTER TABLE gis_california.cal_nad8310_roads_study_500 ADD PRIMARY KEY (uid);

SELECT Populate_Geometry_Columns('gis_california.cal_nad8310_roads_study_500'::regclass);

CREATE INDEX cal_nad8310_roads_study_500_gix ON gis_california.cal_nad8310_roads_study_500 USING GIST (geom);
            ")
