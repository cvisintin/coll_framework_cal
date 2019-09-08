require(RPostgreSQL)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

# Combine and split roads for Victoria
dbSendQuery(con,"
CREATE TABLE gis_victoria.vic_gda9455_roads_state_orig_500 AS
SELECT ROW_NUMBER() OVER (ORDER BY 1) AS uid, ST_LineSubstring(geom, 500.00*n/length,
            CASE
            WHEN 500.00*(n+1) < length THEN 500.00*(n+1)/length
            ELSE 1
            END) AS geom
            FROM
            (SELECT y.name, ST_Length(y.geom) AS length, y.geom AS geom
            FROM
            (SELECT x.ezi_rdname AS name, (ST_Dump(x.geom)).geom AS geom
            FROM
            (SELECT r.ezi_rdname, ST_Multi(ST_LineMerge(ST_Collect(r.geom))) AS geom
            FROM gis_victoria.vic_gda9455_roads_state_orig AS r, gis_victoria.vic_gda9455_admin_state AS p
            WHERE r.road_seal = '1'
            AND r.class_code <= '5'
            AND ST_Intersects(r.geom, p.geom)
            GROUP BY r.ezi_rdname) AS x) AS y
            ORDER BY ST_Length(y.geom) DESC) AS t
            CROSS JOIN generate_series(0,1000) AS n
            WHERE n*500.00/length < 1;
            
            ALTER TABLE gis_victoria.vic_gda9455_roads_state_orig_500 ADD PRIMARY KEY (uid);
            
            SELECT Populate_Geometry_Columns('gis_victoria.vic_gda9455_roads_state_orig_500'::regclass);
            
            CREATE INDEX vic_gda9455_roads_state_orig_500_gix ON gis_victoria.vic_gda9455_roads_state_orig_500 USING GIST (geom);
  ")

# Combine and split roads for California
dbSendQuery(con,"
            CREATE TABLE gis_victoria.vic_gda9455_roads_state_orig_500 AS
            SELECT ROW_NUMBER() OVER (ORDER BY 1) AS uid, ST_LineSubstring(geom, 500.00*n/length,
            CASE
            WHEN 500.00*(n+1) < length THEN 500.00*(n+1)/length
            ELSE 1
            END) AS geom
            FROM
            (SELECT y.name, ST_Length(y.geom) AS length, y.geom AS geom
            FROM
            (SELECT x.ezi_rdname AS name, (ST_Dump(x.geom)).geom AS geom
            FROM
            (SELECT r.ezi_rdname, ST_Multi(ST_LineMerge(ST_Collect(r.geom))) AS geom
            FROM gis_victoria.vic_gda9455_roads_state_orig AS r, gis_victoria.vic_gda9455_admin_state AS p
            WHERE r.road_seal = '1'
            AND r.class_code <= '5'
            AND ST_Intersects(r.geom, p.geom)
            GROUP BY r.ezi_rdname) AS x) AS y
            ORDER BY ST_Length(y.geom) DESC) AS t
            CROSS JOIN generate_series(0,1000) AS n
            WHERE n*500.00/length < 1;
            
            ALTER TABLE gis_victoria.vic_gda9455_roads_state_orig_500 ADD PRIMARY KEY (uid);
            
            SELECT Populate_Geometry_Columns('gis_victoria.vic_gda9455_roads_state_orig_500'::regclass);
            
            CREATE INDEX vic_gda9455_roads_state_orig_500_gix ON gis_victoria.vic_gda9455_roads_state_orig_500 USING GIST (geom);
            ")
