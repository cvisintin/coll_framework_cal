pycno_interpolate <-function(input_path,
                             input_filename,
                             pop_column,
                             celldim,
                             input_clip_path,
                             input_clip_filename,
                             output_path,
                             output_filename) {
  require(rgeos)
  require(sp)
  require(pycno)
  require(rgdal)
  require(raster)
  
  input <- readOGR(input_path, input_filename)
  
  pyc_input <- pycno(x = input,
                     pops = input@data$pop_column,
                     celldim = celldim)
  
  polys <- as(pyc_input, "SpatialPolygonsDataFrame")
  
  bounds <- readOGR("/media/casey/APOC/Admin/California", "CAL_NAD8310_ADMIN_STUDYBOUNDS")
  
  final <- crop(polys, bounds)
 
  writeOGR(final, "/media/casey/APOC/Admin/California", "CAL_NAD8310_DEMO_STUDY_POPDENS", driver = "ESRI Shapefile")
}