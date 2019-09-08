require(pycno)
require(raster)

pycno_interpolate <-function(input_filename,
                             pop_column,
                             celldim,
                             input_clip_filename = NULL,
                             output_filename) {
  
  input <- raster::shapefile(input_filename)
  
  pops <- as.numeric(input@data[, pop_column])
  pops[is.na(pops)] <- 0
  
  pyc_input <- pycno::pycno(x = input,
                     pops = pops,
                     celldim = celldim)
  
  polys <- as(pyc_input, "SpatialPolygonsDataFrame")
  colnames(polys@data) <- "popdens"
  
  if (!is.null(input_clip_filename)) {
    bounds <- raster::shapefile(input_clip_filename)
    final <- raster::intersect(polys, bounds)
  } else {
    final <- polys
  }
  
  shapefile(final, filename = output_filename, overwrite = TRUE)
}

pycno_interpolate(input_filename = "/media/casey/APOC/Admin/Victoria/VIC_GDA9455_DEMO_SA2_POPDENS.shp",
                  pop_column = "POP",
                  celldim = 1000,
                  input_clip_filename = NULL,
                  output_filename = "/media/casey/APOC/Admin/Victoria/VIC_GDA9455_ADMIN_DEMO_POP.shp")

pycno_interpolate(input_filename = "/media/casey/APOC/Admin/California/CAL_NAD8310_DEMO_CCD_POP.shp",
                  pop_column = "DP0010001",
                  celldim = 1000,
                  input_clip_filename = "/media/casey/APOC/Admin/California/CAL_NAD8310_ADMIN_STUDYBOUNDS.shp",
                  output_filename = "/media/casey/APOC/Admin/California/CAL_NAD8310_ADMIN_DEMO_POP.shp")
