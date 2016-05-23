require("data.table")
require("dismo")
require("doMC")
require("maptools")
require("reshape2")
require("rgeos")
require("ALA4R")

#Specify target species
sp.target <- "Eastern Grey Kangaroo"

#Specify start year
yr.start <- 2000

#Specify end year
yr.end <- 2014

#Download background species data - setup parallel data retrieval workers (split over multiple years to reduce server load and address server allowances (i.e. multiple queries originating from identical IP address))
registerDoMC(detectCores() - 1)

raw.sp.data <- foreach(i = c(yr.start:yr.end), .packages = c("ALA4R","maptools"), .combine = rbind) %dopar% {
  query.birds <- c("basis_of_record:HumanObservation","species_group:Birds","species_habitats:Non-marine",paste("year:",i,sep=""),"state:Victoria","coordinate_uncertainty:[0 TO 100] OR coordinate_uncertainty:[101 TO 500]","name_match_metric:exactMatch")
  result.birds <- occurrences(fq=query.birds, fields=c('longitude','latitude','common_name','occurrence_date','class'), download_reason_id=7)$data
  query.mammals <- c("basis_of_record:HumanObservation","species_group:Mammals","species_habitats:Non-marine",paste("year:",i,sep=""),"state:Victoria","coordinate_uncertainty:[0 TO 100] OR coordinate_uncertainty:[101 TO 500]","name_match_metric:exactMatch")
  result.mammals <- occurrences(fq=query.mammals, fields=c('longitude','latitude','common_name','occurrence_date','class'), download_reason_id=7)$data
  query.reptiles <- c("basis_of_record:HumanObservation","species_group:Reptiles OR species_group:Amphibians","species_habitats:Non-marine",paste("year:",i,sep=""),"state:Victoria","coordinate_uncertainty:[0 TO 100] OR coordinate_uncertainty:[101 TO 500]","name_match_metric:exactMatch")
  result.reptiles <- occurrences(fq=query.reptiles, fields=c('longitude','latitude','common_name','occurrence_date','class'), download_reason_id=7)$data
  result <- rbind(result.birds[,1:5],result.mammals[,1:5],result.reptiles[,1:5])
  result <- result[!(result$occurrenceDate == ""), ]
  ll <- SpatialPoints(result[,1:2], proj4string=CRS("+init=epsg:4283"))
  UTM <- data.frame(spTransform(ll, CRS("+init=epsg:28355")))
  names(UTM) <- c('X','Y')
  cbind(UTM,result[,c(3:5)])
}

#save(raw.sp.data, file = "raw_sp_data_ala") #Store a copy since the query can be quite time consuming (10+ minutes)...
#load("raw_sp_data_ala") #Load saved copy of data - alternative method is to save R workspace...

#Combine all background data into single table
#xx <- rbind(raw.sp.data[[1]],raw.sp.data[[2]],raw.sp.data[[3]],raw.sp.data[[4]],raw.sp.data[[5]],raw.sp.data[[6]],raw.sp.data[[7]],raw.sp.data[[8]],raw.sp.data[[9]],raw.sp.data[[10]],raw.sp.data[[11]],raw.sp.data[[12]],raw.sp.data[[13]],raw.sp.data[[14]],raw.sp.data[[15]])
#xx <- as.data.table(do.call(rbind,raw.sp.data))
#all.sp.data <- data.frame("X"=xx$X,"Y"=xx$Y,"year"=xx$year,"month"=xx$month,"day"=xx$day,"commonName"=xx$commonName,"taxo"=xx$class)

raw.sp.data[occurrenceDate!='', c("year", "month", "day") := tstrsplit(occurrenceDate, "-", fixed=TRUE)]

#Sort and group records by time and location of observations to determine sites with multiple species surveys
all.sp.data <- as.data.table(raw.sp.data)

setkey(all.sp.data,year,month,day,X,Y,commonName,class)

sites.spp <- all.sp.data[,c("MULT" = length(unique(commonName))>1 & length(unique(class))>1, "TOTAL" = .N, "SPP" = list(paste(unique(commonName),collapse=", "))), by="X,Y,year,month,day"]

sites.tax <- sites.spp[MULT == TRUE]

#Construct species datsets using reported target species locations as presences (1's) and sites where multiple species were observed excluding target species as background (0's)
sites.1 <- all.sp.data[commonName == sp.target, .N , by="X,Y"]
#sites.0 <- sites.tax[!like(SPP,sp.target)]
set.seed(123)
sites.0 <- all.sp.data[,.N, by="X,Y"]
sites.0 <- sites.0[sample(nrow(sites.0),10000,replace=FALSE), ]
x1 <- cbind(sites.1[,.(X,Y)],"OCC"=rep(1,nrow(sites.1)))
x0 <- cbind(sites.0[,.(X,Y)],"OCC"=rep(0,nrow(sites.0)))
sp.model.data <- rbind(x1,x0)
write.table(sp.model.data, file = "Data/vic_speciesdata.csv", row.names=FALSE, col.names=TRUE, sep=",")
