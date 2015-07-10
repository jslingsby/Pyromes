##############################################################
# Getting all the data together 
################################################################
# set wd
if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/Dropbox/Shared/Pyromes")} #setwd("/Users/jasper/Dropbox/SAEON/Projects/Matt_Restios/") 
if (Sys.getenv("USERNAME")=='nikki') {setwd("C:/Users/nikki/Dropbox/Pyromes");.libPaths("C:/Users/nikki/R_libraries")}
#if (Sys.getenv("USERNAME")=='WINDOZE PC') {datwd=""; giswd=""}

# installing/loading the packages:
library(maptools)
library(raster) 
library(rgdal)
library(rgeos)
library(sp)
require(Hmisc)
require(foreign)
require(fields)
library(shapefiles)
library(GISTools)
require(PBSmapping)

##############################################################
# Load rasters  and polygons
################################################################
rdat25_1 <- raster("clean_raster/rdat25_1.grd")
rdat25_2 <- raster("clean_raster/rdat25_2.grd")
rdat25_3 <- raster("clean_raster/rdat25_3.grd")
rdat25_4 <- raster("clean_raster/rdat25_4.grd")
rdat25_5 <- raster("clean_raster/rdat25_5.grd")
rdat25_6 <- raster("clean_raster/rdat25_6.grd")
rdat25_7 <- raster("clean_raster/rdat25_7.grd")

rdat10_1 <- raster("clean_raster/rdat10_1.grd")
rdat10_2 <- raster("clean_raster/rdat10_2.grd")
rdat10_3 <- raster("clean_raster/rdat10_3.grd")
rdat10_4 <- raster("clean_raster/rdat10_4.grd")
rdat10_5 <- raster("clean_raster/rdat10_5.grd")
rdat10_6 <- raster("clean_raster/rdat10_6.grd")
rdat10_7 <- raster("clean_raster/rdat10_7.grd")

###create a raster stack
rdat25 <- stack(rdat25_1, rdat25_2,rdat25_3, rdat25_4,rdat25_5,rdat25_6,rdat25_7)

rdat10 <- stack(rdat10_1, rdat10_2,rdat10_3, rdat10_4,rdat10_5,rdat10_6,rdat10_7)


###read in centroid files
c1 <- readShapePoints("clean_raster/c1.shp") 
c5 <- readShapePoints("clean_raster/c5.shp") 
c10 <- readShapePoints("clean_raster/c10.shp") 
c25 <- readShapePoints("clean_raster/c25.shp") 

dat25 <- extract(rdat25,c25)
write.csv(dat25,"clean_raster/dat25.csv")

dat10 <- extract(rdat10,c10)
class(dat10)
dat10 <- as.data.frame(dat10)

dat10 <- na.omit(dat10)
write.csv(dat10,"clean_raster/dat10.csv")






