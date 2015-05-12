##############################################################################
######## Code to play with Pyromes data
##############################################################################
######## Compiled by Nikki Stevens and Jasper Slingsby 2015
######## Last edited: 12 May 2015
##############################################################################
######## 
###Steps:
###1) Get libraries and setwd
###2) Read in spatial data using RGdal command which includes the projection files
###3) Make matching fields and combine
###4) Make index raster grids of different cell size and rasterize
##############################################################################

##############################################################################
###1) Get libraries and setwd
##############################################################################

library(maptools)
library(raster)
library(rgdal)

if (Sys.getenv("USER")=='jasper') {setwd("/Users/jasper/Dropbox/Shared/Pyromes")} #setwd("/Users/jasper/Dropbox/SAEON/Projects/Matt_Restios/") 
if (Sys.getenv("USERNAME")=='cpt') {setwd("Z:/Projects/GIS/Projects/Pyromes")}
#if (Sys.getenv("USERNAME")=='WINDOZE PC') {datwd=""; giswd=""}

##############################################################################
###2) Read in spatial data using RGdal command which includes the projection files
##############################################################################

##Cape Point
CP=readOGR (dsn=".","capepenfire2011")
##cape nature data
CN<- readOGR (dsn=".","all_fires_13_14_gw")
## Bontebok
BNP<- readOGR (dsn=".","BNP Firerecords 1972-2012_TKraaij")
## Greater Wilderness
WNP<- readOGR (dsn=".","GRNP Firerecords_TKraaij")

##check and set the projection
proj4string(CP)
proj4string(CN)
proj4string(BNP)
proj4string(WNP)

CP=spTransform(CP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
BNP=spTransform(BNP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
CN=spTransform(CN, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
WNP=spTransform(WNP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##############################################################################
###3) Make matching fields and combine
##############################################################################

#Organize data
WNP$YEAR=substr(WNP$DATE_BURNE, 1, 4)
WNP$MONTH=as.numeric(substr(WNP$DATE_BURNE, 5, 6))
WNP$MONTH[which(WNP$MONTH==99)]=0
WNP$ID=(length(CN)+1):(length(CN)+length(WNP))
WNP$SOURCE=rep("WNP",length(WNP))

BNP$ID=(length(CN)+length(WNP)+1):(length(CN)+length(WNP)+length(BNP))#paste0("BNP",1:length(BNP))
BNP$SOURCE=rep("BNP",length(BNP))
levels(BNP$MONTH)=c(4,4,12,12,2,2,1,1,6,3,5,11,0,0)
BNP$MONTH=as.numeric(as.character(BNP$MONTH))
BNP$MONTH[is.na(BNP$MONTH)]=0

CP$ID=(BNP$ID[length(BNP)]+1):(BNP$ID[length(BNP)]+length(CP))
CP$SOURCE=rep("CP",length(CP))
CP$MONTH=as.numeric(substr(CP$STARTDATE, 5, 6))

CN$ID=1:length(CN)
CN$SOURCE=rep("CN",length(CN))

#Draw out wanted columns
CN=CN[,c(1,4,3)]
WNP=WNP[,c(10,3,9)]
BNP=BNP[,c(9,3,4)]
CP=CP[,c(8,1,10)]

#Relable feature IDs so we can merge
BNP <- spChFIDs(BNP, as.character(BNP$ID))
WNP <- spChFIDs(WNP, as.character(WNP$ID))
CN <- spChFIDs(CN, as.character(CN$ID))
CP <- spChFIDs(CP, as.character(CP$ID))

#Merge datasets
dat=spRbind(CN, WNP)
dat=spRbind(dat, BNP)
dat=spRbind(dat, CP)
#dat <- spChFIDs(dat, as.character(1:length(dat)))
dat$YEAR_MONTH=paste(dat$YEAR, dat$MONTH, sep="_")
#dat$DATE=

##############################################################################
###4) Make index raster grids of different cell size and rasterize
##############################################################################
r1 <- raster(ext=extent(dat), res=1000, crs=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
r5 = aggregate(r1, 5)
r10 = aggregate(r1, 10)
r25 = aggregate(r1, 25)

###Rasterize fire data - YOU ARE HERE!!!
rdat <- rasterize(dat,r10, dat$ID)
rdat <- rasterize(dat,r10, "ID")
rdat <- rasterize(dat,r10, field="ID", fun="count")
plot(dat)
