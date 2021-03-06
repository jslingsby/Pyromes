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

##############################################################################
###2) Read in spatial data using RGdal command which includes the projection files
##############################################################################  

##Cape Point
CP=readOGR(dsn=".",layer="capepenfire2011")
##cape nature data
CN<- readOGR (dsn=".","all_fires_13_14_gw")
## Bontebok
BNP<- readOGR (dsn=".","BNP Firerecords 1972-2012_TKraaij")
## Greater Wilderness
WNP<- readOGR (dsn=".","GRNP Firerecords_TKraaij")
## FIRE INTENSITY VALUES
FI<- readOGR(dsn=".","Fire intensity")

###check and set the projection
proj4string(CP)
proj4string(CN)
proj4string(BNP)
proj4string(WNP)
proj4string(FI)

###Assign proj to be all the same
CP=spTransform(CP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
BNP=spTransform(BNP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
CN=spTransform(CN, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
WNP=spTransform(WNP, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
FI=spTransform(FI, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

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
CP=CP[,c(9,1,11)]

#Relable feature IDs so we can merge
BNP = spChFIDs(BNP, as.character(BNP$ID))
WNP = spChFIDs(WNP, as.character(WNP$ID))
CN = spChFIDs(CN, as.character(CN$ID))
CP = spChFIDs(CP, as.character(CP$ID))

#Merge datasets
dat=spRbind(CN, WNP)
dat=spRbind(dat, BNP)
dat=spRbind(dat, CP)
dat$YEAR_MONTH=paste(dat$YEAR, dat$MONTH, sep="_")
dat$AREA_KM <- sapply(sapply(slot(dat, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000

##############################################################################
###4) Make index raster grids of different cell size
##############################################################################
#r001 <- raster(ext=extent(dat), res=100, crs=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
r1 <- raster(ext=extent(dat), res=1000, crs=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
r5 = aggregate(r1, 5)
r10 = aggregate(r1, 10)
r25 = aggregate(r1, 25)
r100 = aggregate(r1, 100)

##############################################################################
###5) VARIABLE1: Get fire count by cell
##############################################################################

###Rasterize fire data - YOU ARE HERE!!!
#rdat1_1 <- rasterize(dat,r1, dat$ID, fun="count")
#rdat5_1 <- rasterize(dat,r5, dat$ID, fun="count")
#rdat10_1 <- rasterize(dat,r10, dat$ID, fun="count")
rdat25_1 <- rasterize(dat,r25, dat$ID, fun="count")


# writeRaster(rdat1_1, filename='clean_raster/rdat1_1.grd', overwrite=TRUE)
# writeRaster(rdat5_1, filename='clean_raster/rdat5_1.grd', overwrite=TRUE)
# writeRaster(rdat10_1, filename='clean_raster/rdat10_1.grd', overwrite=TRUE)
writeRaster(rdat25_1, filename='clean_raster/rdat25_1.grd', overwrite=TRUE)   

###Code to stack and extract to dataframe
#x=stack(rdat10, rdat10, rdat10) 
#rdat10_df=as.data.frame(x)

##############################################################################
###6) VARIABLE2: MEAN FIRE size (of intersected polygons)
##############################################################################

###Rasterize fire data - YOU ARE HERE!!!
#rdat1_2 <- rasterize(dat,r1, dat$AREA_KM, fun="mean")
#rdat5_2 <- rasterize(dat,r5, dat$AREA_KM, fun="mean")
#rdat10_2 <- rasterize(dat,r10, dat$AREA_KM, fun="mean")
rdat25_2 <- rasterize(dat,r25, dat$AREA_KM, fun="mean")


# writeRaster(rdat1_2, filename='clean_raster/rdat1_2.grd', overwrite=TRUE)
# writeRaster(rdat5_2, filename='clean_raster/rdat5_2.grd', overwrite=TRUE)
# writeRaster(rdat10_2, filename='clean_raster/rdat2_1.grd', overwrite=TRUE)
writeRaster(rdat25_2, filename='clean_raster/rdat25_2.grd', overwrite=TRUE)   


##############################################################################
###7) VARIABLE3: MEAN, MAX, MIN % area burnt per grid cell
##############################################################################

###Make raster grids into SpatialPolygons
#r1sp=rasterToPolygons(r1)
#r5sp=rasterToPolygons(r5)
#r10sp=rasterToPolygons(r10)
r25sp=rasterToPolygons(r25)
r100sp=rasterToPolygons(r100)

###Fix dat to get around weird errors...
gdat=gBuffer(dat,byid=TRUE,width=0)
#?gIsValid #Use this function if you're getting errors...

###Do intersection for each grid size
#x=gIntersection(r1sp, gdat, byid = TRUE)
#x$AREA_KM=sapply(sapply(slot(x, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000
#y=as(x,"SpatialLinesDataFrame")
#rdat1_3 = rasterize(y, r1, "AREA_KM", fun=mean)*100
#rdat1_4 = rasterize(y, r1, "AREA_KM", fun=median)*100
#rdat1_5 = rasterize(y, r1, "AREA_KM", fun=max)*100
# writeRaster(rdat1_3, filename='clean_raster/rdat1_3.grd', overwrite=TRUE)
# writeRaster(rdat1_4, filename='clean_raster/rdat1_4.grd', overwrite=TRUE)
# writeRaster(rdat1_5, filename='clean_raster/rdat1_5.grd', overwrite=TRUE)

###ADD RASTERIZES FOR MAX MIN MEDIAN ETC...

#x=gIntersection(r5sp, gdat, byid = TRUE)
#x$AREA_KM=sapply(sapply(slot(x, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000
#y=as(x,"SpatialLinesDataFrame")
#rdat5_3 = (rasterize(y, r5, "AREA_KM", fun=mean)/25)*100
#rdat5_4 = rasterize(y, r5, "AREA_KM", fun=median)*100
#rdat5_5 = rasterize(y, r5, "AREA_KM", fun=max)*100
# writeRaster(rdat5_3, filename='clean_raster/rdat5_3.grd', overwrite=TRUE)
# writeRaster(rdat5_4, filename='clean_raster/rdat5_4.grd', overwrite=TRUE)
# writeRaster(rdat5_5, filename='clean_raster/rdat5_5.grd', overwrite=TRUE)


#x=gIntersection(r10sp, gdat, byid = TRUE)
#x$AREA_KM=sapply(sapply(slot(x, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000
#y=as(x,"SpatialLinesDataFrame")
#rdat10_3 = rasterize(y, r10, "AREA_KM", fun=mean) #in km, which happens to be % too
#rdat10_4 = rasterize(y, r10, "AREA_KM", fun=median)*100
#rdat10_5 = rasterize(y, r10, "AREA_KM", fun=max)*100
# writeRaster(rdat10_3, filename='clean_raster/rdat10_3.grd', overwrite=TRUE)
# writeRaster(rdat10_4, filename='clean_raster/rdat10_4.grd', overwrite=TRUE)
# writeRaster(rdat10_5, filename='clean_raster/rdat10_5.grd', overwrite=TRUE)

x=gIntersection(r25sp, gdat, byid = TRUE)
x$AREA_KM=sapply(sapply(slot(x, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000
y=as(x,"SpatialLinesDataFrame")
rdat25_3 = (rasterize(y, r25, "AREA_KM", fun=mean)/625)*100
#rdat25_4 = rasterize(y, r25, "AREA_KM", fun=median)*100
#rdat25_5 = rasterize(y, r25, "AREA_KM", fun=max)*100
writeRaster(rdat25_3, filename='clean_raster/rdat25_3.grd', overwrite=TRUE)
# writeRaster(rdat25_4, filename='clean_raster/rdat25_4.grd', overwrite=TRUE)
# writeRaster(rdat25_5, filename='clean_raster/rdat2 5_5.grd', overwrite=TRUE)

# x=gIntersection(r100sp, gdat, byid = TRUE)
# x$AREA_KM=sapply(sapply(slot(x, "polygons"), function(x) sapply(slot(x,"Polygons"), slot, "area")), sum)/1000000
# y=as(x,"SpatialLinesDataFrame")
# rdat25_3 = (rasterize(y, r100, "AREA_KM", fun=mean)/10000)*100
#rdat25_4 = rasterize(y, r25, "AREA_KM", fun=median)*100
#rdat25_5 = rasterize(y, r25, "AREA_KM", fun=max)*100
# writeRaster(rdat25_3, filename='clean_raster/rdat25_3.grd', overwrite=TRUE)
# writeRaster(rdat25_4, filename='clean_raster/rdat25_4.grd', overwrite=TRUE)
# writeRaster(rdat25_5, filename='clean_raster/rdat2 5_5.grd', overwrite=TRUE)


##############################################################################
##############################################################################
rdatyear = rasterize(dat, r25)
a=stack(rdatyear,dat)



"rainperc.s" <- function(dat = dat, perc = 30, userainyear = T){
  ## code for working out the number of months containing less than 30 % of the rainfall - index of length of the dry season
  ### modified from Russell-Smith et al 2007 and used in Archibald et al 2008
  ### perc variable is the percentage of the annual rainfall that is allowed to fall in the dry season. Did a sensitivity analysis of this and 30 seems reasonable 
  ### input data format: yearmonth, rainval
  ### first work out rainyear
  year <- as.numeric(dat$YEAR)
month <- (dat$MONTH - year)*13
rainyear <- year
rainyear[month < 7] <- year[month <7] -1
rainmonth <- dat$MONTH
rainmonth[month < 7] <- rainmonth[month < 7] + 6
rainmonth[month >= 7] <- rainmonth[month >=7] -6

if(userainyear == T){
  year <- rainyear
  month <- rainmonth
}

#for each rainyear, work out rainfall concentration as the sum of 12 vectors (r, thetha), where r is the magnitude of the vector and theta is the angle (expressed in units of arc)
# for a circle of radius r (monthly rainfall) the circumference is 2pi*r, which, divided by 12 gives you s s = 2pi*r/12... thetha is s/r so theta is (2pi*r/12)/r = pi/6
yearcount <- unique(year)
count <- rep(0, length(yearcount))
skip <- rep(0, length(yearcount))
AnnPPT <- tapply(dat[,2], year, sum)
for(i in 1:length(yearcount)){        
  o <- order(dat[year == yearcount[i], 2], decreasing = F)
  temp <- dat[year == yearcount[i],2]                   # get the data to work with
  months <- month[rainyear == yearcount[i]]           # get the months that apply to this data
  temp <- temp[o]                                     # sort them 
  months <- month[o]        
  if(length(temp[!is.na(temp)]) == 12){                               # if there is a full rainyear of data to work with
    test <- 0                                          # create an accumulator variable
    while (test <= (perc/100)*sum(temp)) {             # check whether it exceeds the percentage specified rain
      count[i] <- count[i] + 1          
      test <- test + temp[count[i]]                  # acucmluate the rain for this month
    }                        
    months <- months[1:count[i]]                    # take the months that contain the dry season values
    if(length(months) > 1){                                 # if there are two or more months of dry season 
      for(j in length(months):2){                 
        if(months[j] - months[j-1] > 1) skip[i] <- skip[i] + 1     # count the number of times that the months got skipped
      }
    }
  }
  if(length(temp[!is.na(temp)]) < 12){
    count[i] <- NA
    skip[i] <- NA
  }                   
}
rainperc <- count/12*100
rainperc <- cbind(yearcount, round(AnnPPT,0), round(rainperc,0), skip)
if(userainyear == T) dimnames(rainperc)[[2]] <- c("rainyear", "AnnRain", "rainperc", "skipmonths")
if(userainyear == F) dimnames(rainperc)[[2]] <- c("year", "AnnRain_calendaryear", "rainperc", "skipmonths")
rainperc <- data.frame(rainperc)
rainperc
}
rdatyear@data







##############################################################################

###Useful functions
?getValues
?setValues

# Load centroids derived from polygons 

CPfireA=readOGR (dsn=".","CP fire area")
##cape nature data
CNfireA<- readOGR (dsn=".","CN fire area")
## Bontebok
BNPfireA<- readOGR (dsn=".","BNP fire area")
## Greater Wilderness
GRNPfireA<- readOGR (dsn=".","GRNP fire area")
## FIRE INTENSITY VALUES

## check projections
proj4string(CPfireA)
proj4string(CNfireA)
proj4string(BNPfireA)
proj4string(GRNPfireA)

### make projections the same
CPfireA=spTransform(CPfireA, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
BNPfireA=spTransform(BNPfireA, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
CNfireA=spTransform(CNfireA, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
GRNPfireA=spTransform(GRNPfireA, CRSobj=CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#plot
plot(CNfireA, col="red")
points(CPfireA, col="blue")
points(BNPfireA, col="green")
points(GRNPfireA, col="orange")

 
 
 CNfireA$ID=1:length(CNfireA)
 CNfireA$SOURCE=rep("CNfireA",length(CNfireA))
 CNfireA$areakm2=CNfireA$XAREA/1000000


 GRNPfireA$YEAR=substr(GRNPfireA$DATE_BURNE, 1, 4)
 GRNPfireA$MONTH=as.numeric(substr(GRNPfireA$DATE_BURNE, 5, 6))
 GRNPfireA$MONTH[which(GRNPfireA$MONTH==99)]=0
 GRNPfireA$ID=(length(CNfireA)+1):(length(CNfireA)+length(GRNPfireA))
 GRNPfireA$SOURCE=rep("GRNPfireA",length(GRNPfireA))
 GRNPfireA$areakm2=GRNPfireA$area.m2/1000000
 
 
 BNPfireA$ID=(length(CNfireA)+length(GRNPfireA)+1):(length(CNfireA)+length(GRNPfireA)+length(BNPfireA))#paste0("BNP",1:length(BNP))
 BNPfireA$SOURCE=rep("BNPfireA",length(BNPfireA))
 levels(BNPfireA$MONTH)=c(4,4,12,12,2,2,1,1,6,3,5,11,0,0)
 BNPfireA$MONTH=as.numeric(as.character(BNPfireA$MONTH))
 BNPfireA$MONTH[is.na(BNPfireA$MONTH)]=0
 BNPfireA$areakm2=BNPfireA$area.m2/1000000
 
 
 CPfireA$ID=(BNPfireA$ID[length(BNPfireA)]+1):(BNPfireA$ID[length(BNPfireA)]+length(CPfireA))
 CPfireA$SOURCE=rep("CPfirea",length(CPfireA))
 CPfireA$MONTH=as.numeric(substr(CPfireA$STARTDATE, 5, 6))
 CPfireA$SOURCE=rep("CPfireA",length(CPfireA))
 CPfireA$areakm2=CPfireA$area.m2/1000000
 
 
 #Draw out wanted columns
 CNfireA=CNfireA[,c(1,4,3,34)]
 GRNPfireA=GRNPfireA[,c(11,3,10,13)]
 BNPfireA=BNPfireA[,c(10,3,4,12)]
 CPfireA=CPfireA[,c(9,1,11,12)]
 
 #Relable feature IDs so we can merge
 BNPfireA <- spChFIDs(BNPfireA, as.character(BNPfireA$ID))
 GRNPfireA <- spChFIDs(GRNPfireA, as.character(GRNPfireA$ID))
 CNfireA <- spChFIDs(CNfireA, as.character(CNfireA$ID))
 CPfireA <- spChFIDs(CPfireA, as.character(CPfireA$ID))

##merge datasets
 data=spRbind(CNfireA, GRNPfireA)
 data=spRbind(data, BNPfireA)
 data=spRbind(data, CPfireA)
 #dat <- spChFIDs(dat, as.character(1:length(dat)))
 #data$YEAR_MONTH=paste(data$YEAR, data$MONTH, sep="/")
 #dat$DATE=
 
### Calculate percent area burns
 data$percarea1=data$areakm2/1
 data$percarea5=data$areakm2/5*100
 data$percarea10=data$areakm2/10*100
 data$percarea25=data$areakm2/25*100

 data$percarea25=ifelse(data$percarea25>100,100,data$percarea25) 
 plot(data$percarea25)
 


a <- table(data$MONTH[data$percarea25])
 
 <-  tapply(data$percarea25,data$YEAR_MONTH, max)
format(a,scientific=FALSE)
b=as.matrix(a)
barplot(a)


 
 ### Rasterise these polygons and calculate the mean value per grid
area25 <- rasterize(data,r25, field="areakm2",getCover=TRUE)
area25 <- rasterize(data,a)
area10 <- rasterize(data,r10, field="areakm2", fun=mean)
area5 <- rasterize(data,r5, field="areakm2", fun=mean)
area1 <- rasterize(data,r1, field="areakm2", fun=mean)
plot(area25)
r <- ratify(area25)
plot(r)
summary(r)
names(r)


########Variabe 4####################################################
########MAX area of fires different sizes#########
###################################################################
maxarea25 <- rasterize(data,r25, field="areakm2", fun=max)
maxarea10 <- rasterize(data,r10, field="areakm2", fun=max)
maxarea5 <- rasterize(data,r5, field="areakm2", fun=max)
maxarea1 <- rasterize(data,r1, field="areakm2", fun=max)


########Variabe 5####################################################
########Median area of fires different sizes#########
###################################################################
medarea25 <- rasterize(data,r25, field="areakm2", fun=median)
medarea10 <- rasterize(data,r10, field="areakm2", fun=median)
medarea5 <- rasterize(data,r5, field="areakm2", fun=median)
medarea1 <- rasterize(data,r1, field="areakm2", fun=median)

########Variabe 6####################################################
########CV of fires#########
###################################################################

########Variabe 7####################################################
########Mean fire intensity #########
###################################################################


########Variabe 8####################################################
########Fire seasonality #########
###################################################################













################# PLAYING##############################################

 ## make polygons grids of different sizes
 pol10 <- rasterToPolygons(rdat10, fun=NULL, n=4, na.rm=TRUE, digits=12)
 plot(pol10)
 
 
 
 
 ### Extract mean area from within each 10kmx10km grid
 a <- extract(rdat10,data, fun=mean)
 
 over(data,pol10, fn=mean)
 
 
## calculate area of polygons within r25 grid
pol001 <- rasterToPolygons(rdat, fun=NULL, n=4, na.rm=TRUE, digits=12)
plot(pol001)

pol25 <- rasterToPolygons(r25, fun=NULL, n=4, na.rm=TRUE, digits=12)

a <- extract(rd,pol25, fun="count")
plot(pol001)
)
names(pol001)

###CAlculatemean burnt area
### Convert raster R 10 to polygon

burnarea=gArea(pol001, byid=TRUE)
table(burnarea)
plot(burnarea)

a=calc(rdat,"mean")
plot(a)

plot(pol001, add=TRUE)
plot(pol25)
v <- intersect(pol25, pol001)
plot(v)

grid <- unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

plot(gIntersection(SpatialPolygons(grid@polygons[3]), SpatialPolygons(dat@polygons[6])))

if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() 

areas.list <- sapply(all.polys, function(x) {
  my.area <- dat@polygons[[x]]@Polygons[[1]]@area # the OS data contains area
  intersected.area <- gArea(gIntersection(SpatialPolygons(dat@polygons[x]), SpatialPolygons(dat@polygons[overlaid.poly])))
  print(paste(shp4@data$NAME[x], " (poly ", x, ") area = ", round(my.area, 1), ", intersect = ", round(intersected.area, 1), ", intersect % = ", sprintf("%1.1f%%", 100*intersected.area/my.area), sep = ""))
  return(intersected.area) # return the intersected area for future use
})



a <- extract(rdat,dat)
plot(a)
names(a)

r <- raster(ncol=36, nrow=18)
r[] <- 1:ncell(r)

###############################
# extract values by cell number
###############################
extract(r, c(1:2, 10, 100))
s <- stack(r, sqrt(r), r/r)
extract(s, c(1, 10, 100), layer=2, n=2)

###############################
# extract values with points
###############################
xy <- cbind(-50, seq(-80, 80, by=20))
extract(r, xy)

sp <- SpatialPoints(xy)
extract(r, sp, method='bilinear')

# examples with a buffer
extract(r, xy[1:3,], buffer=1000000)
extract(r, xy[1:3,], buffer=1000000, fun=mean)

## illustrating the varying size of a buffer (expressed in meters) 
## on a longitude/latitude raster
z <- extract(r, xy, buffer=1000000)
s <- raster(r)
for (i in 1:length(z)) { s[z[[i]]] <- i }

## compare with raster that is not longitude/latitude
projection(r) <- "+proj=utm +zone=17" 
xy[,1] <- 50
z <- extract(r, xy, buffer=8)
for (i in 1:length(z)) { s[z[[i]]] <- i }
plot(s)
# library(maptools)
# data(wrld_simpl)
# plot(wrld_simpl, add=TRUE)

###############################
# extract values with lines
###############################

cds1 <- rbind(c(-50,0), c(0,60), c(40,5), c(15,-45), c(-10,-25))
cds2 <- rbind(c(80,20), c(140,60), c(160,0), c(140,-55))
lines <- SpatialLines(list(Lines(list(Line(cds1)), "1"), Lines(list(Line(cds2)), "2") ))

extract(r, lines)
plot(r)
###############################
# extract values with polygons
###############################
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                              Polygons(list(Polygon(cds2)), 2)))

plot(r)
plot(polys, add=TRUE)
v <- extract(r, polys)
v
plot(r)
# mean for each polygon
unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

v <- extract(r, polys, cellnumbers=TRUE)

# weighted mean
v <- extract(r, polys, weights=TRUE, fun=mean)
# equivalent to:
v <- extract(r, polys, weights=TRUE)
sapply(v, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA  )


###############################
# extract values with an extent
###############################
e <- extent(150,170,-60,-40)
extract(r, e)
plot(r)
plot(e, add=T)
