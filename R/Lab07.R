#
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
# A useful function to make sure you don't have anything residually attached
# from your last session.
#siteNo = "0205551460"
#myflowgage=get_usgs_gage(siteNo,begin_date = "2022-01-01",end_date="2023-01-01")
search()
# Or to see what might be attached
intersect(search(), objects())
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.

#
# What is going to change from use case to use case 
Sys.getenv('USER')
LabNo="/Lab07"
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'mbadre@vt.edu' ") 
system("git config --global user.name 'mirbadre' ")
system("git config pull.rebase false")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,EcoHydRology,curl,elevatr,raster,rgdal,
                 data.table,foreign,maptools,dataRetrieval,gdistance)
setwd(datadir)
#
# Note we have a new library to access USGS Waterdata
# https://owi.usgs.gov/R/dataRetrieval.html
# https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-readNWIS/
#
?dataRetrieval  # Review the man page for this package
?readNWISuv
?readNWISdv
?readNWISdata
#
# Need to figure out which data to download. 
# https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units
# 
url="https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
browseURL(url)
#
# Yeah, these databases are complex to get to know, remember our 
# SSURGO?
#
# Before you begin your modeling project, confirm what your model outputs 
# has a value to calibrate against, i.e. match parameter and units. For 
# this lab we are looking for Gage Height, while historically, we have been 
# looking at Discharge. NOT ALL PARAMETERS ARE AVAILABLE!
#
url="https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
browseURL(url)
##############################################
# 0205551460 LICK RUN ABOVE PATTON AVENUE AT ROANOKE, VA
##############################################

make_usgs_gage_list=function(siteNo = "0205551460",parameterCd = c("00060","00065"),start.date = "2017-05-01",  end.date = "2017-11-01"){
  #
  # For each gage location, let's keep the data organized as a 
  # list.
  USGS=list()   # Organize the data in a nice list as in previous labs
  USGS[["flowdata"]]<- readNWISuv(siteNumbers = siteNo,parameterCd = parameterCd,startDate = start.date,endDate = end.date)
  head(USGS$flowdata)  
  #he used view(USGS$flowdata)
  # Note that we have 00060 and 00065...
  #  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
  #1  	USGS 0205551460 2017-05-01 04:00:00      	6.38            	A
  #2  	USGS 0205551460 2017-05-01 04:05:00      	6.38            	A
  #  X_00065_00000 X_00065_00000_cd tz_cd
  #1      	2.74            	A   UTC
  #2      	2.74            	A   UTC
  #
  # And of course we want to work in SI units so:
  USGS$flowdata$depth_m=USGS$flowdata$X_00065_00000*0.3048
  # m/ft depth
  USGS$flowdata$cms=USGS$flowdata$X_00060_00000*.02832
  # m3/ft3 flow
  #
  # Let's add in the USGS gage site information to the list and inspect
  USGS[["site"]]=readNWISsite(siteNo)
  head(USGS$site)
  class(USGS$site$dec_lat_va)
  #
  # Set the Manning Coefficient in the USGS Gage's Site Table
  #
  url="https://www.google.com/search?q=manning%27s+n+for+stream"
  #browseURL(url)
  url="https://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm"
  #browseURL(url)
  USGS$site$man_n=.035/1.49
  #
  # Create a SpatialPointsDataFrame out of the site dataframe in the USGS list
  coordinates(USGS$site)=~dec_long_va+dec_lat_va
  return(USGS)
}

#Now try to run function

testlist = make_usgs_gage_list(siteNo = "0205551460",
                             parameterCd = c("00060","00065"),
                             start.date = "2017-05-01",  # Not frozen to not frozen
                             end.date = "2017-11-01"    # to still not frozen
)
#compare
#pacman::p_load(useful)
#compare.list(testlist,USGS0205551460)
#
# Of course since somebodies said they hate making functions we
# will go through this here to show how fun it is!
#

# If done correctly, your function should be able to populate
# the lists for the remaining gages!
#
USGS02056000=make_usgs_gage_list(siteNo = "02056000")
USGS0205551460=make_usgs_gage_list(siteNo ="0205551460" )
USGS02055100=make_usgs_gage_list(siteNo ="02055100" )
USGS02055000=make_usgs_gage_list(siteNo ="02055000" )
USGS02054530=make_usgs_gage_list(siteNo ="02054530" )

#Data from DEM:
ab_ll=rbind(USGS02056000$site,
                USGS0205551460$site,
                USGS02055100$site,
                USGS02055000$site,
                USGS02054530$site)
class(ab_ll)
ab_ll@proj4string
proj4_utm = paste0("+proj=utm +zone=",
                     trunc((180+coordinates(USGS02055000$site)[1])/6+1), 
                     " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)
# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
proj4string(ab_ll)=proj4_ll
ab_utm=spTransform(ab_ll,crs_utm)
ab_utm@coords
mydem=get_aws_terrain(locations=ab_utm@coords, 
                        z = 12, prj = proj4_utm,expand=1)
#
# Lets plot the DEM and the gage locations so we can guess 
# what gages connect with what gages
#
plot(mydem)
plot(ab_utm,add=T)
text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)

# Streams as USGS sees them, I know I can get an overview of streams with the 
# USGS H
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
streams_utm=spTransform(streams,crs_utm)
plot(streams_utm,col="blue",add=T)

####
#Prob 2, Rating Curve
####
#
# Breaking things for educational purposes
#View(USGS02056000$flowdata)
USGS02056000$flowdata=USGS02056000$flowdata[,c(1,2,3,4,5,8,10)]
#View(USGS02056000$flowdata)
# Oh Noooooo!!!! This gage for some reason doesn't have "Gage height"
# 00065! What can we do!?!? OK, no worries, we do have "Discharge" 00060	
###########################################
# 02056000 ROANOKE RIVER AT NIAGARA, VA
###########################################
# Assume in the inventory link that for this gage, our Gage height is missing. 
head(USGS02056000$flowdata,2)  # Note that we have 00060 but missing 00065...
#  agency_cd  site_no            dateTime X_00060_00000 X_00060_00000_cd tz_cd
#1      USGS 02056000 2017-05-01 04:00:00           876                A   UTC
#2      USGS 02056000 2017-05-01 04:15:00           876                A   UTC
# Hrm? No depth associated with the flow? BUT USGS maintains rating curves
# explain what a rating curve is: https://en.wikipedia.org/wiki/Rating_curve
# and use the readNWISrating() function to grab it for this gage
USGS02056000[["rating"]]=readNWISrating(USGS02056000$site$site_no)
plot(USGS02056000$rating$DEP,USGS02056000$rating$INDEP,xlab="DEP",ylab="INDEP")
#

# Note that this is very similar to what we saw in the previous gage's results
# and as it turns out, we can use it to estimate a 00065 measurement as 
# we did for the previous gage.
USGS02056000$flowdata$X_00065_00000=approx(USGS02056000$rating$DEP,
                                             USGS02056000$rating$INDEP, xout = USGS02056000$flowdata$X_00060_00000, ties = min)$y
points(USGS02056000$flowdata$X_00060_00000,USGS02056000$flowdata$X_00065_00000,
         col="red")
#
USGS02056000$flowdata$depth_m=USGS02056000$flowdata$X_00065_00000*0.3048
# m/ft depth
#
vignette("Overview", package = "gdistance")
# Set the starting and ending locations
# determine the river reach length and slope using the gdistance package.
#
A=SpatialPoints(USGS0205551460$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd <- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS0205551460$site$L=SpatialLinesLengths(AtoB) # km to m
USGS0205551460$site$L # reach length in m

USGS0205551460$site$slope=(extract(mydem,A_utm)-
                               extract(mydem,B_utm))/USGS0205551460$site$L
USGS0205551460$site$slope
# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So))
#Using new flow relationship with -0.8m
USGS0205551460$flowdata$B=(USGS0205551460$site$man_n*
                               USGS0205551460$flowdata$cms)/((USGS0205551460$flowdata$depth_m-0.8)^(5/3)*
                                                               sqrt(USGS0205551460$site$slope))

head(USGS0205551460$flowdata)
#  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
#1  	USGS 05267000 2017-05-01 04:00:00      	6.38            	A
#2  	USGS 05267000 2017-05-01 04:05:00      	6.38            	A
#  X_00065_00000 X_00065_00000_cd tz_cd   	cms  depth_m    	B
#1      	2.74            	A   UTC 0.1806816 0.835152 0.103032
#2      	2.74            	A   UTC 0.1806816 0.835152 0.103032
#
# Lets look at how B changes with flow.    
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$B, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA")
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(USGS0205551460$flowdata$cms,USGS0205551460$flowdata$depth_m - 0.8, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA")




#Prob 4 and last question
#############################
# ck
USGS0205551460$site$slope
USGS0205551460$flowdata$ck = 5/3*((sqrt(USGS0205551460$site$slope))/USGS0205551460$site$man_n)*(USGS0205551460$flowdata$depth_m)^(2/3)
# ANS
mean(USGS0205551460$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS0205551460$flowdata$dt = USGS0205551460$site$L/USGS0205551460$flowdata$ck
mean(USGS0205551460$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$dt)
USGS0205551460$flowdata$outTime=USGS0205551460$flowdata$dateTime+
  USGS0205551460$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.10
USGS0205551460$flowdata$newwave=
  USGS0205551460$flowdata$cms *WaveStartDecPercent <
  data.table::shift(USGS0205551460$flowdata$cms)
summary(USGS0205551460$flowdata$newwave)
# Add plot of the point found
len=length(USGS0205551460$flowdata$newwave)
USGS0205551460$flowdata$newwave[is.na(USGS0205551460$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS0205551460$flowdata$newwave[i]==T &
     USGS0205551460$flowdata$newwave[i-1]==T){
    USGS0205551460$flowdata$newwave[i]=F
  }
}
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,type="l")
points(USGS0205551460$flowdata$dateTime[USGS0205551460$flowdata$newwave],
         USGS0205551460$flowdata$cms[USGS0205551460$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(USGS0205551460$flowdata$newwave == TRUE)
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,
       type="l",xlim=c(USGS0205551460$flowdata$dateTime[1109],
                       USGS0205551460$flowdata$dateTime[1109+200]))
lines(USGS0205551460$flowdata$outTime,USGS0205551460$flowdata$cms,col=2)

#Create function that creates dt plot for wave from gage location to gage USGS02056000
wavedt_func = function(siteA=USGS02055100, siteB=USGS02056000, mydem=cropmydem){
  A=SpatialPoints(siteA$site)# Up gradient site Lick Run
  B=SpatialPoints(siteB$site) # Down gradient site ROA River atNiagara
  proj4string(A)=proj4_ll
  proj4string(B)=proj4_ll
  A_utm=spTransform(A,crs_utm)
  B_utm=spTransform(B,crs_utm)
  # Cut the DEM down to a more manageable size
  #cropmydem=crop(mydem,extend(extent(ab_utm),600))
  #cropmydem=trim(cropmydem)
  #cropmydem=cropmydem*1000.0
  plot(mydem)
  plot(ab_utm,add=T)
  # Set up the weighting functions
  altDiff <- function(x){x[2] - x[1]}
  hd <- transition(mydem, altDiff, 8, symm=FALSE)
  slope <- geoCorrection(hd)
  adj <- adjacent(mydem, cells=1:ncell(mydem), pairs=TRUE, directions=8)
  speed <- slope
  speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
  Conductance <- geoCorrection(speed)
  # Find and plot the flow path
  AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
  plot(AtoB,add=T)
  plot(streams_utm,col="blue",add=T)
  plot(AtoB,add=T)
  SpatialLinesLengths(AtoB)
  siteA$site$L=SpatialLinesLengths(AtoB) # km to m
  siteA$site$L # reach length in m
  
  siteA$site$slope=(extract(mydem,A_utm)-extract(mydem,B_utm))/siteA$site$L
  siteA$site$slope
  # So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
  # B=(n*Q)/(y^(5/3)*sqrt(So))
  siteA$flowdata$B=(siteA$site$man_n*
                             siteA$flowdata$cms)/((siteA$flowdata$depth_m-0.8)^(5/3)*
                                                               sqrt(siteA$site$slope))
  
  head(siteA$flowdata)
  rating_curve <- plot(siteA$flowdata$dateTime,siteA$flowdata$B, main="Site A")
  # Does this seem reasonable (...like order of magnitude reasonable)? You can 
  # perform a quick and dirty check using google earth and measuring the channel 
  # width in a few places.
  #
  plot(siteA$flowdata$cms,siteA$flowdata$depth_m - 0.8, main="Site A")
  
    # ck
  siteA$site$slope
  siteA$flowdata$ck = 5/3*((sqrt(siteA$site$slope))/siteA$site$man_n)*(siteA$flowdata$depth_m)^(2/3)
  # ANS
  mean(siteA$flowdata$ck,na.rm=T)
  # [1] 2.547238 for this example, confirm this result
  siteA$flowdata$dt = siteA$site$L/siteA$flowdata$ck
  mean(siteA$flowdata$dt,na.rm=T)
  # [1] 6328.655  for this example, confirm this result
  plot(siteA$flowdata$dateTime,siteA$flowdata$dt)
  siteA$flowdata$outTime=siteA$flowdata$dateTime+
    siteA$flowdata$dt
  
  # Find the beginning of  Waves assuming a new wave starts at 110% of prior 
  # flow. This might need to change for your homework
  WaveStartDecPercent=3.5
  siteA$flowdata$newwave=
    siteA$flowdata$cms *WaveStartDecPercent <
    data.table::shift(siteA$flowdata$cms)
  summary(siteA$flowdata$newwave)
  # Add plot of the point found
  len=length(siteA$flowdata$newwave)
  siteA$flowdata$newwave[is.na(siteA$flowdata$newwave)]=F
  # Removes repeated finds by going through loop backwords
  for (i in seq(len,2)){
    print(i)
    if(siteA$flowdata$newwave[i]==T &
       siteA$flowdata$newwave[i-1]==T){
      siteA$flowdata$newwave[i]=F
    }
  }
  plot(siteA$flowdata$dateTime,siteA$flowdata$cms,type="l")
  points(siteA$flowdata$dateTime[siteA$flowdata$newwave],
         siteA$flowdata$cms[siteA$flowdata$newwave],col=2)
  
  # Find the time locations where waves begin
  which(siteA$flowdata$newwave == TRUE)
  line_plot <-plot(siteA$flowdata$dateTime,siteA$flowdata$cms,
       type="l",xlim=c(siteA$flowdata$dateTime[1109],
                       siteA$flowdata$dateTime[1109+200]))
  line_plot <- line_plot + lines(siteA$flowdata$outTime,siteA$flowdata$cms,col=2)
  return(line_plot)
}

wavedt_func(siteA=USGS02055100, siteB=USGS02056000, mydem=cropmydem)

wavedt_func(siteA=USGS02054530, siteB=USGS02056000, mydem=cropmydem)

wavedt_func(siteA=USGS02055000, siteB=USGS02056000, mydem=cropmydem)

#copy paste code instead of function
sitea=USGS02055000

A=SpatialPoints(sitea$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd <- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
sitea$site$L=SpatialLinesLengths(AtoB) # km to m
sitea$site$L # reach length in m

sitea$site$slope=(extract(mydem,A_utm)-
                             extract(mydem,B_utm))/sitea$site$L
sitea$site$slope
# So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
# B=(n*Q)/(y^(5/3)*sqrt(So))
#Using new flow relationship with -0.8m
sitea$flowdata$B=(sitea$site$man_n*
                             sitea$flowdata$cms)/((sitea$flowdata$depth_m-0.8)^(5/3)*
                                                             sqrt(sitea$site$slope))

head(sitea$flowdata)
#  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
#1  	USGS 05267000 2017-05-01 04:00:00      	6.38            	A
#2  	USGS 05267000 2017-05-01 04:05:00      	6.38            	A
#  X_00065_00000 X_00065_00000_cd tz_cd   	cms  depth_m    	B
#1      	2.74            	A   UTC 0.1806816 0.835152 0.103032
#2      	2.74            	A   UTC 0.1806816 0.835152 0.103032
#
# Lets look at how B changes with flow.    
plot(sitea$flowdata$dateTime,sitea$flowdata$B, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA")
# Does this seem reasonable (...like order of magnitude reasonable)? You can 
# perform a quick and dirty check using google earth and measuring the channel 
# width in a few places.
#
plot(sitea$flowdata$cms,sitea$flowdata$depth_m - 0.8, main="LICK RUN TO ROANOKE RIVER AT NIAGARA, VA")




#Prob 4 and last question
#############################
# ck
sitea$site$slope
sitea$flowdata$ck = 5/3*((sqrt(sitea$site$slope))/sitea$site$man_n)*(sitea$flowdata$depth_m)^(2/3)
# ANS
mean(sitea$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
sitea$flowdata$dt = sitea$site$L/sitea$flowdata$ck
mean(sitea$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result
plot(sitea$flowdata$dateTime,sitea$flowdata$dt)
sitea$flowdata$outTime=sitea$flowdata$dateTime+
  sitea$flowdata$dt

# Find the beginning of  Waves assuming a new wave starts at 110% of prior 
# flow. This might need to change for your homework
WaveStartDecPercent=1.50
sitea$flowdata$newwave=
  sitea$flowdata$cms *WaveStartDecPercent <
  data.table::shift(sitea$flowdata$cms)
summary(sitea$flowdata$newwave)
# Add plot of the point found
len=length(sitea$flowdata$newwave)
sitea$flowdata$newwave[is.na(sitea$flowdata$newwave)]=F
# Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(sitea$flowdata$newwave[i]==T &
     sitea$flowdata$newwave[i-1]==T){
    sitea$flowdata$newwave[i]=F
  }
}
plot(sitea$flowdata$dateTime,sitea$flowdata$cms,type="l")
points(sitea$flowdata$dateTime[sitea$flowdata$newwave],
       sitea$flowdata$cms[sitea$flowdata$newwave],col=2)

# Find the time locations where waves begin
which(sitea$flowdata$newwave == TRUE)
plot(sitea$flowdata$dateTime,sitea$flowdata$cms,
     type="l",xlim=c(sitea$flowdata$dateTime[1109],
                     sitea$flowdata$dateTime[1109+200]))
lines(sitea$flowdata$outTime,sitea$flowdata$cms,col=2)
