#load all of our packages we need
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#set up my github 
system("git config --global user.email 'mbadre@vt.edu' ") 
system("git config --global user.name 'mirbadre' ")

#housekeeping for saving data
Sys.getenv()
Sys.getenv("HOME")
myhomedir=Sys.getenv("HOME")

#Setting up git directory
getwd()
mygitdir=getwd()

# Setting up to save large datasets through ARC and not my github
datadir=paste0(myhomedir,"/data")
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)

#procuring ecohydrology
setwd(srcdir)
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)

# In the future you might want to search around for this
# mygitdir=paste0(myhomedir,"/2023/BSE5304Lab02")
# Mostly, we want to set where we put our homework PDFs
mypdfdir=paste0(mygitdir,"/pdfs/")

# Packages we think we are going to need today :0
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
# https://cran.r-project.org/web/packages/raster/raster.pdf
# https://cran.r-project.org/web/packages/soilDB/soilDB.pdf
# https://cran.r-project.org/web/packages/rgdal/rgdal.pdf

# Get some flow data from my USGS gauge  0422026250 NORTHRUP CREEK AT NORTH GREECE NY
myflowgage_id="0422026250"
#myflowgage_id="0205551460" #This is the lick run basin for class commented out most of the time
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2019-01-01")

# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# The easy way to get my WX Data apparently
?FillMissWX()
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=1,
                  method = "IDEW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
# A few constants for graphing
MinTempCol <- "#0000ff"
MaxTempCol <- "#ff0000"
PCol <- "#000000"
QCol <- PCol
      
coeff=1
#plotting temp and flow for gauge
p1= ggplot(BasinData, aes(x=date)) +
  geom_line( aes(y=MaxTemp), linewidth=1, color=MaxTempCol) + 
  geom_line( aes(y=MinTemp), linewidth=1, color=MinTempCol) + 
  geom_line( aes(y=Qmm), linewidth=1, color=QCol) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temp(C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Depth(mm)")
  ) + 
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = QCol, size=13)
  ) +
  ggtitle(myflowgage$gagename)

p1
basestr=format(Sys.time(),"%Y%m%d%H%M")
filename=paste0(mypdfdir,basestr,"graph01.pdf")
pdf(filename)
plot(p1)
dev.off()
print("file size")
print(file.size(filename))
print("I finished!")

#Reprojecting
#first get our utm zone
trunc((180+myflowgage$declon)/6+1)
proj4_utm = paste0("+proj=utm +zone=", trunc((180+myflowgage$declon)/6+1), " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)

# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"

# Now we will build our proj4strings which define our “Coordinate 
# Reference Systems” or CRS in future geographic manipulations. 
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
print(crs_ll)
print(crs_utm)
# Double check against Figure 1 to confirm we are in the correct UTM Zone.

#whats our flow gage area
myflowgage$area 

latlon <- cbind(myflowgage$declon,myflowgage$declat)
myflowgage$gagepoint_ll <- SpatialPoints(latlon)
proj4string(myflowgage$gagepoint_ll)=proj4_ll
myflowgage$gagepoint_utm=spTransform(myflowgage$gagepoint_ll,crs_utm)
# Open up maps.google.com to guesstimate area/lengths
url=paste0("https://www.google.com/maps/@",
           myflowgage$declat,",",myflowgage$declon,",18z")
browseURL(url)
# We are going to over estimate our area
sqrt(myflowgage$area)   # guestimating square watershed
# For our search we are going to multiply the area by 8 and
# to get the distance
sqrt(myflowgage$area*8)
searchlength=sqrt(myflowgage$area*8)*1000 
pourpoint=SpatialPoints(myflowgage$gagepoint_utm@coords,proj4string = crs_utm)
bboxpts=myflowgage$gagepoint_utm@coords
bboxpts=rbind(bboxpts,bboxpts+searchlength)
bboxpts=rbind(bboxpts,bboxpts-searchlength)
bboxpts
bboxpts=rbind(bboxpts,c(min(bboxpts[,1]),max(bboxpts[,2])))
bboxpts=rbind(bboxpts,c(max(bboxpts[,1]),min(bboxpts[,2])))
bboxpts
bboxpts=SpatialPoints(bboxpts,proj4string = crs_utm)
# From Lab04, get your DEM
mydem=get_aws_terrain(locations=bboxpts@coords, 
                      z = 12, prj = proj4_utm,src ="aws",expand=1)
res(mydem)
plot(mydem)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

# Write our raster to a geotiff file that can be used with
# OS level hydrologic models 
writeRaster(mydem,filename = "mydem.tif",overwrite=T)
# Our quick intro to terminal where the cloud offerings are usually Linux
# ls; cd ~; pwd;  # Linux/Mac 
# dir; cd ; # Windows

zoomext=myflowgage$gagepoint_utm@coords
zoomext=rbind(zoomext,zoomext+res(mydem)*100)
zoomext=rbind(zoomext,zoomext-res(mydem)*100)
zoomext=SpatialPoints(zoomext,proj4string = crs_utm)  
zoom(mydem,ext=zoomext)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

#command line commands to set up Tau DEM
# cd ~/src      # Set your directory to your home directory
# git clone https://github.com/dtarb/TauDEM.git
# mkdir ~/src/TauDEM/bin
# cd ~/src/TauDEM/src
# make
# # sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
# sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
# sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
# make

old_path <- Sys.getenv("PATH")
old_path
if( ! grepl("~/src/TauDEM/bin",old_path)){
  Sys.setenv(PATH = paste(old_path,
                          paste0(Sys.getenv("HOME"),"/src/TauDEM/bin"), 
                          sep = ":"))
}  

system("mpirun aread8")

#  R script to run TauDEM
setwd(datadir)

z=raster("mydem.tif")
p2 <- plot(z, main="DEM for Northrup Creek in UTM Zone 18",
           xlab="Meters",
           ylab="Meters")
#saving plotted DEM to pdf
filename=paste0(mypdfdir,"Lab02graph01.pdf")
pdf(file=filename) 
p2
dev.off()

# Pitremove
system("mpiexec -n 2 pitremove -z mydem.tif -fel mydemfel.tif")
fel=raster("mydemfel.tif")
p3 <- plot(fel, main="Filled DEM for Northrup Creek in UTM Zone 18",
           xlab="Meters",
           ylab="Meters")
filename=paste0(mypdfdir,"Lab02FDgraph02.pdf")
pdf(filename) 
p3
dev.off()

#Here we should plot the difference between the filled raster and the og DEM
diff<- overlay(fel,z,fun=function(r1, r2){return(r1-r2)})

p4 <- plot(diff,
     main="Difference between Filled and Orginial DEM in UTM Zone 18",
     xlab="Meters",
     ylab="Meters")
filename=paste0(mypdfdir,"Lab02Diffgraph03.pdf")
pdf(filename) 
p4
dev.off()

# D8 flow directions
system("mpiexec -n 2 d8flowdir -p mydemp.tif -sd8 mydemsd8.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
p=raster("mydemp.tif")
plot(p)
sd8=raster("mydemsd8.tif")
plot(sd8)

# Contributing area
system("mpiexec -n 2 aread8 -p mydemp.tif -ad8 mydemad8.tif")
ad8=raster("mydemad8.tif")
plot(log(ad8))
#zoomext2(log(ad8))


# Grid Network 
system("mpiexec -n 2 gridnet -p mydemp.tif -gord mydemgord.tif -plen mydemplen.tif -tlen mydemtlen.tif")
gord=raster("mydemgord.tif")
plot(gord)
#zoom(gord)

# DInf flow directions
system("mpiexec -n 2 dinfflowdir -ang mydemang.tif -slp mydemslp.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
ang=raster("mydemang.tif")
plot(ang)
slp=raster("mydemslp.tif")
plot(slp)


# Dinf contributing area
system("mpiexec -n 2 areadinf -ang mydemang.tif -sca mydemsca.tif")
sca=raster("mydemsca.tif")
plot(log(sca))
#zoom(log(sca))

# Threshold
system("mpiexec -n 2 threshold -ssa mydemad8.tif -src mydemsrc.tif -thresh 100")
src=raster("mydemsrc.tif")
plot(src)
#zoom(src,extent=zoomext2)

# a quick R function to write a shapefile
makeshape.r=function(sname="shape",n=1)
{
  xy=locator(n=n)
  points(xy)
  
  #Point
  dd <- data.frame(Id=1:n,X=xy$x,Y=xy$y)
  ddTable <- data.frame(Id=c(1),Name=paste("Outlet",1:n,sep=""))
  ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
  write.shapefile(ddShapefile, sname, arcgis=T)
}

#Outlet points
outlet=SpatialPointsDataFrame(myflowgage$gagepoint_utm,
                                data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
           driver="ESRI Shapefile", overwrite_layer=TRUE)


# Move Outlets
system("mpiexec -n 2 moveoutletstostrm -p mydemp.tif -src mydemsrc.tif -o approxoutlets.shp -om outlet.shp")
outpt=read.shp("outlet.shp")
approxpt=read.shp("approxoutlets.shp")

plot(src)
points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

#zoom(src)


# Contributing area upstream of outlet
system("mpiexec -n 2 aread8 -p mydemp.tif -o outlet.shp -ad8 mydemssa.tif")
ssa=raster("mydemssa.tif")
plot(ssa) 


# Threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh 2000")
src1=raster("mydemsrc1.tif")
plot(src1)
#zoom(src1, extent=zoomext2)

# Stream Reach and Watershed
system("mpiexec -n 2 streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc1.tif -o outlet.shp -ord mydemord.tif -tree mydemtree.txt -coord mydemcoord.txt -net mydemnet.shp -w mydemw.tif")
plot(raster("mydemord.tif"))
#zoom(raster("mydemord.tif"))
plot(raster("mydemw.tif"), main="Northrup Creek Watershed in UTM Zone 18",
     xlab="Meters",
     ylab="Meters")


