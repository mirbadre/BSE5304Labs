# 
# Start by clearing our environment up
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.
#
# Seeting up basics for this lab 
LabNo="/Lab05"
myflowgage_id="0205551460"  # Old Friendly Gage
#myflowgage_id="0422026250" #My flow gauge from last week
#
# libraries
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'mbadre@vt.edu' ") 
system("git config --global user.name 'mirbadre' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)
#detach("package:EcoHydRology", unload = TRUE)
# remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

#Grab our model functions from github
# Grab out models for Snow and TMWB
# https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R
# becomes: 
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R"
# This will grab the solution for last weeks Lab03 Homework
download.file(url,"TMWBFuncs.R")
#file.edit("TMWBFuncs.R") 
source("TMWBFuncs.R")
# I actually am starting to trust my snow model
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R"
# This will grab the solution for last weeks Lab03 Homework
source(url)
#Ran these once file opened

setwd(datadir)


myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(paste0("reported Area ",myflowgage$area))
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
# Remove this if it syncs with the EcoHydrology Version.
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
TMWB=BasinData
#
# Next steps would be to Delineate and Initialize the basin
# TauDEMBasinInit might be in the github folder!
#
#testing CNmodel DEoptim parameter results
#first get our CNmodel in here :)
source('https://raw.githubusercontent.com/mirbadre/BSE5304Labs/main/R/CNModel.R')
# CNModel =function(BasinData,CNavg = 75,IaFrac = 0.05,fnc_slope=0,fnc_aspect=0,func_DAWC=.3,func_z=1000,fnc_fcres=.3)
#from model we need basin data, CNavg,IaFrac,slope, aspect, DAWC, Z,fcres
CNout <- CNModel(BasinData = BasinData) #run CNmodel
1-NSE (CNout$Qmm,CNout$Qpred)

CNoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  CNout=CNModel(BasinData, CNavg = x1, IaFrac = x2,fnc_slope = x3,
                fnc_aspect = x4, func_DAWC= x5,func_z = x6, fnc_fcres = x7)
  1-NSE (CNout$Qmm,CNout$Qpred)
}
lower <- c(30,0.01,0,0,0.1,300,0.1)
upper <- c(95,0.5,20,360,0.3,3000,0.95)                    #aspect 0-360??

CNoutDeopt <- DEoptim(CNoptFunc, lower, upper,DEoptim.control(NP = 100, itermax=20,F=1.2,CR=0.7))
CNoutDeopt
#Testing DEoptim TMWBmodel parameter results
TMWBout <- TMWBmodel(TMWBdf = TMWB)
1-NSE(TMWBout$Qmm,TMWBout$Qpred)


#TMWBdf,fcres=.3,FldCap=.45,WiltPt=.15,Z=1000, SFTmp=2,bmlt6=4.5,bmlt12=0.0,Tmlt=3,Tlag=1

TMWBoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  x8 <- x[8]
  x9 <- x[9]
  TMWBout=TMWBmodel(TMWBdf = TMWB,fcres = x1,FldCap = x2,WiltPt = x3,Z = x4,
                    SFTmp = x5,bmlt6 = x6,bmlt12 = x7, Tmlt = x8, Tlag = x9)
  1-NSE (TMWBout$Qmm,TMWBout$Qpred)
}
lower <- c(0.1,0.05,0.1,300,1,1,0,1,0)#dont make snomelt temp less than 1
upper <- c(0.95,0.4,0.3,3000,6,5,6,6,1)

outTMWBDeopt <- DEoptim(TMWBoptFunc, lower, upper,
        DEoptim.control(NP = 500,
        itermax=20,F=1.2,CR=0.7))
outTMWBDeopt
detach(TMWB)



#SWAT Stuff
file.edit("~/Lab05SetupDRF.R")
setwd("~/src/")
install.packages(c("ecohydrology/pkg/SWATmodel/"),repos = NULL)
pacman::p_load(SWATmodel)

#make sure we have weather data for every single day
AllDays=data.frame(date=seq(min(myflowgage$flowdata$mdate), by = "day", 
          length.out = max(myflowgage$flowdata$mdate)-min(myflowgage$flowdata$mdate)))
WXData=merge(AllDays,WXData,all=T)
WXData$PRECIP=WXData$P
WXData$PRECIP[is.na(WXData$PRECIP)]=-99
WXData$TMX=WXData$MaxTemp
WXData$TMX[is.na(WXData$TMX)]=-99
WXData$TMN=WXData$MinTemp
WXData$TMN[is.na(WXData$TMN)]=-99
WXData$DATE=WXData$date
#making swat init in the directory with the same name as usgs gagename
build_swat_basic(dirname= myflowgage$gagename, iyr=min(year(WXData$DATE),na.rm=T),
                   nbyr=(max(year(WXData$DATE),na.rm=T)-min(year(WXData$DATE),na.rm=T) +1),
                   wsarea=myflowgage$area, elev=myflowgage$elev, declat=myflowgage$declat,
                   declon=myflowgage$declon, hist_wx=WXData)
# 
# Wait for Dan!
#

build_wgn_file() #wgn func
runSWAT2012() #run swat 
