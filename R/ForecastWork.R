if (!require('pacman')) install.packages('pacman')
pacman::p_load(meteoForecast)
# other packages would need for forecasting flow?
# pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
# pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
#                rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)


gfsvars=grepVar('precip', service='gfs', complete=TRUE)
gfsvarstemp=grepVar('temp', service='gfs', complete=TRUE)

testDay <- Sys.Date() - 1
today = Sys.Date()

myflowgage_id='0205551460'
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",end_date = today)

# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max=today,targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

library(lattice)

#precip
vars <- getPoint(c(myflowgage$declon,myflowgage$declat),
                 vars='Total_precipitation_surface_Mixed_intervals_Accumulation',
                 service='gfs',
                 day=today-1)
precip=aggregate(vars,as.Date(time(vars)),sum)
xyplot(precip)

#temp
tempvars <- getPoint(c(myflowgage$declon,myflowgage$declat),
                 vars='Temperature_surface',
                 service='gfs',
                 day=today-1)

temp_max=aggregate(tempvars,as.Date(time(tempvars)),max)
temp_min=aggregate(tempvars,as.Date(time(tempvars)),min)

plot(temp_min, ylim=c(270,300))
lines(temp_max)

#Here we should add forecast data to WXData
#so that we could forecast flow, but don't worry about it

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")

dir.create("~/pngs")
setwd("~/pngs")
graphdir="~/pngs"
#create temp png
png(paste0(graphdir,"/TempForecast.png"))
plot(temp_min, ylim=c(275,300),xlab="Date", ylab="Kelvin")
lines(temp_max)
title(main="Forecasted Temperature Range in Kelvin")
dev.off()

#create precip png
png(paste0(graphdir,"/PrecipForecast.png"))
plot(precip, ylab ="Precipitation in cm", xlab="Date")
title(main="Forecasted Precipitation")
dev.off()






