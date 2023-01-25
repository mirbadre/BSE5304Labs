if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,hrbrthemes)
print("hello world :)")

#github connection
system("git config --global user.email 'mbadre@vt.edu' ") 
system("git config --global user.name 'mirbadre' ")

# from class
# ThisLanguageIsCaseSensitive = 1:10*pi 
# ThisLanguageIsCaseSensitive
# runif(10)

#Set variables
Gage <- "04231600" #This is the flowgage I have chosen near Pittsford NY
lat <- 43.0906 #lat and long for Pittsford, NY
lon <- -77.5150

install.packages("rnoaa");
library(rnoaa)

# Find nearby weather station
stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=lat,
  long=lon,
  units = "deg",
  radius = 20,
  limit = NULL
)

#Theres a lot of station so loop thru to find one with our criteria
i <- 0
f <- 0
#attempt at loop 
while (f<5){
  i <- i+1
  WXData=meteo_pull_monitors(
    monitors=stns[i,1],    # replace the *** with index you find
    keep_flags = FALSE,
    date_min = "2016-01-01",
    date_max = NULL,
    var = c("TMAX","TMIN","PRCP") 
  )
  f <- ncol(WXData)
}

#now to make a graph of this data

# Value used to transform the data
coeff <- 10 #temperatures are in tenths of a celsius degree for some reason and precip in tenths of mm

# setting colors
tmaxColor <- "#B22222"
tminColor <- "#ED820E"
PrecipColor <- "#0F52BA"
      
#put together the plot
plot1 = ggplot(WXData, aes(x=date)) +
    geom_line( aes(y=tmax/coeff, color=tmaxColor), size=1, color=tmaxColor) + 
    geom_line( aes(y=tmin/coeff, color=tminColor), size=1, color=tminColor) +
    geom_line( aes(y=prcp*2/100, color=PrecipColor), size=1, color=PrecipColor) +
    scale_y_continuous(name = "Temperature (Celsius °)",sec.axis = sec_axis(~.*.5, name="Precipitation (cm)")) +       theme_ipsum() +
    theme(axis.title.y = element_text(color = tmaxColor, size=13),
        axis.title.y.right = element_text(color = PrecipColor, size=13),
        axis.title.x = element_text(color = 'black', size=13),
        plot.title = element_text(size=16)) +
    labs(x = "Date") +
    scale_color_identity(guide = "legend") + 
    ggtitle("Daily Temperature Minimums and Maximums and Precipitation Values near Pittsford NY") 
plot1
    
#now moving onto to flowgage stuff
#watershed stuff, for hw i think??
source("https://goo.gl/Cb8zGn") # source function
myflowgage <- get_usgs_gage(Gage,begin_date="2016-01-01",end_date="2023-02-01")#make end of semester?
class(myflowgage)
str(myflowgage) # just a diff way to look at it
    
#look at our flow check it out
plot(myflowgage$flowdata$mdate,myflowgage$flowdata$flow,
     main=myflowgage$gagename,xlab = "Date",
     ylab="Flow m^3/day",type="l")
    
#transform flow data 
# aka divide flow data by drainage are in square meters to get flow in m per day
# DA is 2474 mi^2 apparently seems high, which is 6407630584.14 meter^2
    
plot2 <- ggplot() + geom_line(data=WXData, aes(x=WXData$date, y=WXData$prcp*4/100, color = 'a'), size=1, color='lightblue') +
  geom_point(data=WXData, aes(x=WXData$date, y=WXData$tmax/coeff, color = 'a'), size=1, color='red') +
  geom_point(data=WXData, aes(x=WXData$date, y=WXData$tmin/coeff, color = 'a'), size=1, color='orange') +
  geom_line(data=myflowgage$flowdata, aes(x=myflowgage$flowdata$mdate, y=myflowgage$flowdata$flow*1000/(6407630584.14), color = 'a'), size=1, color='blue') + 
  theme_ipsum() + 
  scale_y_continuous(name = "Minimum and Maximum Temperature (Celsius °)",sec.axis = sec_axis(~.*.25, name="Water Depth Precipitation in cm/day and Flow (dark blue) in mm/unit area/day")) + 
  labs(x = "Date", color = "Legend") + scale_color_manual(values = colors) + 
  ggtitle("Daily Temperatures, Precipitation Values, and Streamflow near Pittsford NY") 
    
plot2
    
