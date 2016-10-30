# Load required packages
if (!require("maptools")){install.packages("maptools")}
if (!require("leaflet")){install.packages("leaflet")}

# Import and read the John Snow Cholera data 
tmpdir<-tempdir()
url<-'http://rtwilson.com/downloads/SnowGIS_SHP.zip'
file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile1<-paste(tmpdir,"\\SnowGIS_SHP\\Cholera_Deaths",sep="")
deaths<-readShapePoints(shapeFile1)
shapeFile2<-paste(tmpdir,"\\SnowGIS_SHP\\Pumps",sep="")
pumps<-readShapePoints(shapeFile2)

# Transform coordinates
proj4string(deaths)<-CRS("+init=epsg:27700")
proj4string(pumps)<-CRS("+init=epsg:27700")
deaths.trans<-spTransform(deaths,CRS("+proj=longlat +datum=WGS84"))
pumps.trans<-spTransform(pumps,CRS("+proj=longlat +datum=WGS84"))

death<-data.frame(Long=deaths.trans@coords[,1],Lat=deaths.trans@coords[,2],Count=deaths.trans@data$Count,Count.c=paste("Number of deaths: ",deaths.trans@data$Count))
pump<-data.frame(Long=pumps.trans@coords[,1],Lat=pumps.trans@coords[,2],text="Water pump")

# Plot the cholera data on an interactive map using leaflet
leaflet(death) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 3, col="red",
    radius = ~Count*2, popup = ~Count.c
  ) %>% addMarkers(~pump$Long, ~pump$Lat, popup= ~pump$text)


