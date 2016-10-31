#################################################################################################################################################
# The R code below creates an interactive map showing the nmber of people in Denmark commuting between municipalities
# Commuting data can be obtained from statistics Denmark
# Administrative borders corresponding to municipalities are avilable from kortforsyningen
#################################################################################################################################################
 

# Load required packages
if (!require("maptools")){install.packages("maptools")}
if (!require("leaflet")){install.packages("leaflet")}

# Create popup text
pendler_to<-read.csv("C:/Users/zxv412/Documents/02_omicron/commuting-for-roskilde/pendler1.csv")
names(pendler_to)<-c("home","work","n")
pendler_from<-read.csv("C:/Users/zxv412/Documents/02_omicron/commuting-for-roskilde/pendler2.csv")
names(pendler_from)<-c("home","work","n")

municipalities<-unique(pendler_to$home)

popup_to<-vector()
popup_from<-vector()
popup_all<-vector()

for(i in 1:99){
temp01<-subset(pendler_to,home==as.character(municipalities[i]))
popup_to[i]<-paste("<strong>Living in",temp01$home[1],", working in:</strong><br/>",temp01$work[1],":",temp01$n[1],"<br/>",temp01$work[2],":",temp01$n[2],"<br/>",temp01$work[3],":",temp01$n[3],"<br/>",
temp01$work[4],":",temp01$n[4],"<br/>",temp01$work[5],":",temp01$n[5],"<br/>",temp01$work[6],":",temp01$n[6],"<br/>",temp01$work[7],":",temp01$n[7],"<br/> Other:",sum(temp01$n[8:100]),
"<br/><strong>Total working:",sum(temp01$n),"</strong>")

temp01<-subset(pendler_from,work==as.character(municipalities[i]))
popup_from[i]<-paste("<strong>Working in",temp01$work[1],", living in:</strong><br/>",temp01$home[1],":",temp01$n[1],"<br/>",temp01$home[2],":",temp01$n[2],"<br/>",temp01$home[3],":",temp01$n[3],"<br/>",
temp01$home[4],":",temp01$n[4],"<br/>",temp01$home[5],":",temp01$n[5],"<br/>",temp01$home[6],":",temp01$n[6],"<br/>",temp01$home[7],":",temp01$n[7],"<br/> Other:",sum(temp01$n[8:99]),
"<br/><strong>Total working:",sum(temp01$n),"</strong>")

popup_all[i]<-paste(popup_to[i],popup_from[i],sep="<br/><br/>")
}

# Calculate the total number of persons going in and out of the municipality = used for coloring
going_out<-vector()
working_in<-vector()
commuting<-vector()

for(i in 1:99){
# Total number going out
temp01<-subset(pendler_to,home==as.character(municipalities[i]) & as.character(home)!=as.character(work))
going_out[i]<-sum(temp01$n)

# Total number working in
temp01<-subset(pendler_from,work==as.character(municipalities[i]) & as.character(home)!=as.character(work))
working_in[i]<-sum(temp01$n)

commuting[i]<-going_out[i]+working_in[i]
}

commuting<-data.frame(id=municipalities,text=popup_all,n=commuting)

# Merge popup text and number for coloring with .shp file
commuting$id<-gsub("Høje-Taastrup","Høje Taastrup",commuting$id)
commuting$id<-gsub("Århus","Aarhus",commuting$id)
#kommuner<-readShapePoly("C:/Users/zxv412/Documents/DAGIREF_SHAPE_UTM32-EUREF89/ADM/KOMMUNE")
#kommuner<-merge(kommuner,commuting,by.x="KOMNAVN",by.y="id")
#proj4string(kommuner)<-CRS("+proj=utm +zone=32 +datum=WGS84")
#kommuner<-spTransform(kommuner,CRS("+proj=longlat +datum=WGS84"))

kommuner<-readShapePoly("C:/Users/zxv412/Documents/02_omicron/environmental-statistics/spatial-stats-presentation/shape-files/de_98_kommunegraenser_i_danmark")
kommuner<-merge(kommuner,commuting,by.x="STEDNAVN",by.y="id")

# Create interactive map
pal<-colorQuantile("YlGn",NULL,n=15)
leaflet(data=kommuner) %>% addProviderTiles("CartoDB.Positron") %>% addPolygons(fillColor=~pal(n),fillOpacity=0.8,color="#BDBDC3",weight=1,popup=~text)



