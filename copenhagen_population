###################################################################################################################################
# - The code below puts Copenhagen population data on an interactive map using the leaflet package in R
# - The colors of the interactive map corresponds to the population in 2015 by roder
# - Pop-ups are added showing the population in 1992, the population in 2015, the percentage increase in the population from 
#   1992 to 2015 as well as the age distribution in 2015
###################################################################################################################################

# Load required packages
if (!require("maptools")){install.packages("maptools")}
if (!require("leaflet")){install.packages("leaflet")}

# Import administrative borders (in this case the almost 400 roder in Copenhagen)
tmpdir<-tempdir()
url<-'http://wfs-kbhkort.kk.dk/k101/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=k101:rode&outputFormat=SHAPE-ZIP&SRSNAME=EPSG:4326'
file<-'roder.zip'
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile<-paste(tmpdir,"\\rode",sep="")
roder.cph<-readShapePoly(shapeFile)

# Import population data
pop1<-read.csv(url("http://data.kk.dk/dataset/dfbe2720-1c0d-4dee-8f48-2ae1f25091b0/resource/e287642f-c16c-4fcd-ab86-9053ad0521c0/download/befolkningkbh.csv"))

# Calculate population by rode for year 2015
pop2<-subset(pop1,AAR==2015)
pop3<-aggregate(PERSONER~RODE,pop2,sum)
names(pop3)<-c("RODE","p2")

# Calculate population by rode for year 1992
pop4<-subset(pop1,AAR==1992)
pop5<-aggregate(PERSONER~RODE,pop4,sum)
names(pop5)<-c("RODE","p1")

# Calculate by age and rode for year 2015
pop6<-subset(pop1,AAR==2015 & ALDER<20)
pop6.1<-aggregate(PERSONER~RODE,pop6,sum)
names(pop6.1)<-c("RODE","age1")

pop7<-subset(pop1,AAR==2015 & ALDER>=20 & ALDER<40)
pop7.1<-aggregate(PERSONER~RODE,pop7,sum)
names(pop7.1)<-c("RODE","age2")

pop8<-subset(pop1,AAR==2015 & ALDER>=40 & ALDER<60)
pop8.1<-aggregate(PERSONER~RODE,pop8,sum)
names(pop8.1)<-c("RODE","age3")

pop9<-subset(pop1,AAR==2015 & ALDER>=60 & ALDER<80)
pop9.1<-aggregate(PERSONER~RODE,pop9,sum)
names(pop9.1)<-c("RODE","age4")

pop10<-subset(pop1,AAR==2015 & ALDER>=80)
pop10.1<-aggregate(PERSONER~RODE,pop10,sum)
names(pop10.1)<-c("RODE","age5")

# Merge population data
pop11<-merge(pop3,pop5,by="RODE",all=TRUE)
pop12<-merge(pop11,pop6.1,by="RODE",all=TRUE)
pop13<-merge(pop12,pop7.1,by="RODE",all=TRUE)
pop14<-merge(pop13,pop8.1,by="RODE",all=TRUE)
pop15<-merge(pop14,pop9.1,by="RODE",all=TRUE)
pop16<-merge(pop15,pop10.1,by="RODE",all=TRUE)
pop17<-subset(pop16,!is.na(p2))

# Identify roder in spatial data object without population data for 2015
pop18<-merge(roder.cph@data,pop17,by.x="rode_nr",by.y="RODE",sort=FALSE,all.x=TRUE)
pop19<-subset(pop18,is.na(p2),select=c("rode_nr",names(pop17)[2:8]))
names(pop19)<-names(pop17)

# Add these to population data
pop20<-rbind(pop17,pop19)

# Merge again to add population data
pop21<-merge(roder.cph@data,pop20,by.x="rode_nr",by.y="RODE",sort=FALSE,all.x=TRUE)

# Create pop-up text
popup_txt<-vector()
for(i in 1:dim(pop21)[1]){

popup_txt[i]<-
paste("<strong>",pop21$rodenavn[i],"</strong><br/>Population in 2015:",pop21$p2[i],"<br/>Population in 1992:",pop21$p1[i],
"<br/>Percent increase 1992-2015:",round(((pop21$p2[i]-pop21$p1[i])*100)/pop21$p1[i],digits=1),
"<br/><br/>Age distribution in 2015:<br/>0-19 years:",round((pop21$age1[i]/pop21$p2[i])*100,digits=1),"%",
"<br/>20-39 years:",round((pop21$age2[i]/pop21$p2[i])*100,digits=1),"%",
"<br/>40-59 years:",round((pop21$age3[i]/pop21$p2[i])*100,digits=1),"%",
"<br/>60-79 years:",round((pop21$age4[i]/pop21$p2[i])*100,digits=1),"%",
"<br/>80- years:",round((pop21$age5[i]/pop21$p2[i])*100,digits=1),"%")
}

pop21$txt<-popup_txt
roder.cph@data<-pop21

# Map population data using leaflet
pal<-colorQuantile("YlGn",NULL,n=200)
leaflet(data=roder.cph) %>% addProviderTiles("CartoDB.Positron") %>% 
addPolygons(fillColor=~pal(p2),fillOpacity=0.8,color="#BDBDC3",weight=1,popup=~as.character(roder.cph@data$txt))
