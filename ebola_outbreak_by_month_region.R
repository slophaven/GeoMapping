###############################################################################################################################
# The R-code below maps the cumulative number of confirmed ebola cases by month and region based on the patient database
###############################################################################################################################

# Load required packages
if (!require("maptools")){install.packages("maptools")}
if (!require("leaflet")){install.packages("leaflet")}
if (!require("OpenStreetMap")){install.packages("OpenStreetMap")}
if (!require("sp")){install.packages("sp")}
if (!require("ggplot2")){install.packages("ggplot2")}
if (!require("Hmisc")){install.packages("Hmisc")}

tmpdir<-tempdir()

# Import data for Guinea and transform coordinates to OSM
gin.cases<-read.csv("data/guinea1.csv",header=T)

url<-'http://www.maplibrary.org/library/stacks/Africa/Guinea/GIN_admin_SHP.zip'
file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile1<-paste(tmpdir,"\\GIN",sep="")
gin.adm<-readShapePoly(shapeFile1)

url<-'http://www.maplibrary.org/library/stacks/Africa/Guinea/GIN_outline_SHP.zip'
file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile2<-paste(tmpdir,"\\GIN_outline",sep="")
gin.outline<-readShapePoly(shapeFile2)

proj4string(gin.adm)<-CRS("+proj=longlat +datum=WGS84")
proj4string(gin.outline)<-CRS("+proj=longlat +datum=WGS84")
gin.adm.trans<-spTransform(gin.adm,osm())
gin.outline.trans<-spTransform(gin.outline,osm())

# Import data for Liberia and transform coordinates to OSM
lib.cases<-read.csv("data/liberia1.csv",header=T)

url<-'http://www.maplibrary.org/library/stacks/Africa/Liberia/LIB-level_1_SHP.zip'

file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile3<-paste(tmpdir,"\\LIB-level_1",sep="")
lib.adm<-readShapePoly(shapeFile3)

url<-'http://www.maplibrary.org/library/stacks/Africa/Liberia/LIB_outline_SHP.zip'
file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile4<-paste(tmpdir,"\\LIB_outline",sep="")
lib.outline<-readShapePoly(shapeFile4)

proj4string(lib.adm)<-CRS("+proj=longlat +datum=WGS84")
proj4string(lib.outline)<-CRS("+proj=longlat +datum=WGS84")
lib.adm.trans<-spTransform(lib.adm,osm())
lib.outline.trans<-spTransform(lib.outline,osm())


# Import data for Sierra Leone and transform coordinates to OSM
sil.cases<-read.csv("data/sierra-leone1.csv",header=T)

url<-'http://www.maplibrary.org/library/stacks/Africa/Sierra%20Leone/SIL_admin_SHP.zip'

file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile5<-paste(tmpdir,"\\SIL",sep="")
sil.adm<-readShapePoly(shapeFile5)

url<-'http://www.maplibrary.org/library/stacks/Africa/Sierra%20Leone/SIL_outline_SHP.zip'
file<-basename(url)
download.file(url,file,mode="wb")
unzip(file,exdir=tmpdir)
shapeFile6<-paste(tmpdir,"\\SIL_outline",sep="")
sil.outline<-readShapePoly(shapeFile6)

proj4string(sil.adm)<-CRS("+proj=longlat +datum=WGS84")
proj4string(sil.outline)<-CRS("+proj=longlat +datum=WGS84")
sil.adm.trans<-spTransform(sil.adm,osm())
sil.outline.trans<-spTransform(sil.outline,osm())

# Some processing
gin.adm.trans1<-fortify(gin.adm.trans,region="ID")
gin.outline.trans1<-fortify(gin.outline.trans)
gin.by.month.2<-merge(gin.adm.trans@data,gin.cases,by.x="ADM2",by.y="loc",all.x=TRUE,sort=FALSE)
gin.month1<-merge(gin.adm.trans1,gin.by.month.2,by.x="id",by.y="ID",all=TRUE)

lib.adm.trans1<-fortify(lib.adm.trans,region="ID")
lib.outline.trans1<-fortify(lib.outline.trans)
lib.by.month.2<-merge(lib.adm.trans@data,lib.cases,by.x="ID",by.y="loc",all.x=TRUE,sort=FALSE)
lib.month1<-merge(lib.adm.trans1,lib.by.month.2,by.x="id",by.y="ID",all=TRUE)

sil.adm.trans1<-fortify(sil.adm.trans,region="ID")
sil.outline.trans1<-fortify(sil.outline.trans)
sil.by.month.2<-merge(sil.adm.trans@data,sil.cases,by.x="ADM2",by.y="loc",all.x=TRUE,sort=FALSE)
sil.month1<-merge(sil.adm.trans1,sil.by.month.2,by.x="id",by.y="ID",all=TRUE)

# Plotting
months<-c("January 2014","February 2014","March 2014","April 2014","May 2014","June 2014","July 2014","August 2014","September 2014","October 2014","November 2014",
"December 2014","January 2015","February 2015","March 2015","April 2015","May 2015","June 2015","July 2015","August 2015","September 2015","October 2015",
"November 2015","December 2015","January 2016")
mycol<-c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")
thm<-theme(axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.position="none")
ebola.map<-openmap(c(lat=13,lon=-16),c(lat=4,lon=-6.5),type="nps")
tit.loc<-data.frame(x=-1480000,y=550000)

for(jj in 1:25){

# Guinea
for(i in 1:dim(gin.month1)[1]){

if(gin.month1[i,24+jj]==0){gin.month1$Num[i]<-0}
if(gin.month1[i,24+jj]>0 & gin.month1[i,24+jj]<=10){gin.month1$Num[i]<-1}
if(gin.month1[i,24+jj]>10 & gin.month1[i,24+jj]<=50){gin.month1$Num[i]<-2}
if(gin.month1[i,24+jj]>50 & gin.month1[i,24+jj]<=100){gin.month1$Num[i]<-3}
if(gin.month1[i,24+jj]>100 & gin.month1[i,24+jj]<=250){gin.month1$Num[i]<-4}
if(gin.month1[i,24+jj]>250 & gin.month1[i,24+jj]<=500){gin.month1$Num[i]<-5}
if(gin.month1[i,24+jj]>500 & gin.month1[i,24+jj]<=750){gin.month1$Num[i]<-6}
if(gin.month1[i,24+jj]>750 & gin.month1[i,24+jj]<=1000){gin.month1$Num[i]<-7}
if(gin.month1[i,24+jj]>1000){gin.month1$Num[i]<-8}

}
gin.month1$Num.c<-as.factor(gin.month1$Num)

# Liberia
for(i in 1:dim(lib.month1)[1]){

if(lib.month1[i,10+jj]==0){lib.month1$Num[i]<-0}
if(lib.month1[i,10+jj]>0 & lib.month1[i,10+jj]<=10){lib.month1$Num[i]<-1}
if(lib.month1[i,10+jj]>10 & lib.month1[i,10+jj]<=50){lib.month1$Num[i]<-2}
if(lib.month1[i,10+jj]>50 & lib.month1[i,10+jj]<=100){lib.month1$Num[i]<-3}
if(lib.month1[i,10+jj]>100 & lib.month1[i,10+jj]<=250){lib.month1$Num[i]<-4}
if(lib.month1[i,10+jj]>250 & lib.month1[i,10+jj]<=500){lib.month1$Num[i]<-5}
if(lib.month1[i,10+jj]>500 & lib.month1[i,10+jj]<=750){lib.month1$Num[i]<-6}
if(lib.month1[i,10+jj]>750 & lib.month1[i,10+jj]<=1000){lib.month1$Num[i]<-7}
if(lib.month1[i,10+jj]>1000){lib.month1$Num[i]<-8}

}
lib.month1$Num.c<-as.factor(lib.month1$Num)

# Sierra Lione
for(i in 1:dim(sil.month1)[1]){

if(sil.month1[i,24+jj]==0){sil.month1$Num[i]<-0}
if(sil.month1[i,24+jj]>0 & sil.month1[i,24+jj]<=10){sil.month1$Num[i]<-1}
if(sil.month1[i,24+jj]>10 & sil.month1[i,24+jj]<=50){sil.month1$Num[i]<-2}
if(sil.month1[i,24+jj]>50 & sil.month1[i,24+jj]<=100){sil.month1$Num[i]<-3}
if(sil.month1[i,24+jj]>100 & sil.month1[i,24+jj]<=250){sil.month1$Num[i]<-4}
if(sil.month1[i,24+jj]>250 & sil.month1[i,24+jj]<=500){sil.month1$Num[i]<-5}
if(sil.month1[i,24+jj]>500 & sil.month1[i,24+jj]<=750){sil.month1$Num[i]<-6}
if(sil.month1[i,24+jj]>750 & sil.month1[i,24+jj]<=1000){sil.month1$Num[i]<-7}
if(sil.month1[i,24+jj]>1000){sil.month1$Num[i]<-8}

}
sil.month1$Num.c<-as.factor(sil.month1$Num)

plot.tit<-months[jj]

autoplot(ebola.map)+
geom_polygon(aes(x=long,y=lat,group=group,fill=Num.c),data=gin.month1)+geom_path(aes(x=long,y=lat,group=group),data=gin.month1,color='gray')+
geom_path(aes(x=long,y=lat,group=group),data=gin.outline.trans1,color='black',size=1)+
geom_polygon(aes(x=long,y=lat,group=group,fill=Num.c),data=lib.month1)+geom_path(aes(x=long,y=lat,group=group),data=lib.month1,color='gray')+
geom_path(aes(x=long,y=lat,group=group),data=lib.outline.trans1,color='black',size=1)+
geom_polygon(aes(x=long,y=lat,group=group,fill=Num.c),data=sil.month1)+geom_path(aes(x=long,y=lat,group=group),data=sil.month1,color='gray')+
geom_path(aes(x=long,y=lat,group=group),data=sil.outline.trans1,color='black',size=1)+
geom_text(data=tit.loc,aes(x=x,y=y,label=plot.tit),size=5,col='black',fontface="bold")+
scale_fill_manual(values=mycol)+thm


file<-gsub(" ", "",months[jj],fixed=TRUE)
ggsave(paste("outputs/",file,".png",sep=""),width=4,height=4)

}

##############################################################################################################################
# Creating a color label
##############################################################################################################################
png(file="outputs/label.png",width=1100,height=40)
par(mar=c(0,0,0,0))
plot(c(0,26.5),c(0,1),axes=F,xlab="",ylab="",type="n")

k<-0
for(i in 1:9){
polygon(c(k,k,k+1,k+1),c(0,1,1,0),col=paste(mycol[i]),lwd=2)
k<-k+3
}
text(1.4,0.5,"0",cex=1.3)
text(4.7,0.5,"1-10",cex=1.3)
text(7.8,0.5,"11-50",cex=1.3)
text(10.9,0.5,"51-100",cex=1.3)
text(13.9,0.5,"101-250",cex=1.3)
text(17,0.5,"251-500",cex=1.3)
text(20.0,0.5,"501-750",cex=1.3)
text(23.0,0.5,"751-1000",cex=1.3)
text(25.8,0.5,"1001+",cex=1.3)

dev.off()
