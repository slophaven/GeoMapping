###############################################################################################################################################
#
# The R code below maps commutes by lines using ggplot
#
###############################################################################################################################################

# Load required packages
if (!require("maptools")){install.packages("maptools")}
if (!require("plyr")){install.packages("plyr")}
if (!require("ggplot2")){install.packages("ggplot2")}

# Define ggplot theme
thm <- theme(axis.line = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        panel.background = element_rect(fill='black',colour='black'), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.background = element_blank(), plot.title = element_text(face = "bold"),legend.position="none")


# Import data with commuters and midpoints of polygons
flows<-read.csv(file="data/flows.csv")

# Calculate distance between midpoints
flows$dist<-sqrt((flows$x_home-flows$x_work)^2+(flows$y_home-flows$y_work)^2)

# Define cities to mark
cities<-flows[1:97,c(2,5,6)]
cph<-subset(cities,V2=="København")
ode<-subset(cities,V2=="Odense")
aal<-subset(cities,V2=="Aalborg")
aar<-subset(cities,V2=="Aarhus")
esb<-subset(cities,V2=="Esbjerg")

xquiet<-scale_x_continuous("", breaks=NULL) 
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)


# Merge all polygons
dk<-readShapePoly("data/opstillingskreds")
dk@data$help<-1
test<-unionSpatialPolygons(dk,ID=dk@data$help)
dt<-data.frame(help=1)
test1<-SpatialPolygonsDataFrame(test,data=dt)
test1<-fortify(test1,region="help")
test1$long<-test1$long/1000
test1$lat<-test1$lat/1000
test1<-subset(test1,long<750)
blue1<-data.frame(district=1,pct=1)

######################################################################################################

mycol<-rgb(4,32,41,max=255)

pdf(file="outputs/flows.pdf",15,15)

ggplot()+
geom_map(data=blue1,aes(map_id=district,fill=pct),map=test1)+expand_limits(x=test1$long,y=test1$lat)+
scale_fill_gradient(breaks=1,labels=c("1"),low=mycol,high=mycol)+
geom_segment(data=flows[which(flows$value>40),],aes(x=x_home,y=y_home,xend=x_work,yend=y_work,alpha=value),col="white")+
scale_alpha_continuous(range = c(0.07,0.7))+
geom_text(data=cph,aes(x=x_home+5,y=y_home-15,label="Copenhagen"),size=8,col="cyan",alpha=0.9)+
geom_text(data=ode,aes(x=x_home,y=y_home+5,label="Odense"),size=8,col="cyan",alpha=0.9)+
geom_text(data=aar,aes(x=x_home,y=y_home+5,label="Aarhus"),size=8,col="cyan",alpha=0.9)+
geom_text(data=aal,aes(x=x_home,y=y_home+5,label="Aalborg"),size=8,col="cyan",alpha=0.9)+
geom_text(data=esb,aes(x=x_home,y=y_home+5,label="Esbjerg"),size=8,col="cyan",alpha=0.9)+
geom_text(data=esb,aes(x=x_home+15,y=y_home-90,label="the commuting Denmark"),size=10,col="cyan",alpha=0.9)+
thm+quiet+coord_equal()

dev.off()
