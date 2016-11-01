#########################################################################################################################
# The R-code below plots the number of confirmed cases with ebola infection by week 
# The raw data can be obtained via http://apps.who.int/gho/data/node.ebola-sitrep.ebola-country?lang=en
#########################################################################################################################

gui<-read.csv(file="data/guinea2.csv")
lib<-read.csv(file="data/liberia2.csv")
sil<-read.csv(file="data/sierra-leone2.csv")

par(xpd=TRUE,cex=1.2,mar=c(2,4,1,2))
barplot(gui$sum,col="#31a354",ylim=c(0,250),ylab="Number of laboratory confirmed ebola cases")
text(65,250,"Guinea",cex=1.5)
text(0,-10,"Jan 2014")
text(65,-10,"Jan 2015")
text(130,-10,"Jan 2016")

par(xpd=TRUE,cex=1.2,mar=c(2,3,1,2))
barplot(lib$sum,col="#636363",ylim=c(0,250),ylab="Number of laboratory confirmed ebola cases")
text(65,250,"Liberia",cex=1.5)
text(0,-10,"Jan 2014")
text(65,-10,"Jan 2015")
text(130,-10,"Jan 2016")

par(xpd=TRUE,cex=1.2,mar=c(2,3,1,2))
barplot(lib$sum,col="#1c9099",ylim=c(0,250),ylab="Number of laboratory confirmed ebola cases")
text(65,250,"Sierra Leone",cex=1.5)
text(0,-10,"Jan 2014")
text(65,-10,"Jan 2015")
text(130,-10,"Jan 2016")
