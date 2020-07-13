#################################################################################
#                  National Green Infrastructure Facility                       #
#     Priming Laboratory EXperiments on infrastructure and Urban Systems        #
#                                PLEXUS                                         #
#                   written by Anil Yildiz, Dr. sc.                             #
#                         Created on 14.10.2019                                 #
#                       Last edited on 28.10.2019                               #
#################################################################################
#-------------------------------------------------------------------------------#
#                       Data from 03.05.2019 until 18.07.2019                   #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2020-01-01 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#sets the working directory for home
#-------------------------------------------------------------------------------#
setwd("C:/Users/Anil/Desktop/Meteo/SoilTemperature")

library(imager)
logo <- load.image(file="logo.png")

soiltemp <- read.csv("SoilTemperature_15Min.csv",header=T,stringsAsFactors=F)
airtemp <- read.csv("AirTemp_15Min.csv",header=T,stringsAsFactors=F)

depth <- as.numeric(substr(colnames(soiltemp[2:14]),start=2,stop=4))
temp <- seq(-1,29,1)
time <- seq(startdate,enddate,60*15)
time <- time[-c(1,23329)]

for(j in 1:nrow(soiltemp))
{
   png(paste0("Animation2/Fig",c("0000","000","00","0","")[nchar(j)],j,".png"),width=165,height=165,res=480,units="mm")
   layout(matrix(c(1,1,2,3),nrow=2,ncol=2),widths=c(10,2),heights=c(10,2))
   par(mar=c(2,4.5,2.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,xlim=c(-900,900),ylim=c(950,-50),type="l",axes=F,xlab=NA,ylab=NA)
   axis(2,tck=0.00,at=c(-50,0,as.numeric(depth),950),labels=c("Air temp.",0,as.numeric(depth),950),line=0,lwd=0)
   for(i in 1:length(depth))
   {
      rect(xleft=-900,ybottom=depth[i]-15,xright=900,ytop=depth[i]+15,col=heat.colors(30)[31-findInterval(soiltemp[j,i+1],temp)],border=NA)
   }
   rect(xleft=-900,ybottom=-50,xright=900,ytop=-20,col=heat.colors(31)[32-floor(airtemp[j,2])],border=T)
   rect(xleft=-900,ybottom=950,xright=900,ytop=0,col=rgb(0,0,0,0))
   rect(xleft=-900,ybottom=150,xright=900,ytop=0,col=rgb(0,0,0,0))
   par(las=0)
   mtext(as.character(time[j]),side=3,line=0)
   mtext("Depth [mm]",side=2,line=1.25)
   mtext("Contact anil.yildiz@ncl.ac.uk & ross.stirling@ncl.ac.uk for more information",side=1)
   
   par(mar=c(0.25,0.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,axes=F,xlab=NA,ylab=NA,pch="",ylim=c(0,1),xlim=c(0,1))
   legend("center",c(rev(as.character(temp))),col=heat.colors(31),pch=15,
          y.intersp=0.72,bty="n",xpd = T)
   mtext("Temperature \n[deg C]",side=1)

   par(mar=c(0.25,0.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(logo,axes=F,xlab=NA,ylab=NA)
   dev.off()
}