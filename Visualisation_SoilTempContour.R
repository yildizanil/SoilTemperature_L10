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

soiltemp <- read.csv("SoilTemperature_15Min.csv",header=T,stringsAsFactors=F)
airtemp <- read.csv("AirTemp_15Min.csv",header=T,stringsAsFactors=F)

axis_seq <- as.character(seq.Date(as.Date(startdate+1)-2,as.Date(enddate+1),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)

depth <- as.numeric(substr(colnames(soiltemp[2:14]),start=2,stop=4))
time <- seq(startdate,enddate,60*60*0.25)
temp <- seq(-1,29,1)

pdf("SoilTemperature.pdf",width=190/25.4,height=140/25.4)
layout(matrix(c(1,2),nrow=1,ncol=2),widths=c(10,2))
par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(950,-50),type="l",axes=F,xlab=NA,ylab=NA)
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=950,col=rgb(0,0,0,0.1))

axis(1,tck=0.02,labels=NA,at=axis_days)
axis(1,tck=0.03,labels=axis_names,at=axis_months)
axis(2,tck=0.02,at=depth,labels=depth)
box()
for(j in 1:length(depth))
{
   for(i in 1:nrow(airtemp))
   {
      rect(xleft=as.POSIXct(airtemp[i,1],"UTC"),ybottom=-50,
           xright=as.POSIXct(airtemp[i+1,1],"UTC"),ytop=-10,
           col=heat.colors(30)[31-findInterval(airtemp[i,2],temp)],border=NA)
      
      rect(xleft=as.POSIXct(soiltemp[i,1],"UTC"),ybottom=depth[j]-20,
           xright=as.POSIXct(soiltemp[i+1,1],"UTC"),ytop=depth[j]+20,
           col=heat.colors(30)[31-findInterval(soiltemp[i,j+1],temp)],border=NA)
   }
}
rect(xleft=startdate,ybottom=-10,xright=enddate,ytop=-50,border = T)
rect(xleft=startdate,ybottom=950,xright=enddate,ytop=150,border = T)
rect(xleft=startdate,ybottom=150,xright=enddate,ytop=0,border = T)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext("Time [dd-mm-2019]",side=1,line=1)
text(startdate,-55,"Air temperature [°C]",adj=c(0,0))
par(mar=c(0.25,0.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="",ylim=c(0,1),xlim=c(0,1))
legend("center",c(rev(as.character(temp))),col=heat.colors(31),pch=15,
       y.intersp=0.72,bty="n",xpd = T)
text(0.5,1,"Temperature \n° C",adj=c(0.5,1))
dev.off()


