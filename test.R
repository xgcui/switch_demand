################################################################################

# @version: August 08, 2016
# @author: Xinguang Cui
# @note: This script is used to check whether the load shape is same according to building types.
####      Also, it tests whether the load shape is same according to commerical end use  
################################################################################


library (gdata)
library (gplots)
library(xlsx)

rm (list= ls())

proc.date = format(Sys.Date(),format='%Y%m%d')
Sys.setenv(TZ = "GMT")
PATH.IN = '../data/'
PATH.OUT = '../res/'
FR.IN='text.xls'
tmp.dat = read.xls(file.path (PATH.IN, FR.IN),sheet=1,header=FALSE,stringsAsFactors=FALSE)
tmp.type.annual.mag=read.xls(file.path (PATH.IN, FR.IN),sheet=2,header=FALSE,stringsAsFactors=FALSE)

tmp.year=tmp.dat[1,-1]
tmp.hour=as.numeric(tmp.dat[-1,1])
tmp.type.name='Unrefrigerated Warehouse'
tmp.type.hour.mag=tmp.dat[-1,-1]
tmp.sector.name=tmp.type.annual.mag[-1,1]
tmp.sector.annual.mag=tmp.type.annual.mag[-1,-1]

tmp.sector.annual.mag.vec=as.numeric(as.character(gsub(',','',unlist(tmp.sector.annual.mag))))
tmp.sector.annual.mag.mat=matrix(tmp.sector.annual.mag.vec, dim(tmp.sector.annual.mag)[1],dim(tmp.sector.annual.mag)[2])

tmp.hour.shape=matrix(NA,dim(tmp.type.hour.mag)[1],dim(tmp.type.hour.mag)[2])
for (ii in 1:45){
  tmp.hour.shape[,ii]=tmp.type.hour.mag[,ii]/tmp.sector.annual.mag.mat[11,ii]
}

plt.f =  paste (PATH.OUT,'Comparing.load.shape.at.different.years.for.commercial.building.type__', tmp.type.name, "__", ".2006-2046_XG_", proc.date, ".pdf",sep="")
pdf(plt.f,width=16,height=8)

xlims = as.POSIXct(c(strptime("2009-01-01-00",format="%Y-%m-%d-%H"),strptime("2009-12-31-23",format="%Y-%m-%d-%H")))
all.dates =seq(min(xlims),max(xlims),by="1 hour")
loctime.Z = as.numeric (format(all.dates, format="%Y%m%d%H"))
plt.time = strptime (loctime.Z, "%Y%m%d%H")

########################load magnitude######################################################################
plot (plt.time, tmp.type.hour.mag[,1], xaxt='n', type='p', col='black', ylab='Load Magnitude(MW)',
      xlab='Time', main=tmp.type.name, ylim = c(0, 1),pch=16,cex=0.5)
points(plt.time,tmp.type.hour.mag[,11],col='red',cex=0.6)
points(plt.time,tmp.type.hour.mag[,21],col='blue',cex=0.7)
points(plt.time,tmp.type.hour.mag[,31],col='cyan',cex=0.8)
points(plt.time,tmp.type.hour.mag[,41],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
        col=c('black','red','blue','cyan','brown'), pch=16, cex=0.5)
########################load magnitude######################################################################
########################load shape###########################################################################
plot (plt.time, tmp.hour.shape[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main=tmp.type.name, ylim = c(0, 0.0003),pch=16,cex=0.5)
points(plt.time,tmp.hour.shape[,11],col='red',cex=0.6)
points(plt.time,tmp.hour.shape[,21],col='blue',cex=0.7)
points(plt.time,tmp.hour.shape[,31],col='cyan',cex=0.8)
points(plt.time,tmp.hour.shape[,41],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
        col=c('black','red','blue','cyan','brown'), pch=c(16), cex=1)
#######################load shape###########################################################################
dev.off()

###################
load.shape.sector=matrix(NA,8760,10)
Aij=t(tmp.sector.annual.mag.mat[1:10,1:10])
for (kk in 1:8760){
yi=as.numeric(as.vector(unlist(tmp.type.hour.mag[kk,1:10])))
xi=solve(Aij,yi)
load.shape.sector[kk,]=xi
}

plt.f =  paste (PATH.OUT,'load.shape.of.end.use.computed.from.the.building.type.',tmp.type.name, "__", ".2006-2046_XG_", proc.date, ".pdf",sep="")
pdf(plt.f,width=16,height=8)
########################load magnitude######################################################################
plot (plt.time, load.shape.sector[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main='sample sctors', ylim = c(-60, 100),pch=16,cex=0.5)
points(plt.time,load.shape.sector[,2],col='red',cex=0.6)
points(plt.time,load.shape.sector[,3],col='blue',cex=0.7)
points(plt.time,load.shape.sector[,4],col='cyan',cex=0.8)
points(plt.time,load.shape.sector[,5],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.sector.name[1:5]),         
        col=c('black','red','blue','cyan','brown'), pch=c(16), cex=1)
dev.off()
stop()






