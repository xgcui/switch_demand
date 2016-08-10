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
FR.IN='Draft Results Tech Com.xlsx'
#stop()

###########import load data from excel or RDATA#######################################
###########excel is a little show, it can save to RData at first######################
######################################################################################
convert.excel.to.rdata=T
sheet.num=13
load.file=paste(PATH.OUT,'load.data.at.sheet.num__',shape.num,'.RData',sep='')
if(convert.excel.to.rdata){
  tmp.dat = read.xls(file.path (PATH.IN, FR.IN),sheet=sheet.num,header=TRUE) ##,stringsAsFactors=FALSE) ##colClasses=c(rep('character',3),rep('numeric',47)))

  save(tmp.dat,file=load.file)
}else{
 load(load.file)
}
###load('../res/tmp.dat.RData')

##################get the year, hour and annual value based on building type or end use sectors#############################################
tmp.year=as.character(unlist(tmp.dat[50,6:50]))
tmp.hour=as.numeric(unlist(tmp.dat[51:8810,5]))
tmp.building.type.name=as.character(unlist(tmp.dat[25:37,3]))
tmp.sector.name=as.character(unlist(tmp.dat[39:49,3]))
tmp.type.name='total'

tmp.sector.annual.mag=tmp.dat[39:49,6:50]
tmp.sector.annual.mag.vec=as.numeric(as.character(gsub(',','',unlist(tmp.sector.annual.mag))))
tmp.sector.annual.mag.mat=matrix(tmp.sector.annual.mag.vec, dim(tmp.sector.annual.mag)[1],dim(tmp.sector.annual.mag)[2])

tmp.building.type.annual.mag=tmp.dat[25:37,6:50]
tmp.building.type.annual.mag.vec=as.numeric(as.character(gsub(',','',unlist(tmp.building.type.annual.mag))))
tmp.building.type.annual.mag.mat=matrix(tmp.building.type.annual.mag.vec, dim(tmp.building.type.annual.mag)[1],dim(tmp.building.type.annual.mag)[2])

tmp.type.hour.mag=tmp.dat[51:8810,6:50]
tmp.type.hour.mag.vec=as.numeric(as.character(unlist(tmp.type.hour.mag)))
tmp.type.hour.mag.mat=matrix(tmp.type.hour.mag.vec, dim(tmp.type.hour.mag)[1],dim(tmp.type.hour.mag)[2])
#######################################

#####################compute and plot the load shape#####################
tmp.hour.shape=matrix(NA,dim(tmp.type.hour.mag)[1],dim(tmp.type.hour.mag)[2])
for (ii in 1:45){
  tmp.hour.shape[,ii]=tmp.type.hour.mag.mat[,ii]/tmp.building.type.annual.mag.mat[13,ii]
}

plt.f =  paste (PATH.OUT,'Comparing.load.shape.at.different.years.for.commercial.building.type__', tmp.type.name, "__", ".2006-2046_XG_", proc.date, ".pdf",sep="")
pdf(plt.f,width=16,height=8)

xlims = as.POSIXct(c(strptime("2009-01-01-00",format="%Y-%m-%d-%H"),strptime("2009-12-31-23",format="%Y-%m-%d-%H")))
all.dates =seq(min(xlims),max(xlims),by="1 hour")
loctime.Z = as.numeric (format(all.dates, format="%Y%m%d%H"))
plt.time = strptime (loctime.Z, "%Y%m%d%H")

########################load magnitude######################################################################
plot (plt.time, tmp.type.hour.mag.mat[,1], xaxt='n', type='p', col='black', ylab='Load Magnitude(MW)',
      xlab='Time', main=tmp.type.name, ylim = c(5, 22),pch=16,cex=0.5)
points(plt.time,tmp.type.hour.mag.mat[,11],col='red',cex=0.6)
points(plt.time,tmp.type.hour.mag.mat[,21],col='blue',cex=0.7)
points(plt.time,tmp.type.hour.mag.mat[,31],col='cyan',cex=0.8)
points(plt.time,tmp.type.hour.mag.mat[,41],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
        col=c('black','red','blue','cyan','brown'), pch=16, cex=0.5)
########################load magnitude######################################################################
########################load shape###########################################################################
plot (plt.time, tmp.hour.shape[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main=tmp.type.name, ylim = c(0.00005, 0.00025),pch=16,cex=0.5)
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

#################################################################################################################################################
###################compute the load shape of each end use sector based on CA total###############################################################



plt.f =  paste (PATH.OUT,'load.shape.of.end.use.computed.from.the.building.type.',tmp.type.name, "__", ".2006-2046_XG_", proc.date, ".pdf",sep="")
pdf(plt.f,width=16,height=8)


########################load magnitude######################################################################

## compute load shape based on assumption that the load shape is same in this period########################
col.key=c(1:10) #######choose different period########################
load.shape.sector=matrix(NA,8760,10)
Aij=t(tmp.sector.annual.mag.mat[1:10,col.key])
for (kk in 1:8760){
  yi=tmp.type.hour.mag.mat[kk,col.key]
  xi=solve(Aij,yi)
  load.shape.sector[kk,]=xi
}

###col.pred=max(col.key)+1
###load.shape.prediction=(load.shape.sector%*%tmp.sector.annual.mag.mat[1:10,col.pred])/(tmp.sector.annual.mag.mat[11,col.pred])

plot (plt.time, load.shape.sector[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main='end use sctors: using the data at 2006-2015', ylim = c(-0.001, 0.003),cex=0.5)
points(plt.time,load.shape.sector[,2],col='red',cex=0.6)
points(plt.time,load.shape.sector[,3],col='blue',cex=0.7)
points(plt.time,load.shape.sector[,4],col='cyan',cex=0.8)
points(plt.time,load.shape.sector[,5],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.sector.name[1:5]),         
        col=c('black','red','blue','cyan','brown'), pch=c(rep(1,5),rep(8,5)), cex=1)


col.key=c(11:20)
load.shape.sector=matrix(NA,8760,10)
Aij=t(tmp.sector.annual.mag.mat[1:10,col.key])
for (kk in 1:8760){
  yi=tmp.type.hour.mag.mat[kk,col.key]
  xi=solve(Aij,yi)
  load.shape.sector[kk,]=xi
}
col.pred=max(col.key)+1
load.shape.prediction=(load.shape.sector%*%tmp.sector.annual.mag.mat[1:10,col.pred])/(tmp.sector.annual.mag.mat[11,col.pred])

plot (plt.time, load.shape.sector[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main='end use sctors: using the data at 2016-2025', ylim = c(-0.001, 0.003),cex=0.5)
points(plt.time,load.shape.sector[,2],col='red',cex=0.6)
points(plt.time,load.shape.sector[,3],col='blue',cex=0.7)
points(plt.time,load.shape.sector[,4],col='cyan',cex=0.8)
points(plt.time,load.shape.sector[,5],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.sector.name[1:5]),         
        col=c('black','red','blue','cyan','brown'), pch=c(rep(1,5),rep(8,5)), cex=1)

col.key=c(21:30)
load.shape.sector=matrix(NA,8760,10)
Aij=t(tmp.sector.annual.mag.mat[1:10,col.key])
for (kk in 1:8760){
  yi=tmp.type.hour.mag.mat[kk,col.key]
  xi=solve(Aij,yi)
  load.shape.sector[kk,]=xi
}
col.pred=max(col.key)+1
###load.shape.prediction=(load.shape.sector%*%tmp.sector.annual.mag.mat[1:10,col.pred])/(tmp.sector.annual.mag.mat[11,col.pred])

plot (plt.time, load.shape.sector[,1], xaxt='n', type='p', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main='end use sctors: using the data at 2026-2035', ylim = c(-0.02, 0.02),cex=0.5)
points(plt.time,load.shape.sector[,2],col='red',cex=0.6)
points(plt.time,load.shape.sector[,3],col='blue',cex=0.7)
points(plt.time,load.shape.sector[,4],col='cyan',cex=0.8)
points(plt.time,load.shape.sector[,5],col='brown',cex=0.9)

r <- as.POSIXct(range(plt.time),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="months"), format="%b-%d")
legend ('topright', legend=c(tmp.sector.name[1:5]),         
        col=c('black','red','blue','cyan','brown'), pch=c(rep(1,5),rep(8,5)), cex=1)
dev.off()