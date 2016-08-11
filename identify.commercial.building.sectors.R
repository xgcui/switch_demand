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
#push
###########import load data from excel or RDATA#######################################
###########excel is a little show, it can save to RData at first######################
######################################################################################

read.data.from.a.excel.with.13sheet.to.rdata=F;

##read the data from a big excel,choose which sheet to read and give the building type name 
read.data.from.a.excel.with.one.sheet.and.one.building.type=F;

###
load.rdata=F

if(read.data.from.a.excel.with.13sheet.to.rdata){
  sheet.num=13;tmp.type.name='total' 
  print('read the data from excel uncluding all building type')
  FR.IN = 'Draft Results Tech Com.xlsx'
  tmp.dat = read.xls(file.path (PATH.IN, FR.IN),sheet=sheet.num,header=TRUE) ##,stringsAsFactors=FALSE) ##colClasses=c(rep('character',3),rep('numeric',47)))
  save(tmp.dat,file='../res/tmp.data.RData')
  
  
}else if(read.data.from.a.excel.with.one.sheet.and.one.building.type){
  sheet.num=1; 
  tmp.type.names=c('college','food_store','health','large_office','lodging','miscellaneous','refrigerated_warehouse','restaurant',
                   'retail','school','small_office','unrefrigerated_warehouse','total')
  tmp.type.name=tmp.type.names[1]
  print('read the data from excel uncluding only one building type')
  FR.IN=paste(tmp.type.name,'.xlsx',sep='')
  tmp.dat = read.xls(file.path (PATH.IN, FR.IN),sheet=sheet.num,header=TRUE) ##,stringsAsFactors=FALSE) ##colClasses=c(rep('character',3),rep('numeric',47)))
  save(tmp.dat,file='../res/tmp.data.RData')
  
  }else if(load.rdata){
  load.file='../res/tmp.dat.RData'
  load(load.file)
  print('read the data from rdata file')
}

## here, I used a loop for handling different sectors.
## it can comment out and then read with one excel 'Draft Results Tech Com.xlsx'
## these excels generated from 'Draft Results Tech Com.xlsx'

tmp.type.names=c('college','food_store','health','large_office','lodging','miscellaneous','refrigerated_warehouse','restaurant',
                 'retail','school','small_office','unrefrigerated_warehouse','total')

for (tmp.type.name in tmp.type.names){
  sheet.num=1; 
  print('read the data from excel uncluding only one building type')
  print(tmp.type.name)
  FR.IN=paste(tmp.type.name,'.xlsx',sep='')
  tmp.dat = read.xls(file.path (PATH.IN, FR.IN),sheet=sheet.num,header=TRUE)
  
##################get the year, hour and annual value based on building type or end use sectors#############################################
tmp.year=as.character(unlist(tmp.dat[50,6:50]))
tmp.hour=as.numeric(unlist(tmp.dat[51:8810,5]))
tmp.building.type.name=as.character(unlist(tmp.dat[25:37,3]))
tmp.sector.name=as.character(unlist(tmp.dat[39:49,3]))


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
pdf(plt.f,width=16,height=12)
par(mfrow=c(2,1))

xlims = as.POSIXct(c(strptime("2006-01-01-00",format="%Y-%m-%d-%H"),strptime("2006-12-31-23",format="%Y-%m-%d-%H")))
all.dates =seq(min(xlims),max(xlims),by="1 hour")
loctime.Z = as.numeric (format(all.dates, format="%Y%m%d%H"))
month= as.numeric (format(all.dates, format="%m"))
plt.time = strptime (loctime.Z, "%Y%m%d%H")



########################load magnitude######################################################################
for (jj in 1:12){
month.id=which(month==jj)
Y.LIM=max(tmp.type.hour.mag.mat)*1.1

plot (plt.time[month.id], tmp.type.hour.mag.mat[,1][month.id], xaxt='n', type='l', col='black', ylab='Load Magnitude(MW)',
      xlab='Time', main=paste('load___buidling type: ', tmp.type.name,';',"month: ",jj,sep=''), ylim = c(0, Y.LIM),pch=16,cex=0.5,lwd=2)
points(plt.time[month.id],tmp.type.hour.mag.mat[,11][month.id],col='red',cex=0.6,type='l',lwd=2)
points(plt.time[month.id],tmp.type.hour.mag.mat[,21][month.id],col='blue',cex=0.7,type='l',lwd=2)
points(plt.time[month.id],tmp.type.hour.mag.mat[,31][month.id],col='cyan',cex=0.8,type='l',lwd=2)
points(plt.time[month.id],tmp.type.hour.mag.mat[,41][month.id],col='brown',cex=0.9,type='l',lwd=2)

r <- as.POSIXct(range(plt.time[month.id]),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%b-%d")
legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
        col=c('black','red','blue','cyan','brown'), pch=16, cex=1.2)
########################load magnitude######################################################################
########################load shape###########################################################################
plot (plt.time[month.id], tmp.hour.shape[,1][month.id], xaxt='n', type='l', col='black', ylab='Load shape (1/hour)',
      xlab='Time', main=paste('load shape___buidling type: ', tmp.type.name,';',"month: ",jj,sep=''), ylim = c(0.00005, 0.00025),pch=16,cex=0.5,lwd=2)
points(plt.time[month.id],tmp.hour.shape[,11][month.id],col='red',cex=0.6,type='l',lwd=2)
points(plt.time[month.id],tmp.hour.shape[,21][month.id],col='blue',cex=0.7,type='l',lwd=2)
points(plt.time[month.id],tmp.hour.shape[,31][month.id],col='cyan',cex=0.8,type='l',lwd=2)
points(plt.time[month.id],tmp.hour.shape[,41][month.id],col='brown',cex=0.9,type='l',lwd=2)

r <- as.POSIXct(range(plt.time[month.id]),"days")
axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%b-%d")
legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
        col=c('black','red','blue','cyan','brown'), pch=c(16), cex=1.2)
}
#######################load shape###########################################################################
dev.off()

  ##########################3
  ##########################3
  ##########################3
  tmp.hour.shape.avg=apply(tmp.hour.shape[,1:45],1,mean)
  tmp.hour.shape.diff=matrix(NA,dim(tmp.type.hour.mag)[1],dim(tmp.type.hour.mag)[2])
  tmp.hour.shape.diff.ratio=matrix(NA,dim(tmp.type.hour.mag)[1],dim(tmp.type.hour.mag)[2])
  for (ii in 1:45){
    tmp.hour.shape.diff[,ii]=tmp.hour.shape[,ii]-tmp.hour.shape.avg
    tmp.hour.shape.diff.ratio[,ii]=tmp.hour.shape.diff[,ii]/tmp.hour.shape.avg
  }
  
  plt.f =  paste (PATH.OUT,'load.shape.diff.with.avg.load.shape.at2006-2050.and.the.ratio.to.avg.load.shape.different.years.for.commercial.building.type__', tmp.type.name, "__", ".2006-2046_XG_", proc.date, ".pdf",sep="")
  pdf(plt.f,width=16,height=12)
  par(mfrow=c(2,1))
  
  xlims = as.POSIXct(c(strptime("2006-01-01-00",format="%Y-%m-%d-%H"),strptime("2006-12-31-23",format="%Y-%m-%d-%H")))
  all.dates =seq(min(xlims),max(xlims),by="1 hour")
  loctime.Z = as.numeric (format(all.dates, format="%Y%m%d%H"))
  month= as.numeric (format(all.dates, format="%m"))
  plt.time = strptime (loctime.Z, "%Y%m%d%H")
  
  
  
  ########################load magnitude######################################################################
  for (jj in 1:12){
    month.id=which(month==jj)
    Y.LIM.MAX=max(tmp.hour.shape.diff)*1.05
    Y.LIM.MIN=min(tmp.hour.shape.diff)*1.05
    plot (plt.time[month.id], tmp.hour.shape.diff[,1][month.id], xaxt='n', type='l', col='black', ylab='Load shape difference (1/hour)',
          xlab='Time', main=paste('load shape difference.between.one.year.and.avg.data(2006-2050)___buidling type: ', tmp.type.name,';',"month: ",jj,sep=''), 
          ylim = c(Y.LIM.MIN, Y.LIM.MAX),pch=16,cex=0.5,lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff[,11][month.id],col='red',cex=0.6,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff[,21][month.id],col='blue',cex=0.7,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff[,31][month.id],col='cyan',cex=0.8,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff[,41][month.id],col='brown',cex=0.9,type='l',lwd=2)
    
    r <- as.POSIXct(range(plt.time[month.id]),"days")
    axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%b-%d")
    legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
            col=c('black','red','blue','cyan','brown'), pch=16, cex=1.2)
    ########################load magnitude######################################################################
    ########################load shape###########################################################################
    Y.LIM.MAX=max(tmp.hour.shape.diff.ratio)*1.05
    Y.LIM.MIN=min(tmp.hour.shape.diff.ratio)*1.05
    plot (plt.time[month.id], tmp.hour.shape.diff.ratio[,1][month.id], xaxt='n', type='l', col='black', ylab='Load difference ratio',
          xlab='Time', main=paste('load shape difference.ratio.between.one.year.and.avg.data(2006-2050)___buidling type: ', tmp.type.name,';',"month: ",jj,sep=''), 
          ylim = c(Y.LIM.MIN, Y.LIM.MAX),pch=16,cex=0.5,lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff.ratio[,11][month.id],col='red',cex=0.6,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff.ratio[,21][month.id],col='blue',cex=0.7,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff.ratio[,31][month.id],col='cyan',cex=0.8,type='l',lwd=2)
    points(plt.time[month.id],tmp.hour.shape.diff.ratio[,41][month.id],col='brown',cex=0.9,type='l',lwd=2)
    
    r <- as.POSIXct(range(plt.time[month.id]),"days")
    axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format="%b-%d")
    legend ('topright', legend=c(tmp.year[1],tmp.year[11],tmp.year[21],tmp.year[31],tmp.year[41]),         
            col=c('black','red','blue','cyan','brown'), pch=c(16), cex=1.2)
  }
  #######################load shape###########################################################################
  dev.off()
}
stop()



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
