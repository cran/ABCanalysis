 ABCcurve=function(Data,p){
# res = ABCcurve(Data,GiniSteigung)
# ABC Curve : cumulative fraction of largest Data in population vs fraction of population
#
# INPUT
# Data(1:n)          data vector,only positive data will be used
#
# OPTIONAL
#
# p                  x-werte fuer Spline Interpolation: wenn vorgegeben dann werden diese genommen
#
# OUTPUT List V with
# Curve										A list of Effort and Yield
#																		Effort: fraction of population in [0,1]
#																		Yield:  cumulative fraction of largest Datas in [0,1]
#CleanedData							vector [1:m], columnvector containing Data>=0 and zeros for all NA, NaN and negative values in Data(1:n)
#Slope										A list of p and dABC
#																		p: x-werte fuer Spline Interpolation, defualt: p = (0:0.01:1)
#																		dABC: first deviation of the functio ABC(p)=Effort(Yield)
#
#
#author: MT 11/2014
# 1.Editor MT 01/2015
# 2.Editor: FL
# 3.Editor: MT 11/2017: Doku neu
cleanData=ABCcleanData(Data)$CleanedData
rows=length(cleanData)
if(missing(p)){
  if(rows<101){ p=seq(from=0,to=1,by=0.01)
  }else{ p=seq(from=0,to=1,by=0.001)}
}

sorted=sort(na.last=T,cleanData,decreasing=TRUE)
#N=sum(cleanData)
#Anteil=sorted/N
Anteil=sorted
y=cumsum(Anteil)
y=y/tail(y,1)
x=(1:rows)/rows
## Die Kurve muss durch 2 Punkte gehen 0 und 1

 if(head(y,1)>0){
   x=c(0,x)
   y=c(0,y)
 }
 if(tail(x,1)<1){ #Nach matlab Implementation, ueberfluessig?
   x=c(x,1)
   y=c(y,1)
 }

## Spline Interpolation

V=spline(x,y,xout=p)
Effort=V$x
Yield=V$y
#Fehlerabfang der Interpolation
inds=which(Yield>=1)
ind1=min(inds)
if(ind1<length(Yield))
  Yield[c(ind1:length(Yield))]=1

n=length(Effort)
Curvengleichung=splinefun(Effort,Yield)
ableitung=Curvengleichung(1:n/n,1)
return(list(Curve=cbind(Effort=Effort,Yield=Yield),CleanedData=cleanData,Slope=cbind(p=p,dABC=ableitung)))
}
