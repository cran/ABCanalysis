ABCcleanData=function(Data,RemoveSmallYields=FALSE){
# V= ABCcleanData(Data,RemoveSmallYields)
# Data cleanning for ABC analysis
# only the first column of Data is used
# Data <0 are set to zero,  NA in Data are set to zero
# if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
# 0.5# of the total sum (yield) is removed 
#
# INPUT
# Data(1:n)           the data set, may contain NaN, negative values and very small values
# 
# OPTIONAL
# RemoveSmallYields   bool, if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
#                                  1# of the total sum (yield) is removed 
#
# OUTPUT      List V with
# V$CleanedData(1:n1)   columnvector containing Data>=0 and zeros for all NaN and negative values in Data(1:n)  
# V$Data2CleanInd       Index such that CleanedData = nantozero(Data(Data2CleanInd));
# V$RemovedInd          Index such that Data(RemovedInd) is the data that has been removed if RemoveSmallYields==1
#
# author MT 01/2015, reimplemented from ALUs matlab version
if(!is.vector(Data)){
  n=nrow(Data)
  d=ncol(Data)
  warning('Only vectors should be used!')
  if(d>1){ #Data is Matrix or data.frame
    warning('Using only first column of data')
    UncleanData=as.vector(Data[,1]) # use only first column
  }else{
    UncleanData=Data
  }
}else{
  UncleanData=Data
}
  UncleanData=as.numeric(unname(UncleanData))#Automatische Umwandlung voh Chars/string in NA
  rowsbefore=length(UncleanData)
#NA durch Nullen ersetzen
  nabools=is.finite(UncleanData)
  Data2CleanInd=which(nabools==FALSE)
  CleanData=UncleanData
  if(length(Data2CleanInd)) CleanData[Data2CleanInd]=0
#Negative Werte entfernen

  DataNeg=CleanData[CleanData<0]
  cols=1
  bools=CleanData %in% DataNeg
  CleanData[bools]<-0

  rows=rowsbefore-sum(bools)-sum(!nabools)
if(rowsbefore>rows){
     warning(paste0(rows,' of ',rowsbefore,' items are positive and beeing used for further calculations.'))
    # warning('Please use Data[Data>0], before using Data[Aind] etc.')    
}
RemovedInd=NULL
if(RemoveSmallYields){  #die kleinsten Daten, die zusammen weniger als 0.50# ausmachen identifizieren
  SortedData=sort(CleanData,decreasing=FALSE)
  TotalYield = sum(SortedData)
  CumSumPercentage=round(cumsum(SortedData/TotalYield*100),0)
 SmallInd=which(CumSumPercentage<0.5) #In Prozent

    if(length( SmallInd) >0){
     # print('Removing the smallest data up to a cumulated sum of less than 0.5% of the total sum (yield):')
      SchwellenIndex=tail(SmallInd,1)+1 # Index des letzten zu kleinen Datensatzes
      Schwelle = CleanData[SchwellenIndex]      # der Datensatz der gerade noch verbleiben kann
      Data2CleanInd = which(CleanData>=Schwelle)  # diese Daten koennen bleiben
      RemovedInd    = which(CleanData<Schwelle)  # diese Daten fallen weg
      CleanData   = CleanData[Data2CleanInd]  # bereinigen      
     # print(paste0(length(RemovedInd),' items removed.'))
    }
 
}
return(list(CleanedData=CleanData,Data2CleanInd=Data2CleanInd,RemovedInd=RemovedInd))
}
