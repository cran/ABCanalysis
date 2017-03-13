GiniIndex= function(Data,p){
#[Gini,p,ABC,CleanedData] = GiniIndex(Data,p) 
# Gini = GiniIndex(Data)  # calculation of the Gini-Index
# calculation of the Gini-Index from Data 
# uses ABCcurve and Gini4ABC
#
# INPUT
# Data(1:n)          data set,  it is cleaned using  CleanedData = ABCcleanData(Data)
#                    before results are calculated
# 
# OPTIONAL
# p                  x-values for Spline Interpolation of ABC curve
#
# OUTPUT
# Gini              gini index i.e. the integral over  Area *200 -100 
#                   given in percent i.e in [0..100]
# [p,ABC]           ABC curve spline interpolated for x values in p
# CleanedData       = ABCcleanData(Data) 
# author: MT, reimplemented from ALUs matlab version
  
V= ABCcurve(Data,p)

#ABC=V$Slope$dABC
ABCx=V$Curve[,1]
ABCy=V$Curve[,2]
Gini = Gini4ABC(ABCx,ABCy )
if(missing(p)) p=ABCx
return(list(Gini=Gini,p=p,ABC=ABCy,CleanedData=V$CleanedData))
}