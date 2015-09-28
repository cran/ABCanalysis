\name{ABCRemoveSmallYields}
\alias{ABCRemoveSmallYields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extended Data cleaning for ABC analysis
}
\description{
 Only the first column of Data is used, anything not beeing positive numerical value is set to zero
}
\usage{
ABCRemoveSmallYields(Data,CumSumSmallestPercentage)
}
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}
  \item{CumSumSmallestPercentage}{
 (default =0.5),the smallest data up to a cumulated sum of less than CumSumSmallestPercentage}
}
\details{
 Data <0 are set to zero,  non-numeric values (NA,NaN,etc.) in Data are set to zero
 strings and chars are set to zero   
 infinitive numbers are set to max(Data)
 the smallest data up to a cumulated sum of less than
                                  CumSumSmallestPercentage of the total sum (yield) is removed
}
\value{
Output is of type list which's parts are described in the following
	\item{SubstantialData}{columnvector containing Data>=0 and zeros for all NaN and negative values in Data(1:n)  }
	\item{Data2CleanInd}{ Index such that SubstantialData = nantozero(Data(Data2SubstantialInd))}
	\item{RemovedInd}{Data(RemovedInd) is the data that has been removed }
}

\author{
	\url{http://www.uni-marburg.de/fb12/datenbionik}

	
Michael Thrun
}
