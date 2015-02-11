\name{ABCcleanData}
\alias{ABCcleanData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data cleaning for ABC analysis
}
\description{
 Only the first column of Data is used, anything not beeinh positive numerical value is set to zero
}
\usage{
ABCcleanData(Data,RemoveSmallYields)
}
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}
  \item{RemoveSmallYields}{
		bool, if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
                                  1\% of the total sum (yield) is removed 
}
}
\details{
 Data <0 are set to zero,  non-numeric values (NA,NaN,etc.) in Data are set to zero
 strings and chars are set to zero   
 infinitive numbers are set to max(Data)
 if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
 0.5 \% of the total sum (yield) is removed 
}
\value{
Output is of type list which's parts are described in the following
	\item{CleanedData}{vector [1:m], columnvector containing Data>=0 and zeros for all NA, NaN and negative values in Data(1:n)}
	\item{Data2CleanInd}{vector [1:k], Index such that CleanedData = nantozero(Data(Data2CleanInd))}
	\item{RemovedInd}{vector [1:l], Index such that Data(RemovedInd) is the data that has been removed if RemoveSmallYields==1}
}

\author{
	\url{http://www.uni-marburg.de/fb12/datenbionik}

	
Michael Thrun
}
