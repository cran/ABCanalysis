\name{GiniIndex}
\alias{GiniIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Gini-Index
}
\description{
calculation of the Gini-Index from Data 
}
\usage{
GiniIndex(Data,p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}

  \item{p}{
		optional, an vector of values specifying where interpolation takes place, created by \code{\link{seq}} of package base}
}
\details{
uses ABCcurve and Gini4ABC
}
\value{
  \item{Gini}{
               gini index i.e. the integral over  Area *200 -100 
                   given in percent i.e in [0..100]
}

	 \item{p}{vector [1:k], cumulative population in percent}

	\item{ABC}{ vector [1:k], cumulative high data in percent}
	
	\item{CleanedData}{vector [1:m], columnvector containing Data>=0 and zeros for all NA, NaN and negative values in Data(1:n)}
}

\author{
Michael Thrun
}

