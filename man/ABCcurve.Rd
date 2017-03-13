\name{ABCcurve}
\alias{ABCcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{calculates ABC Curve
}
\description{
 Calculates cumulative percentage of largest data (effort) and cumulative percentages of sum of largest Data (yield)
 with spline interpolation (second order, piecewise) of values in-between.
}
\usage{
ABCcurve(Data, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}

  \item{p}{
		optional, an vector of values specifying where interpolation takes place, created by \code{\link{seq}} of package base}
}

\value{
Output is of type list which parts are described in the following
	\item{Curve}{
	 A list with
	 
	 \code{Effort}:vector [1:k], cumulative population in percent

	\code{Yield}: vector [1:k], cumulative high data in percent
	
	}
	\item{CleanedData}{vector [1:m], columnvector containing Data>=0 and zeros for all NA, NaN and negative values in Data(1:n)}
		\item{Slope}{

	 A list with
	 
	 \code{p}: X-values for spline interpolation, defualt: p = (0:0.01:1)

	\code{dABC}: first deviation of the functio ABC(p)=Effort(Yield
	
}
}

\author{
Michael Thrun

	\url{http://www.uni-marburg.de/fb12/datenbionik}

}
\references{
Ultsch. A ., Lotsch J.: Computed ABC Analysis for Rational Selection of Most Informative Variables in Multivariate Data, PloS one, Vol. 10(6), pp. e0129767. doi 10.1371/journal.pone.0129767, 2015.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ABCcurve}
\keyword{Lorenz curve}
\keyword{Lorenz}% __ONLY ONE__ keyword per line
\keyword{ABC curve}% __ONLY ONE__ keyword per line
