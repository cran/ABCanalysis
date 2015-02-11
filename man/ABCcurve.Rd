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
ABCcurve(Data,RemoveSmallYields, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}
  \item{RemoveSmallYields}{
		boolean, optional, if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
                                  1\% of the total sum (yield) is removed, default=FALSE
}
  \item{p}{
		optional, an vector of values specifying where interpolation takes place, created by \code{\link{seq}} of package base}
}

\value{
Output is of type list which parts are described in the following
	\item{ABCx}{vector [1:k], cumulative population in percent}
	\item{ABCy}{vector [1:k], cumulative high data in percent}
}

\author{
Michael Thrun

	\url{http://www.uni-marburg.de/fb12/datenbionik}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ABCcurve}
\keyword{Lorenz curve}
\keyword{Lorenz}% __ONLY ONE__ keyword per line
\keyword{ABC curve}% __ONLY ONE__ keyword per line
