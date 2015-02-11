\name{ABCplot}
\alias{ABCplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
displays an ABC Curve as an alternative to an Lorenz curve
}
\description{
Plots cumulative percentage of largest data (effort) vs. cumulative percentage of sum of largest data (yield)
}
\usage{
ABCplot(Data, LineType = 0, LineWidth = 3,RemoveSmallYields, ShowUniform = TRUE,
 title, ABCcurvedata,defaultAxes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
   vector[1:n], describes an array of data: n cases in rows of one variable
}
  \item{LineType}{
		for plot default:  LineType=0 for a line, other line codes see documentation about \code{pch} in \link{par}
}
  \item{LineWidth}{
		integer, width of Line, see \code{lwd} in \link{par}
}
  \item{RemoveSmallYields}{
		bool, if RemoveSmallYields ==TRUE => the smallest data up to a cumulated sum of less than
                                  1\% of the total sum (yield) is removed 
}
  \item{ShowUniform}{
		bool, =TRUE: the ABC curve of the uniform distribution is shown in plot
}
  \item{title}{
  	string, optional, see parameter \code{main} in \link{plot}
}

  \item{ABCcurvedata}{
  optional, see \link{ABCcurve}
}
  \item{defaultAxes}{
  optional, boolean, see parameter \code{axes} in \link{plot}
}
}

\value{
Output is of type list which parts are described in the following
	\item{ABCx}{vector [1:k], cumulative population in percent}
	\item{ABCy}{vector [1:k], cumulative high Data in percent}
}
\note{
The diagonal from (1,0) to (0,1) is the Equilibrium, where effort equals yield
}

\author{
Michael Thrun

	\url{http://www.uni-marburg.de/fb12/datenbionik}
	
}

\examples{
data("SwissInhabitants")
vec=ABCplot(SwissInhabitants)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Lorenz curve}
\keyword{ABC}
\keyword{ABC analysis}
\keyword{Lorenz}% __ONLY ONE__ keyword per line
