\name{ABCanalysis}
\alias{ABCanalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computed ABC analysis: calculates a division of the data in 3 classes A, B and C 
}
\description{
 divide the Data in 3 classes A, B and C such that
 
 A=Data[Aind] : with low effort much yield
 
 B=Data[Bind] : yield and effort are about equal
 
 C=Data[Cind] : with much effort low yield
}
\usage{
ABCanalysis(Data,PlotIt,ABCcurvedata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
vector(1:n) describes an array of data: n cases in rows of one variable, if matrix or dataframe then first column will be used.
}
  \item{PlotIt}{
	if variable is used, a plot is made, set with arbitrary value
}
  \item{ABCcurvedata}{
	only for internal usage, list from \link{ABCcurve}
}
}
\details{
Pareto point: Minimum distance to (0,1) = minimal unrealized potential

BreakEven Point: \code{B_x} is the x value of the point, where the slope of ABCcurve equals one.

For further description to \code{p} in variable \code{AlimitIndInInterpolation} see \link{ABCcurve}
}
\value{
Output is of type list which parts are described in the following
	\item{Aind}{vector [1:j],  A==Data(Aind) : with little effort much Yield}
	\item{Bind}{vector [1:l],  B==Data(Bind) : effort and Yield are balanced}
	\item{Cind}{(vector [1:m],  C==Data(Cind) : much effort for little Yield}
	\item{ABexchanged}{Boolean, TRUE if Point A is the Break Even and point B is the Pareto Point, FALSE otherwise}
	\item{A}{c(Ax,Ay), Pareto point or BreakEven Point indicated by ABexchanged}
	\item{B}{c(Bx,By), Pareto point or BreakEven Point indicated by ABexchanged}
	\item{C}{Submarginal point: minimum distance to \code{[B_x,1]} }
	\item{smallestAData}{Boundary AB, defined by point A or B with ABexchanged}
	\item{smallestBData}{Boundary BC, defined by point C}
	\item{AlimitIndInInterpolation}{index  of AB Boundary in [\code{p}, ABC], the interpolation of the ABC plot}
	\item{BlimitIndInInterpolation}{index  of BC Boundary in [\code{p}, ABC], the interpolation of the ABC plot}
	
}

\author{
Michael Thrun

	\url{http://www.uni-marburg.de/fb12/datenbionik}

}

\seealso{
\code{\link{ABCplot}}
}
\examples{
  data("SwissInhabitants")
	abc=ABCanalysis(SwissInhabitants,PlotIt=TRUE)
	A=abc$Aind
	B=abc$Bind
	C=abc$Cind
	Agroup=SwissInhabitants[A]
	Bgroup=SwissInhabitants[B]
	Cgroup=SwissInhabitants[C]

}
\references{
Ultsch. A ., Lotsch J.: Computed ABC Analysis for rational Selection of most informative Variables in multivariate Data, PLoS One, Jun 10, 10(6), e0129767, 2015.
}
\keyword{ABC}
\keyword{ABCanalysis}
\keyword{ABC analysis}
\keyword{Lorenz curve}
\keyword{Lorenz}% __ONLY ONE__ keyword per line
