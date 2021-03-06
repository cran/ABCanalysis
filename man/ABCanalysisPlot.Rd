\name{ABCanalysisPlot}
\alias{ABCanalysisPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Displays ABC plot with ABCanalysis
}
\description{
 Displays ABC Curve : cumulative percentage of largest Data (effort) vs cumlative percentage of sum of largest data (yield)
 with set limits generated by an calculated ABCanalysis.
}
\usage{
ABCanalysisPlot(Data, LineType = 0, LineWidth = 3, 
ShowUniform = TRUE,title, limits = TRUE, MarkPoints = TRUE,
ABCcurvedata,ResetPlotDefaults=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
vector[1:n] describes an array of data: n cases in rows of one variable
}

  \item{LineType}{
	integer, optional, for plot default:  LineType=0 for solid line; for other line codes see documentation about pch
}
  \item{LineWidth}{
		integer, optional, width of Line, see \code{lwd} in \link{par}
}
  \item{ShowUniform}{
boolean, optional, the ABC curve of the uniform distribution is shown in plot if TRUE (default)
}
  \item{title}{string, optional, see parameter \code{main} in \link{plot}}
  \item{limits}{
boolean, = TRUE, lines of division in A, B and C are drawn, default = FALSE
}
  \item{MarkPoints}{
boolean, optional, default= TRUE, Mark the three points of interest}
  \item{ABCcurvedata}{
  optional, see \link{ABCcurve}
}
  \item{ResetPlotDefaults}{
  optional, default =TRUE. If ResetPlotDefaults=FALSE, multiple plots in one window possible, but no resetting of plot to default parameters.
}
}

\value{
object is a list of items with
 \item{ABC}{Output of \link{ABCplot}}
 \item{ABCanalysis}{Output of \link{ABCanalysis}}
}

\author{
Michael Thrun

	\url{http://www.uni-marburg.de/fb12/datenbionik}
	
}
\note{
The Break Even point is always marked with a green star.

The diagonal from (0,1) to (1,0) is the equilibrium, where effort equals yield.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{ABCanalysis}}
}
\examples{
## Standard Example
	data("SwissInhabitants")
	abc=ABCanalysisPlot(SwissInhabitants)
##	Multiple plots in one Window:
	m=runif(4,100,200)
	s=runif(4,1,10)
	Data=sapply(1:4,FUN=function(x,m,s) rnorm(1000,m,s),m,s)
	# windows() #screen devices should not be used in examples etc
	par(mfrow=c(2,2))
	for (i in 1:4)
	{
		ABCanalysisPlot(Data[,i],ResetPlotDefaults=FALSE)
	}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ABCanalysis}
\keyword{ABC analysis}% __ONLY ONE__ keyword per line
