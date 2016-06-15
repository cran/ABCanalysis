\name{ABCanalysis-package}
\alias{ABCanalysis-package}
\alias{ABCanalyse}
\alias{dbt.ABC}
\alias{dbt.ABCanalyse}
\alias{dbt.ABCanalysis}
\docType{package}
\title{
Computed ABC analysis 
}
\description{
Computed ABC Analysis allows the optimal calculation of three disjoint subsets A,B,C in data sets containing positive values:

subset A containing few most profitable values, i.e. largest data values ("the important few"), 
subset B containing data, where the profit gain equals effort required to obtain this gain, and the
subset C of non-profitable values, i.e. the smallest data sets ("the trivial many").

This package calculates the three subsets A, B and C by means of an algorithm based on 
statistically valid definitions of thresholds for the three sets A,B and C.
}
%\details{
%\tabular{ll}{
%Package: \tab dbt.ABC2\cr
%Type: \tab Package\cr
%Version: \tab 1.0\cr
%Date: \tab 2015-02-11\cr
%License: \tab What license is it under?\cr
%}
%~~ An overview of how to use the package, ~~
%~~ including the most important functions ~~
%}

\author{
Michael Thrun, Jorn Lotsch, Alfred Ultsch

  \url{http://www.uni-marburg.de/fb12/datenbionik}
	
\email{mthrun@mathematik.uni-marburg.de}
}
%~~ Optionally other standard keywords, one per ~~
%~~ line, from file KEYWORDS in the R ~~
%~~ documentation directory ~~
\keyword{ package }

\examples{
  data("SwissInhabitants")
	abc=ABCanalysis(SwissInhabitants,PlotIt=TRUE)
	SetA=SwissInhabitants[abc$Aind]
	SetB=SwissInhabitants[abc$Bind]
	SetC=SwissInhabitants[abc$Cind]
}
\references{
Ultsch. A ., Lotsch J.: Computed ABC Analysis for Rational Selection of Most Informative Variables in Multivariate Data, PloS one, Vol. 10(6), pp. e0129767. doi 10.1371/journal.pone.0129767, 2015.
}