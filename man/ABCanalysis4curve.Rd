\name{ABCanalysis4curve}
\alias{ABCanalysis4curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{calculate ABC Analysis from a given curve.}
\description{
  calculate points A B C of the ABC Analysis from a given curve.}

\arguments{
  \item{p[1:m]}{a vector of values specifying where interpolation took place}
	\item{ABC[1:m]}{given values of the curve at positions from p}
}

\value{
  BreakEvenPunktIndex = BreakEvenPunktIndex,
              ParetoPunktIndex = ParetoPunktIndex,
              SubmarginalPunktIndex = SubmarginalPunktIndex,
              ABx = Effort[AB],
              ABy = Yield[AB],
              BCx = Effort[BC],
              BCy = Yield[BC],
              Bx = Effort[B],
              By = Yield[B]))
	\item{BreakEvenPunktIndex}{Index of breakeven point}
	\item{ParetoPunktIndex}{Index of pareto point}
  \item{SubmarginalPunktIndex}{Index of submarginal point}
  \item{ABx}{Position of AB point on x axis}
  \item{ABy}{Position of AB point on y axis}
  \item{BCx}{Position of BC point on x axis}
  \item{BCy}{Position of BC point on y axis}
  \item{Bx}{Position of the unused point (breakeven or pareto) on the x axis}
  \item{By}{Position of the unused point (breakeven or pareto) on the y axis}
}

\author{Florian Lerch}
