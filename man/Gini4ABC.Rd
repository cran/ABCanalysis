\name{Gini4ABC}
\alias{Gini4ABC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Gini index
}
\description{
 Gini index for an ABC curve
}
\usage{
Gini4ABC(p, ABC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	 \item{p}{vector [1:k], cumulative population in percent}

	\item{ABC}{ vector [1:k], cumulative high data in percent}
}

\value{
   Gini              gini index i.e. the integral over ABC(p) / 0.5  *100
   
                     given in percent i.e in [0..100]
}

\author{
FL?MT?
}
