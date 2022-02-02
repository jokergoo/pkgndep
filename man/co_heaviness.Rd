\name{co_heaviness}
\alias{co_heaviness}
\title{
Co-heaviness for pairs of parent packages
}
\description{
Co-heaviness for pairs of parent packages
}
\usage{
co_heaviness(x, rel = FALSE, a = 10)
}
\arguments{

  \item{x}{An object returned by \code{\link{pkgndep}}.}
  \item{rel}{Whether to return the absolute measure or the relative measure.}
  \item{a}{A constant added for calculating the relative measure.}

}
\details{
Denote a package as P and its two strong parent packages as A and B, i.e., parent packages in "Depends", "Imports" and "LinkingTo", 
the co-heaviness for A and B is calculated as follows.

Denote S_A as the set of reduced dependency packages when only moving A to "Suggests" of P, and denote S_B as the set of reduced dependency
packages when only moving B to "Suggests" of P, denote S_AB as the set of reduced dependency packages when moving A and B together to "Suggests" of P,
the co-heaviness of A, B on P is calculatd as \code{length(setdiff(S_AB, union(S_A, S_B)))}, which is the number of reduced package only caused by co-action of A and B.

Note the co-heaviness is only calculated for parent packages in "Depends", "Imports" and "LinkingTo".
}
\examples{
\dontrun{
x = pkgndep("DESeq2")
hm = co_heaviness(x)
ComplexHeatmap::Heatmap(hm)
}
}
