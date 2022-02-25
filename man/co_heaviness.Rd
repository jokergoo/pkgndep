\name{co_heaviness}
\alias{co_heaviness}
\title{
Co-heaviness for pairs of parent packages
}
\description{
Co-heaviness for pairs of parent packages
}
\usage{
co_heaviness(x, rel = FALSE, a = 10, jaccard = FALSE)
}
\arguments{

  \item{x}{An object returned by \code{\link{pkgndep}}.}
  \item{rel}{Whether to return the absolute measure or the relative measure.}
  \item{a}{A constant added for calculating the relative measure.}
  \item{jaccard}{Whether to return Jaccard coeffcient?}

}
\details{
Denote a package as P and its two strong parent packages as A and B, i.e., parent packages in "Depends", "Imports" and "LinkingTo", 
the co-heaviness for A and B is calculated as follows.

Denote S_A as the set of reduced dependency packages when only moving A to "Suggests" of P, and denote S_B as the set of reduced dependency
packages when only moving B to "Suggests" of P, denote S_AB as the set of reduced dependency packages when moving A and B together to "Suggests" of P,
the co-heaviness of A, B on P is calculatd as \code{length(setdiff(S_AB, union(S_A, S_B)))}, which is the number of reduced package only caused by co-action of A and B.

Note the co-heaviness is only calculated for parent packages in "Depends", "Imports" and "LinkingTo".

When \code{jaccard} is set to \code{TRUE}, the function returns jaccard coeffcient. \code{setdiff(S_AB, union(S_A, S_B))} is actually
the set of dependencies imported by and only by two parent packages A and B. Thus the jaccard coeffcient is calculated as
\code{length(setdiff(S_AB, union(S_A, S_B)))/length(S_AB)}.
}
\examples{
\dontrun{
x = pkgndep("DESeq2")
hm = co_heaviness(x)
ComplexHeatmap::Heatmap(hm)
co_heaviness(x, jaccard = TRUE)
}
}
