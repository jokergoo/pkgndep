\name{parent_dependency}
\alias{parent_dependency}
\title{
Get parent dependency for a package
}
\description{
Get parent dependency for a package
}
\usage{
parent_dependency(package, fields = NULL)
}
\arguments{

  \item{package}{Package name.}
  \item{fields}{Which fields in DESCRIPTION? Values should be in \code{Depends}, \code{Imports}, \code{LinkingTo}, \code{Suggests} and \code{Enhances}.}

}
\details{
The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
}
\value{
A data frame with parent packages as well as their heaviness on \code{pacakge}.
}
\examples{
parent_dependency("ComplexHeatmap")
}
