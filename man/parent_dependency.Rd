\name{parent_dependency}
\alias{parent_dependency}
\title{
Get parent dependency for a package
}
\description{
Get parent dependency for a package
}
\usage{
parent_dependency(package, fields = NULL, snapshot = TRUE)
}
\arguments{

  \item{package}{Package name.}
  \item{fields}{Which fields in DESCRIPTION? Values should be in \code{Depends}, \code{Imports}, \code{LinkingTo}, \code{Suggests} and \code{Enhances}.}
  \item{snapshot}{If it is \code{TRUE}, the package database generated on 2021-10-28 is used. If it is \code{FALSE}, the pakage database is directly retrieved from CRAN/Bioconductor.}

}
\details{
The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.

}
\value{
A data frame with parent packages as well as their heaviness on \code{pacakge}. If \code{snapshot} is set to \code{FALSE}, heaviness on child packages
is set to NA.
}
\examples{
\dontrun{
parent_dependency("ComplexHeatmap")
}
}
