\name{child_dependency}
\alias{child_dependency}
\title{
Get child dependency for a package
}
\description{
Get child dependency for a package
}
\usage{
child_dependency(package, fields = NULL)
}
\arguments{

  \item{package}{Package name.}
  \item{fields}{Which fields in DESCRIPTION? Values should be in \code{Depends}, \code{Imports}, \code{LinkingTo}, \code{Suggests} and \code{Enhances}.}

}
\details{
The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
}
\value{
A data frame with child packages as well as its heaviness on its child packages.
}
\examples{
\dontrun{
child_dependency("ComplexHeatmap")
}
}
