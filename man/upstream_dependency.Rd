\name{upstream_dependency}
\alias{upstream_dependency}
\title{
Get upstream dependency for a package
}
\description{
Get upstream dependency for a package
}
\usage{
upstream_dependency(package, snapshot = TRUE)
}
\arguments{

  \item{package}{Package name.}
  \item{snapshot}{If it is \code{TRUE}, the package database generated on 2021-10-28 is used. If it is \code{FALSE}, the pakage database is directly retrieved from CRAN/Bioconductor.}

}
\details{
Upstream packages with relations of "Depends", "Imports" and "LinkingTo" are retrieved.
}
\value{
A data frame with all upstream packages.
}
\examples{
\dontrun{
upstream_dependency("ComplexHeatmap")
}
}
