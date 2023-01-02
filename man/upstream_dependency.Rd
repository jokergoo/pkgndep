\name{upstream_dependency}
\alias{upstream_dependency}
\title{
Get upstream dependency for a package
}
\description{
Get upstream dependency for a package
}
\usage{
upstream_dependency(package, online = FALSE)
}
\arguments{

  \item{package}{Package name.}
  \item{online}{Whether use the newest package database directly from CRAN/Bioconductor or the pre-computated package database?  The version of the pre-computated package database can be set via \code{\link{pkgndep_opt}}$heaviness_db_version.}

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
