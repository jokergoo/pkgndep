\name{downstream_dependency}
\alias{downstream_dependency}
\title{
Get downstream dependency for a package
}
\description{
Get downstream dependency for a package
}
\usage{
downstream_dependency(package, online = FALSE)
}
\arguments{

  \item{package}{Package name.}
  \item{online}{Whether use the newest package database directly from CRAN/Bioconductor or the pre-computated package database?  The version of the pre-computated package database can be set via \code{\link{pkgndep_opt}}$heaviness_db_version.}

}
\details{
Downstream packages with relations of \code{Depends}, \code{Imports} and \code{LinkingTo} are retrieved.
}
\value{
A data frame with all downstream packages.
}
\examples{
\dontrun{
downstream_dependency("ComplexHeatmap")
}
}
