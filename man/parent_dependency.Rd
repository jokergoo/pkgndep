\name{parent_dependency}
\alias{parent_dependency}
\title{
Get parent dependency for a package
}
\description{
Get parent dependency for a package
}
\usage{
parent_dependency(package, fields = NULL, online = FALSE)
}
\arguments{

  \item{package}{Package name.}
  \item{fields}{Which fields in DESCRIPTION? Values should be in \code{Depends}, \code{Imports}, \code{LinkingTo}, \code{Suggests} and \code{Enhances}. The value can also be set to \code{strong} or \code{weak}.}
  \item{online}{Whether use the newest package database directly from CRAN/Bioconductor or the pre-computated package database?  The version of the pre-computated package database can be set via \code{\link{pkgndep_opt}}$heaviness_db_version.}

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
