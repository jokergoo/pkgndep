\name{heaviness_from_upstream}
\alias{heaviness_from_upstream}
\title{
Heaviness from all upstream packages
}
\description{
Heaviness from all upstream packages
}
\usage{
heaviness_from_upstream(package)
}
\arguments{

  \item{package}{A package name.}

}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A named vector.
}
\examples{
# There is no example
NULL

}
