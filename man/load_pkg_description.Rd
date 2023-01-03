\name{load_pkg_description}
\alias{load_pkg_description}
\title{
Load DESCRIPTION files of all packages
}
\description{
Load DESCRIPTION files of all packages
}
\usage{
load_pkg_description()
}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A list of character vectors.
}
\examples{
\dontrun{
lt = load_pkg_description()
lt[1:2]
}
}
