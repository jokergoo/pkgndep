\name{load_pkg_namespace}
\alias{load_pkg_namespace}
\title{
Load NAMESPACE files of all packages
}
\description{
Load NAMESPACE files of all packages
}
\usage{
load_pkg_namespace()
}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A list of character vectors.
}
\examples{
\dontrun{
lt = load_pkg_namespace()
lt[1:2]
}
}
