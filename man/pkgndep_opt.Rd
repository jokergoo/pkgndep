\name{pkgndep_opt}
\alias{pkgndep_opt}
\title{
Global parameters for pkgndep
}
\description{
Global parameters for pkgndep
}
\usage{
pkgndep_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Arguments for the parameters, see "details" section}
  \item{RESET}{Reset to default values.}
  \item{READ.ONLY}{Please ignore.}
  \item{LOCAL}{Pllease ignore.}
  \item{ADD}{Please ignore.}

}
\details{
There are following parameters:

\describe{
  \item{\code{bioc_version}}{The bioconductor version. By default it is the version corresponding to the R version under use. Please note this option is only for switching between bioc release version and development version, while not for switching to very old bioc versions.}
  \item{\code{heaviness_db_version}}{The version of the heaviness database. The value can be the corresponding bioc version, the R version or the corresponding date for the bioc release. All supported values are in the object \code{\link{ALL_BIOC_RELEASES}}.}
}
}
\examples{
pkgndep_opt
}
