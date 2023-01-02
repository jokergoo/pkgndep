\name{dependency_website}
\alias{dependency_website}
\title{
Database of package dependency heaviness of all R packages
}
\description{
Database of package dependency heaviness of all R packages
}
\usage{
dependency_website(version = pkgndep_opt$heaviness_db_version)
}
\arguments{

  \item{version}{Version of the heaviness database. See \code{\link{pkgndep_opt}}$heaviness_db_version.}

}
\examples{
if(interactive()) {
    dependency_website()
}
}
