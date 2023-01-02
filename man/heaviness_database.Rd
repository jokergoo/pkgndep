\name{heaviness_database}
\alias{heaviness_database}
\title{
Database of package dependency heaviness of all R packages
}
\description{
Database of package dependency heaviness of all R packages
}
\usage{
heaviness_database(version = pkgndep_opt$heaviness_db_version)
}
\arguments{

  \item{version}{Version of the heaviness database. See \code{\link{pkgndep_opt}}$heaviness_db_version.}

}
\examples{
if(interactive()) {
    heaviness_database()
}
}
