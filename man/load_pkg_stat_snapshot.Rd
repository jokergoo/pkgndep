\name{load_pkg_stat_snapshot}
\alias{load_pkg_stat_snapshot}
\title{
Load all package dependency statistics
}
\description{
Load all package dependency statistics
}
\usage{
load_pkg_stat_snapshot()
}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A data frame.
}
\examples{
\dontrun{
df = load_pkg_stat_snapshot()
head(df)
}
}
