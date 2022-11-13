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
It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2022-06-08.
}
\value{
A data frame of various columns.
}
\examples{
\dontrun{
df = load_pkg_stat_snapshot()
head(df)
}
}
