\name{load_all_pkg_dep}
\alias{load_all_pkg_dep}
\title{
Load dependency analysis results of all packages
}
\description{
Load dependency analysis results of all packages
}
\usage{
load_all_pkg_dep(hash = TRUE)
}
\arguments{

  \item{hash}{Whether to convert the named list to a hash table by \code{\link[hash]{hash}}.}

}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A list (as a hash table) of \code{pkgndep} objects where each element corresponds to the analysis on one package.
}
\examples{
\dontrun{
lt = load_all_pkg_dep()
length(lt)
head(names(lt))
lt[["ggplot2"]]
}
}
