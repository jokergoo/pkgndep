\name{load_all_pkg_dep}
\alias{load_all_pkg_dep}
\title{
Load dependency data of all packages
}
\description{
Load dependency data of all packages
}
\usage{
load_all_pkg_dep(hash = TRUE)
}
\arguments{

  \item{hash}{Whether to convert the named list to a hash table by \code{\link[hash]{hash}}.}

}
\details{
It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
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
