\name{load_all_pkg_dep}
\alias{load_all_pkg_dep}
\title{
Load all package dependency data
}
\description{
Load all package dependency data
}
\usage{
load_all_pkg_dep()
}
\details{
It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
}
\value{
A list of \code{pkgndep} objects where each element corresponds to the analysis on one package.
}
\examples{
\dontrun{
lt = load_all_pkg_dep()
length(lt)
head(names(lt))
lt[["ggplot2"]]
}
}
