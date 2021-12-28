\name{dependency_report}
\alias{dependency_report}
\title{
HTML report for package dependency analysis
}
\description{
HTML report for package dependency analysis
}
\usage{
dependency_report(pkg, file = NULL)
}
\arguments{

  \item{pkg}{An object from \code{\link{pkgndep}}.}
  \item{file}{The path of the html file. If it is not specified, the report will be automatically opened in the web browser.}

}
\value{
The path of the HTML file of the report.
}
\examples{
if(interactive()) {
    x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
    dependency_report(x)
}
}
