\name{required_dependency_packages}
\alias{required_dependency_packages}
\title{
Required dependency packages
}
\description{
Required dependency packages
}
\usage{
required_dependency_packages(x, all = FALSE)
}
\arguments{

  \item{x}{An object from \code{\link{pkgndep}}.}
  \item{all}{Whether to include the packages required if also including packages from "Suggests"/"Enhances" field.}

}
\details{
The function returns all upstream packages.
}
\value{
A vector of package names.
}
\examples{
\dontrun{
x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
required_dependency_packages(x)
}
}
