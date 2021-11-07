\name{upstream_dependency}
\alias{upstream_dependency}
\title{
Get upstream dependency for a package
}
\description{
Get upstream dependency for a package
}
\usage{
upstream_dependency(package)
}
\arguments{

  \item{package}{Package name.}

}
\details{
The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
}
\value{
A data frame with all upstream packages.
}
\examples{
upstream_dependency("ComplexHeatmap")
}
