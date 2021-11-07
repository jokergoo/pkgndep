\name{downstream_dependency}
\alias{downstream_dependency}
\title{
Get downstream dependency for a package
}
\description{
Get downstream dependency for a package
}
\usage{
downstream_dependency(package)
}
\arguments{

  \item{package}{Package name}

}
\details{
The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
}
\value{
A data frame with all downstream packages.
}
\examples{
downstream_dependency("ComplexHeatmap")
}
