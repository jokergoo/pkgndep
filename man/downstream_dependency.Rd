\name{downstream_dependency}
\alias{downstream_dependency}
\title{
Get downstream dependency for a package
}
\description{
Get downstream dependency for a package
}
\usage{
downstream_dependency(package, snapshot = TRUE)
}
\arguments{

  \item{package}{Package name.}
  \item{snapshot}{If it is \code{TRUE}, the package database generated on 2021-10-28 is used. If it is \code{FALSE}, the pakage database is directly retrieved from CRAN/Bioconductor.}

}
\details{
Downstream packages with relations of \code{Depends}, \code{Imports} and \code{LinkingTo} are retrieved.
}
\value{
A data frame with all downstream packages.
}
\examples{
\dontrun{
downstream_dependency("ComplexHeatmap")
}
}
