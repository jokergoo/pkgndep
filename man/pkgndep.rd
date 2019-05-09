\name{pkgndep}
\alias{pkgndep}
\title{
Number of Loaded Packages
}
\description{
Number of Loaded Packages
}
\usage{
pkgndep(pkg)
}
\arguments{

  \item{pkg}{A string of package name or the path of the package on the local disc.}

}
\details{
It tells you how many packages are loaded if only one
package in Depends or Imports field is loaded in a fresh R session.
}
\examples{
pkgndep("ComplexHeatmap")
}
