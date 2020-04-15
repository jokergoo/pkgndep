\name{pkgndep}
\alias{pkgndep}
\title{
Number of Dependency Packages
}
\description{
Number of Dependency Packages
}
\usage{
pkgndep(pkg, verbose = TRUE)
}
\arguments{

  \item{pkg}{Package name or the path of the package.}
  \item{verbose}{Whether print messages. }

}
\details{
For each package listed in the "Depends", "Imports" and "Suggests" fields
in the DESCRIPTION file, this function opens a new R session, loads the package
and counts the number of namespaces that are loaded.
}
\examples{
x = pkgndep("ComplexHeatmap")
x
plot(x)
}
