\name{heaviness}
\alias{heaviness}
\title{
Heaviness of dependency packages
}
\description{
Heaviness of dependency packages
}
\usage{
heaviness(x, rel = FALSE, a = 10, reverse = FALSE)
}
\arguments{

  \item{x}{An object returned by \code{\link{pkgndep}}.}
  \item{rel}{Whether to return the absolute measure or the relative measure.}
  \item{a}{A contant added for calculating the relative measure.}
  \item{reverse}{Whether to look at the reverse dependency, i.e. the heaviness the package \code{x} contribute to all its children packages.}

}
\details{
The heaviness of a dependent package is calculated as follows. If package B is in the "Depends/Imports" fields of package A,
which means, package B will be loaded when loading package A, denote \code{v1} as the total namespaces when loading package A,
and \code{v2} as the total number of namespaces if moving package B to "Suggests" (which means, now B is not loaded when loading A).
The absolute measure is simply \code{v1 -  v2} and relative measure is \code{(v1 + a)/(v2 + a)}.

In the second scenario, if B is in the "Suggests/Enhances" fields of package A, now \code{v2} is the total number of namespaces if moving
B to "Depends/Imports", the absolute measure is \code{v2 - v1} and relative measure is \code{(v2 + a)/(v1 + a)}.
}
\value{
A numeric vector.
}
\examples{
x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
heaviness(x)
heaviness(x, rel = TRUE)

heaviness("ggplot2", reverse = TRUE)
heaviness("Hmisc", reverse = TRUE)
}
