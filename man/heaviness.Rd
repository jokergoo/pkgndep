\name{heaviness}
\alias{heaviness}
\title{
Heaviness from parent packages
}
\description{
Heaviness from parent packages
}
\usage{
heaviness(x, rel = FALSE, a = 10)
}
\arguments{

  \item{x}{An object returned by \code{\link{pkgndep}}.}
  \item{rel}{Whether to return the absolute measure or the relative measure.}
  \item{a}{A constant added for calculating the relative measure.}

}
\details{
The heaviness from a parent package is calculated as follows: If package B is in the \code{Depends}/\code{Imports}/\code{LinkingTo} fields of package A,
which means, package B is necessary for package A, denote \code{v1} as the total numbers of packages required for package A,
and \code{v2} as the total number of required packages if moving package B to \code{Suggests} (which means, now B is not necessary for A).
The absolute measure is simply \code{v1 -  v2} and relative measure is \code{(v1 + a)/(v2 + a)}.

In the second scenario, if B is in the \code{Suggests}/\code{Enhances} fields of package A, now \code{v2} is the total number of required packages if moving
B to \code{Imports}, the absolute measure is \code{v2 - v1} and relative measure is \code{(v2 + a)/(v1 + a)}.
}
\value{
A numeric vector.
}
\examples{
x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
heaviness(x)
heaviness(x, rel = TRUE)
}
