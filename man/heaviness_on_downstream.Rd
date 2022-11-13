\name{heaviness_on_downstream}
\alias{heaviness_on_downstream}
\title{
Heaviness on all downstream packages
}
\description{
Heaviness on all downstream packages
}
\usage{
heaviness_on_downstream(package, add_values_attr = FALSE, via = NULL)
}
\arguments{

  \item{package}{A package name.}
  \item{add_values_attr}{Whether to include "values" attribute? Internally used.}
  \item{via}{Whether to only consider downstream packages via a intermediate package?}

}
\value{
The value is the mean heaviness of the package on all its downstream packages. Denote \code{n} as the number of all its downstream packages,
\code{k_i} as the number of required packages for package i,
\code{v_1} as the total number of required packages for all downstream packages, i.e. \code{v_1 = sum_i^n {k_i}}. Denote \code{p_i} as the number of required packages if moving \code{package} to \code{Suggests},
and \code{v_2} as the total number of required packages, i.e. \code{v_1 = sum_i^n {p_i}}. The final heaviniss on downstream packages is \code{(v_1 - v_2)/n}.

Note since the interaction from \code{package} to its downstream packages may go through several intermediate packages, which means, the reduction of required packages
for a downstream package might be joint effects from all its upstream packages, thus, to properly calculate the heaviness of a package to its downstream packages, we first make 
a copy of the package database and move \code{package} to \code{Suggests} for all packages which depends on \code{package}. Then for all downstream packages of \code{package}, dependency analysis
by \code{\link{pkgndep}} is redone with the modified package database. Finally, the heaviness on downstream packages is collected and the mean heaviness is calculated.
}
\examples{
\dontrun{
heaviness_on_downstream("ComplexHeatmap")
}
}
