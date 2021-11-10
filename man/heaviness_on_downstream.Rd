\name{heaviness_on_downstream}
\alias{heaviness_on_downstream}
\title{
Heaviness on all downstream packages
}
\description{
Heaviness on all downstream packages
}
\usage{
heaviness_on_downstream(package, move_to_suggests = TRUE, add_values_attr = FALSE)
}
\arguments{

  \item{package}{A package name.}
  \item{move_to_suggests}{Whether to move an \code{Imports} packages to \code{Suggests} or move a \code{Suggesets} package to \code{Imports}?}
  \item{add_values_attr}{Whether to include "values" attribute? Internally used.}

}
\value{
The value is the mean heaviness of the package on all its downstream packages. Denote \code{n} as the number of all its downstream packages,
\code{k_i} as the number of required packages for package i (i.e. total packages loaded when only loading packages in \code{Depends}, \code{Imports} and \code{LinkingTo}),
\code{v_1} is the sum of required packages: \code{v_1 = sum_i^n{k_i}}. Denote \code{p_i} as the number of required packages if moving \code{package} to \code{Suggests}s,
and \code{v_2} as the sum of required packages: \code{v_1 = sum_i^n{p_i}}. The final heaviniss on downstream packages is \code{(v_1 - v_2)/n}.
}
\examples{
\dontrun{
heaviness_on_downstream("ComplexHeatmap")
}
}
