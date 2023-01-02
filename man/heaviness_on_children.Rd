\name{heaviness_on_children}
\alias{heaviness_on_children}
\title{
Heaviness on all child packages
}
\description{
Heaviness on all child packages
}
\usage{
heaviness_on_children(package, add_values_attr = FALSE, total = FALSE)
}
\arguments{

  \item{package}{A package name.}
  \item{add_values_attr}{Whether to include "values" attribute? Internally used.}
  \item{total}{Whether to return the total heaviness?}

}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
The value is the mean heaviness of the package on all its child packages.
}
\examples{
\dontrun{
heaviness_on_children("ComplexHeatmap")
}
}
