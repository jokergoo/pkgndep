\name{heaviness_on_children}
\alias{heaviness_on_children}
\title{
Heaviness on all child packages
}
\description{
Heaviness on all child packages
}
\usage{
heaviness_on_children(package, add_values_attr = FALSE)
}
\arguments{

  \item{package}{A package name.}
  \item{add_values_attr}{Whether to include "values" attribute? Internally used.}

}
\value{
The value is the mean heaviness of the package on all its child packages.
}
\examples{
\dontrun{
heaviness_on_children("ComplexHeatmap")
}
}
