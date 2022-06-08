\name{gini_index}
\alias{gini_index}
\title{
Gini index
}
\description{
Gini index
}
\usage{
gini_index(v)
}
\arguments{

  \item{v}{A numeric vector.}

}
\examples{
x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
gini_index(x$heaviness[x$which_required])
}
