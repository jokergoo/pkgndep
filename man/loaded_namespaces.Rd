\name{loaded_namespaces}
\alias{loaded_namespaces}
\title{
Loaded namespaces
}
\description{
Loaded namespaces
}
\usage{
loaded_namespaces(x, include_suggests = FALSE)
}
\arguments{

  \item{x}{An object from \code{\link{pkgndep}}.}
  \item{include_suggests}{Whether to include the namespaces that are loaded if also loading packages from "Suggests"/"Enhances" field.}

}
\value{
A vector of namespace names.
}
\examples{
# There is no example
NULL

}
