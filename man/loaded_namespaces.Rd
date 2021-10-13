\name{loaded_namespaces}
\alias{loaded_namespaces}
\title{
Loaded namespaces
}
\description{
Loaded namespaces
}
\usage{
loaded_namespaces(x, include_suggests = TRUE)
}
\arguments{

  \item{x}{The object from \code{\link{pkgndep}}.}
  \item{include_suggests}{Whether include the namespaces that are loaded if loading the packages from "Suggests" field.}

}
\value{
A vector of namespace names.
}
\examples{
# There is no example
NULL
}
