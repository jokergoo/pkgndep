\name{check_pkg}
\alias{check_pkg}
\title{
Check whether a package is available
}
\description{
Check whether a package is available
}
\usage{
check_pkg(pkg, bioc = FALSE)
}
\arguments{

  \item{pkg}{The name of the package.}
  \item{bioc}{Whether it is a Bioconductor package.}

}
\details{
One of the suggestions to avoid heavy dependencies is to put parent packages that are not frequently used
to 'Suggests' and to load them when the corresponding functions are used. Here the \code{\link{check_pkg}}
function helps to check whether these parent packages are avaiable and if not, it prints messages to guide users to install the corresponding packages.
}
\examples{
# There is no example
NULL

}
