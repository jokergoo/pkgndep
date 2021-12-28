\name{loaded_packages}
\alias{loaded_packages}
\title{
Loaded packages
}
\description{
Loaded packages
}
\usage{
loaded_packages(pkg, verbose = TRUE)
}
\arguments{

  \item{pkg}{A package name.}
  \item{verbose}{Whether to print messages.}

}
\details{
It loads \code{pkg} into a new R session and collects which other packages are loaded by parsing the output from \code{\link[utils]{sessionInfo}}.
}
\value{
A data frame.
}
\examples{
loaded_packages("ComplexHeatmap")
}
