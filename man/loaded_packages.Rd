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
It loads package \code{pkg} in a new R session and collects which other packages are loaded from \code{\link[utils]{sessionInfo}}.
}
\value{
A data frame.
}
\examples{
loaded_packages("ComplexHeatmap")
}
