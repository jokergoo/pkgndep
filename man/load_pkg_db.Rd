\name{load_pkg_db}
\alias{load_pkg_db}
\title{
Load package database
}
\description{
Load package database
}
\usage{
load_pkg_db(lib = NULL, snapshot = FALSE, verbose = TRUE)
}
\arguments{

  \item{lib}{Local library path. If the value is \code{NA}, only remote package database is used.}
  \item{snapshot}{Internally used. If it is \code{TRUE}, the package database generated on 2021-10-28 is used.}
  \item{verbose}{Whetehr to print messages.}

}
\details{
It loads the package database from CRAN/Bioconductor and locally installed packages.

The database object internaly is cached for repeated use of other functions in this package.
}
\value{
A \code{pkg_db} class object.
}
\examples{
\dontrun{
pkg_db = load_pkg_db(lib = NA)
pkg_db
}
}
