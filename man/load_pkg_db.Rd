\name{load_pkg_db}
\alias{load_pkg_db}
\title{
Load package database
}
\description{
Load package database
}
\usage{
load_pkg_db(lib = NULL, online = TRUE, db = NULL, verbose = TRUE)
}
\arguments{

  \item{lib}{Local library path. If the value is \code{NA}, only remote package database is used.}
  \item{online}{If the value is \code{TRUE}, it will directly use the newest package database file from CRAN/Bioconductor. If the  value is \code{FALSE}, it uses the pre-computated package database on a specific CRAN/Bioconductor snapshot.  The version of the pre-computated package database can be set via \code{\link{pkgndep_opt}}$heaviness_db_version.}
  \item{db}{A pre-computed \code{pkg_db} object.}
  \item{verbose}{Whetehr to print messages.}

}
\details{
It loads the package database from CRAN/Bioconductor and locally installed packages.

The database object internaly is cached for repeated use of other functions in this package.
}
\value{
A \code{pkg_db} class object. See \code{\link{reformat_db}} for how to use the \code{pkg_db} object.
}
\examples{
\dontrun{
pkg_db = load_pkg_db(lib = NA)
pkg_db
}
}
