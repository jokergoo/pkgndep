\name{reformat_db}
\alias{reformat_db}
\title{
Format the package database
}
\description{
Format the package database
}
\usage{
reformat_db(db, version = NULL)
}
\arguments{

  \item{db}{A data frame returned from \code{\link[utils]{available.packages}} or \code{\link[utils]{installed.packages}}.}
  \item{version}{Version of the database, a self-defined text.}

}
\details{
It reformats the data frame of the package database into a \code{pkg_db} class object.
}
\value{
A \code{pkg_db} class object. There are the following methods:

\describe{
  \item{\code{pkg_db$get_meta(package,field=NULL)}}{\code{field} can take values in "Package", "Version" and "Repository".}
  \item{\code{pkg_db$get_dependency_table(package)}}{Get the dependency table.}
  \item{\code{pkg_db$get_rev_dependency_table(package)}}{Get the reverse dependency table.}
  \item{\code{pkg_db$package_dependencies(package,recursive=FALSE,reverse=FALSE,which="strong",simplify=FALSE)}}{All the arguments are the same as in \code{\link[tools]{package_dependencies}}. Argument \code{simplify} controls whether to return a data frame or a simplied vector.}
}
}
\examples{
\dontrun{
db = available.packages()
db2 = reformat_db(db)

# a pkg_db object generated on 2021-10-28 can be loaded by load_pkg_db()
db2 = load_pkg_db(online = FALSE)
db2
db2$get_meta("ComplexHeatmap")
db2$get_dependency_table("ComplexHeatmap")
db2$get_rev_dependency_table("ComplexHeatmap")
db2$package_dependencies("ComplexHeatmap")
db2$package_dependencies("ComplexHeatmap", recursive = TRUE)
}
}
