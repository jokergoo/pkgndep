\name{get_all_functions_imported_to_children}
\alias{get_all_functions_imported_to_children}
\title{
Get functions that are imported to its child packages
}
\description{
Get functions that are imported to its child packages
}
\usage{
get_all_functions_imported_to_children(package)
}
\arguments{

  \item{package}{Package name.}

}
\details{
The information is based on pre-computated results for a specific CRAN/Bioconductor snapshot. See \code{\link{pkgndep}}$heaviness_db_version for how to set the version of the snapshot.
}
\value{
It returns a list of function names that are imported to every of its child packages.
}
\examples{
\dontrun{
get_all_functions_imported_to_children("circlize")
}
}
