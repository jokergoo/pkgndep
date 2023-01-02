\name{load_pkg_downstream_dependency_path_snapshot}
\alias{load_pkg_downstream_dependency_path_snapshot}
\title{
Load downstream dependency paths for all packages
}
\description{
Load downstream dependency paths for all packages
}
\usage{
load_pkg_downstream_dependency_path_snapshot()
}
\details{
It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via \code{\link{pkgndep_opt}}$heaviness_db_version.
}
\value{
A list.
}
\examples{
\dontrun{
downstream_path_list = load_pkg_downstream_dependency_path_snapshot()
downstream_path_list[["ComplexHeatmap"]]
}
}
