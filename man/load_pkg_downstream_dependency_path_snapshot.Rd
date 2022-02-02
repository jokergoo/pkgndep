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
It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
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
