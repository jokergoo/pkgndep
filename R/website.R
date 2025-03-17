
#' @rdname dependency_database
#' @export
dependency_website = function(version = pkgndep_opt$heaviness_db_version) {
	dependency_database(version)
}



#' Database of package dependency heaviness
#'
#' @param version Version of the heaviness database. See `pkgndep_opt`$heaviness_db_version.
#'
#' @export
#' @rdname dependency_database
#' @examples
#' if(interactive()) {
#'     dependency_database()
#' }
dependency_database = function(version = pkgndep_opt$heaviness_db_version) {

	check_pkg("knitr", bioc = FALSE)
	check_pkg("igraph", bioc = FALSE)
	check_pkg("ggrepel", bioc = FALSE)
	check_pkg("callr", bioc = FALSE)
	check_pkg("Rook", bioc = FALSE)
	check_pkg("cowplot", bioc = FALSE)

	callr::rscript(system.file("website", "app.R", package = "pkgndep"), cmdargs = version)
}



#' @rdname dependency_database
#' @export
heaviness_database = function(version = pkgndep_opt$heaviness_db_version) {
	dependency_database(version)
}
