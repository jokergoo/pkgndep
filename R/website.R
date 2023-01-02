
# == title
# Database of package dependency heaviness of all R packages
#
# == param
# -version Version of the heaviness database. See `pkgndep_opt`$heaviness_db_version.
#
# == example
# if(interactive()) {
#     dependency_website()
# }
dependency_website = function(version = pkgndep_opt$heaviness_db_version) {

	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook", "cowplot")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop_wrap(paste0("Package ", p, " is needed for the database, please install it from CRAN."))
		}
	}

	callr::rscript(system.file("website", "app.R", package = "pkgndep"), cmdargs = version)
	
}



# == title
# Database of package dependency heaviness of all R packages
#
# == param
# -version Version of the heaviness database. See `pkgndep_opt`$heaviness_db_version.
#
# == example
# if(interactive()) {
#     dependency_database()
# }
dependency_database = function(version = pkgndep_opt$heaviness_db_version) {
	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook", "cowplot")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop_wrap(paste0("Package ", p, " is needed for the database, please install it from CRAN."))
		}
	}

	callr::rscript(system.file("website", "app.R", package = "pkgndep"), cmdargs = version)
}


# == title
# Database of package dependency heaviness of all R packages
#
# == param
# -version Version of the heaviness database. See `pkgndep_opt`$heaviness_db_version.
#
# == example
# if(interactive()) {
#     heaviness_database()
# }
heaviness_database = function(version = pkgndep_opt$heaviness_db_version) {
	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook", "cowplot")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop_wrap(paste0("Package ", p, " is needed for the database, please install it from CRAN."))
		}
	}

	callr::rscript(system.file("website", "app.R", package = "pkgndep"), cmdargs = version)
}
