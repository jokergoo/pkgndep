
# == title
# The database of package dependency analysis
#
# == details
# The analyis is based on all 22076 CRAN/Bioconductor packages retrieved on 2022-06-08.
#
# == example
# if(interactive()) {
#     dependency_website()
# }
dependency_website = function() {

	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook", "cowplot")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop(paste0("Package ", p, " is needed for the website, please install it from CRAN."))
		}
	}

	callr::rscript(system.file("website", "app.R", package = "pkgndep"))
	
}

