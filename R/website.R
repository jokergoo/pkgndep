
# Open the online website of package dependency analysis
#
#
# == details
# The analyis is based on all CRAN/Bioconductor packages retrieved on 2021-10-28.
#
dependency_website = function() {

	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop(paste0("Package ", p, " is needed for the website, please install it from CRAN."))
		}
	}

	callr::rscript(system.file("website", "app.R", package = "pkgndep"))
	
}

