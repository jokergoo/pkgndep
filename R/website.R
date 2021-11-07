# == title
# Open the online website of package dependency analysis
#
open_website = function() {

	pkgs = c("knitr", "igraph", "ggrepel", "callr", "Rook")
	for(p in pkgs) {
		if(!requireNamespace(p, quietly = TRUE)) {
			stop(paste0("Package ", p, " is needed for the website, please install it from CRAN."))
		}
	}
	callr::rscript(system.file("website", "app.R", package = "pkgndep"))
}
