
.onAttach = function(libname, pkgname) {
	version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
CRAN page: https://CRAN.R-project.org/package=pkgndep
Github page: https://github.com/jokergoo/pkgndep
Documentation: https://jokergoo.github.io/pkgndep/

If you use it in published research, please cite any of them:
Gu, Z. Pkgndep: a tool for analyzing dependency heaviness 
  of R packages. Bioinformatics 2022.
Gu, Z. On the dependency heaviness of CRAN/Bioconductor 
  ecosystem. Journal of Systems and Software 2023.

This message can be suppressed by:
  suppressPackageStartupMessages(library(pkgndep))
========================================
")	

    packageStartupMessage(msg)

}


.onLoad = function(libname, pkgname) {
	set_var()
}


set_var = function() {
	installed_tb = installed.packages()
	BASE_PKGS = names(which(installed_tb[ ,"Priority"] == "base", ))
	RECOMMENDED_PKGS = names(which(installed_tb[ ,"Priority"] == "recommended", ))

	assign('BASE_PKGS', BASE_PKGS, envir = topenv())
	assign('RECOMMENDED_PKGS', RECOMMENDED_PKGS, envir = topenv())
}
