
.onAttach = function(libname, pkgname) {
	version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
CRAN page: https://CRAN.R-project.org/package=pkgndep
Github page: https://github.com/jokergoo/pkgndep
Documentation: https://jokergoo.github.io/pkgndep/
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
