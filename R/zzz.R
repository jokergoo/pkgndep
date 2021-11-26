
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
