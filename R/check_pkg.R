
# == title
# Check whether a package is available
#
# == param
# -pkg The name of the package.
# -bioc Whether it is a Bioconductor package.
#
# == details
# One of the suggestions to avoid heavy dependencies is to put parent packages that are not frequently used
# to 'Suggests' and to load them when the corresponding functions are used. Here the `check_pkg`
# function helps to check whether these parent packages are avaiable and if not, it prints messages to guide users to install the corresponding packages.
#
check_pkg = function(pkg, bioc = FALSE) {
	if(requireNamespace(pkg, quietly = TRUE)) {
		return(NULL)
	} else {

		if(!interactive()) {
			if(bioc) {
				stop(paste0("You need to manually install package '", pkg, "' from Bioconductor."))
			} else {
				stop(paste0("You need to manually install package '", pkg, "' from CRAN."))
			}
		}

		if(bioc) {
			answer = readline(paste0("Package '", pkg, "' is required but not installed. Do you want to install it from Bioconductor? [y|n] "))
		} else {
			answer = readline(paste0("Package '", pkg, "' is required but not installed. Do you want to install it from CRAN? [y|n] "))
		}

		if(bioc) {
			if(tolower(answer) %in% c("y", "yes")) {
				if(!requireNamespace("BiocManager", quietly = TRUE)) {
					install.packages("BiocManager")
				}
				BiocManager::install(pkg)
			} else {
				stop(paste0("You need to manually install package '", pkg, "' from Bioconductor."))
			}
		} else {
			if(tolower(answer) %in% c("y", "yes")) {
				install.packages(pkg)
			} else {
				stop(paste0("You need to manually install package '", pkg, "' from CRAN."))
			}
		}
	}
}
