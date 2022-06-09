

# == title
# HTML report for package dependency analysis
#
# == param
# -pkg An object from `pkgndep`.
# -file The path of the html file. If it is not specified, the report will be automatically opened in the web browser.
#
# == value
# The path of the HTML file of the report.
#
# == example
# if(interactive()) {
#     x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
#     dependency_report(x)
# }
dependency_report = function(pkg, file = NULL) {
	
	template_file = system.file("extdata", "package_report.html", package = "pkgndep")

	if(!requireNamespace("base64", quietly = TRUE)) {
		stop("Please install base64 package.")
	}

	open_browser = FALSE
	if(is.null(file)) {
		file = tempfile(fileext = ".html")
		open_browser = TRUE
	}
	
	tb = NULL
	n_total = nrow(pkg$df_imports)

	if(n_total > 0) {

		required_pkgs = rowSums(pkg$dep_mat)
		row_order = order(factor(pkg$dep_fields, levels = FIELDS), -pkg$heaviness)
		tb = as.data.frame(pkg$df_imports)
		tb = cbind(field = pkg$dep_fields, tb)
		tb$required_pkgs = required_pkgs
		tb$heaviness = pkg$heaviness

		tb = tb[row_order, , drop = FALSE]
		tb = cbind("Package"= rownames(tb), tb)
		tb = as.matrix(tb)
	}

	brew::brew(template_file, file)
	
	if(open_browser) {
		browseURL(file)
	}

	invisible(file)
}
