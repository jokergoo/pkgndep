# == title
# Number of Loaded Packages
#
# == param
# -pkg A string of package name or the path of the package on the local disc.
#
# == details
# It tells you how many packages are loaded if only one
# package in Depends or Imports field is loaded in a fresh R session.
#
# == example
# pkg_n_dep("ComplexHeatmap")
pkg_n_dep = function(pkg) {
	if(file.exists(pkg)) {
		x = read.dcf(paste0(pkg, "/DESCRIPTION"))
		x = as.data.frame(x)
	} else {
		x = packageDescription(pkg)
	}

	depends = x$Depends
	depends = gsub("\\s\\(.*?\\)", "", depends)
	depends = strsplit(depends, ",\\s*")[[1]]
	depends = depends[depends != "R"]

	imports = x$Imports
	imports = gsub("\\s\\(.*?\\)", "", imports)
	imports = strsplit(imports, ",\\s*")[[1]]

	dep_lt = lapply(depends, dep)
	names(dep_lt) = depends
	imp_lt = lapply(imports, dep)
	names(imp_lt) = imports

	all_pkg = c(depends, imports)
	all_pkg_dep = unique(unlist(c(lapply(dep_lt, function(x) x[, 1]), lapply(imp_lt, function(x) x[, 1]))))

	m = matrix(NA, nrow = length(all_pkg), ncol = length(all_pkg_dep), dimnames = list(all_pkg, all_pkg_dep))
	for(nm in names(dep_lt)) {
		y = structure(dep_lt[[nm]][, 2], names = dep_lt[[nm]][, 1])
		m[nm, names(y)] = y
	}
	for(nm in names(imp_lt)) {
		y = structure(imp_lt[[nm]][, 2], names = imp_lt[[nm]][, 1])
		m[nm, names(y)] = y
	}
	str_dist = function(x, y) {
		1 - sum(x == y, na.rm = TRUE)/length(x)
	}

	base_pkgs = c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
		"parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
	ht = Heatmap(m, row_split = c(rep("Depends", length(dep_lt)), rep("Imports", length(imports))),
		column_split = ifelse(colnames(m) %in% base_pkgs, "Base packages", "Others"),
		heatmap_legend_param = list(nrow = 1, title = ""), rect_gp = gpar(col = "#DDDDDD"),
		clustering_distance_rows = str_dist, clustering_distance_columns = str_dist,
		show_row_dend = FALSE, show_column_dend = FALSE,
		col = c("basePkgs" = "red", "loadedOnly" = "blue", "otherPkgs" = "darkgreen"),
		right_annotation = rowAnnotation(n_pkg = anno_barplot(apply(m, 1, function(x) sum(!is.na(x))), width = unit(2, "cm")),
			annotation_name_side = "top", annotation_name_rot = 0))
	draw(ht, heatmap_legend_side = "bottom", adjust_annotation_extension = FALSE,
		column_title = qq("In total @{ncol(m)} packages are loaded directly or indirectly"))

	return(invisible(c(dep_lt, imp_lt)))
}


dep = function(pkg) {
	message(paste0("loading ", pkg))
	cmd = qq("Rscript --no-init-file -e 'suppressPackageStartupMessages(library(\"@{pkg}\")); foo = sessionInfo(); df = rbind(data.frame(pkg = foo$basePkgs, type=rep(\"basePkgs\", length(foo$basePkgs))), data.frame(pkg = names(foo$loadedOnly), type=rep(\"loadedOnly\", length(foo$loadedOnly))), data.frame(pkg = names(foo$otherPkgs), type=rep(\"otherPkgs\", length(foo$otherPkgs)))); df = df[df[, 1] != \"@{pkg}\" ,]; print(df, row.names = FALSE)'")
    read.table(pipe(cmd), header = TRUE, stringsAsFactors = FALSE)
}
