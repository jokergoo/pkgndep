

# == title
# Get parent dependency for a package
#
# == param
# -package Package name.
# -fields Which fields in DESCRIPTION? Values should be in ``Depends``, ``Imports``, ``LinkingTo``, ``Suggests`` and ``Enhances``.
#
# == details
# The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
#
# == value
# A data frame with parent packages as well as their heaviness on ``pacakge``.
#
# == example
# parent_dependency("ComplexHeatmap")
parent_dependency = function(package, fields = NULL) {
	load_all_pkg_dep()
	lt = env$all_pkg_dep

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	load_pkg_db(snapshot = TRUE)

	tb = env$pkg_db_snapshot$get_dependency_table(package)
	if(is.null(tb)) {
		return(tb)
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	l = rep(TRUE, nrow(tb))
	if(!is.null(fields)) {
		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)

	x = lt[[ package ]]
	if(is.null(x)) {
		heaviness = rep(0, nrow(x$dep_mat))
	} else {
		heaviness = x$heaviness[ structure(1:nrow(x$dep_mat), names = rownames(x$dep_mat))[tb[, "parents"]] ][l]
	}
	tb$heaviness = heaviness

	tb
}

# == title
# Get child dependency for a package
#
# == param
# -package Package name.
# -fields Which fields in DESCRIPTION? Values should be in ``Depends``, ``Imports``, ``LinkingTo``, ``Suggests`` and ``Enhances``.
#
# == details
# The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
#
# == value
# A data frame with child packages as well as its heaviness on its child packages.
#
# == example
# \dontrun{
# child_dependency("ComplexHeatmap")
# }
child_dependency = function(package, fields = NULL) {

	load_all_pkg_dep()
	lt = env$all_pkg_dep

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	load_pkg_db(snapshot = TRUE)

	tb = env$pkg_db_snapshot$get_rev_dependency_table(package)
	if(is.null(tb)) {
		return(tb)
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	if(!is.null(fields)) {
		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)
	heaviness = numeric(nrow(tb))
	for(i in seq_len(nrow(tb))) {
		x = lt[[ tb[i, "children"] ]]
		if(is.null(x)) {
			heaviness[i] = 0
		} else {
			heaviness[i] = x$heaviness[rownames(x$dep_mat) == package]
		}
	}
	tb$heaviness = heaviness

	tb
}

# == title
# Get upstream dependency for a package
#
# == param
# -package Package name.
#
# == details
# The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
#
# == value
# A data frame with all upstream packages.
#
# == example
# upstream_dependency("ComplexHeatmap")
upstream_dependency = function(package) {

	tb = parent_dependency(package, fields = c("Depends", "Imports", "LinkingTo"))

	if(nrow(tb) == 0) {
		return(tb)
	}

	dep_pkg = tb$parents
	tbl = list(tb)
	dep_pkg = tb[, "parents"]
	while(length(dep_pkg)) {
		tbl2 = lapply(dep_pkg, function(p) {
			parent_dependency(p, fields = c("Depends", "Imports", "LinkingTo"))
		})
		tbl2 = tbl2[sapply(tbl2, nrow) > 0]

		tbl = c(tbl, tbl2)
		dep_pkg = unique(unlist(lapply(tbl2, function(x) x[, "parents"])))
	}
	tb = do.call(rbind, tbl)

	tb
}

# == title
# Get downstream dependency for a package
#
# == param
# -package Package name
#
# == details
# The dependency information is based on packages retrieved from CRAN/Bioconductor on 2021-10-28.
#
# == value
# A data frame with all downstream packages.
#
# == example
# downstream_dependency("ComplexHeatmap")
downstream_dependency = function(package) {

	tb = child_dependency(package, fields = c("Depends", "Imports", "LinkingTo"))

	if(nrow(tb) == 0) {
		return(tb)
	}

	children_pkg = tb$children
	tbl = list(tb)
	children_pkg = tb[, "children"]
	while(length(children_pkg)) {
		tbl2 = lapply(children_pkg, function(p) {
			child_dependency(p, fields = c("Depends", "Imports", "LinkingTo"))
		})
		tbl2 = tbl2[sapply(tbl2, nrow) > 0]

		tbl = c(tbl, tbl2)
		children_pkg = unique(unlist(lapply(tbl2, function(x) x[, "children"])))
	}
	tb = do.call(rbind, tbl)

	tb
}

nrow = function(x) {
	if(is.null(x)) {
		return(0)
	} else {
		base::nrow(x)
	}
}
