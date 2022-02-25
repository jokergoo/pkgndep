

# == title
# Get parent dependency for a package
#
# == param
# -package Package name.
# -fields Which fields in DESCRIPTION? Values should be in ``Depends``, ``Imports``, ``LinkingTo``, ``Suggests`` and ``Enhances``.
# -snapshot If it is ``TRUE``, the package database generated on 2021-10-28 is used. If it is ``FALSE``, the pakage database is directly retrieved from CRAN/Bioconductor.
#
# == value
# A data frame with parent packages as well as their heaviness on ``pacakge``. If ``snapshot`` is set to ``FALSE``, heaviness on child packages
# is set to NA.
#
# == example
# \dontrun{
# parent_dependency("ComplexHeatmap")
# }
parent_dependency = function(package, fields = NULL, snapshot = TRUE) {

	if(snapshot) {
		load_all_pkg_dep()
		lt = env$all_pkg_dep
	}

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	if(snapshot) {
		load_pkg_db(snapshot = TRUE)
		tb = env$pkg_db_snapshot$get_dependency_table(package)
	} else {
		load_pkg_db(snapshot = FALSE)
		tb = env$pkg$get_dependency_table(package)
	}
	if(is.null(tb)) {
		return(data.frame(parents = character(0), children = character(0), dep_fields = character(0), heaviness = numeric(0)))
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	l = rep(TRUE, nrow(tb))
	if(!is.null(fields)) {
		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)

	if(snapshot) {
		x = lt[[ package ]]
		if(is.null(x)) {
			heaviness = rep(0, nrow(x$dep_mat))
		} else {
			heaviness = x$heaviness[ structure(1:nrow(x$dep_mat), names = rownames(x$dep_mat))[tb[, "parents"]] ][l]
		}
		tb$heaviness = heaviness
	} else {
		tb$heaviness = NA
	}

	tb
}

# == title
# Get child dependency for a package
#
# == param
# -package Package name.
# -fields Which fields in DESCRIPTION? Values should be in ``Depends``, ``Imports``, ``LinkingTo``, ``Suggests`` and ``Enhances``.
# -snapshot If it is ``TRUE``, the package database generated on 2021-10-28 is used. If it is ``FALSE``, the pakage database is directly retrieved from CRAN/Bioconductor.
#
# == value
# A data frame with child packages as well as its heaviness on its child packages. If ``snapshot`` is set to ``FALSE``, heaviness on child packages
# is set to NA.
#
# == example
# \dontrun{
# child_dependency("ComplexHeatmap")
# }
child_dependency = function(package, fields = NULL, snapshot = TRUE) {

	if(snapshot) {
		load_all_pkg_dep()
		lt = env$all_pkg_dep
	}

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	if(snapshot) {
		load_pkg_db(snapshot = TRUE)
		tb = env$pkg_db_snapshot$get_rev_dependency_table(package)
	} else {
		load_pkg_db(snapshot = FALSE)
		tb = env$pkg_db$get_rev_dependency_table(package)
	}
	if(is.null(tb)) {
		return(data.frame(parents = character(0), children = character(0), dep_fields = character(0), heaviness = numeric(0)))
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	if(!is.null(fields)) {
		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)
	tb$dep_fields = factor(tb$dep_fields, levels = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
	tb = tb[order(tb$dep_fields, tb$children), , drop = FALSE]
	tb = tb[!duplicated(tb$children), , drop = FALSE]

	if(snapshot) {
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
	} else {
		tb$heaviness = NA
	}

	tb
}

# == title
# Get upstream dependency for a package
#
# == param
# -package Package name.
# -snapshot If it is ``TRUE``, the package database generated on 2021-10-28 is used. If it is ``FALSE``, the pakage database is directly retrieved from CRAN/Bioconductor.
#
# == details
# Upstream packages with relations of "Depends", "Imports" and "LinkingTo" are retrieved.
#
# == value
# A data frame with all upstream packages.
#
# == example
# \dontrun{
# upstream_dependency("ComplexHeatmap")
# }
upstream_dependency = function(package, snapshot = TRUE) {

	tb = parent_dependency(package, fields = c("Depends", "Imports", "LinkingTo"), snapshot = snapshot)

	if(nrow(tb) == 0) {
		return(tb)
	}

	dep_pkg = tb$parents
	tbl = list(tb)
	dep_pkg = tb[, "parents"]
	while(length(dep_pkg)) {
		tbl2 = lapply(dep_pkg, function(p) {
			parent_dependency(p, fields = c("Depends", "Imports", "LinkingTo"), snapshot = snapshot)
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
# -package Package name.
# -snapshot If it is ``TRUE``, the package database generated on 2021-10-28 is used. If it is ``FALSE``, the pakage database is directly retrieved from CRAN/Bioconductor.
#
# == details
# Downstream packages with relations of ``Depends``, ``Imports`` and ``LinkingTo`` are retrieved.
#
# == value
# A data frame with all downstream packages.
#
# == example
# \dontrun{
# downstream_dependency("ComplexHeatmap")
# }
downstream_dependency = function(package, snapshot = TRUE) {

	tb = child_dependency(package, fields = c("Depends", "Imports", "LinkingTo"), snapshot = snapshot)

	if(nrow(tb) == 0) {
		return(tb)
	}

	children_pkg = tb$children
	tbl = list(tb)
	children_pkg = tb[, "children"]
	while(length(children_pkg)) {
		tbl2 = lapply(children_pkg, function(p) {
			child_dependency(p, fields = c("Depends", "Imports", "LinkingTo"), snapshot = snapshot)
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

# == title
# Test the parent-child relationship
#
# == param
# -parent A vector of package names.
# -child A single package name.
# -... Pass to `parent_dependency`.
#
# == value
# A logical vector.
is_parent = function(parent, child, ...) {
	df = parent_dependency(child, ...)
	parent %in% df$parents 
}

# == title
# Test upstream - downstream relationship
#
# == param
# -upstream A vector of package names.
# -package A single package name.
# -... Pass to `upstream_dependency`.
#
# == value
# A logical vector.
is_upstream = function(upstream, package, ...) {
	df = upstream_dependency(package, ...)
	upstream %in% df$parents
}

