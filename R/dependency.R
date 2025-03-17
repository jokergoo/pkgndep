

#' Get parent/child/upstream/downstream dependency of a package
#'
#' @param package A single package name.
#' @param fields Which dependency relations? The value should be a vector with values in `"Depends"`, `"Imports"`, 
#'               `"LinkingTo"`, `"Suggests"` and `"Enhances"`. The value can also be
#'               set to `"strong"` or `"weak"`, meaning storng or weak dependency relations.
#'               This argument only works for querying direct dependency relations with `parent_dependency()`
#'               and `child_dependency()`. If the value is `NULL`, all five dependency relations are included. 
#'               For remote dependency relations, only the strong dependency relations are included.
#' @parma online The query on dependency relations in the package ecosystem depends on a specific snapshot
#'               of the "package database". The function can use the newest package database directly obtained 
#'               from CRAN/Bioconductor by setting `online = TRUE`, or it uses the pre-computated package database
#'               which correspond to various R/Bioconductor versions. The version of the pre-computated package 
#'               database can be set via `pkgndep_opt`$heaviness_db_version`.
#'
#' @return
#' A data frame with four columns:
#' - parents Parent packages.
#' - children Child packages.
#' - dep_fields Dependency category.
#' - heaviness Heaviness from the parent to the child package.
#' 
#' For `upstream_dependency()` and `downstream_dependency()`, dependency relations are 
#' representated as a list of parent-child relations in the rows of the returned data frame, where
#' the heaviness column corresponds to the local heaviness from the parent to the child package.
#' 
#' @export
#' @rdname parent_dependency
#' @examples
#' \dontrun{
#' parent_dependency("ComplexHeatmap")
#' parent_dependency("ComplexHeatmap", online = TRUE)
#' parent_dependency("ComplexHeatmap", field = "strong")
#' child_dependency("ComplexHeatmap")
#' upstream_dependency("ComplexHeatmap")
#' downstream_dependency("ComplexHeatmap")
#' }
parent_dependency = function(package, fields = NULL, online = FALSE) {

	if(!online) {
		load_all_pkg_dep()
		lt = ENV$all_pkg_dep
	}

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	if(!online) {
		load_pkg_db(online = FALSE)
		tb = ENV$pkg_db_snapshot$get_dependency_table(package)
	} else {
		load_pkg_db(online = TRUE)
		tb = ENV$pkg_db$get_dependency_table(package)
	}
	if(is.null(tb)) {
		return(data.frame(parents = character(0), children = character(0), dep_fields = character(0), heaviness = numeric(0)))
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	l = rep(TRUE, nrow(tb))
	if(!is.null(fields)) {

		if("strong" %in% fields) {
			fields = c(fields, c("Depends", "Imports", "LinkingTo"))
		}
		if("weak" %in% fields) {
			fields = c(fields, c("Suggests", "Enhances"))
		}

		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)

	if(!online) {
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

#' @export
#' @rdname parent_dependency
child_dependency = function(package, fields = NULL, online = FALSE) {

	if(!online) {
		load_all_pkg_dep()
		lt = ENV$all_pkg_dep
	}

	if(inherits(package, "pkgndep")) {
		package = package$package
	}

	if(!online) {
		load_pkg_db(online = FALSE)
		tb = ENV$pkg_db_snapshot$get_rev_dependency_table(package)
	} else {
		load_pkg_db(online = TRUE)
		tb = ENV$pkg_db$get_rev_dependency_table(package)
	}
	if(is.null(tb)) {
		return(data.frame(parents = character(0), children = character(0), dep_fields = character(0), heaviness = numeric(0)))
	}
	tb = tb[, c(2, 1, 3), drop = FALSE]
	colnames(tb) = c("parents", "children", "dep_fields")

	if(!is.null(fields)) {

		if("strong" %in% fields) {
			fields = c(fields, c("Depends", "Imports", "LinkingTo"))
		}
		if("weak" %in% fields) {
			fields = c(fields, c("Suggests", "Enhances"))
		}
		l = tb[, "dep_fields"] %in% fields
		tb = tb[l, , drop = FALSE]
	}

	tb = as.data.frame(tb)
	tb$dep_fields = factor(tb$dep_fields, levels = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
	tb = tb[order(tb$dep_fields, tb$children), , drop = FALSE]
	tb = tb[!duplicated(tb$children), , drop = FALSE]

	if(!online) {
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



#' @export
#' @rdname parent_dependency
upstream_dependency = function(package, online = FALSE) {

	tb = parent_dependency(package, fields = c("Depends", "Imports", "LinkingTo"), online = online)

	if(nrow(tb) == 0) {
		return(tb)
	}

	dep_pkg = tb$parents
	tbl = list(tb)
	dep_pkg = tb[, "parents"]
	while(length(dep_pkg)) {
		tbl2 = lapply(dep_pkg, function(p) {
			parent_dependency(p, fields = c("Depends", "Imports", "LinkingTo"), online = online)
		})
		tbl2 = tbl2[sapply(tbl2, nrow) > 0]

		tbl = c(tbl, tbl2)
		dep_pkg = setdiff(unique(unlist(lapply(tbl2, function(x) x[, "parents"]))), unlist(lapply(tbl, function(x) x[, "children"])))
	}
	tb = do.call(rbind, tbl)

	tb
}



#' @export
#' @rdname parent_dependency
downstream_dependency = function(package, online = FALSE) {

	tb = child_dependency(package, fields = c("Depends", "Imports", "LinkingTo"), online = online)

	if(nrow(tb) == 0) {
		return(tb)
	}

	children_pkg = tb$children
	tbl = list(tb)
	children_pkg = tb[, "children"]
	while(length(children_pkg)) {
		tbl2 = lapply(children_pkg, function(p) {
			child_dependency(p, fields = c("Depends", "Imports", "LinkingTo"), online = online)
		})
		tbl2 = tbl2[sapply(tbl2, nrow) > 0]

		tbl = c(tbl, tbl2)
		children_pkg = setdiff(unique(unlist(lapply(tbl2, function(x) x[, "children"]))), unlist(lapply(tbl, function(x) x[, "parents"])))
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

#' Test the parent-child/upstream-downstream relationship
#'
#' @param parent A vector of package names.
#' @param package A single package name.
#' @param ... Pass to [`parent_dependency()`] or [`upstream_dependency()`].
#'
#' @return A logical vector.
#' @export
#' @rdname is_parent
#' @examples
#' \dontrun{
#' is_parent(c("grid", "colorspace"), "ComplexHeatmap")
#' }
is_parent = function(parent, package, ...) {
	df = parent_dependency(package, ...)
	parent %in% df$parents 
}


#' @param upstream A vector of package names.
#' @export
#' @rdname is_parent
#' @examples
#' \dontrun{
#' is_upstream("testthat", "RTCGA")
#' }
is_upstream = function(upstream, package, ...) {
	df = upstream_dependency(package, ...)
	upstream %in% df$parents
}

