
# == title
# Heaviness from parent packages
#
# == param
# -x An object returned by `pkgndep`.
# -rel Whether to return the absolute measure or the relative measure.
# -a A constant added for calculating the relative measure.
#
# == details
# The heaviness from a parent package is calculated as follows: If package B is in the ``Depends``/``Imports``/``LinkingTo`` fields of package A,
# which means, package B is necessary for package A, denote ``v1`` as the total numbers of packages required for package A,
# and ``v2`` as the total number of required packages if moving package B to ``Suggests`` (which means, now B is not necessary for A).
# The absolute measure is simply ``v1 -  v2`` and relative measure is ``(v1 + a)/(v2 + a)``. 
#
# In the second scenario, if B is in the ``Suggests``/``Enhances`` fields of package A, now ``v2`` is the total number of required packages if moving
# B to ``Imports``, the absolute measure is ``v2 - v1`` and relative measure is ``(v2 + a)/(v1 + a)``.
#
# == value
# A numeric vector.
#
# == example
# x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
# heaviness(x)
# heaviness(x, rel = TRUE)
heaviness = function(x, rel = FALSE, a = 10) {

	v1 = length(required_dependency_packages(x, FALSE))
	nr = nrow(x$dep_mat)
	v = numeric(nr)
	for(i in seq_len(nr)) {
		x2 = x

		if(is_field_required(x$dep_fields[i])) {
			x2$dep_fields[i] = "Suggests"
			x2$which_required[i] = FALSE
			x2$which_required_but_not_loaded[i] = FALSE
			v2 = length(required_dependency_packages(x2, FALSE))
			if(rel) {
				v[i] = (v1 + a)/(v2 + a)
			} else {
				v[i] = v1 - v2
			}
		} else {
			x2$dep_fields[i] = "Imports"
			x2$which_required[i] = TRUE
			v2 = length(required_dependency_packages(x2, FALSE))
			if(rel) {
				v[i] = (v2 + a)/(v1 + a)
			} else {
				v[i] = v2 - v1
			}
		}
	}
	names(v) = rownames(x$dep_mat)
	v
}


# == title
# Heaviness on all child packages
#
# == param
# -package A package name.
# -add_values_attr Whether to include "values" attribute? Internally used.
#
# == Value
# The value is the mean heaviness of the package on all its child packages.
#
# == example
# \dontrun{
# heaviness_on_children("ComplexHeatmap")
# }
heaviness_on_children = function(package, add_values_attr = FALSE) {
	tb = child_dependency(package, fields = c("Depends", "Imports", "LinkingTo"))
	if(nrow(tb)) {
		hv = tb$heaviness
		if(length(hv) == 0) {
			v = 0
			attr(v, "all_children_pkgs") = 0
			attr(v, "gini_index") = 0
			if(add_values_attr) attr(v, "values") = numeric(0)
		} else {
			v = mean(hv)
			attr(v, "all_children_pkgs") = length(hv)
			attr(v, "gini_index") = gini_index(hv + 2)
			if(add_values_attr) attr(v, "values") = structure(hv, names = tb[, 2])
		}
	} else {
		v = 0
		attr(v, "all_children_pkgs") = 0
		attr(v, "gini_index") = 0
		if(add_values_attr) attr(v, "values") = numeric(0)
	}
	v
}

# == title
# Heaviness on all downstream packages
#
# == param
# -package A package name.
# -add_values_attr Whether to include "values" attribute? Internally used.
#
# == Value
# The value is the mean heaviness of the package on all its downstream packages. Denote ``n`` as the number of all its downstream packages,
# ``k_i`` as the number of required packages for package i,
# ``v_1`` as the total number of required packages for all downstream packages, i.e. ``v_1 = sum_i^n {k_i}``. Denote ``p_i`` as the number of required packages if moving ``package`` to ``Suggests``,
# and ``v_2`` as the total number of required packages, i.e. ``v_1 = sum_i^n {p_i}``. The final heaviniss on downstream packages is ``(v_1 - v_2)/n``.
#
# Note since the interaction from ``package`` to its downstream packages may go through several intermediate packages, which means, the reduction of required packages
# for a downstream package might be joint effects from all its upstream packages, thus, to properly calculate the heaviness of a package to its downstream packages, we first make 
# a copy of the package database and move ``package`` to ``Suggests`` for all packages which depends on ``package``. Then for all downstream packages of ``package``, dependency analysis
# by `pkgndep` is redone with the modified package database. Finally, the heaviness on downstream packages is collected and the mean heaviness is calculated.
#
# == example
# \dontrun{
# heaviness_on_downstream("ComplexHeatmap")
# }
heaviness_on_downstream = function(package, add_values_attr = FALSE) {

	if(inherits(package, "pkgndep")) package = package$package

	load_pkg_db(snapshot = TRUE)
	load_all_pkg_dep()

	pkg_db = env$pkg_db_snapshot$copy()

	# if(move_to_suggests) {
		pkg_db$dependency = lapply(pkg_db$dependency, function(db) {
			l = db[, "dependency"] == package
			if(any(l)) {
				db[l, "dep_fields"] = "Suggests"
			}
			db
		})
	# } else {
	# 	pkg_db$dependency = lapply(pkg_db$dependency, function(db) {
	# 		l = db[, "dependency"] == package
	# 		if(any(l)) {
	# 			db[l, "dep_fields"] = "Imports"
	# 		}
	# 		db
	# 	})
	# }

	# only rerun for downstrema packages
	lt = env$all_pkg_dep
	tb = downstream_dependency(package)

	if(nrow(tb) == 0) {
		if(interactive()) qqcat("no downstream dependency found for @{package}.\n")
		return(structure(0, all_downstream_pkgs = 0))
	}

	pkg = unique(tb[, "children"])

	pkg = intersect(pkg, names(lt))

	n = length(pkg)
	s1 = s2 = numeric(n)
	for(i in seq_len(n)) {
		if(interactive()) cat(strrep("\r", 100))
		if(interactive()) cat("recalculating dependency for", i, "/", n, "packages...")
		x = lt[[ pkg[i] ]]
		s1[i] = x$n_by_strong
		x = pkgndep_simplified(x$package, pkg_db)
		s2[i] = x$n_by_strong
	}
	if(interactive()) cat("\n")

	s = abs(s1 - s2)
	v = mean(s)
	attr(v, "all_downstream_pkgs") = length(pkg)
	attr(v, "gini_index") = gini_index(s + 2)
	if(add_values_attr) attr(v, "values") = structure(s, names = pkg)

	v
}


# == title
# Gini index
#
# == param
# -v A numeric vector.
#
# == example
# x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
# gini_index(x$heaviness[x$which_required] + 2)
gini_index = function(v) {
	if(length(v) < 2) {
		0
	} else {
		v = sort(v)
		n = length(v)
        s = 2 * sum(v * 1:n)/(n * sum(v)) - 1 - 1/n
        s = n/(n - 1)*s
        max(0, s)
	}
}
