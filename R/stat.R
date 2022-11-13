
# == title
# Heaviness from parent packages
#
# == param
# -x An object returned by `pkgndep`.
# -rel Whether to return the absolute measure or the relative measure.
# -a A constant added for calculating the relative measure.
# -only_strong_dep Whether to only return the heaviness for strong parents.
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
heaviness = function(x, rel = FALSE, a = 10, only_strong_dep = FALSE) {

	if(is.character(x)) {
		stop("`x` should be returned by `pkgndep()`.")
	}

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
	if(only_strong_dep) {
		v[x$which_required]
	} else {
		v
	}
}


heaviness_by_pair = function(x, i, j, rel = FALSE, a = 10, jaccard = FALSE) {
	p = required_dependency_packages(x, FALSE)
	v1 = length(p)
	v = 0
	if(x$which_required[i] & x$which_required[j]) {

		x2 = x
		x2$dep_fields[i] = "Suggests"
		x2$which_required[i] = FALSE
		x2$which_required_but_not_loaded[i] = FALSE
		p_i = setdiff(p, required_dependency_packages(x2, FALSE))

		x2 = x
		x2$dep_fields[j] = "Suggests"
		x2$which_required[j] = FALSE
		x2$which_required_but_not_loaded[j] = FALSE
		p_j = setdiff(p, required_dependency_packages(x2, FALSE))

		x2 = x
		x2$dep_fields[c(i, j)] = "Suggests"
		x2$which_required[c(i, j)] = FALSE
		x2$which_required_but_not_loaded[c(i, j)] = FALSE
		p_ij = setdiff(p, required_dependency_packages(x2, FALSE))

		p_common = setdiff(p_ij, c(p_i, p_j))
		v2 = length(p_common)

		if(jaccard) {
			if(length(p_ij) == 0) {
				v = 0
			} else {
				v = v2/length(p_ij)
			}
		} else if(rel) {
			v = (v1 + a)/((v1 - v2) + a)
		} else {
			v = v2
		}
	}
	v
}

# == title
# Co-heaviness for pairs of parent packages
#
# == param
# -x An object returned by `pkgndep`.
# -rel Whether to return the absolute measure or the relative measure.
# -a A constant added for calculating the relative measure.
# -jaccard Whether to return Jaccard coeffcient?
#
# == details
# Denote a package as P and its two strong parent packages as A and B, i.e., parent packages in "Depends", "Imports" and "LinkingTo", 
# the co-heaviness for A and B is calculated as follows.
#
# Denote S_A as the set of reduced dependency packages when only moving A to "Suggests" of P, and denote S_B as the set of reduced dependency
# packages when only moving B to "Suggests" of P, denote S_AB as the set of reduced dependency packages when moving A and B together to "Suggests" of P,
# the co-heaviness of A, B on P is calculatd as ``length(setdiff(S_AB, union(S_A, S_B)))``, which is the number of reduced package only caused by co-action of A and B.
#
# Note the co-heaviness is only calculated for parent packages in "Depends", "Imports" and "LinkingTo".
#
# When ``jaccard`` is set to ``TRUE``, the function returns jaccard coeffcient. ``setdiff(S_AB, union(S_A, S_B))`` is actually
# the set of dependencies imported by and only by two parent packages A and B. Thus the jaccard coeffcient is calculated as
# ``length(setdiff(S_AB, union(S_A, S_B)))/length(S_AB)``.
#
# == example
# \dontrun{
# x = pkgndep("DESeq2")
# hm = co_heaviness(x)
# ComplexHeatmap::Heatmap(hm)
# co_heaviness(x, jaccard = TRUE)
# }
co_heaviness = function(x, rel = FALSE, a = 10, jaccard = FALSE) {

	nr = nrow(x$dep_mat)
	m = matrix(NA, nrow = nr, ncol = nr)
	rownames(m) = colnames(m) = rownames(x$dep_mat)
	diag(m) = heaviness(x, rel = rel, a = a)
	if(jaccard) {
		diag(m) = sign(diag(m))
	}

	if(nr <= 1) {
		return(m)
	}

	max_pair = NULL
	v = 0
	for(i in 1:(nr-1)) {
		for(j in (i+1):nr) {
			m[i, j] = m[j, i] = heaviness_by_pair(x, i, j, rel, a, jaccard)
			if(m[i, j] > v) {
				v = m[i, j]
				max_pair = c(i, j)
			}
		}
	}
	if(length(max_pair)) {
		max_pair = rownames(m)[max_pair]
	}
	m2 = m[x$which_required, x$which_required, drop = FALSE]
	if(nrow(m2) > 1) {
		attr(m2, "max") = max(m2[upper.tri(m2)])
		attr(m2, "max_pair") = sort(max_pair)
	}
	m2
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
			if(add_values_attr) attr(v, "values") = numeric(0)
		} else {
			v = mean(hv)
			attr(v, "all_children_pkgs") = length(hv)
			if(add_values_attr) attr(v, "values") = structure(hv, names = tb[, 2])
		}
	} else {
		v = 0
		attr(v, "all_children_pkgs") = 0
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
# -via Whether to only consider downstream packages via a intermediate package?
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
heaviness_on_downstream = function(package, add_values_attr = FALSE, via = NULL) {

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

	if(is.null(via)) {
		v = mean(s)
		attr(v, "all_downstream_pkgs") = length(pkg)
		if(add_values_attr) attr(v, "values") = structure(s, names = pkg)
	} else {
		tb2 = downstream_dependency(via)
		pkg2 = intersect(pkg, unique(tb2[, 2]))
		names(s) = pkg
		s2 = s[pkg2]
		v = mean(s2)
		attr(v, "all_downstream_pkgs") = length(pkg2)
		if(add_values_attr) attr(v, "values") = structure(s2, names = pkg2)
	}
	v
}

# == title
# Heaviness from all upstream packages
# 
# == param
# -package A package name.
#
# == value
# A named vector.
#
heaviness_from_upstream = function(package) {
	df = load_pkg_stat_snapshot()

	upstream_pkgs = unique(upstream_dependency(package)[, 1])
	upstream_pkgs = setdiff(upstream_pkgs, BASE_PKGS)
	n_total = length(upstream_pkgs)	

	upstream_tb = data.frame(package = character(n_total), heaviness = numeric(n_total))
		
	if(n_total) {
		upstream_tb$package = upstream_pkgs
		for(i in seq_along(upstream_pkgs)) {
			s = df[upstream_pkgs[i], "hv_downstream_values"][[1]]
			if(!is.null(s)) {
				upstream_tb[i, "heaviness"] = s[package]
			}
		}
	}

	structure(upstream_tb$heaviness, names = upstream_tb$package)
}


# == title
# Gini index
#
# == param
# -v A numeric vector.
#
# == example
# x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
# gini_index(x$heaviness[x$which_required])
gini_index = function(v) {

	if(inherits(v, "pkgndep")) {
		return(gini_index(v$heaviness[v$which_required]))
	}
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
