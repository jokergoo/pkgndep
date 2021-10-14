


load_all_pkg_dep = function() {
	if(is.null(env$lt)) {
		lt = readRDS(system.file("extdata", "all_pkg.rds", package = "pkgndep"))
		env$lt = lt

		nt = do.call(rbind, lapply(lt, function(pkg) {
			p = rownames(pkg$m)
			data.frame(pkg = rep(pkg$package, length(p)), dep = p, category = pkg_category(pkg), stringsAsFactors = FALSE)
		}))

		env$nt = nt
	}
	invisible(NULL)
}


# == title
# Heaviness of dependency packages
#
# == param
# -x An object returned by `pkgndep`.
# -rel Whether to return the absolute measure or the relative measure.
# -a A contant added for calculating the relative measure.
# -reverse Whether to look at the reverse dependency, i.e. the heaviness the package ``x`` contribute to all its children packages.
#
# == details
# The heaviness of a dependent package is calculated as follows. If package B is in the "Depends/Imports" fields of package A,
# which means, package B will be loaded when loading package A, denote ``v1`` as the total namespaces when loading package A,
# and ``v2`` as the total number of namespaces if moving package B to "Suggests" (which means, now B is not loaded when loading A).
# The absolute measure is simply ``v1 -  v2`` and relative measure is ``(v1 + a)/(v2 + a)``. 
#
# In the second scenario, if B is in the "Suggests/Enhances" fields of package A, now ``v2`` is the total number of namespaces if moving
# B to "Depends/Imports", the absolute measure is ``v2 - v1`` and relative measure is ``(v2 + a)/(v1 + a)``.
#
# == value
# A numeric vector.
#
# == example
# x = readRDS(system.file("extdata", "ComplexHeatmap_dep.rds", package = "pkgndep"))
# heaviness(x)
# heaviness(x, rel = TRUE)
#
# heaviness("ggplot2", reverse = TRUE)
# heaviness("Hmisc", reverse = TRUE)
heaviness = function(x, rel = FALSE, a = 10, reverse = FALSE) {

	if(!reverse) {
		m = x$m[x$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
		nm = rownames(m)

		v1 = sum(apply(m, 2, function(x) any(!is.na(x))))
		v = numeric(0)
		for(i in seq_along(nm)) {
			v2 = sum(apply(m[rownames(m) != nm[i], , drop = FALSE], 2, function(x) any(!is.na(x))))
			if(rel) {
				v[i] = (v1 + a)/(v2 + a)
			} else {
				v[i] = v1 - v2
			}
		}
		names(v) = nm

		mi = x$m[!x$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
		nm = rownames(mi)
		vi = numeric(0)
		for(i in seq_along(nm)) {
			v2 = sum(apply(x$m[c(rownames(m), nm[i]), , drop = FALSE], 2, function(x) any(!is.na(x))))
			if(rel) {
				vi[i] = (v2 + a)/(v1 + a)
			} else {
				vi[i] = v2 - v1
			}
		}
		names(vi) = nm

		v = c(v, vi)
		v[rownames(x$m)]
	} else {

		load_all_pkg_dep()

		pp = x$package
		l = env$nt$dep == pp & env$nt$category %in% c("Depends", "Imports")
		if(any(l)) {
			rev_pkg = env$nt[l, "pkg"]
			rev_cate = env$nt[l, "category"]

			sapply(env$lt[rev_pkg], function(pkg) {
				pkg$heaviness[pp]
			})
		} else {
			numeric(0)
		}

	}
}

# == title
# Gini index on the heaviness
#
# == param
# -x An object returned by `pkgndep`.
# -a A constant added to heaviness.
#
gini_index = function(x, a = 2) {
	l = x$pkg_category %in% c("Imports", "Depends")
	if(sum(l) < 2) {
		0
	} else {
		v = x$heaviness[l] + a
		v = sort(v)
		n = length(v)
        s = 2 * sum(v * 1:n)/(n * sum(v)) - 1 - (1/n)
        s = n/(n - 1)*s
        max(0, s)
	}
}
