


load_all_pkg_dep = function() {
	if(is.null(env$lt)) {
		lt = readRDS(system.file("extdata", "all_pkgs.rds", package = "pkgndep"))
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
		m = x$mat[x$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
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

		mi = x$mat[!x$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
		nm = rownames(mi)
		vi = numeric(0)
		for(i in seq_along(nm)) {
			v2 = sum(apply(x$mat[c(rownames(m), nm[i]), , drop = FALSE], 2, function(x) any(!is.na(x))))
			if(rel) {
				vi[i] = (v2 + a)/(v1 + a)
			} else {
				vi[i] = v2 - v1
			}
		}
		names(vi) = nm

		v = c(v, vi)
		v[rownames(x$mat)]
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
	l = x$which_imported
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



nt_heaviness = function(package, type = "end") {

	load_all_pkg_dep()
	lt = env$lt
	nt = env$nt

	pkg = lt[[package]]
		
	if(type == "end") {
		v = pkg$heaviness[pkg$which_imported]
		v = v[v > 0]

		pp = names(v)
		tb = data.frame(dep = pp, package = rep(package, length(v)), heaviness = v)

		while(1) {
			pp2 = NULL
			for(p in pp) {
				pkg = lt[[p]]
				v = pkg$heaviness[pkg$which_imported]
				l = !(names(v) %in% tb$dep & p %in% tb$package) & v > 0
				v = v[l]
				if(length(v)) {
					tb = rbind(tb, data.frame(dep = names(v), package = rep(p, length(v)), heaviness = v))
					pp2 = c(pp2, names(v))
				}
			}
			if(length(pp2) == 0) {
				break
			}
			pp = pp2
		}
		tb
	} else {
		l = nt$dep == pkg$package & nt$category %in% c("Depends", "Imports")
		
		rev_pkg = nt[l, "pkg"]

		v = sapply(lt[rev_pkg], function(pkg) {
			unname(pkg$heaviness[package])
		})
		v = v[v > 0]

		pp = names(v)
		tb = data.frame(dep = rep(package, length(v)), package = pp, heaviness = v)

		while(1) {
			pp2 = NULL
			for(p in pp) {
				l = nt$dep == p & nt$category %in% c("Depends", "Imports")
				rev_pkg = nt[l, "pkg"]
				if(length(rev_pkg) == 0) next
				v = sapply(lt[rev_pkg], function(pkg) {
					unname(pkg$heaviness[p])
				})
				v = v[v > 0 & !(names(v) %in% tb$package & p %in% tb$dep)] 

				if(length(v)) {
					tb = rbind(tb, data.frame(dep = rep(p, length(v)), package = names(v), heaviness = v))
					pp2 = c(pp2, names(v))
				}
			}
			if(length(pp2) == 0) {
				break
			}
			pp = pp2
		}
		tb
	}
}

parent_dependency = function(package) {
	if(inherits(package, "pkgndep")) {
		pkg = package
	} else {
		load_all_pkg_dep()
		pkg = env$lt[[package]]
	}

	tb = data.frame(parents = rownames(pkg$mat), 
		children = rep(pkg$package, nrow(pkg$mat)),
		category = pkg_category(pkg))
	tb$heaviness = pkg$heaviness

	tb
}

children_dependency = function(package) {

	load_all_pkg_dep()
	lt = env$lt
	nt = env$nt

	if(inherits(package, "pkgndep")) {
		package = package$package
	}
	pp = package
	l = nt$dep == pp & nt$category %in% c("Depends", "Imports")
	if(any(l)) {
		rev_pkg = nt[l, "pkg"]
		rev_cate = nt[l, "category"]

		tb = data.frame(parents = rep(pp, length(rev_pkg)), 
			children = rev_pkg, 
			category = rev_cate)
		
		tb$heaviness = sapply(lt[rev_pkg], function(pkg) {
			pkg$heaviness[pp]
		})
	} else {
		tb = data.frame(parents = character(0), children = character(0), 
			category = character(0), heaviness = numeric(0))
	}
	tb
}

heaviness_on_children = function(package) {
	tb = children_dependency(package)
	if(nrow(tb)) {
		mean(tb$heaviness)
	} else {
		0
	}
}

adjust_all_by_removing_to_suggests = function(package = NULL) {

	load_all_pkg_dep()
	lt = env$lt
	nt = env$nt

	lt2 = lt
	# assign value
	for(i in seq_along(lt2)) {
		if(is.null(package)) { 
			pkg = lt2[[i]]
			m = pkg$m[pkg$which_imported, , drop = FALSE]
			x = rownames(m)

			if(length(x)) {
				v = pkg$heaviness
				v = v[x]
				move = rownames(m) == x[which.max(v)]
				# qqcat("package '@{pkg$package}': move '@{x[which.max(v)]}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		} else {
			pkg = lt2[[i]]
			m = pkg$m[pkg$which_imported, , drop = FALSE]
			x = rownames(m)
			move = which(rownames(m) == package)
			if(length(move)) {
				# qqcat("package '@{pkg$package}': move '@{package}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		}
	}

	prev_hash = ""
	round = 0
	while(1) {
		round = round + 1
		# now adjust m and n_by_imports_suggests
		imp_lt = lapply(lt2, function(pkg) {
			loaded_namespaces(pkg, include_all = FALSE)
		})

		hash = digest::digest(imp_lt)
		if(prev_hash == hash) {
			break
		} else {
			prev_hash = hash
		}

		for(i in seq_along(lt2)) {
			pkg = lt2[[i]]
			m = pkg$m[pkg$which_imported, , drop = FALSE]

			for(nm in rownames(m)) {
				l = setdiff(colnames(pkg$m), imp_lt[[nm]])
				if(length(l)) {
					# qqcat("round @{round} @{pkg$package} (i = @{i}): removed @{sum(l)} namespaces for '@{nm}'\n")
					pkg$m[nm, l] = NA
				}	
			}
			
			n_by_depends_imports = sum(apply(pkg$m[pkg$which_imported, , drop = FALSE], 2, function(x) any(!is.na(x))))
			if(n_by_depends_imports != pkg$n_by_depends_imports) {
				# qqcat("  - @{pkg$package}: n_by_depends_imports has been adjusted from @{pkg$n_by_depends_imports} to @{n_by_depends_imports}\n")
			}
			pkg$n_by_depends_imports = n_by_depends_imports
			lt2[[i]] = pkg
		}
	}

	return(lt2)
}


