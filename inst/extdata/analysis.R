
.libPaths("/Volumes/Elements/all_pkg_lib/")

library(pkgndep, lib.loc = .libPaths()[2])
library(GetoptLong, lib.loc = .libPaths()[2])

setwd("~/project/development/pkgndep_analysis/")

# load package db from remote
load_pkg_db(lib = NA)

# perform pkgndep analysis for all packages
all_pkg = pkgndep:::env$pkg_db$meta$Package

lt = list()
for(i in seq_along(all_pkg)) {

	qqcat("\n++++++++++++++ @{i}/@{length(all_pkg)} ++++++++++++++\n")
	pkg = all_pkg[i]
	x = pkgndep(pkg)

	lt[[pkg]] = x
}

# pkgndep results
saveRDS(lt, file = "all_pkgs.rds", compress = "xz")
file.copy("all_pkgs.rds", "~/project/development/pkgndep.db/inst/extdata/all_pkgs.rds", overwrite = TRUE
# lt = readRDS("all_pkgs.rds")

# pkg_db
saveRDS(pkgndep:::env$pkg_db, file = "pkg_db_snapshot.rds", compress = "xz")
file.copy("pkg_db_snapshot.rds", "~/project/development/pkgndep.db/inst/extdata/pkg_db_snapshot.rds", overwrite = TRUE)


## a data frame that contains various statistics
df = data.frame(
	package = sapply(lt, function(x) x$package),
	repository = sapply(lt, function(x) x$repository),
	n_by_strong = sapply(lt, function(x) x$n_by_strong),
	n_by_all = sapply(lt, function(x) x$n_by_all),
	n_parents = sapply(lt, function(x) sum(x$which_required)),
	gini_index = sapply(lt, function(x) gini_index(x$heaviness[x$which_required] + 2))
)

df$max_heaviness_from_parents = sapply(lt, function(x) {
	if(any(x$which_required)) {
		max(x$heaviness[x$which_required])
	} else {
		0
	}
})

df$adjusted_max_heaviness_from_parents = df$max_heaviness_from_parents*(df$n_parents+30)/max(df$n_parents)

df$max_heaviness_parent_info = sapply(lt, function(x) {
	if(any(x$which_required)) {
		i = which.max(x$heaviness[x$which_required])
		if(is.infinite(x$df_imports[i, 1])) {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. It is listed in &lsquo;Depends&rsquo but no object from parent is imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		} else if(x$df_imports[i, 1] < 0) {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. The whole set of functions/methods/classes from parent package excluding @{-x$df_imports[i, 1]} functions is imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		} else if(x$df_imports[i, 2] > 0) {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 2]} S4 methods are imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		} else if(x$df_imports[i, 3] > 0) {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 3]} S4 classes are imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		} else if(x$df_imports[i, 1] == 0) {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. The whole set of functions/methods/classes from parent package is imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		} else {
			info = qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 1]} functions/objects are imported to the namespace of &lsquo;@{x$package}&rsquo;.")
		}
	} else {
		info = qq("&lsquo;@{x$package}&rsquo has no parent package.")
	}
	info
})


df$total_heaviness_from_parents = sapply(lt, function(x) {
	if(any(x$which_required)) {
		sum(x$heaviness[x$which_required])
	} else {
		0
	}
})

df$adjusted_total_heaviness_from_parents = df$total_heaviness_from_parents*sqrt(df$n_parents + 20)/max(sqrt(df$n_parents))


df$reducible = grepl("functions/objects are imported.", df$max_heaviness_parent_info)

df$max_co_heaviness_from_parents = sapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	m = co_heaviness(x)
	if(nrow(m) > 1) {
		max(m[upper.tri(m)])
	} else {
		0
	}
})

df$heaviness_on_children = sapply(lt, function(x) {
	heaviness_on_children(x$package)
})

df$n_children = sapply(lt, function(x) {
	tb = child_dependency(x, fields = c("Depends", "Imports", "LinkingTo"))
	v = nrow(tb)
	if(is.null(v)) v = 0
	v
})

score = lapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	v = heaviness_on_downstream(x, add_values_attr = TRUE)
	attr(v, "package") = x
	v
})

df$heaviness_on_downstream = sapply(score, function(x) {
	x
})

df$n_downstream = sapply(score, function(x) {
	attr(x, "all_downstream_pkgs")
})

df$hv_downstream_values = I(lapply(score, function(x) {
	attr(x, "values")
}))

df$adjusted_heaviness_on_children = sapply(lt, function(x) {
	s = heaviness_on_children(x$package, add_values_attr = TRUE)
	sum(attr(s, "values"))/(attr(s, "all_children_pkgs") + 	10)
})

df$adjusted_heaviness_on_downstream = sapply(score, function(x) {
	sum(attr(x, "values"))/(attr(x, "all_downstream_pkgs") + 15)
})


### downstream without direct child packages
df$heaviness_on_downstream_no_children = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	if(length(p)) {
		mean(v[p])
	} else {
		0
	}
})

df$n_downstream_no_children = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	length(p)
})

df$hv_downstream_no_children_values = I(lapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	v[p]
}))

df$adjusted_heaviness_on_downstream_no_children = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	if(length(p)) {
		sum(v[p])/(length(p) + 6)
	} else {
		0
	}
})


saveRDS(score, file = "pkg_stat_score.rds", compress = "xz")
saveRDS(df, file = "pkg_stat_snapshot.rds", compress = "xz")
file.copy("pkg_stat_snapshot.rds", "~/project/development/pkgndep.db/inst/extdata/pkg_stat_snapshot.rds", overwrite = TRUE)

######


### select a cutoff for adjusted heaviness

select_a_for_adjusted_heaviness = function(which = "children", all_a = 0:30, rank_diff = 50) {

	if(which == "children") {
		xy = sapply(score, function(x) {
			s = heaviness_on_children(attr(x, "package")$package, add_values_attr = TRUE)
			c(sum(attr(s, "values")), attr(s, "all_children_pkgs"))
		})
	} else if(which == "downstream") {
		xy = sapply(score, function(x) {
			c(sum(attr(x, "values")), attr(x, "all_downstream_pkgs"))
		})
	} else {
		xy = sapply(score, function(x) {
			v = attr(x, "values")
			children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

			p = setdiff(names(v), children)
			if(length(p)) {
				c(sum(v[p]), length(p))
			} else {
				c(0, 0)
			}
		})
	}

	m = NULL
	for(a in all_a) {
		v = xy[1, ]/(xy[2, ] + a)
		m = cbind(m, v)

	}

	colnames(m) = all_a
	l = df[[qq("n_@{which}")]] > 0
	m = m[l, ]

	d = NULL
	for(i in 2:ncol(m)) {
		rk = abs(rank(m[, i-1]) - rank(m[, i]))
		d[i-1] = sum(rk > rank_diff)
	}

	plot(all_a[-1], d, xlab = "value of a", ylab = "sum(abs(rank(v) - rank(prev_v)) > 50)")

	return(data.frame(a = all_a[-1], v = d))
}

d1 = select_a_for_adjusted_heaviness("children")
plot(d1[, 1], d1[, 2], xlab = "value of a", ylab = "#{abs(rank(v) - rank(prev_v)) > 50}", main = "Select a for adjusted heaviness on child packages"); abline(v = 10, col = "red")

d2 = select_a_for_adjusted_heaviness("downstream")
plot(d2[, 1], d2[, 2], xlab = "value of a", ylab = "#{abs(rank(v) - rank(prev_v)) > 50}", main = "Select a for adjusted heaviness on downstream packages"); abline(v = 15, col = "red")

d3 = select_a_for_adjusted_heaviness("downstream_no_children")
plot(d3[, 1], d3[, 2], xlab = "value of a", ylab = "#{abs(rank(v) - rank(prev_v)) > 50}", main = "Select a for adjusted heaviness on downstream packages excluding children"); abline(v = 6, col = "red")


saveRDS(list(children = d1, downstream = d2, downstream_no_children = d3), file = "adjusted_heaviness_select_a.rds", compress = "xz")
file.copy("adjusted_heaviness_select_a.rds", "~/project/development/pkgndep.db/inst/extdata/adjusted_heaviness_select_a.rds", overwrite = TRUE)

#### dependency path to all downstream packages

library(igraph)
df = load_pkg_stat_snapshot()

downstream_path_list = list()
for(package in names(lt)) {

	qqcat("========= @{package} ===========\n")
	pkg = lt[[package]]

	el = downstream_dependency(pkg$package)

	# no downstream dependency
	if(nrow(el) == 0) {
		downstream_path_list[[package]] = NULL
		cat("  no downstream dependency.\n")
		next
	}

	g = igraph::graph.edgelist(as.matrix(unique(el[, 1:2])))

	downstream_hv = df[["hv_downstream_values"]][[package]]

	downstream_hv = downstream_hv[downstream_hv > 10]
	
	# for each of its downstream package
	pl = list()
	for(node in setdiff(names(downstream_hv), package)) {
		sp = igraph::all_shortest_paths(g, pkg$package, node)$res
		sp = lapply(sp, names)

		pl = c(pl, sp)
	}
	qqcat("  @{length(pl)} shortest paths from @{length(pl)} downstream packages.\n")

	downstream_path_list[[package]] = pl
}

saveRDS(downstream_path_list, file = "pkg_downstream_dependency_path_snapshot.rds", compress = "xz")
file.copy("pkg_downstream_dependency_path_snapshot.rds", "~/project/development/pkgndep.db/inst/extdata/pkg_downstream_dependency_path_snapshot.rds", overwrite = TRUE)


path_list_to_igraph = function(pl) {
	df = data.frame(from = character(0), to = character(0))
	for(i in seq_along(pl)) {
		n = length(pl[[i]])
		df2 = data.frame(from = pl[[i]][1:(n-1)], to = pl[[i]][2:n])
		df = rbind(df, df2)
	}
	igraph::graph.edgelist(as.matrix(unique(df)))
}

g = path_list_to_igraph(pl)

d_out = degree(g, mode = "out")
leaf_nodes = names(d_out[d_out == 0])

g2 = induced_subgraph(g, names(d_out[d_out > 0]))
df = as_edgelist(g2)
df = as.data.frame(df)
colnames(df) = c("parent", "child")
df$heaviness = NA

sp = shortest.paths(g, mode = "out")
leaf_nodes_list = apply(sp, 1, function(x) {
	x = x[x == 1]
	intersect(names(x), leaf_nodes)
})

n_leaf_nodes = sapply(leaf_nodes_list, length)


