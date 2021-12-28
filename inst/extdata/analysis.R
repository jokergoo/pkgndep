
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
file.copy("all_pkgs.rds", "../pkgndep/docs/files/all_pkgs.rds", overwrite = TRUE
# lt = readRDS("all_pkgs.rds")

# pkg_db
saveRDS(pkgndep:::env$pkg_db, file = "pkg_db_snapshot.rds", compress = "xz")
file.copy("pkg_db_snapshot.rds", "../pkgndep/docs/files/pkg_db_snapshot.rds", overwrite = TRUE)


## a data frame that contains various statistics
df = data.frame(
	package = names(lt),
	repository = sapply(lt, function(x) x$repository),
	n_by_strong = sapply(lt, function(x) x$n_by_strong),
	n_by_all = sapply(lt, function(x) x$n_by_all),
	n_parents = sapply(lt, function(x) sum(x$which_required)),
	gini_index = sapply(lt, function(x) gini_index(x$heaviness[x$which_required] + 2))
)

df$max_heaviness_from_parent = sapply(lt, function(x) {
	if(any(x$which_required)) {
		max(x$heaviness[x$which_required])
	} else {
		0
	}
})

df$max_heaviness_parent_info = sapply(lt, function(x) {
	if(any(x$which_required)) {
		i = which.max(x$heaviness[x$which_required])
		if(is.infinite(x$df_imports[i, 1])) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. It is listed in &lsquo;Depends&rsquo but no object from parent is imported to the namespace of '@{x$package}'.")
		} else if(x$df_imports[i, 1] < 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. The whole set of functions/methods/classes from parent package excluding @{-x$df_imports[i, 1]} functions is imported to the namespace of '@{x$package}'.")
		} else if(x$df_imports[i, 2] > 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 2]} S4 methods are imported to the namespace of '@{x$package}'.")
		} else if(x$df_imports[i, 3] > 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 3]} S4 classes are imported to the namespace of '@{x$package}'.")
		} else if(x$df_imports[i, 1] == 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. The whole set of functions/methods/classes from parent package is imported to the namespace of '@{x$package}'.")
		} else {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness on &lsquo;@{x$package}&rsquo;. @{x$df_imports[i, 1]} functions/objects are imported to the namespace of '@{x$package}'.")
		}
	} else {
		"&lsquo;@{x$package}&rsquo has no parent package."
	}
})

df$reducible = grepl("functions/objects are imported.", df$max_heaviness_parent_info)

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
	heaviness_on_downstream(x, add_values_attr = TRUE)
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


saveRDS(score, file = "pkg_stat_score.rds", compress = "xz")
saveRDS(df, file = "pkg_stat_snapshot.rds", compress = "xz")
file.copy("pkg_stat_snapshot.rds", "../pkgndep/docs/files/pkg_stat_snapshot.rds", overwrite = TRUE)

######


### select a cutoff for adjusted heaviness

y = df$heaviness_on_children
x = df$n_children
l = df$n_children > 0

x = x[l]
y = y[l]

# breaks = NULL
# tb = table(x)

# s = 0
# new_interval = TRUE
# for(i in seq_along(tb)) {
# 	s = tb[i] + s
# 	if(new_interval) {
# 		b1 = as.numeric(names(tb)[i])
# 	}
# 	if(s > 20) {
# 		b2 = as.numeric(names(tb)[i])
# 		breaks = rbind(breaks, c(b1, b2, s))
# 		new_interval = TRUE
# 		s = 0
# 	} else {
# 		new_interval = FALSE
# 	}
# }
# breaks[nrow(breaks), 2] = as.numeric(names(tb)[i])
# breaks[nrow(breaks), 3] = breaks[nrow(breaks), 3] + s
# colnames(breaks) = c("start", "end", "n_points")


# ## median and MAD in each interval
# me = apply(breaks, 1, function(b) {
# 	l = x %in% seq(b[1], b[2])
# 	mean(y[l])
# })
# ma = apply(breaks, 1, function(b) {
# 	l = x %in% seq(b[1], b[2])
# 	max(y[l]) - min(y[l])
# })


# y2 = sapply(1:length(x), function(i) {
# 	ind = which(breaks[, 1] <= x[i] & breaks[, 2] >= x[i])
# 	(y[i] - me[ind])/ma[ind]
# })
# ####
# q = apply(breaks, 1, function(b) {
# 	l = x %in% seq(b[1], b[2])
# 	qpois(0.9, var(y[l]))
# })


# l = sapply(1:length(x), function(i) {
# 	ind = which(breaks[, 1] <= x[i] & breaks[, 2] >= x[i])
# 	y[i] > q[ind]
# })

# plot(x, y, log = "x", col = ifelse(l, "red", "black"))
# lines(rowMeans(breaks[, 1:2]), q)


m = NULL
for(a in 0:30) {
	print(a)
	# adjusted_heaviness_on_children = sapply(lt, function(x) {
	# 	s = heaviness_on_children(x$package, add_values_attr = TRUE)
	# 	sum(attr(s, "values"))/(attr(s, "all_children_pkgs") + a)
	# })
	adjusted_heaviness_on_children = sapply(score, function(x) {
		sum(attr(x, "values"))/(attr(x, "all_downstream_pkgs") + a)
	})

	m = cbind(m, adjusted_heaviness_on_children)

}

colnames(m) = 0:30
m = m[df$n_children > 0, ]

for(i in 1:ncol(m)) {
	Sys.sleep(1)
	plot(x, m[, i], log = "x", main = i)
}

foo = numeric(0)
for(i in 2:ncol(m)) {
	rk = abs(rank(m[, i-1]) - rank(m[, i]))
	foo[i-1] = sum(rk > 50)
}

plot(foo)
abline(v = 10, col = "red")

#### dependency path to all downstream packages

library(igraph)
lt = load_all_pkg_dep()
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


