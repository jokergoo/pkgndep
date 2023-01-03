
# .libPaths("/Volumes/Elements/all_pkg_lib/")

# options(repos = BiocManager::repositories(version = "3.15"))

# options(timeout = 10000000)

# # get available package names
# av_df <- available.packages(repos = BiocManager::repositories(version = "3.15"))

# av = av_df[, 1]

# exc = dir("/Volumes/Elements/all_pkg_lib")

# ins <- av[!av %in% exc]

# for(i in seq_len(floor(length(ins)/50))) {
# 	ind = 50*(i-1) + 1:50
# 	ind = intersect(ind, 1:length(ins))
# 	try(BiocManager::install(ins[ind], 
# 		type = "source", dependency =TRUE,
# 		lib = "/Volumes/Elements/all_pkg_lib"))
# }


# tb1 = available.packages(repos = BiocManager::repositories(version = "3.15"))
# tb2 = installed.packages(lib.loc = "/Volumes/Elements/all_pkg_lib/")

# cn = intersect(tb1[, "Package"], tb2[, "Package"])

# l = which(tb1[cn, "Version"] != tb2[cn, "Version"])

# ins = cn[l]

# ###############

# .libPaths("/Volumes/Elements/all_pkg_lib/")



#### start from here

# for(r_version in rev(ALL_BIOC_RELEASES$R)) {
# 	GetoptLong:::source("~/project/development/pkgndep/inst/extdata/analysis.R", argv = paste0("-r ", r_version))
# }

r_version = "4.0.0"
library(GetoptLong)
GetoptLong(
	"r_version=s", "R version"
)

library(pkgndep)

l = ALL_BIOC_RELEASES$R == r_version
if(!any(l)) {
	stop("Cannot find", r_version, ".")
}
date = ALL_BIOC_RELEASES$Date[l]
bioc_version = ALL_BIOC_RELEASES$Release[l]

dir.create(qq("~/project/development/pkgndep.github.io/@{date}"))

.version = list(
	date = date,
	bioc_version = bioc_version
)

saveRDS(.version, qq("~/project/development/pkgndep.github.io/@{date}/version.rds"), compress = "xz")


# setwd("~/project/development/pkgndep_analysis/")

# load package db from remote
pkgndep_opt$db_file_template = function(file, version) paste0("~/project/development/pkgndep.github.io/", version, "/", file)
pkgndep_opt$heaviness_db_version = date
# db = load_pkg_db(lib = NA)

db = readRDS(qq("~/project/development/pkgndep.github.io/@{date}/pkg_df_@{bioc_version}.rds"))
db = db[!db$Package %in% pkgndep:::BASE_PKGS, , drop = FALSE]
db = reformat_db(db, version = date)
load_pkg_db(db = db)

# pkg_db
attr(db, ".version") = date
saveRDS(db, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_db_snapshot_@{bioc_version}.rds"), compress = "xz")


# perform pkgndep analysis for all packages
all_pkg = db$meta$Package

lt = list()
for(i in seq_along(all_pkg)) {

	qqcat("\n++++++++++++++ @{i}/@{length(all_pkg)} ++++++++++++++\n")
	pkg = all_pkg[i]
	x = pkgndep(pkg, online = FALSE)

	lt[[pkg]] = x
}

# pkgndep results
attr(lt, ".version") = date
saveRDS(lt, file = qq("~/project/development/pkgndep.github.io/@{date}/all_pkgs_@{bioc_version}.rds"), compress = "xz")
# lt = readRDS("all_pkgs.rds")


# # total number of dependencies
# sum(sapply(pkgndep:::ENV$pkg_db$dependency, nrow))


## a data frame that contains various statistics
df = data.frame(
	package = sapply(lt, function(x) x$package),
	version = sapply(lt, function(x) x$version),
	repository = sapply(lt, function(x) x$repository),
	n_by_strong = sapply(lt, function(x) x$n_by_strong),
	n_by_all = sapply(lt, function(x) x$n_by_all),
	n_parents = sapply(lt, function(x) sum(x$which_required))
)

df$max_rel_heaviness_from_parents = sapply(lt, function(x) {
	qqcat("===== @{x$package} ======\n")
	if(any(x$which_required)) {
		max(heaviness(x, rel = TRUE)[x$which_required])
	} else {
		1
	}
})

df$max_heaviness_from_parents = sapply(lt, function(x) {
	if(any(x$which_required)) {
		max(x$heaviness[x$which_required])
	} else {
		0
	}
})

df$adjusted_max_heaviness_from_parents = df$max_heaviness_from_parents*(df$n_parents+30)/max(df$n_parents)


df$heaviest_parent = sapply(lt, function(x) {
	if(any(x$which_required)) {
		v = (x$heaviness[x$which_required])
		names(v)[which.max(v)]
	} else {
		NA
	}
})

df$max_heaviness_parent_info = sapply(lt, function(x) {
	if(any(is.na(x$df_imports[, 1]))) {
		info = "Not available."
	} else if(any(x$which_required)) {
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

df$reducible = grepl("functions/objects are imported.", df$max_heaviness_parent_info)

df$max_co_heaviness_from_parents = sapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	m = co_heaviness(x)
	if(nrow(m) > 1) {
		attr(m, "max")
	} else {
		0
	}
})

df$max_co_heaviness_parents_pair = sapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	m = co_heaviness(x)
	if(nrow(m) > 1) {
		paste(attr(m, "max_pair"), collapse = ",")
	} else {
		""
	}
})

df$max_co_heaviness_parents_pair_type = ""

df$heaviness_on_children = sapply(lt, function(x) {
	heaviness_on_children(x$package)
})

df$n_children = sapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
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
df$heaviness_on_indirect_downstream = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	if(length(p)) {
		mean(v[p])
	} else {
		0
	}
})

df$n_indirect_downstream = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	length(p)
})

df$hv_indirect_downstream_values = I(lapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	v[p]
}))

df$adjusted_heaviness_on_indirect_downstream = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	if(length(p)) {
		sum(v[p])/(length(p) + 6)
	} else {
		0
	}
})
attr(df, ".version") = date
attr(score, ".version") = date

saveRDS(score, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_stat_score_@{bioc_version}.rds"), compress = "xz")
saveRDS(df, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_stat_snapshot_@{bioc_version}.rds"), compress = "xz")

## update `df` because `heaviness_from_upstream` needs pkg_stat_score.rds
df$max_co_heaviness_parents_pair_type = sapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	m = co_heaviness(x)
	v = ""
	if(nrow(m) > 1) {
		p = attr(m, "max_pair")
		if(length(p)) {
			if(is_parent(p[1], p[2])) {
				v = "parent-child"
			} else if(is_parent(p[2], p[1])) {
				v = "parent-child"
			} else if(is_upstream(p[1], p[2])) {
				v = "upstream-downstream"
			} else if(is_parent(p[2], p[1])) {
				v = "upstream-downstream"
			} else {
				up1 = heaviness_from_upstream(p[1])
				up2 = heaviness_from_upstream(p[2])
				# the max heaviness of common upstream
				max = attr(m, "max")
				cn = intersect(names(up1[up1 > max*0.75]), names(up2[up2 > max*0.75]))
				if(length(cn)) {
					v = "have-common-upstream"
				}
			}
		} 
	}
	v
})
saveRDS(df, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_stat_snapshot_@{bioc_version}.rds"), compress = "xz")

######


### select a cutoff for adjusted heaviness

select_a_for_adjusted_heaviness = function(which = "children", all_a = 0:30, rank_diff = max(min(50, length(lt)*0.005), 5)) {

	if(which == "children") {
		xy = sapply(score, function(x) {
			s = heaviness_on_children(attr(x, "package")$package, add_values_attr = TRUE)
			c(sum(attr(s, "values")), attr(s, "all_children_pkgs"))
		})
	} else if(which == "downstream") {
		xy = sapply(score, function(x) {
			c(sum(attr(x, "values")), attr(x, "all_downstream_pkgs"))
		})
	} else if(which == "indirect_downstream") {
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
	} else {
		xy = rbind(df$max_heaviness_from_parents, df$n_parents)
	}

	m = NULL
	for(a in all_a) {
		if(which != "parents") {
			v = xy[1, ]/(xy[2, ] + a)
		} else {
			v = xy[1, ]*(xy[2, ] + a)/max(xy[2, ])
		}
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

	return(data.frame(a = all_a[-1], v = d, rank_diff = rank_diff))
}

d1 = select_a_for_adjusted_heaviness("children")

d2 = select_a_for_adjusted_heaviness("downstream")

d3 = select_a_for_adjusted_heaviness("indirect_downstream")

d4 = select_a_for_adjusted_heaviness("parents", all_a = 1:100)

obj = list(children = d1, downstream = d2, indirect_downstream = d3, max_heaviness_from_parents = d4)

adj_cutoff = list(
	heaviness_on_children = 10,
	heaviness_on_downstream = 15,
	heaviness_on_indirect_downstream = 6,
	max_heaviness_from_parents = 30
)

knee_finder2 = function (x, y, plot = FALSE) {
    
    n = length(x)
    a = (y[n] - y[1])/(x[n] - x[1])
    b = y[1] - a * x[1]
    d = a * x + b - y
    border = max(d)*0.95
    x1 = x[ max(which(d >= border)) ]
    if (all(d <= 0))
        x1 = NA
    if (plot) {
        op = par(no.readonly = TRUE)
        par(mfrow = c(1, 2))
        plot(x, y, xlab = "index", ylab = "value")
        abline(a = b, b = a)
        abline(v = x1, col = "green")
        plot(d, xlab = "inidex", ylab = "distance to diagonal line")
        abline(v = x1, col = "green")
        par(op)
    }
    return(x1)
}

adj_cutoff$heaviness_on_children = knee_finder2(d1[, 1], d1[, 2])
adj_cutoff$heaviness_on_downstream = knee_finder2(d2[, 1], d2[, 2])
adj_cutoff$heaviness_on_indirect_downstream = knee_finder2(d3[, 1], d3[, 2])
adj_cutoff$max_heaviness_from_parents = knee_finder2(d4[, 1], d4[, 2])

obj$cutoff = adj_cutoff

par(mfrow = c(2, 2))
plot(d1[, 1], d1[, 2], xlab = "value of a", ylab = "sum(abs(rank(v) - rank(prev_v)) > v)", main = "Select a for adjusted heaviness on child packages"); abline(v = adj_cutoff$heaviness_on_children)
plot(d2[, 1], d2[, 2], xlab = "value of a", ylab = "sum(abs(rank(v) - rank(prev_v)) > v)", main = "Select a for adjusted heaviness on downstream packages"); abline(v = adj_cutoff$heaviness_on_downstream)
plot(d3[, 1], d3[, 2], xlab = "value of a", ylab = "sum(abs(rank(v) - rank(prev_v)) > v)", main = "Select a for adjusted heaviness on downstream packages excluding children"); abline(v = adj_cutoff$heaviness_on_indirect_downstream)
plot(d4[, 1], d4[, 2], xlab = "value of a", ylab = "sum(abs(rank(v) - rank(prev_v)) > v)", main = "Select a for adjusted max heaviness on parent packages"); abline(v = adj_cutoff$max_heaviness_from_parents)


attr(obj, ".version") = date
saveRDS(obj, file = qq("~/project/development/pkgndep.github.io/@{date}/adjusted_heaviness_select_a_@{bioc_version}.rds"), compress = "xz")


### calculate adjusted heaviness
df$adjusted_heaviness_on_children = sapply(lt, function(x) {
	s = heaviness_on_children(x$package, add_values_attr = TRUE)
	sum(attr(s, "values"))/(attr(s, "all_children_pkgs") + 	adj_cutoff$heaviness_on_children)
})

df$adjusted_heaviness_on_downstream = sapply(score, function(x) {
	sum(attr(x, "values"))/(attr(x, "all_downstream_pkgs") + adj_cutoff$heaviness_on_downstream)
})

df$adjusted_heaviness_on_indirect_downstream = sapply(score, function(x) {
	v = attr(x, "values")
	children = child_dependency(attr(x, "package"), fields = c("Depends", "Imports", "LinkingTo"))[, 2]

	p = setdiff(names(v), children)
	if(length(p)) {
		sum(v[p])/(length(p) + adj_cutoff$heaviness_on_indirect_downstream)
	} else {
		0
	}
})
df$adjusted_max_heaviness_from_parents = df$max_heaviness_from_parents*(df$n_parents+adj_cutoff$max_heaviness_from_parents)/max(df$n_parents)
saveRDS(df, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_stat_snapshot_@{bioc_version}.rds"), compress = "xz")


#### dependency path to all downstream packages

library(igraph)
# df = load_pkg_stat_snapshot()

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

attr(downstream_path_list, ".version") = date
saveRDS(downstream_path_list, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_downstream_dependency_path_snapshot_@{bioc_version}.rds"), compress = "xz")



library(igraph)
nt_all = data.frame(parents = character(0), children = character(0), heaviness = numeric(0))
for(package in names(lt)) {
    tb = parent_dependency(package, fields = c("Depends", "Imports", "LinkingTo"))[, c(1, 2, 4)]
    nt_all = rbind(nt_all, tb)
}
nt_all = unique(nt_all)
g = igraph::graph.edgelist(as.matrix(nt_all[, 1:2]))
E(g)$heaviness = nt_all[, 3]
saveRDS(g, file = qq("~/project/development/pkgndep.github.io/@{date}/pkg_igraph_@{bioc_version}.rds"), compress = "xz")



# path_list_to_igraph = function(pl) {
# 	df = data.frame(from = character(0), to = character(0))
# 	for(i in seq_along(pl)) {
# 		n = length(pl[[i]])
# 		df2 = data.frame(from = pl[[i]][1:(n-1)], to = pl[[i]][2:n])
# 		df = rbind(df, df2)
# 	}
# 	igraph::graph.edgelist(as.matrix(unique(df)))
# }

# g = path_list_to_igraph(pl)

# d_out = degree(g, mode = "out")
# leaf_nodes = names(d_out[d_out == 0])

# g2 = induced_subgraph(g, names(d_out[d_out > 0]))
# df = as_edgelist(g2)
# df = as.data.frame(df)
# colnames(df) = c("parent", "child")
# df$heaviness = NA

# sp = shortest.paths(g, mode = "out")
# leaf_nodes_list = apply(sp, 1, function(x) {
# 	x = x[x == 1]
# 	intersect(names(x), leaf_nodes)
# })

# n_leaf_nodes = sapply(leaf_nodes_list, length)

# #### the dependency adjacent list
# edg = NULL
# for(nm in names(lt)) {
# 	qqcat("=== @{nm} ===\n")
# 	edg = rbind(edg, child_dependency(nm))
# }



