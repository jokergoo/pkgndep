
.libPaths("/Volumes/Elements/all_pkg_lib/")

library(pkgndep, lib.loc = .libPaths()[2])
library(GetoptLong, lib.loc = .libPaths()[2])

setwd("~/project/development/pkgndep_analysis/")

load_pkg_db(lib = NA)

all_pkg = pkgndep:::env$pkg_db$meta$Package

lt = list()
for(i in seq_along(all_pkg)) {

	qqcat("\n++++++++++++++ @{i}/@{length(all_pkg)} ++++++++++++++\n")
	pkg = all_pkg[i]
	x = pkgndep(pkg)

	lt[[pkg]] = x
}

saveRDS(lt, file = "all_pkgs.rds", compress = "xz")
file.copy("all_pkgs.rds", "../pkgndep/docs/files/all_pkgs.rds", overwrite = TRUE)

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
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. It is listed in &lsquo;Depends&rsquo; but no object from the namespace is imported.")
		} else if(x$df_imports[i, 1] < 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. The whole namespace excluding @{-x$df_imports[i, 1]} objects is imported.")
		} else if(x$df_imports[i, 2] > 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. @{x$df_imports[i, 2]} S4 methods are imported.")
		} else if(x$df_imports[i, 3] > 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. @{x$df_imports[i, 3]} S4 classes are imported.")
		} else if(x$df_imports[i, 1] == 0) {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. The whole namespace is imported.")
		} else {
			qq("Parent package &lsquo;@{rownames(x$df_imports)[i]}&rsquo; contibutes the highest heaviness. @{x$df_imports[i, 1]} functions/objects are imported.")
		}
	} else {
		"Nothing is imported."
	}
})

df$improvable = grepl("functions/objects are imported.", df$max_heaviness_parent_info)

df$heaviness_on_children = sapply(lt, function(x) {
	heaviness_on_children(x$package)
})

df$n_children = sapply(lt, function(x) {
	tb = children_dependency(x, fields = c("Depends", "Imports", "LinkingTo"))
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
	sum(attr(x, "values"))/(attr(x, "all_downstream_pkgs") + 10)
})


saveRDS(score, file = "pkg_stat_score.rds", compress = "xz")
saveRDS(df, file = "pkg_stat_snapshot.rds", compress = "xz")
file.copy("pkg_stat_snapshot.rds", "../pkgndep/docs/files/pkg_stat_snapshot.rds", overwrite = TRUE)



## path

library(igraph)

package = "ggplot2"

pl = lapply(lt, function(x) {
	qqcat("========= @{x$package} ===========\n")
	
	package = x$package

	downstream_hv = df[["hv_downstream_values"]][[package]]
	downstream_hv = downstream_hv[downstream_hv > 10]
	downstream_pkg = names(downstream_hv)
	
	if(length(downstream_hv) == 0) {
		return(NULL)
	}

	el = downstream_dependency(package)
	g = igraph::graph.edgelist(as.matrix(unique(el[, 1:2])))
	    
	pl = list()
	for(i in seq_along(downstream_pkg)) {

	      sp = igraph::all_shortest_paths(g, package, downstream_pkg[i])$res
	      pl = c(pl, lapply(sp, function(x) names(x)))
	}
	pl
})

saveRDS(pl, file = "pkg_dependency_path_snapshot.rds", compress = "xz")
file.copy("pkg_dependency_path_snapshot.rds", "../pkgndep/idocs/files/pkg_dependency_path_snapshot.rds", overwrite = TRUE)


generate_ht = function(path_list) {

	if(is.null(path_list)) {
		return(NULL)
	}
	nt = matrix(nrow = 0, ncol = 2)
	n = sapply(path_list, length)
	
	l = n == 2
	if(any(l)) {
		tb = table(sapply(path_list[l], function(x) x[1]))
		nt = rbind(nt, cbind(names(tb), paste0(tb, ifelse(tb == 1, " package", " packages"))))
	}
	
	l = n > 2
	if(any(l)) {
		nt = rbind(nt, as.matrix(unique(data.frame(sapply(path_list[l], function(x) x[1]), sapply(path_list[l], function(x) x[2])))))
	}

	if(any(l)) {
		path_list = path_list[l]
		path_list = lapply(path_list, function(x) x[-1])
		nt = rbind(nt, generate_ht(path_list))
	}

	rownames(nt) = NULL
	colnames(nt) = c("parent", "child")
	nt
}

nt = lapply(pl, generate_ht)



saveRDS(nt, file = "pkg_dependency_network_snapshot.rds", compress = "xz")
file.copy("pkg_dependency_network_snapshot.rds", "../pkgndep/docs/files/pkg_dependency_network_snapshot.rds", overwrite = TRUE)

