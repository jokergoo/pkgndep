
CUTOFF = list()
CUTOFF$adjusted_max_heaviness_from_parents = c(50, 60)
CUTOFF$adjusted_total_heaviness_from_parents = c(70, 90)
CUTOFF$adjusted_heaviness_on_children = c(15, 30)
CUTOFF$adjusted_heaviness_on_indirect_downstream = c(10, 20)

page_select = function(current_page, n_page, param_str = '') {
	pages = seq(current_page - 4, current_page + 4)
	n_select = length(pages)

	if(pages[1] < 1 & pages[n_select] > n_page) {
		pages = seq(1, n_page)
	} else if(pages[1] < 1) {
		pages = seq(1, min(c(n_page, n_select)))
	} else if(pages[n_select] > n_page) {
		pages = seq(max(c(1, pages[1])), n_page)
	}
	
	if(n_page == 1) {
		pages_html = ""
	} else {
		l = pages == current_page
		i = which(l)
		pages_html = ifelse(l, qq("<li class='active'><a href='?page=@{pages}&@{param_str}'>@{pages}</a></li>\n", collapse = FALSE),
			qq("<li><a href='?page=@{pages}&@{param_str}'>@{pages}</a></li>\n", collapse = FALSE))

		pages_html = c(qq("<li><a href='?page=1&@{param_str}'>First</a></li>\n"),
			pages_html,
			qq("<li><a href='?page=@{n_page}&@{param_str}'>Last</a></li>\n"))
	}
	pages_html = c("<nav><ul class='pagination'>", pages_html, "</ul></nav>\n")
	paste(pages_html, collapse = " ")
}

page_select2 = function(current_page, n_page, which_table, package, records_per_page = 20, other_param = "") {
	pages = seq(current_page - 4, current_page + 4)
	n_select = length(pages)

	if(pages[1] < 1 & pages[n_select] > n_page) {
		pages = seq(1, n_page)
	} else if(pages[1] < 1) {
		pages = seq(1, min(c(n_page, n_select)))
	} else if(pages[n_select] > n_page) {
		pages = seq(max(c(1, pages[1])), n_page)
	}
	
	if(n_page == 1) {
		pages_html = ""
	} else {
		l = pages == current_page
		i = which(l)
		pages_html = ifelse(l, qq("<li class='active'><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{pages}, @{records_per_page}, \"@{other_param}\");false;'>@{pages}</a></li>\n", collapse = FALSE),
			qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{pages}, @{records_per_page}, \"@{other_param}\");false;'>@{pages}</a></li>\n", collapse = FALSE))

		pages_html = c(qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", 1, @{records_per_page}, \"@{other_param}\");false;'>First</a></li>\n"),
			pages_html,
			qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{n_page}, @{records_per_page}, \"@{other_param}\");false;'>Last</a></li>\n"))
	}
	pages_html = c("<nav><ul class='pagination'>", pages_html, "</ul></nav>\n")
	paste(pages_html, collapse = " ")
}

html_template = function(template, vars = list()) {

	if(identical(unname(Sys.info()[c("sysname", "user")]), c("Darwin", "guz"))) {
		template_dir = "~/project/development/pkgndep/inst/website/template"
	} else {
		template_dir = system.file("website", "template", package = "pkgndep")
	}

	f = tempfile()

	on.exit(file.remove(f))
	for(nm in names(vars)) {
		assign(nm, vars[[nm]])
	}
	brew::brew(paste0(template_dir, "/", template, ".html"), f)
	paste(readLines(f), collapse = "\n")
}

# html for main page
html_main_page = function(response, package = "", order_by = NULL, page = 1, records_per_page = 20, only_reducible = FALSE, 
	exclude_children = FALSE) {
	
	load_all_pkg_dep()
	df = load_pkg_stat_snapshot()

	if(is.null(package)) {
		package = ""
	}

	response$write(html_template("header", 
		vars = list(title = "Dependency table",
			        all_pkgs = df$package,
			        package = package)))

	if(is.null(order_by)) {
		order_by = "adjusted_heaviness_on_children"
	}

	if(!order_by %in% colnames(df)) {
		order_by = "adjusted_heaviness_on_children"
	}

	n_cran = sum(!grepl('bioconductor', df$repository))
	n_bioc = sum(grepl('bioconductor', df$repository))

	if(only_reducible) {
		df = df[df$reducible, , drop = FALSE]
	}

	if(package != "") {
		l = df$package == package
		if(any(l)) {
			df2 = df[l, , drop = FALSE]
		} else {
			l = grepl(package, df$package, ignore.case = TRUE)
			df2 = df[l, , drop = FALSE]
		}
		ind = NULL
	} else {

		ind = (page - 1)*records_per_page + seq_len(records_per_page)
		ind = intersect(ind, 1:nrow(df))
		if(length(ind) == 0) {
			ind = seq_len(records_per_page)
		}

		if(order_by == "package") {
			df2 = df[order(df$package)[ind], , drop = FALSE]
		} else {
			df2 = df[order(df[, order_by], decreasing = TRUE)[ind], , drop = FALSE]
		}
	}

	if(nrow(df2) > 0) {
		
		pkgs = df2[, 1]
		df2[, 1] = qq("<a href='package?package=@{pkgs}'>@{pkgs}</a>", collapse = FALSE)
		l = df2[, "adjusted_heaviness_on_children"] >= CUTOFF$adjusted_heaviness_on_children[1] & df2[, "adjusted_heaviness_on_children"] < CUTOFF$adjusted_heaviness_on_children[2]
		df2[l, 1] = paste0("<span class='heaviness-median'>", df2[l, 1], "</span>")
		l = df2[, "adjusted_heaviness_on_children"] >= CUTOFF$adjusted_heaviness_on_children[2]
		df2[l, 1] = paste0("<span class='heaviness-high'>", df2[l, 1], "</span>")

		df2[, "max_heaviness_from_parents"] = round(df2[, "max_heaviness_from_parents"], 1)
		df2[, "heaviness_on_children"] = round(df2[, "heaviness_on_children"], 1)
		df2[, "adjusted_heaviness_on_children"] = round(df2[, "adjusted_heaviness_on_children"], 1)
		df2[, "heaviness_on_downstream"] = round(df2[, "heaviness_on_downstream"], 1)
		df2[, "adjusted_heaviness_on_downstream"] = round(df2[, "adjusted_heaviness_on_downstream"], 1)
		df2[, "heaviness_on_indirect_downstream"] = round(df2[, "heaviness_on_indirect_downstream"], 1)
		df2[, "adjusted_heaviness_on_indirect_downstream"] = round(df2[, "adjusted_heaviness_on_indirect_downstream"], 1)

		df2$max_heaviness_from_parents = qq("<a href='package?package=@{pkgs}' title='@{df2$max_heaviness_parent_info}'>@{df2$max_heaviness_from_parents}</a>", collapse = FALSE)
		max_co_heaviness_from_parents_title = qq("Two parent packages '@{gsub(',', '\\' and \\'', df2$max_co_heaviness_parents_pair)}' contribute the highest co-heaviness on '@{pkgs}'.", collapse = FALSE)
		max_co_heaviness_from_parents_title[df2$max_co_heaviness_from_parents == 0] = "There is no pair of parents having co-heaviness."
		max_co_heaviness_from_parents_title = ifelse(df2$max_co_heaviness_parents_pair_type == "", max_co_heaviness_from_parents_title, qq("@{max_co_heaviness_from_parents_title} The relation of the two parents is '@{df2$max_co_heaviness_parents_pair_type}'.", collapse = FALSE))
		df2$max_co_heaviness_from_parents = qq("<a href='package?package=@{pkgs}' title=\"@{max_co_heaviness_from_parents_title}\">@{df2$max_co_heaviness_from_parents}</a>", collapse = FALSE)
		
		l = grepl("functions/objects are imported", df2$max_heaviness_parent_info)
		if(any(l)) {
			df2$max_heaviness_from_parents[l] = paste0(qq(" <span class='reducible'><a title='This heaviness can be reduced by moving parent packages to &lsquo;Suggests&rsquo; of &lsquo;@{pkgs[l]}&rsquo;.'>reducible</a></span> ", collapse = FALSE), df2$max_heaviness_from_parents[l])
		}

		if(exclude_children) {
			df2 = df2[, c("package", "repository", "n_by_strong", "n_by_all", "n_parents", "max_heaviness_from_parents", "max_co_heaviness_from_parents",
				"heaviness_on_children",  "n_children", 
				"heaviness_on_indirect_downstream", "n_indirect_downstream"), drop = FALSE]
		} else {
			df2 = df2[, c("package", "repository", "n_by_strong", "n_by_all", "n_parents", "max_heaviness_from_parents", "max_co_heaviness_from_parents",
				"heaviness_on_children",  "n_children", 
				"heaviness_on_downstream", "n_downstream"), drop = FALSE]
		}
		df2$repository = ifelse(grepl("bioconductor", df2$repository), "Bioconductor", "CRAN")

		response$write(html_template("dependency_table",
			vars = list(df = df,
				        df2 = df2,
				        ind = ind,
				        n_cran = n_cran,
				        n_bioc = n_bioc,
				        records_per_page = records_per_page,
				        page = page,
				        package = package,
				        order_by = order_by,
				        only_reducible = only_reducible,
				        exclude_children = exclude_children
				        )))
	} else {
		response$write(html_template("error",
			vars = list(error_message = "No result found.")))
	}

	response$write(html_template("footer"))
}


html_single_package = function(response, package) {

	load_pkg_db(snapshot = TRUE)
	lt = load_all_pkg_dep()

	df = load_pkg_stat_snapshot()

	if(is.null(package)) {
		response$write(html_template("header",
			vars = list(title = qq("Dependency heatmap"),
			all_pkgs = df$package,
			package = "")))

		response$write(html_template("error",
			vars = list(error_message = qq("No package has been selected."))))
		response$write(html_template("footer"))
		return(NULL)
	}

	pkg = lt[[package]]
	
	if(is.null(pkg)) {
		response$write(html_template("header",
			vars = list(title = qq("Dependency heatmap"),
			all_pkgs = df$package,
			package = "")))

		if(package %in% BASE_PKGS) {
			response$write(html_template("error",
				vars = list(error_message = qq("Base package <b>'@{package}'</b> is not included in this analysis."))))
		} else {
			response$write(html_template("error",
				vars = list(error_message = qq("Package <b>'@{package}'</b> is not included in this analysis."))))
		}
		response$write(html_template("footer"))
		return(NULL)
	} else {
		response$write(html_template("header",
			vars = list(title = qq("Dependency heatmap for package '@{pkg$package}'"),
			all_pkgs = df$package,
			package = "")))

	}

	response$write(html_template("package",
		vars = list(pkg = pkg,
			        df = df)))

	response$write(html_template("footer"))
}


html_upstream_dependency = function(response, package, page) {

	lt = load_all_pkg_dep()

	pkg = lt[[package]]

	df = load_pkg_stat_snapshot()

	upstream_pkgs = unique(upstream_dependency(pkg$package)[, 1])
	upstream_pkgs = setdiff(upstream_pkgs, BASE_PKGS)
	n_total = length(upstream_pkgs)	

	records_per_page = 100

	nt = NULL
	min_depth = 2
	max_depth = 0
	if(length(upstream_pkgs)) {
		upstream_tb = data.frame(package = upstream_pkgs, path = "", path_len = 0, heaviness = 0)
		for(i in seq_along(upstream_pkgs)) {
			s = df[upstream_pkgs[i], "hv_downstream_values"][[1]]
			if(!is.null(s)) {
				upstream_tb[i, "heaviness"] = s[pkg$package]
			}
		}
		row_order = order(-upstream_tb$heaviness)
		upstream_tb = upstream_tb[row_order, , drop = FALSE]

		n_used = sum(upstream_tb$heaviness > 5)
		upstream_tb = upstream_tb[upstream_tb$heaviness > 5, , drop = FALSE]

		## construct the network
		el = upstream_dependency(pkg$package)
	  	g = igraph::graph.edgelist(as.matrix(unique(el[, 1:2])))
	  	nt_parent = character(0)
	  	nt_child = character(0)
	  	nt_heaviness = numeric(0)
	    for(i in seq_len(nrow(upstream_tb))) {

	    	sp = igraph::all_shortest_paths(g, upstream_tb[i, 1], pkg$package)$res
	    	min_depth = min(min_depth, length(sp[[1]]))
	    	max_depth = max(max_depth, length(sp[[1]]))
	    	for(k in seq_along(sp)) {
	    		nodes = names(sp[[k]])
	    		nn = length(nodes)
	    		nt_parent = c(nt_parent, nodes[1:(nn-1)])
	    		nt_child = c(nt_child, nodes[2:nn])
	    		nt_heaviness = c(nt_heaviness, sapply(1:(nn-1), function(i) df[["hv_downstream_values"]][[ nodes[i] ]][ nodes[i+1] ]))
	    	}
	    }
	    nt = data.frame(parent = nt_parent, child = nt_child, heaviness = nt_heaviness)

	    if(nrow(upstream_tb) > 0) {
		    ind = (page - 1)*records_per_page + seq_len(records_per_page)
			ind = intersect(ind, 1:nrow(upstream_tb))
			if(length(ind) == 0) {
				ind = seq_len(records_per_page)
			}
			upstream_tb = upstream_tb[ind, , drop = FALSE]
		}
		
	} else {
		upstream_tb = NULL
		n_used = 0
	}

	
	response$write(html_template("upstream_dependency",
		vars = list(pkg = pkg,
			        upstream_tb = upstream_tb, 
			        n_total = n_total,
			        n_used = n_used,
			        nt = nt,
			        min_depth = min_depth,
			        max_depth = max_depth,
			        page = page,
			        df = df)))
}


html_downstream_dependency = function(response, package, page, records_per_page = 20, min_depth = 0, max_depth = Inf) {

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	df = load_pkg_stat_snapshot()

	downstream_hv = df[["hv_downstream_values"]][[pkg$package]]
	n_total = length(downstream_hv)

	downstream_path_list = load_pkg_downstream_dependency_path_snapshot()
	pl = downstream_path_list[[package]]

	if(length(pl) == 0) {
		global_min_depth = 0
		global_max_depth = 0
		min_depth = 0
		max_depth = 0
	} else {
		global_min_depth = min(sapply(pl, length)) - 1
		global_max_depth = max(sapply(pl, length)) - 1
		min_depth = max(global_min_depth, min_depth)
		max_depth = min(global_max_depth, max_depth)
	}

	depth_tb = NULL
	if(length(downstream_hv)) {
		downstream_tb = data.frame(package = names(downstream_hv), path = "", path_len = 0, heaviness = downstream_hv)
		row_order = order(-downstream_tb$heaviness)
		downstream_tb = downstream_tb[row_order, , drop = FALSE]

		all_heaviness = downstream_tb$heaviness 
		downstream_tb = downstream_tb[downstream_tb$heaviness > 10, , drop = FALSE]
		
		if(length(pl) == 0) {
			depth_tb = NULL
		} else {
			depth_tb = table(sapply(pl, length) - 1)
		}

		# filter by depth
		if( !( (min_depth == 0 && max_depth == 0) || (min_depth == global_min_depth && max_depth == global_max_depth) ) ) {
			l = sapply(pl, function(x) {
				len = length(x)
				len - 1 >= min_depth & len - 1 <= max_depth
			})
			pl = pl[l]
			downstream_tb = downstream_tb[ downstream_tb[, 1] %in% sapply(pl, function(x) x[length(x)]) , ,drop = FALSE]
		}

		n_used = nrow(downstream_tb)

		if(nrow(downstream_tb) > 0) {
			ind = (page - 1)*records_per_page + seq_len(records_per_page)
			ind = intersect(ind, 1:nrow(downstream_tb))
			if(length(ind) == 0) {
				ind = seq_len(records_per_page)
			}
			downstream_tb = downstream_tb[ind, , drop = FALSE]
		}

	} else {
		downstream_tb = NULL
		n_used = 0
		all_heaviness = NULL
		min_depth = 0
		max_depth = 0
	}

	nt = NULL

	path_list_to_nt = function(pl) {
		nt = data.frame(from = character(0), to = character(0))
		for(i in seq_along(pl)) {
			n = length(pl[[i]])
			nt2 = data.frame(from = pl[[i]][1:(n-1)], to = pl[[i]][2:n])
			nt = rbind(nt, nt2)
		}
		nt = unique(nt)
		nt$heaviness = 0

		for(i in seq_len(nrow(nt))) {
			nt$heaviness[i] = df[["hv_downstream_values"]][[ nt[i, 1] ]][ nt[i, 2] ]
		}
		nt
	}

	if(length(downstream_hv)) {
		if(length(pl)) {
			nt = path_list_to_nt(pl)
			g = graph.edgelist(as.matrix(nt[, 1:2]))
			nt$betweenness = edge_betweenness(g, directed = TRUE)
		}
	}
	
	response$write(html_template("downstream_dependency",
		vars = list(pkg = pkg,
			        downstream_tb = downstream_tb, 
			        all_heaviness = all_heaviness,
			        n_total = n_total,
			        n_used = n_used,
			        global_min_depth = global_min_depth,
			        global_max_depth = global_max_depth,
			        min_depth = min_depth,
			        max_depth = max_depth,
			        page = page,
			        records_per_page = records_per_page,
			        df = df,
			        nt = nt,
			        depth_tb = depth_tb)))
}



html_parent_dependency = function(response, package, page) {

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	tb = NULL
	n_total = nrow(pkg$df_imports)

	records_per_page = 50

	df = load_pkg_stat_snapshot()

	if(n_total > 0) {

		required_pkgs = rowSums(pkg$dep_mat)
		row_order = order(factor(pkg$dep_fields, levels = FIELDS), -pkg$heaviness)
		tb = as.data.frame(pkg$df_imports)
		tb = cbind(field = pkg$dep_fields, tb)
		tb$required_pkgs = required_pkgs
		tb$heaviness = pkg$heaviness

		tb = tb[row_order, , drop = FALSE]
		tb = cbind("Package"= rownames(tb), tb)
		tb = as.matrix(tb)

		ind = (page - 1)*records_per_page + seq_len(records_per_page)
		ind = intersect(ind, 1:nrow(tb))
		if(length(ind) == 0) {
			ind = seq_len(records_per_page)
		}
		tb = tb[ind, , drop = FALSE]
	}

	response$write(html_template("parent_dependency",
		vars = list(pkg = pkg,
			        tb = tb, 
			        n_total = n_total,
			        page = page,
			        records_per_page = records_per_page)))
}

html_child_dependency = function(response, package, page, records_per_page = 20, child_dep_prioritize_reducible = FALSE, child_dep_internal_ordering = FALSE) {

	pkg_db_snapshot = load_pkg_db(snapshot = TRUE)

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	df = load_pkg_stat_snapshot()

	rev_pkg_tb = pkg_db_snapshot$package_dependencies(package, reverse = TRUE)
	rev_pkg_tb = as.data.frame(rev_pkg_tb)
	rev_pkg_tb$dep_fields = factor(rev_pkg_tb$dep_fields, levels = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))
	rev_pkg_tb = rev_pkg_tb[order(rev_pkg_tb$dep_fields, rev_pkg_tb$package), , drop = FALSE]
	rev_pkg_tb = rev_pkg_tb[!duplicated(rev_pkg_tb$package), , drop = FALSE]

	n_total = nrow(rev_pkg_tb)
	n_used = 0

	if(n_total > 0) {

		rev_pkg = rev_pkg_tb[, 1]
		rev_feild = rev_pkg_tb[, 3]
		l = duplicated(rev_pkg)
		rev_pkg = rev_pkg[!l]
		rev_feild = rev_feild[!l]

		rev_tb = data.frame(package = rev_pkg, field = rev_feild)
		rev_tb$import = sapply(lt[rev_pkg], function(pkg) {
			pkg$df_imports[package, "imports"]
		})[rev_pkg]
		rev_tb$importMethods = sapply(lt[rev_pkg], function(pkg) {
			pkg$df_imports[package, "importMethods"]
		})[rev_pkg]
		rev_tb$importClasses = sapply(lt[rev_pkg], function(pkg) {
			pkg$df_imports[package, "importClasses"]
		})[rev_pkg]
		rev_tb$heaviness = sapply(lt[rev_pkg], function(pkg) {
			unname(pkg$heaviness[which(rownames(pkg$dep_mat) == package)])
		})[rev_pkg]

		rev_tb$required_pkgs = sapply(rev_tb$package, function(x) {
			sum(lt[[x]]$which_required)
		})

		if(child_dep_prioritize_reducible && child_dep_internal_ordering) {
			row_order = order(ifelse(rev_tb$import > 0 & rev_tb$importMethods == 0 & rev_tb$importClasses == 0, 1, 2), -rev_tb$heaviness*rev_tb$required_pkgs)
		} else if(!child_dep_prioritize_reducible && child_dep_internal_ordering) {
			row_order = order(-rev_tb$heaviness*rev_tb$required_pkgs, factor(rev_tb$field, levels = FIELDS), -rev_tb$heaviness, -rev_tb$required_pkgs)
		} else if(child_dep_prioritize_reducible && !child_dep_internal_ordering) {
			row_order = order(ifelse(rev_tb$import > 0 & rev_tb$importMethods == 0 & rev_tb$importClasses == 0, 1, 2), factor(rev_tb$field, levels = FIELDS), -rev_tb$heaviness, -rev_tb$required_pkgs)
		} else {
			row_order = order(factor(rev_tb$field, levels = FIELDS), -rev_tb$heaviness, -rev_tb$required_pkgs)
		}

		rev_tb$field = paste0("Reverse ", rev_tb$field)
		rev_tb = rev_tb[row_order, , drop = FALSE]

		n_used = sum(rev_tb$heaviness > 10)
		all_heaviness = rev_tb$heaviness
		rev_tb = rev_tb[rev_tb$heaviness > 10, , drop = FALSE]

		ind = (page - 1)*records_per_page + seq_len(records_per_page)
		ind = intersect(ind, 1:nrow(rev_tb))
		if(length(ind) == 0) {
			ind = seq_len(records_per_page)
		}
		rev_tb = rev_tb[ind, , drop = FALSE]
	
	} else {
		rev_tb = NULL
		all_heaviness = NULL
	}

	response$write(html_template("child_dependency",
		vars = list(pkg = pkg,
			        rev_tb = rev_tb, 
			        all_heaviness = all_heaviness,
			        pkg_db_snapshot = pkg_db_snapshot,
			        n_total = n_total,
			        n_used = n_used,
			        page = page,
			    	records_per_page = records_per_page,
			    	child_dep_prioritize_reducible = child_dep_prioritize_reducible,
			    	child_dep_internal_ordering = child_dep_internal_ordering,
			        df = df)))
}


img = function (file, alt = "image", style = "") {
    input <- normalizePath(file, mustWork = TRUE)
    buf <- readBin(input, raw(), file.info(input)$size)
    base64 <- openssl::base64_encode(buf, linebreaks = FALSE)
    qq('<img src="data:image/png;base64,\n@{base64}" style="@{style}" />')
}


html_global_heaviness_analysis = function(response) {

	df = load_pkg_stat_snapshot()

	response$write(html_template("global_heaviness_analysis",
		vars = list(df = df)))
}


network_in_json = function(edge) {
	node = unique(c(edge[, 1], edge[, 2]))
	node_lt = lapply(node, function(x) {
		list(data = list(id = x, group = ifelse(grepl("\\d+ leaves", x), "leafgroup", "node")))
	})
	edge_lt = lapply(seq_len(nrow(edge)), function(i) {
		data_lt = list(id = paste(edge[i, 1], edge[i, 2], sep = "|"),
			             source = edge[i, 1],
			             target = edge[i, 2],
			             weight = edge[i, 3])
		if(ncol(edge) > 3) {
			for(nm in colnames(edge)[-(1:3)]) {
				data_lt[[nm]] = edge[i, nm]
			}
		}
		list(data = data_lt)
	})
	nt_json = list(nodes = node_lt, edges = edge_lt)
	nt_json = paste0("var nt = ", rjson::toJSON(nt_json), ";\n")
	return(nt_json)
}


make_heaviness_plot = function() {

	df = load_pkg_stat_snapshot()

	cat("  - generate plots for heaviness on child packages.\n")
	png(paste0(env$figure_dir, "/plot-child-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	heaviness = ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[2], "high", ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_children, heaviness_on_children, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggplot2::scale_x_continuous(trans='log10') +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of child packages", y = "Heaviness") +
			ggplot2::ggtitle("Heaviness on child packages") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	png(paste0(env$figure_dir, "/plot-child-adjusted-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	heaviness = ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[2], "high", ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_children, adjusted_heaviness_on_children, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_children >= CUTOFF$adjusted_heaviness_on_children[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggplot2::scale_x_continuous(trans='log10') +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of child packages", y = "Adjusted heaviness") +
			ggplot2::ggtitle("Adjusted heaviness on child packages for all CRAN/Bioconductor packages") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	cat("  - generate plots for heaviness on downstream packages.\n")
	png(paste0(env$figure_dir, "/plot-downstream-no-children-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	heaviness = ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[2], "high", ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_indirect_downstream, heaviness_on_indirect_downstream, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggplot2::scale_x_continuous(trans='log10') +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of indirect downstream packages", y = "Heaviness") +
			ggplot2::ggtitle("Heaviness on indirect downstream packages for all CRAN/Bioconductor packages") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	png(paste0(env$figure_dir, "/plot-downstream-no-children-adjusted-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	heaviness = ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[2], "high", ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_indirect_downstream, adjusted_heaviness_on_indirect_downstream, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_indirect_downstream >= CUTOFF$adjusted_heaviness_on_indirect_downstream[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggplot2::scale_x_continuous(trans='log10') +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of indirect downstream packages", y = "Adjusted heaviness") +
			ggplot2::ggtitle("Adjusted heaviness on indirect downstream packages for all CRAN/Bioconductor packages") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	cat("  - generate plots for heaviness from parent packages.\n")
	png(paste0(env$figure_dir, "/plot-parent-max-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	v = df$adjusted_max_heaviness_from_parents
	heaviness = ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[2], "high", ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_parents, max_heaviness_from_parents, color = heaviness, 
				label = ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of parent packages", y = "Max heaviness from parents") +
			ggplot2::ggtitle("Max heaviness from parents") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	png(paste0(env$figure_dir, "/plot-parent-adjusted-max-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	v = df$adjusted_max_heaviness_from_parents
	heaviness = ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[2], "high", ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_parents, v, color = heaviness, 
				label = ifelse(v >= CUTOFF$adjusted_max_heaviness_from_parents[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of parent packages", y = "Adjusted max heaviness from parents") +
			ggplot2::ggtitle("Adjusted max heaviness from parents") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off() 

	png(paste0(env$figure_dir, "/plot-parent-total-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	v = df$adjusted_total_heaviness_from_parents
	heaviness = ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[2], "high", ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_parents, total_heaviness_from_parents, color = heaviness, 
				label = ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of parent packages", y = "Total heaviness from parents") +
			ggplot2::ggtitle("Total heaviness from parents") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off()

	png(paste0(env$figure_dir, "/plot-parent-adjusted-total-heaviness.png"), width = 1000*1.5, height = 500*1.5, res = 72*2)
	v = df$adjusted_total_heaviness_from_parents
	heaviness = ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[2], "high", ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[1], "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_parents, v, color = heaviness, 
				label = ifelse(v >= CUTOFF$adjusted_total_heaviness_from_parents[2], df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of parent packages", y = "Adjusted total heaviness from parents") +
			ggplot2::ggtitle("Adjusted total heaviness from parents") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off() 

	cat("  - generate plots for comparing downstream and indirect downstream (exluding children) packages.\n")
	png(paste0(env$figure_dir, "/plot-compare-downstream-and-downstream2.png"), width = 800*1.5, height = 500*1.5, res = 72*2)
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2)))

	pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
	pushViewport(viewport(width = 0.9))
	correspond_between_two_rankings(x1 = df$heaviness_on_children, x2 = df$heaviness_on_downstream, 
		name1 = "children", name2 = "downstream", top_n = 500, newpage = FALSE)
	upViewport()
	upViewport()

	pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
	pushViewport(viewport(width = 0.9))
	correspond_between_two_rankings(x1 = df$heaviness_on_children, x2 = df$heaviness_on_indirect_downstream, 
		name1 = "children", name2 = "indirect downstream\n(excluding children)", top_n = 500, newpage = FALSE)
	upViewport()
	upViewport()

	upViewport()
	dev.off()

	png(paste0(env$figure_dir, "/plot-top-500-children-downstream-pct.png"), width = 800*1.5, height = 500*1.5, res = 72*2)
	ind = intersect(order(-df$heaviness_on_children)[1:500], order(-df$heaviness_on_downstream)[1:500])
	r = sort(df$n_children[ind]/df$n_downstream[ind])
	p = ggplot(data.frame(x = seq_along(ind), y = r), aes(x = x, y = y)) +
	    geom_point() + geom_line() +
	    labs(x = "Packages ordered by the fraction", y = "fraction = n_child/n_downstream") +
	    ggtitle("Fraction of child in downstream of top packages with the highest heaviness")
	print(p)
	dev.off()

	N = nrow(df)
	lta = load_from_pkgndep_db("adjusted_heaviness_select_a.rds")
	png(paste0(env$figure_dir, "/plot-select-a-adjusted-heaviness-children.png"), width = 600*1.5, height = 500*1.5, res = 72*2)
	d1 = lta$children; d1$v = 1 - d1$v/N
	p1 = ggplot(d1, aes(x = a, y = v)) + geom_point() + geom_line() + geom_vline(xintercept= 10, col = "red", lty =2) +
		labs(x = "Value of a", y = "Stability of rankings of all packages compared to previous a") +
		ggtitle("Adjust heaviness on child packages")
	print(p1)
	dev.off()

	png(paste0(env$figure_dir, "/plot-select-a-adjusted-heaviness-downstream.png"), width = 600*1.5, height = 500*1.5, res = 72*2)
	d2 = lta$downstream; d2$v = 1 - d2$v/N
	p2 = ggplot(d2, aes(x = a, y = v)) + geom_point() + geom_line() + geom_vline(xintercept= 6, col = "red", lty =2) +
		labs(x = "Value of a", y = "Stability of rankings of all packages compared to previous a") +
		ggtitle("Adjust heaviness on downstream packages")
	print(p2)
	dev.off()

	png(paste0(env$figure_dir, "/plot-select-a-adjusted-heaviness-downstream-no-children.png"), width = 600*1.5, height = 500*1.5, res = 72*2)
	d3 = lta$indirect_downstream; d3$v = 1 - d3$v/N
	p3 = ggplot(d3, aes(x = a, y = v)) + geom_point() + geom_line() + geom_vline(xintercept= 6, col = "red", lty =2) +
		labs(x = "Value of a", y = "Stability of rankings of all packages compared to previous a") +
		ggtitle("Adjust heaviness on indirect downstream packages")
	print(p3)
	dev.off()

	invisible(NULL)
}


correspond_between_two_rankings = function(x1, x2, name1, name2, 
	col1 = 2, col2 = 3, top_n = round(0.25*length(x1)), transparency = 0.9, 
	pt_size = unit(1, "mm"), newpage = TRUE, ratio = c(1, 0.8, 1)) {
	
	if(newpage) {
		grid.newpage()
	}

	if(length(x1) != length(x2)) {
		stop("Length of `x1` and `x2` should be the same.")
	}

	r1 = rank(x1, ties.method = "random")
	r2 = rank(x2, ties.method = "random")

	if(missing(name1)) name1 = deparse(substitute(x1))
	if(missing(name2)) name2 = deparse(substitute(x2))

	n = length(x1)
	text_height = grobHeight(textGrob("foo\nfoo"))*2
	pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 3, widths = unit(ratio, "null")), 
		width = unit(1, "npc") - unit(2, "mm"),
		height = unit(1, "npc") - text_height - unit(1, "cm"), y = unit(1, "cm"), just = "bottom"))
	
	max_x1 = max(x1)
	pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1, 
		xscale = c(0, max_x1), yscale = c(0, n + 1)))
	grid.segments(max_x1 - x1, r1, max_x1, r1, default.units = "native", gp = gpar(col = "#EFEFEF"))
	l = r2 >= n - top_n
	grid.points(max_x1 - x1[l], r1[l], default.units = "native", pch = 16, size = pt_size, gp = gpar(col = add_transparency(col2, 0.5)))
	grid.text(name1, x = 1, y = unit(n + 1, "native") + unit(1, "mm"), default.units = "npc", just = c("right", "bottom"))
	upViewport()

	max_x2 = max(x2)
	pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3, 
		xscale = c(0, max_x2), yscale = c(0, n + 1)))
	grid.segments(0, r2, x2, r2, default.units = "native", gp = gpar(col = "#EFEFEF"))
	l = r1 >= n - top_n
	grid.points(x2[l], r2[l], default.units = "native", pch = 16, size = pt_size, gp = gpar(col = add_transparency(col1, 0.5)))
	grid.text(name2, x = 0, y = unit(n + 1, "native") + unit(1, "mm"), default.units = "native", just = c("left", "bottom"))
	upViewport()

	pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2, xscale = c(0, 1), yscale = c(0, n + 1)))
	l = r1 >= n - top_n | r2 >= n - top_n
	# if(sum(!l)) grid.segments(0, r1[!l], 1, r2[!l], default.units = "native", gp = gpar(col = "#EEEEEE80"))
	if(sum(l)) {
		grid.segments(0, r1[l], 1, r2[l], default.units = "native", gp = gpar(col = add_transparency("#000000", transparency)))
		# for(ind in which(l)) {
		# 	grid.bezier(c(0, 1, 0, 1), c(r1[ind], r1[ind], r2[ind], r2[ind]), default.units = "native", gp = gpar(col = add_transparency("#000000", transparency)))
		# }
	}
	grid.segments(c(0, 1), c(1, 1), c(0, 1), c(n - top_n, n - top_n), default.units = "native", gp = gpar(col = "#EEEEEE"))
	grid.segments(c(0, 1), c(n - top_n, n - top_n), c(0, 1), c(n, n), default.units = "native", gp = gpar(lwd = 4, col = c(col1, col2)))
	upViewport()

	upViewport()

	# add a venn diagram at the bottom
	n_intersect = length(intersect(order(x1, decreasing = TRUE)[1:top_n], order(x2, decreasing = TRUE)[1:top_n]))
	n_union = 2*top_n - n_intersect
	grid.roundrect(x = unit(0.5 - n_intersect/2/top_n*0.4, "npc"), y = unit(0.4, "cm"), width = unit(0.4, "npc"), 
		height = unit(0.4, "cm"), gp = gpar(fill = add_transparency(col2, 0.5), col = NA), just = "left")
	grid.roundrect(x = unit(0.5 + n_intersect/2/top_n*0.4, "npc"), y = unit(0.4, "cm"), width = unit(0.4, "npc"), 
		height = unit(0.4, "cm"), gp = gpar(fill = add_transparency(col1, 0.5), col = NA), just = "right")
	grid.text(qq("top @{top_n}/@{length(x1)}"), x = unit(0.5, "npc"), y = unit(0.7, "cm"), just = "bottom", gp = gpar(fontsize = 8))

}


add_transparency = function (col, transparency = 0) {
    rgb(t(col2rgb(col)/255), alpha = 1 - transparency)
}


