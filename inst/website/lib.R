

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

page_select2 = function(current_page, n_page, which_table, package) {
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
		pages_html = ifelse(l, qq("<li class='active'><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{pages});false;'>@{pages}</a></li>\n", collapse = FALSE),
			qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{pages});false;'>@{pages}</a></li>\n", collapse = FALSE))

		pages_html = c(qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", 1);false;'>First</a></li>\n"),
			pages_html,
			qq("<li><a style='cursor: pointer;' onclick='update_ajax_table(\"@{which_table}\", \"@{package}\", @{n_page});false;'>Last</a></li>\n"))
	}
	pages_html = c("<nav><ul class='pagination'>", pages_html, "</ul></nav>\n")
	paste(pages_html, collapse = " ")
}

html_template = function(template, vars = list()) {

	template_dir = "~/project/development/pkgndep/inst/website/template"

	f = tempfile()

	on.exit(file.remove(f))
	for(nm in names(vars)) {
		assign(nm, vars[[nm]])
	}
	brew::brew(paste0(template_dir, "/", template, ".html"), f)
	paste(readLines(f), collapse = "\n")
}

# html for main page
html_main_page = function(response, package = "", order_by = NULL, page = 1, records_per_page = 20, only_improvable = FALSE) {
	
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

	if(only_improvable) {
		df = df[df$improvable, , drop = FALSE]
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
		l = df2[, "adjusted_heaviness_on_children"] >= 10 & df2[, "adjusted_heaviness_on_children"] < 20
		df2[l, 1] = paste0("<span class='heaviness-median'>", df2[l, 1], "</span>")
		l = df2[, "adjusted_heaviness_on_children"] >= 20
		df2[l, 1] = paste0("<span class='heaviness-high'>", df2[l, 1], "</span>")

		df2[, "max_heaviness_from_parent"] = round(df2[, "max_heaviness_from_parent"], 1)
		df2[, "heaviness_on_children"] = round(df2[, "heaviness_on_children"], 1)
		df2[, "adjusted_heaviness_on_children"] = round(df2[, "adjusted_heaviness_on_children"], 1)
		df2[, "heaviness_on_downstream"] = round(df2[, "heaviness_on_downstream"], 1)
		df2[, "adjusted_heaviness_on_downstream"] = round(df2[, "adjusted_heaviness_on_downstream"], 1)

		df2$max_heaviness_from_parent = qq("<a href='package?package=@{pkgs}' title='@{df2$max_heaviness_parent_info}'>@{df2$max_heaviness_from_parent}</a>", collapse = FALSE)
		l = grepl("functions/objects are imported", df2$max_heaviness_parent_info)
		if(any(l)) {
			df2$max_heaviness_from_parent[l] = paste0(qq(" <span class='improvable'><a title='This heaviness can be reduced by moving &lsquo;@{pkgs[l]}&rsquo; to &lsquo;Suggests&rsquo;.'>improvable</a></span> ", collapse = FALSE), df2$max_heaviness_from_parent[l])
		}

		df2 = df2[, c("package", "repository", "n_by_strong", "n_by_all", "n_parents", "max_heaviness_from_parent", 
			"heaviness_on_children",  "n_children", 
			"heaviness_on_downstream", "n_downstream"), drop = FALSE]
		df2$repository = ifelse(grepl("bioconductor", df2$repository), "Bioconductor", "CRAN")

		
		response$write(html_template("dependency_table",
			vars = list(df = df,
				        df2 = df2,
				        ind = ind,
				        records_per_page = records_per_page,
				        page = page,
				        package = package,
				        order_by = order_by,
				        only_improvable = only_improvable
				        )))
	} else {
		response$write(html_template("error",
			vars = list(error_message = "No results found.")))
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

		response$write(html_template("error",
			vars = list(error_message = qq("Cannot make dependency heatmap for package '@{package}'"))))
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

	records_per_page = 25

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
	} else {
		upstream_tb = NULL
		n_used = 0
	}

	ind = (page - 1)*records_per_page + seq_len(records_per_page)
	ind = intersect(ind, 1:nrow(upstream_tb))
	if(length(ind) == 0) {
		ind = seq_len(records_per_page)
	}
	upstream_tb = upstream_tb[ind, , drop = FALSE]
	
	response$write(html_template("upstream_dependency",
		vars = list(pkg = pkg,
			        upstream_tb = upstream_tb, 
			        n_total = n_total,
			        n_used = n_used,
			        page = page,
			        df = df)))
}


html_downstream_dependency = function(response, package, page) {

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	df = load_pkg_stat_snapshot()

	downstream_hv = df[["hv_downstream_values"]][[pkg$package]]
	n_total = length(downstream_hv)

	records_per_page = 25

	if(length(downstream_hv)) {
		downstream_tb = data.frame(package = names(downstream_hv), path = "", path_len = 0, heaviness = downstream_hv)
		row_order = order(-downstream_tb$heaviness)
		downstream_tb = downstream_tb[row_order, , drop = FALSE]

		n_used = sum(downstream_tb$heaviness > 10)
		downstream_tb = downstream_tb[downstream_tb$heaviness > 10, , drop = FALSE]
	} else {
		downstream_tb = NULL
		n_used = 0
	}

	ind = (page - 1)*records_per_page + seq_len(records_per_page)
	ind = intersect(ind, 1:nrow(downstream_tb))
	if(length(ind) == 0) {
		ind = seq_len(records_per_page)
	}
	downstream_tb = downstream_tb[ind, , drop = FALSE]
	
	response$write(html_template("downstream_dependency",
		vars = list(pkg = pkg,
			        downstream_tb = downstream_tb, 
			        n_total = n_total,
			        n_used = n_used,
			        page = page,
			        df = df)))
}



html_parent_dependency = function(response, package, page) {

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	tb = NULL
	n_total = nrow(pkg$df_imports)

	records_per_page = 25

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
			        page = page)))
}

html_children_dependency = function(response, package, page) {

	pkg_db_snapshot = load_pkg_db(snapshot = TRUE)

	lt = load_all_pkg_dep()
	pkg = lt[[package]]

	df = load_pkg_stat_snapshot()

	rev_pkg_tb = pkg_db_snapshot$package_dependencies(package, reverse = TRUE)
	n_total = nrow(rev_pkg_tb)
	n_used = 0

	records_per_page = 25

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

		row_order = order(factor(rev_tb$field, levels = FIELDS), -rev_tb$heaviness)
		rev_tb$field = paste0("Reverse ", rev_tb$field)
		rev_tb = rev_tb[row_order, , drop = FALSE]

		n_used = sum(rev_tb$heaviness > 10)

		ind = (page - 1)*records_per_page + seq_len(records_per_page)
		ind = intersect(ind, 1:nrow(rev_tb))
		if(length(ind) == 0) {
			ind = seq_len(records_per_page)
		}
		rev_tb = rev_tb[ind, , drop = FALSE]

		rev_tb = rev_tb[rev_tb$heaviness > 10, , drop = FALSE]
	} else {
		rev_tb = NULL
	}

	response$write(html_template("children_dependency",
		vars = list(pkg = pkg,
			        rev_tb = rev_tb, 
			        n_total = n_total,
			        n_used = n_used,
			        page = page,
			        df = df)))
}


img = function (file, alt = "image", style = "") {
    input <- normalizePath(file, mustWork = TRUE)
    buf <- readBin(input, raw(), file.info(input)$size)
    base64 <- openssl::base64_encode(buf, linebreaks = FALSE)
    qq('<img src="data:image/png;base64,\n@{base64}" style="@{style}" />')
}


html_global_heaviness_plot = function(response) {

	df = load_pkg_stat_snapshot()

	tmp_file = paste0(env$figure_dir, "/heaviness_scatterplot.png")

	png(tmp_file, width = 1200*1.2, height = 600*1.2, res = 72*1.8)
	heaviness = ifelse(df$adjusted_heaviness_on_children >= 30, "high", ifelse(df$adjusted_heaviness_on_children >= 15, "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_children, heaviness_on_children, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_children >= 30, df$package, ""))) +
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

	html = img(tmp_file, style="width:1200px")

	html = paste0("
<p>The heaviness categories are based on the following criterions:</p>
<ul>
<li>Red: adjusted heaviness on child packages larger than 20.</li>
<li>Orange: adjusted heaviness on child packages between 10 and 20.</li>
<li>grey: adjusted heaviness on child packages less than 10.</li>
</ul>
", html)

	tmp_file = paste0(env$figure_dir, "/heaviness_deepness.png")

	png(tmp_file, width = 1200*1.2, height = 600*1.2, res = 72*1.8)
	heaviness = ifelse(df$adjusted_heaviness_on_downstream >= 30, "high", ifelse(df$adjusted_heaviness_on_downstream >= 15, "median", "low"))
	repo = ifelse(grepl("bioconductor", df$repository), "Bioconductor", "CRAN")
	df$repo = factor(repo, levels = c("CRAN", "Bioconductor"))
	suppressWarnings({
		p = ggplot2::ggplot(df, ggplot2::aes(n_downstream, heaviness_on_downstream, color = heaviness, 
				label = ifelse(df$adjusted_heaviness_on_downstream >= 30, df$package, ""))) +
			ggplot2::geom_point() + 
			ggplot2::scale_color_manual(values = c("high" = "red", "median" = "orange", "low" = "grey")) +
			ggplot2::scale_x_continuous(trans='log10') +
			ggrepel::geom_text_repel(min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf, show.legend = FALSE, size =3) +
			ggplot2::labs(x = "Number of downstream packages", y = "Heaviness") +
			ggplot2::ggtitle("Heaviness on downstream packages") +
			ggplot2::facet_wrap(ggplot2::vars(repo))
		ggplot2:::print.ggplot(p)
	})
	dev.off() 

	html = paste0(html, "
<p>In the following plot, if a package has higher heaviness on downstream than on the child packages, it means it also affects more indirect downstream packages.</p>
\n", img(tmp_file, style="width:1200px"))

	response$write(html)
}
