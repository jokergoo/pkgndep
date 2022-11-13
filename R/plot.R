
# == title
# Make the dependency heatmap
#
# == param
# -x An object from `pkgndep`.
# -... Other arguments.
#
# == details
# Please use `dependency_heatmap` instead.
#
plot.pkgndep = function(x, ...) {
	dependency_heatmap(x, ...)
}

# == title
# Make the dependency heatmap
#
# == param
# -x An object from `pkgndep`.
# -pkg_fontsize Font size for the package names.
# -title_fontsize Font size for the title.
# -legend_fontsize Font size for the legends.
# -fix_size Should the rows and columns in the heatmap have fixed size?
# -cex A factor multiplicated to all font sizes.
# -help Whether to print help message?
# -file A path of the figure. The size of the figure is automatically calculated.
# -res Resolution of the figure (only for png and jpeg).
#
# == details
# If ``fix_size`` is set to ``TRUE``. The size of the whole plot can be obtained by:
#
#     size = dependency_heatmap(x, fix_size = TRUE)
#
# where ``size`` is a numeric vector of length two which are the width and height of the whole heatmap.
# If ``file`` argument is set, the size of the figure is automatically calculated.
#
# If there are no dependency packages stored in ``x``, ``NULL`` is returned.
# 
# == value
# A vector of two numeric values (in inches) that correspond to the width and height of the plot.
#
# == example
# # See examples in `pkgndep()`.
#
dependency_heatmap = function(x, pkg_fontsize = 10*cex, title_fontsize = 12*cex, 
	legend_fontsize = 10*cex, fix_size = !dev.interactive(), cex = 1, 
	help = TRUE, file = NULL, res = 144) {

	if(!is.null(file)) {
		if(grepl("\\.png$", file, ignore.case = TRUE)) {
			tmp_file = tempfile(fileext = ".png")
			png(tmp_file, width = 1000)
			size = plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
				legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
			dev.off()
			file.remove(tmp_file)

			if(!is.null(size)) {
				size[1] = max(size[1], 7)
				size[2] = max(size[2], 3)
				png(file, width = size[1], height = size[2], units = "in", res = res)
				plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
					legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
				dev.off()
			} else {
				png(file, width = 5, height = 1, units = "in", res = res)
				if(nrow(x$dep_mat) > 0 && ncol(x$dep_mat) == 0) {
					grid.text(qq("All required packages for '@{x$package}' are base packages."), 0.5, 0.5)
				} else {
					grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
				}
				dev.off()
			}

			return(invisible(NULL))
		} else if(grepl("\\.(jpg|jpeg)$", file, ignore.case = TRUE)) {
			tmp_file = tempfile()
			jpeg(tmp_file)
			size = plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
				legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
			dev.off()
			file.remove(tmp_file)

			if(!is.null(size)) {
				size[1] = max(size[1], 7)
				size[2] = max(size[2], 3)
				jpeg(file, width = size[1], height = size[2], units = "in", res = res)
				plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
					legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
				dev.off()
			} else {
				jpeg(file, width = 5, height = 1, units = "in", res = res)
				if(nrow(x$dep_mat) > 0 && ncol(x$dep_mat) == 0) {
					grid.text(qq("All required packages for '@{x$package}' are base packages."), 0.5, 0.5)
				} else {
					grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
				}
				dev.off()
			}

			return(invisible(NULL))
		} else if(grepl("\\.pdf$", file, ignore.case = TRUE)) {
			pdf(NULL)
			size = plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
				legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
			dev.off()

			if(!is.null(size)) {
				size[1] = max(size[1], 7)
				size[2] = max(size[2], 3)
				pdf(file, width = size[1], height = size[2])
				plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
					legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
				dev.off()
			} else {
				pdf(file, width = 5, height = 1)
				if(nrow(x$dep_mat) > 0 && ncol(x$dep_mat) == 0) {
					grid.text(qq("All required packages for '@{x$package}' are base packages."), 0.5, 0.5)
				} else {
					grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
				}
				dev.off()
			}

			return(invisible(NULL))
		} else if(grepl("\\.svg$", file, ignore.case = TRUE)) {
			tmp_file = tempfile()
			svg(tmp_file)
			size = plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
				legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
			dev.off()
			file.remove(tmp_file)

			if(requireNamespace("svglite", quietly = TRUE)) {
				svg_fun = svglite::svglite
			} else {
				svg_fun = svg
			}

			if(!is.null(size)) {
				size[1] = max(size[1], 7)
				size[2] = max(size[2], 3)
				svg_fun(file, width = size[1], height = size[2])
				plot(x, fix_size = TRUE, pkg_fontsize = pkg_fontsize, title_fontsize = title_fontsize,
					legend_fontsize = legend_fontsize, cex = cex, help = FALSE, file = NULL)
				dev.off()
			} else {
				svg_fun(file, width = 5, height = 1)
				if(nrow(x$dep_mat) > 0 && ncol(x$dep_mat) == 0) {
					grid.text(qq("All required packages for '@{x$package}' are base packages."), 0.5, 0.5)
				} else {
					grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
				}
				dev.off()
			}

			return(invisible(NULL))
		} else {
			stop("`file` only allows extensions of 'png/jpg/svg/pdf'.")
		}
	}

	#get.gpar() # test whether the graphics window can be opened or not
	if(missing(fix_size) && cex != 1) {
		fix_size = TRUE
	}

	m = x$dep_mat
	row_split = x$dep_fields

	if(ncol(m) == 0) {
		if(help) message(qq("No dependency found for package '@{x$package}'. Won't generate the plot.\n"))
		return(invisible(NULL))
	}

	line_height = grobHeight(textGrob("A", gp = gpar(fontsize = pkg_fontsize)))*1.5

	fix_size = fix_size
	
	m2 = m
	m2[, colnames(m) %in% BASE_PKGS] = m2[, colnames(m) %in% BASE_PKGS] * 3
	m2[, colnames(m) %in% RECOMMENDED_PKGS] = m2[, colnames(m) %in% RECOMMENDED_PKGS] * 2

    dep_fields_col = structure(c("#d9534f", "#f0ad4e", "#337ab7", "#5cb85c", "#5bc0de"), names =  c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"))

	ht = Heatmap(m2, 
		name = x$package,
		row_split = factor(row_split, levels = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")),
		left_annotation = rowAnnotation(dep_fields = x$dep_fields, simple_anno_size = unit(2, "mm"),
			col = list(dep_fields = dep_fields_col),
			show_annotation_name = FALSE, annotation_legend_param = list(dep_fields = list(title = "", at = intersect(names(dep_fields_col), as.character(row_split))))),
		column_split = factor(ifelse(colnames(m) %in% BASE_PKGS, "Base packages", "Other packages"), levels = c("Base packages", "Other packages")),
		rect_gp = gpar(col = "#EEEEEE"),
		show_row_dend = FALSE, 
		show_column_dend = FALSE,
		col = c("1" = "#1f78b4", "2" = "darkgreen", "3" = "#e31a1c", "0" = "#CCCCCC"),
		column_names_gp = gpar(fontsize = pkg_fontsize),
		column_names_rot = 60,
		row_names_gp = gpar(fontsize = pkg_fontsize),
		column_title_gp = gpar(fontsize = title_fontsize),
		row_title_gp = gpar(fontsize = title_fontsize),
		row_title_rot = 90,
		cluster_rows = FALSE,
		cluster_columns = FALSE,
		bottom_annotation = HeatmapAnnotation(
			required1 = anno_simple(
				ifelse(colnames(m) %in% required_dependency_packages(x) & colSums(m[x$which_required, , drop = FALSE]) > 0, "yes", "no"),
				col = c("yes" = "purple", "no" = "white"),
				height = unit(1, "mm")
			), 
			loaded1 = if(length(x$pkg_from_session_info) == 0) NULL else anno_simple(
				ifelse(colnames(m) %in% x$pkg_from_session_info, "yes", "no"),
				col = c("yes" = "white", "no" = "white"),
				pch = ifelse(colnames(m) %in% x$pkg_from_session_info, 16, NA), 
				pt_size = unit(2, "mm"), pt_gp = gpar(col = "purple"),
				height = unit(2, "mm")
			),
			show_annotation_name = FALSE),
		width = if(fix_size) ncol(m)*line_height else NULL,
		height = if(fix_size) nrow(m)*line_height else NULL,
		right_annotation = if(any(is_field_required(x$dep_fields))) {
			rowAnnotation(
				required2 = anno_simple(
					ifelse(rownames(m) %in% required_dependency_packages(x) & is_field_required(x$dep_fields), "yes", "no"),
					col = c("yes" = "purple", "no" = "white"),
					width = unit(1, "mm")
				), 
				loaded2 = if(length(x$pkg_from_session_info) == 0) NULL else anno_simple(
					ifelse(rownames(m) %in% x$pkg_from_session_info, "yes", "no"),
					col = c("yes" = "white", "no" = "white"),
					pch = ifelse(rownames(m) %in% x$pkg_from_session_info, 16, NA), 
					pt_size = unit(2, "mm"), pt_gp = gpar(col = "purple"),
					width = unit(2, "mm")
				),
				show_annotation_name = FALSE)
		} else {
			NULL
		},
		show_heatmap_legend = FALSE
	)

	graphics = list(
        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#e31a1c", col = "white")),
        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "darkgreen", col = "white")),
        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#1f78b4", col = "white")),
        function(x, y, w, h) grid.rect(x, y, w, unit(1.6, "mm"), gp = gpar(fill = "purple", col = "white")),
        function(x, y, w, h) grid.points(x, y, pch = 16, size = unit(2, "mm"), gp = gpar(col = "purple"))
    )
    legend_labels = c("Base packages that are required", 
    	              "Recommended packages that are required", 
    	              "Contributed packages that are required", 
    	              qq("Packages that are required for installing '@{x$package}'"),
    	              qq("Packages that are loaded after 'library(@{x$package})'"))

    graphics_ind = seq_along(graphics)

    if(!any(colnames(m2) %in% RECOMMENDED_PKGS)) {
    	graphics_ind = setdiff(graphics_ind, 2)
    }

    if(length(x$pkg_from_session_info) == 0) {
    	graphics_ind = graphics_ind[-length(graphics_ind)]
    }

    graphics = graphics[graphics_ind]
    legend_labels = legend_labels[graphics_ind]
	
	# for the lower version of ComplexHeatmap
    if(packageVersion("ComplexHeatmap") > "2.7.1") {
    	lgd0 = Legend(labels = legend_labels,
			graphics = graphics,
			title = "", labels_gp = gpar(fontsize = legend_fontsize))
		lgd_list = list(lgd0)
	} else {
		lgd_list = NULL
	}
	
	df_imports = x$df_imports
	if(any(x$which_suggested_but_also_loaded)) {
		df_imports[x$which_suggsted_but_also_loaded, 1] = -Inf
	}
	ht = ht + rowAnnotation("n_import" = anno_nimports_barplot(df_imports, x$dep_fields, width = unit(1, "cm"),
			gp = gpar(fill = c("#ff7f00", "#cab2d6", "#8dd3c7"), col = NA),
			axis_param = list(gp = gpar(fontsize = 8*cex))),
			annotation_label = "Imported\nmethods", annotation_name_rot = 60,
			annotation_name_gp = gpar(fontsize = pkg_fontsize),
			annotation_name_offset = unit(8, "mm"))

	ht = ht + rowAnnotation(n_pkg = anno_barplot(rowSums(x$dep_mat), width = unit(1, "cm"),
								axis_param = list(gp = gpar(fontsize = 8*cex)), gp = gpar(fill = "#808080", col = NA)),
			annotation_label = "Required\npackages", annotation_name_rot = 60,
			annotation_name_gp = gpar(fontsize = pkg_fontsize),
			annotation_name_offset = unit(8, "mm"))

	ht = ht + rowAnnotation(heaviness = anno_barplot(x$heaviness, width = unit(1, "cm"), ylim = if(all(x$heaviness == 0)) c(0, 1) else NULL,
								axis_param = list(gp = gpar(fontsize = 8*cex)), gp = gpar(fill = "#808080", col = NA)),
			annotation_label = "Heaviness from \nparents", annotation_name_rot = 60,
			annotation_name_gp = gpar(fontsize = pkg_fontsize),
			annotation_name_offset = unit(8, "mm"))
	
	lgd1 = NULL
	if(any(rowSums(df_imports) > 0)) {
		if(all(df_imports[, 1] == 0 & df_imports[, 2] == 0)) {
			ind = 3
		} else if(all(df_imports[, 1] == 0 & df_imports[, 3] == 0)) {
			ind = 2
		} else if(all(df_imports[, 2] == 0 & df_imports[, 3] == 0)) {
			ind = 1
		} else if(all(df_imports[, 1] == 0)) {
			ind = 2:3
		} else if(all(df_imports[, 2] == 0)) {
			ind = c(1, 3)
		} else if(all(df_imports[, 3] == 0)) {
			ind = 1:2
		} else {
			ind = 1:3
		}
		lgd1 = Legend(title = "", at = c("Imported functions", "Imported S4 methods", "Imported S4 classes")[ind], 
			graphics = list(
				function(x, y, w, h) grid.rect(x, y, w, unit(1.6, "mm"), gp = gpar(fill = "#ff7f00", col = "white")),
				function(x, y, w, h) grid.rect(x, y, w, unit(1.6, "mm"), gp = gpar(fill = "#cab2d6", col = "white")),
				function(x, y, w, h) grid.rect(x, y, w, unit(1.6, "mm"), gp = gpar(fill = "#8dd3c7", col = "white"))
			)[ind],
			grid_width = unit(0.6, "cm"),
			labels_gp = gpar(fontsize = legend_fontsize))
	}

	lgd2 = NULL
	mdf = x$df_imports[is_field_required(x$dep_fields), , drop = FALSE]
	ind = numeric(0)
	if(any(mdf[, 1] == 0 & mdf[, 2] == 0 & mdf[, 3] == 0)) {
		ind = c(ind, 1)
	}
	if(any(mdf[, 1] < 0 & is.finite(mdf[, 1]))) {
		ind = c(ind, 2)
	}
	if(any(is.infinite(mdf[, 1]))) {
		ind = c(ind, 3)
	}
	if(length(ind)) {
		lgd2 = Legend(title = "", at = c("The whole namespace is imported", "The whole namespace is imported except some functions", "Package is listed in 'Imports' but namespace is not imported")[ind], 
			type = "lines",
			legend_gp = gpar(lty = 2, col = c("red", "blue", "#808080")[ind]), grid_width = unit(0.6, "cm"), 
			labels_gp = gpar(fontsize = legend_fontsize))
	}

	if(!is.null(lgd1) && !is.null(lgd2)) {
		lgd_list = c(lgd_list, list(packLegend(lgd1, lgd2, row_gap = unit(2, "pt"), max_height = unit(10, "cm"))))
	} else if(!is.null(lgd1)) {
		lgd_list = c(lgd_list, list(lgd1))
	} else if(!is.null(lgd2)) {
		lgd_list = c(lgd_list, list(lgd2))
	}

	ht = ht + rowAnnotation(pkg = anno_text(rownames(m), gp = gpar(fontsize = pkg_fontsize)))
	
	ht = draw(ht, 
		heatmap_legend_side = "bottom", 
		adjust_annotation_extension = FALSE,
		column_title = GetoptLong::qq("In total @{x$n_by_strong} packages are required directly or indirectly (@{x$n_by_all}) when installing '@{x$package}' (@{x$version})"),
		column_title_gp = gpar(fontsize = title_fontsize),
		heatmap_legend_list = lgd_list)

	w = convertWidth(ComplexHeatmap:::width(ht), "in", valueOnly = TRUE)
	h = convertHeight(ComplexHeatmap:::height(ht), "in", valueOnly = TRUE)

	if(fix_size && help) {
		ds = dev.size()
		if(w - ds[1] > 0.1 || h - ds[2] > 0.1) {
			message(paste0("The best device size to visualize the complete plot is ", 
				ceiling(w*100)/100, " x ", 
				ceiling(h*100)/100, " (in inches),\nor use `plot(obj, fix_size = FALSE)` so that heatmap cells are not in fixed sizes."))
		}
	}

	invisible(c(width = w, height = h))
}

scoreCol = function(x) {
	score = 0
	for(i in 1:length(x)) {
		if(!is.na(x[i])) {
			score = score + 2^(length(x)-i)
		}
	}
	return(score)
}

anno_nimports_barplot = function(x, category,
	bar_width = 0.6, ylim = NULL, axis = TRUE, gp = gpar(),
	axis_param = list(side = "top", labels_rot = 0),
	width = NULL, height = NULL, border = TRUE) {

	which = "row"

	anno_size = ComplexHeatmap:::anno_width_and_height(which, width, height, unit(2, "cm"))

	l = x[, 1] > 0 | x[, 2] > 0 | x[, 3] > 0
	if(!any(l) && is.null(ylim)) ylim = c(0, 1)

	if(any(l)) {
		data_scale = c(0, max(rowSums(x[l, , drop = FALSE])))
	} else {
		data_scale = c(0, 1)
	}

	if(is.null(ylim)) ylim = data_scale

	row_fun = function(index, k, n) {
		x2 = x
		x2[x2 < 0] = 0
		fun = anno_barplot(x2, gp = gp, which = "row", ylim = ylim,
			baseline = 0, width = anno_size$width, border = border, bar_width = bar_width,
			axis = axis, axis_param = axis_param)@fun
		fun(index, k, n)

		n = length(index)
		pushViewport(viewport(xscale = c(0, 1), yscale = c(0.5, n + 0.5)))
			
		if(is_field_required(category[index[1]])) {
			v = x[index, , drop = FALSE]
			l = v[, 1] == 0 & v[, 2] == 0 & v[, 3] == 0
			if(any(l)) {
				grid.segments(0, n - which(l) + 1, 1, n - which(l) + 1, gp = gpar(lty = 2, col = "red"), default.units = "native")
			}
			l = v[, 1] < 0 & is.finite(v[, 1])
			if(any(l)) {
				grid.segments(0, n - which(l) + 1, 1, n - which(l) + 1, gp = gpar(lty = 2, col = "blue"), default.units = "native")
			}
			l = is.infinite(v[, 1])
			if(any(l)) {
				grid.segments(0, n - which(l) + 1, 1, n - which(l) + 1, gp = gpar(lty = 2, col = "#808080"), default.units = "native")
			}
		}
		popViewport()
	}

	fun = row_fun

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_barplot",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		var_import = list(x, category, border, bar_width, axis, axis_param, anno_size, ylim, gp, is_field_required)
	)
	
	cl = new("AnnotationFunction")
	if("subsettable" %in% slotNames(cl)) {
		anno@subsettable = TRUE
	} else {
		anno@subsetable = FALSE
	}

	axis_param = ComplexHeatmap:::validate_axis_param(axis_param, which)
	axis_grob = if(axis) ComplexHeatmap:::construct_axis_grob(axis_param, which, data_scale) else NULL
	anno@extended = ComplexHeatmap:::update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}
