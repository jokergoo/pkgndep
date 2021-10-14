
# == title
# Plot method
#
# == param
# -x An object from `pkgndep`.
# -pkg_fontsize Fontsize for the package names.
# -title_fontsize Fontsize for the title.
# -legend_fontsize Fontsize for the legends.
# -fix_size Should the rows and columns in the heatmap have fixed size?
# -cex A factor multiplicated to all font sizes.
# -help Whether to print help message?
# -file A path of the figure. The size of the figure is automatically calculated.
# -res Resolution of the figure (only for png and jpeg).
# -... Other arguments.
#
# == details
# If ``fix_size`` is set to ``TRUE``. The size of the whole plot can be obtained by:
#
#     size = plot(x, fix_size = TRUE)
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
plot.pkgndep = function(x, pkg_fontsize = 10*cex, title_fontsize = 12*cex, 
	legend_fontsize = 10*cex, fix_size = !dev.interactive(), cex = 1, 
	help = TRUE, file = NULL, res = 144, ...) {

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
				png(file, width = 3, height = 1, units = "in", res = res)
				grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
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
				jpeg(file, width = 3, height = 1, units = "in", res = res)
				grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
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
				svg_fun(file, width = 3, height = 1)
				grid.text(qq("No dependency found for package '@{x$package}'"), 0.5, 0.5)
				dev.off()
			}

			return(invisible(NULL))
		} else {
			stop("`file` only allows extensions of 'png/jpg/svg'.")
		}
	}

	get.gpar() # test whether the graphics window can be opened or not
	if(missing(fix_size) && cex != 1) {
		fix_size = TRUE
	}

	m = x$mat
	row_split = x$pkg_category

	if(ncol(m) == 0) {
		if(help) message(qq("No dependency found for package '@{x$package}'. Won't generate the plot.\n"))
		return(invisible(NULL))
	}

	row_order = order(apply(m, 1, function(x) sum(!is.na(x))))

	# a rude way to move all packages which are attached by imported packages before those by suggested packages
	column_order_by = apply(m, 2, function(x) sum(!is.na(x)))
	l = row_split %in% c("Depends", "Imports")
	l2 = apply(m[l, ,drop = FALSE], 2, function(x) sum(!is.na(x))) > 0
	column_order_by[l2] = column_order_by[l2] + 10000
	column_order = order(column_order_by, -apply(m[row_order, , drop = FALSE], 2, function(x) which(!is.na(x))[1]), decreasing = TRUE)

	line_height = grobHeight(textGrob("A", gp = gpar(fontsize = pkg_fontsize)))*1.5

	fix_size = fix_size
	ht = Heatmap(m, 
		name = x$package,
		row_split = row_split,
		column_split = ifelse(colnames(m) %in% BASE_PKGS, "Base packages", "Other packages"),
		rect_gp = gpar(col = "#EEEEEE"),
		show_row_dend = FALSE, 
		show_column_dend = FALSE,
		col = c("basePkgs" = "#e31a1c", "loadedOnly" = "#1f78b4", "otherPkgs" = "#33a02c"),
		row_order = row_order,
		column_order = column_order,
		column_names_gp = gpar(fontsize = pkg_fontsize),
		column_names_rot = 60,
		row_names_gp = gpar(fontsize = pkg_fontsize),
		column_title_gp = gpar(fontsize = title_fontsize),
		row_title_gp = gpar(fontsize = title_fontsize),
		row_title_rot = 90,
		bottom_annotation = HeatmapAnnotation(loaded = ifelse(apply(m[x$which_imported, , drop = FALSE], 2, function(x) any(!is.na(x))), "yes", "no"),
			col = list(loaded = c("yes" = "purple", "no" = "white")), simple_anno_size = unit(1, "mm"), 
			show_legend = FALSE, show_annotation_name = FALSE),
		width = if(fix_size) ncol(m)*line_height else NULL,
		height = if(fix_size) nrow(m)*line_height else NULL,
		right_annotation = if(any(x$pkg_category %in% c("Imports", "Depends"))) {
			rowAnnotation(loaded2 = ifelse(x$pkg_category %in% c("Imports", "Depends"), "yes", "no"),
					col = list(loaded2 = c("yes" = "purple", "no" = "white")), simple_anno_size = unit(1, "mm"), 
					show_legend = FALSE, show_annotation_name = FALSE)
		} else {
			NULL
		},
		show_heatmap_legend = FALSE
	)

	lgd0 = Legend(labels = c("Base packages that are attached", "Other attached packages", "Packages whose namespaces are loaded but are not attached", qq("Packages that are directly loaded after library(@{x$package})")),
			graphics = list(
		        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#e31a1c", col = "white")),
		        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#1f78b4", col = "white")),
		        function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "#33a02c", col = "white")),
		        function(x, y, w, h) grid.rect(x, y, w, unit(1, "mm"), gp = gpar(fill = "purple", col = "white"))
		    ),
			title = "", labels_gp = gpar(fontsize = legend_fontsize))
	lgd_list = list(lgd0)

	ht = ht + rowAnnotation(n_pkg = anno_barplot(apply(m, 1, function(x) sum(!is.na(x))), width = unit(2, "cm"),
								axis_param = list(gp = gpar(fontsize = 8*cex)), gp = gpar(fill = "#808080", col = NA)),
			annotation_label = "Loaded\nnamespaces", annotation_name_rot = 60,
			annotation_name_gp = gpar(fontsize = pkg_fontsize),
			annotation_name_offset = unit(8, "mm"))

	df_imports = x$df_imports
	df_imports[df_imports < 0] = 0
	ht = ht + rowAnnotation("n_import" = anno_nimports_barplot(df_imports, x$pkg_category, width = unit(2, "cm"),
			gp = gpar(fill = c("#ff7f00", "#cab2d6", "#8dd3c7"), col = NA),
			axis_param = list(gp = gpar(fontsize = 8*cex))),
			annotation_label = "Imported\nmethods", annotation_name_rot = 60,
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
			legend_gp = gpar(fill = c("#ff7f00", "#cab2d6", "#8dd3c7")[ind]), 
			labels_gp = gpar(fontsize = legend_fontsize))
	}

	lgd2 = NULL
	if(any(rowSums(df_imports)[x$pkg_category %in% c("Imports", "Depends")] == 0)) {
		lgd2 = Legend(title = "", at = c("The whole namespace is imported"), type = "lines",
			legend_gp = gpar(lty = 2, col = "#808080"), grid_width = unit(0.6, "cm"), 
			labels_gp = gpar(fontsize = legend_fontsize))
	}
	if(!is.null(lgd1) && !is.null(lgd2)) {
		lgd_list = c(lgd_list, list(packLegend(lgd1, lgd2)))
	} else if(!is.null(lgd1)) {
		lgd_list = c(lgd_list, list(lgd1))
	} else if(!is.null(lgd2)) {
		lgd_list = c(lgd_list, list(lgd2))
	}

	ht = ht + rowAnnotation(pkg = anno_text(rownames(m), 
			gp = gpar(fontsize = pkg_fontsize, 
				col = ifelse(x$pkg_available, "black", "#AAAAAA"),
				fontface = ifelse(x$pkg_available, "plain", 'italic'))))
	
	n_total1 = length(unique(c(rownames(x$m), colnames(x$m))))
	l1 = x$pkg_category %in% c("Depends", "Imports")
	l2 = apply(x$m[l1, , drop = FALSE], 2, function(x) any(!is.na(x)))
	n_total2 = length(unique(unlist(dimnames(x$m[l1, l2, drop = FALSE]))))
	ht = draw(ht, 
		heatmap_legend_side = "bottom", 
		adjust_annotation_extension = FALSE,
		column_title = GetoptLong::qq("In total @{n_total2} namespaces are loaded directly or indirectly (@{n_total1}) when loading '@{x$package}' (@{x$version})"),
		column_title_gp = gpar(fontsize = title_fontsize),
		heatmap_legend_list = lgd_list)

	w = convertWidth(ComplexHeatmap:::width(ht), "in", valueOnly = TRUE)
	h = convertHeight(ComplexHeatmap:::height(ht), "in", valueOnly = TRUE)

	if(fix_size && help) {
		ds = dev.size()
		if(w - ds[1] > 0.1 || h - ds[2] > 0.1) {
			message(paste0("The best device size to visualize the complete plot is ", 
				ceiling(w*100)/100, " x ", 
				ceiling(h*100)/100, " (in inches),\nor use plot(obj, fix_size = FALSE) so that heatmap cells are not in fixed sizes."))
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

	if(all(rowSums(x) == 0) && is.null(ylim)) ylim = c(0, 1)

	row_fun = function(index, k, n) {
		fun = anno_barplot(x, gp = gp, which = "row", ylim = ylim,
			baseline = 0, width = anno_size$width, border = border, bar_width = bar_width,
			axis = axis, axis_param = axis_param)@fun
		fun(index, k, n)

		if(category[index[1]] %in% c("Imports", "Depends")) {
			v = x[index, , drop = FALSE]
			l = rowSums(v) == 0
			if(any(l)) {
				n = length(index)
				pushViewport(viewport(xscale = c(0, 1), yscale = c(0.5, n + 0.5)))
				grid.segments(0, n - which(l) + 1, 1, n - which(l) + 1, gp = gpar(lty = 2, col = "#808080"), default.units = "native")
				popViewport()
			}
		}
	}

	fun = row_fun

	anno = AnnotationFunction(
		fun = fun,
		fun_name = "anno_barplot",
		which = which,
		width = anno_size$width,
		height = anno_size$height,
		var_import = list(x, category, border, bar_width, axis, axis_param, anno_size, ylim, gp)
	)
		
	anno@subsetable = TRUE

	data_scale = c(0, max(rowSums(x)))
	if(data_scale[2] == 0) data_scale[2] = 1

	axis_param = ComplexHeatmap:::validate_axis_param(axis_param, which)
	axis_grob = if(axis) ComplexHeatmap:::construct_axis_grob(axis_param, which, data_scale) else NULL
	anno@extended = ComplexHeatmap:::update_anno_extend(anno, axis_grob, axis_param)

	return(anno) 
}
