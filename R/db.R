
prepare_db = function(lib = NULL, verbose = TRUE) {
	if(verbose) cat("retrieve package database from CRAN/Bioconductor...\n")
	suppressMessages(db_remote <- available.packages(repos = BiocManager::repositories(version = getFromNamespace(".version_choose_best", ns = "BiocManager")())))

	db_fields = c("Package", "Version", "Depends", "Imports", "LinkingTo", "Suggests", "Enhances", "Repository")
		
	if(identical(lib, NA)) { # only remotes
		db = db_remote[, db_fields]
		db = cbind(db, Local = "no")
		if(verbose) qqcat("  - @{nrow(db)} remote packages on CRAN/Bioconductor.\n")
	} else {
		db_local = installed.packages(lib.loc = lib)
		db_local = cbind(db_local, Repository = NA)
		# db_local = db_local[setdiff(rownames(db_local), BASE_PKGS), , drop = FALSE]

		p1 = db_remote[, "Package"]
		p2 = db_local[, "Package"]
		
		cn = intersect(p1, p2)
		if(length(cn)) {
			db_local[cn, "Repository"] = db_remote[cn, "Repository"]
		}

		db = db_remote[, db_fields]
		
		cn = intersect(p1, p2)
		if(length(cn)) {
			db[cn, ] = db_local[cn, db_fields]
		}

		cn = setdiff(p2, p1)
		if(length(cn)) {
			db = rbind(db, db_local[cn, db_fields])
		}
		db = cbind(db, Local = "no")
		db[p2, "Local"] = "yes"

		if(verbose) qqcat("  - @{nrow(db) - nrow(db_local)} remote packages on CRAN/Bioconductor.\n")
		if(verbose) qqcat("  - @{nrow(db_local)} packages installed locally.\n")
	}

	reformat_db(db)
}

# == title
# Format the package database
#
# == param
# -db A data frame returned from `utils::available.packages` or `utils::installed.packages`.
#
# == details
# It reformats the data frame of the package database into a ``pkg_db`` class object.
#
# == value
# A ``pkg_db`` class object. There are the following methods:
#
# -``pkg_db$get_meta(package,field=NULL)`` ``field`` can take values in "Package", "Version" and "Repository".
# -``pkg_db$get_dependency_table(package)`` Get the dependency table.
# -``pkg_db$get_rev_dependency_table(package)`` Get the reverse dependency table.
# -``pkg_db$package_dependencies(package,recursive=FALSE,reverse=FALSE,which="strong",simplify=FALSE)`` All the arguments are the same as in `tools::package_dependencies`. Argument ``simplify`` controls whether to return a data frame or a simplied vector.
#
# == example
# \dontrun{
# db = available.packages()
# db2 = reformat_db(db)
# }
#
# # a pkg_db object generated on 2021-10-28 can be loaded by load_pkg_db()
# db2 = load_pkg_db(snapshot = TRUE)
# db2
# db2$get_meta("ComplexHeatmap")
# db2$get_dependency_table("ComplexHeatmap")
# db2$get_rev_dependency_table("ComplexHeatmap")
# db2$package_dependencies("ComplexHeatmap")
# db2$package_dependencies("ComplexHeatmap", recursive = TRUE)
# 
reformat_db = function(db) {

	get_package_list_from_text = function(x) {
		if(is.na(x)) {
			return(character(0))
		}
		x = gsub("\\s*\\(.*?\\)", "", x)
		x = strsplit(x, "\\s*,\\s*")[[1]]
		setdiff(x, c("R", ""))
	}

	n = nrow(db)
	fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")

	cat("prepare dependency table...\n")
	lt = lapply(seq_len(n), function(i) {
		xl = lapply(fields, function(f) get_package_list_from_text(db[i, f]))
		d = unlist(xl)
		cbind(package = rep(db[i, "Package"], length(d)),
		       dependency = d,
		       dep_fields = rep(fields, times = sapply(xl, length)))
	})
	names(lt) = db[, "Package"]

	tb = do.call(rbind, lt)
	rownames(tb) = NULL

	cat("prepare reverse dependency table...\n")
	lt_ind = split(seq_len(nrow(tb)), tb[, "dependency"])
	lt_rev = lapply(lt_ind, function(ind) tb[ind, , drop = FALSE])
	names(lt_rev) = names(lt_ind)

	meta_df = data.frame(Package = db[, "Package"],
			              Version = db[, "Version"])
	if("Local" %in% colnames(db)) {
		meta_df$Local = db[, "Local"]
	}
	if("Repository" %in% colnames(db)) {
		meta_df$Repository = db[, "Repository"]
	}
	rownames(meta_df) = meta_df$Package

	pkg_db(
		meta = meta_df,
		dependency = lt,
		rev_dependency = lt_rev,
		dep_ind_hash = hash::hash(names(lt), seq_along(lt)),
		rev_dep_ind_hash = hash::hash(names(lt_rev), seq_along(lt_rev))
	)
}


pkg_db = setRefClass("pkg_db",
	fields = list(
		"meta" = "data.frame",
		"dependency" = "list",
		"rev_dependency" = "list",
		"dep_ind_hash" = "ANY",
		"rev_dep_ind_hash" = "ANY"
	),
	methods = list(
		show = function() {
			qqcat("A package database of @{nrow(.self$meta)} packages.\n")
			if("Local" %in% colnames(.self$meta)) {
				n1 = sum(.self$meta$Local == "yes")
				n2 = sum(.self$meta$Local == "no")
				qqcat("  - @{n1} local / @{n2} remote packages.\n")
			}
			if("Repository" %in% colnames(.self$meta)) {
				n1 = sum(grepl("(bioc|books|annotation|experiment|workflows)/src/contrib$", .self$meta$Repository))
				n2 = sum(grepl("src/contrib$", .self$meta$Repository)) - n1
				n3 = nrow(.self$meta) - n1 - n2
				qqcat("  - @{n1} Bioconductor / @{n2} CRAN / @{n3} other packages.\n")
			}
		})
)

pkg_db$methods(
	get_meta = function(package, field = NULL) {
		if(is.null(field)) {
			.self$meta[package, ]
		} else {
			.self$meta[package, field]
		}
	}
)

pkg_db$methods(
	get_dependency_table = function(package) {
		ind = .self$dep_ind_hash[[package]]
		if(is.null(ind)) {
			return(NULL)
		}
		.self$dependency[[ ind ]]
	}
)

pkg_db$methods(
	get_rev_dependency_table = function(package) {
		ind = .self$rev_dep_ind_hash[[package]]
		if(is.null(ind)) {
			return(NULL)
		}
		.self$rev_dependency[[ ind ]]
	}
)

pkg_db$methods(
	package_dependencies = function(package, recursive = FALSE, reverse = FALSE, 
		which = "strong", simplify = FALSE) {
		
		if(identical(which,"strong")) {
			which = c("Depends", "Imports", "LinkingTo")
		} else if(identical(which, "all")) {
			which = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
		} else if (identical(which, "most")) {
			which = c("Depends", "Imports", "LinkingTo", "Suggests")
		}

		empty_tb = matrix(nrow = 0, ncol = 3)
		colnames(empty_tb) = c("package", "dependency", "dep_fields")

		if(reverse) {
			tb = .self$get_rev_dependency_table(package)
			if(is.null(tb)) tb = empty_tb
			tb = tb[tb[, "dep_fields"] %in% which, , drop = FALSE]

			if(simplify) {
				tb[, "package"][!duplicated(tb[, "package"])]
			} else {
				tb
			}
		} else {
			if(recursive) {
				tb = .self$get_dependency_table(package)
				if(is.null(tb)) tb = empty_tb
				tb = tb[tb[, "dep_fields"] %in% which, , drop = FALSE]

				if(nrow(tb) > 0) {
					tbl = list(tb)
					dep_pkg = tb[, "dependency"]
					while(length(dep_pkg)) {
						tbl2 = lapply(dep_pkg, function(p) {
							tb = .self$get_dependency_table(p)
							if(is.null(tb)) tb = empty_tb
							tb[tb[, "dep_fields"] %in% which, , drop = FALSE]
						})
						tbl2 = tbl2[sapply(tbl2, nrow) > 0]

						tbl = c(tbl, tbl2)
						dep_pkg = unique(unlist(lapply(tbl2, function(x) x[, "dependency"])))
					}
					tb = do.call(rbind, tbl)
				}				
			} else {
				tb = .self$get_dependency_table(package)
				if(is.null(tb)) tb = empty_tb
				tb = tb[tb[, "dep_fields"] %in% which, , drop = FALSE]
			}

			if(simplify) {
				tb[, "dependency"][!duplicated(tb[, "dependency"])]
			} else {
				tb
			}
		}
	}
)
