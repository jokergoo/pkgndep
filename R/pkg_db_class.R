
get_bioc_repo = function(version) {
	if(as.numeric_version(version) < as.numeric_version("1.8")) {
		stop("Bioc 'version' should be no earlier than 1.8.")
	}
	version = as.character(version)
	
	c("BioCsoft" = paste0("https://bioconductor.org/packages/", version, "/bioc"),
      "BioCann"  = paste0("https://bioconductor.org/packages/", version, "/data/annotation"),
      "BioCexp"  = paste0("https://bioconductor.org/packages/", version, "/data/experiment")
     )
}

#' Build package database from remote repositories
#' 
#' @param lib Library pathes. If the value is `NA`, package database is only built with remote
#'       CRAN/Bioconductor packages. If the value is `NULL` or self-provided library pathes, 
#'       package database is built with locally installed packages and remote CRAN/Bioconductor packages.
#' @param bioc_version The Bioconductor version. By default it is the version corresponding to 
#'        the R version under use.
#' @param verbose Whether to print messages.
#' 
#' @return A `pkg_db` object.
#' @importFrom utils chooseCRANmirror available.packages installed.packages
#' @import GetoptLong
#' @export
#' @examples
#' \dontrun{
#' db = build_pkg_db_from_repo(lib = NA)
#' }
build_pkg_db_from_repo = function(lib = NULL, bioc_version = pkgndep_opt$bioc_version, verbose = TRUE) {

	op = getOption("repos")
	on.exit(options(repos = op))
	
	cran_repo = getOption("repos")[["CRAN"]]
	if(!grepl("http", cran_repo, ignore.case = TRUE)) {
		cran_repo = "https://cran.rstudio.com/"
	}
	bioc_repo = get_bioc_repo(bioc_version)
	repos = c(cran_repo, bioc_repo)

	if(verbose) qqcat("retrieve package database from CRAN/Bioconductor (@{bioc_version})...\n")
	oe = try(suppressMessages({
		db_cran = available.packages(repos = repos[1], filters = character(0))
		db_cran[, "Repository"] = "CRAN"
		db_bioc = available.packages(repos = repos[-1], filters = character(0))
		db_bioc[, "Repository"] = "Bioconductor"
		db_remote = rbind(db_cran, db_bioc)
	}))

	if(inherits(oe, "try-error")) {
		warning("Can not load package database from remote repositories, use the snapshot database.")
		return(pkgndep.db::load_from_heaviness_db(paste0("pkg_db_snapshot", pkgndep_opt$heaviness_db_version, ".rds")))
	}

	# add BASE packages, note when `filter = character(0)` was set, recommended packages are also included
	lt_dcf = lapply(c(setdiff(BASE_PKGS, "base")), function(x) {
		m = read.dcf(system.file("DESCRIPTION", package = x))
		m2 = matrix(NA_character_, nrow = 1, ncol = ncol(db_remote))
		colnames(m2) = colnames(db_remote)

		cn = intersect(colnames(m), colnames(db_remote))
		m2[, cn] = m[, cn]
		m2[, "Repository"] = "CRAN"
		m2
	})

	db_remote = rbind(db_remote, do.call(rbind, lt_dcf))
	db_remote = db_remote[!duplicated(db_remote[, "Package"]), , drop = FALSE]
	rownames(db_remote) = db_remote[, "Package"]

	db_fields = c("Package", "Version", "Depends", "Imports", "LinkingTo", "Suggests", "Enhances", "Repository")
		
	if(identical(lib, NA)) { # only remotes
		db = db_remote[, db_fields]
		db = cbind(db, Local = "no")
		if(verbose) qqcat("  - @{nrow(db)} remote packages on CRAN/Bioconductor.\n")
		
	} else {
		db_local = installed.packages(lib.loc = lib)
		db_local = cbind(db_local, Repository = NA)

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
	
	build_pkg_db(db, version = as.character(Sys.Date()))
}

#' Format package database
#' 
#' @param db A matrix returned from [`utils::available.packages()`] or [`utils::installed.packages()`].
#' @param version A text label of the version of the database.
#'
#' @details
#' It reformats the data frame of the package database into a `pkg_db` class object.
#'
#' @return
#' A `pkg_db` class object. There are the following methods:
#'
#' -``pkg_db$get_meta(package,field=NULL)`` ``field`` can take values in "Package", "Version" and "Repository".
#' -``pkg_db$get_dependency_table(package)`` Get the dependency table.
#' -``pkg_db$get_rev_dependency_table(package)`` Get the reverse dependency table.
#' -``pkg_db$package_dependencies(package,recursive=FALSE,reverse=FALSE,which="strong",simplify=FALSE)`` All the arguments are the same as in `tools::package_dependencies`. Argument ``simplify`` controls whether to return a data frame or a simplied vector.
#'
#' @examples
#' \dontrun{
#' db = available.packages(filters = character(0))
#' db2 = reformat_pkg_db(db, version = "today")
#'
#' # a pkg_db object generated on 2021-10-28 can be loaded by load_pkg_db()
#' db2 = load_pkg_db(online = FALSE)
#' db2
#' db2$get_meta("ComplexHeatmap")
#' db2$get_dependency_table("ComplexHeatmap")
#' db2$get_rev_dependency_table("ComplexHeatmap")
#' db2$package_dependencies("ComplexHeatmap")
#' db2$package_dependencies("ComplexHeatmap", recursive = TRUE)
#' }
build_pkg_db_from_mat = function(db, name = "R packages", version = "today") {

	get_package_list_from_text = function(x) {
		if(is.na(x)) {
			return(character(0))
		}
		x = gsub("\\s*\\(.*?\\)", "", x)
		x = strsplit(x, "\\s*,\\s*")[[1]]
		setdiff(x, c("R", ""))
	}

	db = db[!duplicated(db[, "Package"]), , drop = FALSE]

	n = nrow(db)
	fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")

	cat("prepare dependency table...\n")

	lt = lapply(seq_len(n), function(i) {
		xl = lapply(fields, function(f) get_package_list_from_text(db[i, f]))
		d = unlist(xl)
		structure(d, names = rep(fields, times = sapply(xl, length)))
	})
	
	children = rep(db[, "Package"], times = sapply(lt, length))
	parents = unlist(lt)
	relations = names(parents)
	children = unname(children)
	parents = unname(parents)

	# it is possible dependent packages are not in the package database
	all_pkgs = sort(union(db[, "Package"], parents))
	external = !(all_pkgs %in% db[, "Package"])
	name_to_ind = structure(names = all_pkgs, seq_along(all_pkgs))
	n_pkg = length(all_pkgs)

	meta_lt = list(
		package = all_pkgs,
		version = rep(NA_character_, n_pkg)
	)

	meta_lt$version[ name_to_ind[db[, "Package"]] ] = db[, "Version"]

	meta_lt$repository = rep(NA_character_, n_pkg)
	if("Repository" %in% colnames(db)) {
		meta_lt$repository[ name_to_ind[db[, "Package"]] ] = ifelse(grepl("(bioc|books|annotation|experiment|workflows)/src/contrib$", db[, "Repository"]), "Bioconductor", "CRAN")
	}

	meta_lt$local = rep(FALSE, n_pkg)
	if("Local" %in% colnames(db)) {
		meta_lt$local[ name_to_ind[db[, "Package"]] ] = db[, "Local"] == "yes"
	}

	meta_df = data.frame(meta_lt)
	rownames(meta_df) = NULL

	parents = unname(name_to_ind[parents])
	children = unname(name_to_ind[children])

	lt_parents = rep(list(), n_pkg)
	lt_foo = split( parents, children )
	lt_parents[as.numeric(names(lt_foo))] = lt_foo

	lt_children = rep(list(), n_pkg)
	lt_foo = split( children, parents )
	lt_children[as.numeric(names(lt_foo))] = lt_foo

	relation_names = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
	relation_types = c("strong", "strong", "strong", "weak", "weak")

	relation_to_ind = structure(names = relation_names, seq_along(relation_names))
	relations = unname(relation_to_ind[relations])

	lt_parent_relations = rep(list(), n_pkg)
	lt_foo = split( relations, children )
	lt_parent_relations[as.numeric(names(lt_foo))] = lt_foo

	lt_child_relations = rep(list(), n_pkg)
	lt_foo = split( relations, parents )
	lt_child_relations[as.numeric(names(lt_foo))] = lt_foo

	meta_relations = data.frame(relation = relation_names, type = relation_types)

	package_db(
		meta_packages = meta_df,
		n_packages = nrow(meta_df),
		external = external,
		lt_parents = lt_parents,
		lt_children = lt_children,
		lt_parent_relations = lt_parent_relations,
		lt_child_relations = lt_child_relations,
		ind_hash = hash::hash(all_pkgs, seq_along(all_pkgs)),
		meta_relations = meta_relations,
		name = name,
		version = version
	)
}


package_db = setClass("package_db",
	slots = c(
		"meta_packages" = "data.frame",  # the first column is package name
		"n_packages" = "integer",
		"external" = "logical", # whether the package is external of the repo 
		"lt_parents" = "list",
		"lt_children" = "list",
		"lt_parent_relations" = "list",
		"lt_child_relations" = "list",
		"ind_hash" = "hash",
		"meta_relations" = "data.frame", # the first column is the relation name, the second column is the strong/weak
		"name" = "character",
		"version" = "ANY"
	)
)



setMethod("show",
	sig = "package_db",
	definition = function(object) {

	qqcat("A @{class(object)} object:\n")
	qqcat("  Name: @{object@name}\n")
	qqcat("  Version: @{object@version}\n")
	if(any(object@external)) {
		qqcat("  Packages: @{object@n_packages} (@{sum(object@external)} external packages)\n")
	} else {
		qqcat("  Packages: @{object@n_packages}\n")
	}
	l_strong = object@meta_relations[, 2] == "strong"
	qqcat("  Strong relations: @{paste(object@meta_relations[l_strong, 1], collapse = ', ')}\n")
	if(any(!l_strong)) {
		qqcat("  Weak relations: @{paste(object@meta_relations[!l_strong, 1], collapse = ', ')}\n")
	}
	
})

setMethod("pkg_get_index", signature = "package_db",
	definition = function(pkg_db, package) {

	if(length(package) != 1) {
		stop("Length of `package` can only be one.")
	}
	pkg_db@ind_hash[[package]]
})


setMethod("pkg_get_meta", signature = "package_db",
	definition = function(pkg_db, package) {
		
	ind = pkg_get_index(pkg_db, package)

	pkg_db@meta_packages[ind, , drop = FALSE]
})

setMethod("pkg_dependency_table", signature = "package_db",
	definition = function(pkg_db, package) {

	ind = pkg_get_index(pkg_db, package)

	if(is.null(ind)) {
		return(data.frame(parent = character(0),
			              child = character(0),
			              relation = character(0)))
	}

	parents = object@lt_parents[[ind]]
	child = object@meta_packages[ind, "package"]
	relations = object$lt_parent_relations[[ind]]
	
	data.frame(parent = parents, 
		child = children, 
		relation = object@meta_relations[, 1][relations]
	)
})

setMethod("pkg_normalize_relation_types", signature = "package_db",
	definition = function(pkg_db, dep_relations, use_code = FALSE) {

	meta_relations = object@meta_relations
	if("strong" %in% dep_relations) {
		dep_relations = c(dep_relations, meta_relations[, 1][meta_relations[, 2] == "strong"])
	}
	if("weak" %in% dep_relations) {
		dep_relations = c(dep_relations, meta_relations[, 1][meta_relations[, 2] == "weak"])
	}
	dep_relations = intersect(dep_relations, meta_relations[, 1])
	if(length(dep_relations) == 0) {
		stop("Cannot find any relation type.")
	}

	if(use_code) {
		unname(structure(seq_len(nrow(meta_relations)), names = meta_relations[, 1])[dep_relations])
	} else {
		dep_relations
	}
})

setMethod("pkg_local_dependency", signature = "package_db",
	definition = function(pkg_db, package, which = c("parents", "children"), 
		dep_relations = "strong", use_id = FALSE) {
		
	ind = pkg_get_index(pkg_db, package)
	if(is.null(ind)) {
		return(NULL)
	}

	dep_relations = pkg_normalize_relation_types(dep_relations)

	which = match.arg(which)[1]
	if(which == "parents") {
		children = ind
		parents = object@lt_parents[[ind]]
		relations = object@lt_parent_relations[[ind]]
	} else {
		children = object@lt_children[[ind]]
		parent = ind
		relations = object@lt_child_relations[[ind]]
	}

	if(use_id) {
		df = data.frame(
			parent = parents, 
			child = ind, 
			relation = object@meta_relations[, 1][relations]
		)
	} else {
		parents = object@meta_packages[parents, "package"]
		child = object@meta_packages[ind, "package"]
		df = data.frame(
			parent = parents, 
			child = child, 
			relation = object@meta_relations[, 1][relations]
		)
	}
	df[df$relation %in% dep_relations, , drop = FALSE]

})


setMethod("pkg_parent_dependency", signature = "package_db", 
	definition = function(pkg_db, package, dep_relations = "strong", use_id = FALSE) {

	pkg_local_dependency(pkg_db = pkg_db, package = package, which = "parents", dep_relations = dep_relations,
		use_id = use_id)

})


setMethod("pkg_child_dependency", signature = "package_db", 
	definition = function(pkg_db, package, dep_relations = "strong", use_id = FALSE) {

	pkg_local_dependency(pkg_db = pkg_db, package = package, which = "children", dep_relations = dep_relations,
		use_id = use_id)

})


setMethod("pkg_remote_dependency", signature = "package_db", which = c("upstream", "downstream"),
	definition = function(pkg_db, package, dep_relations = "strong", use_id = FALSE)) {

	ind = pkg_get_index(pkg_db, package)
	if(is.null(ind)) {
		return(NULL)
	}

	dep_relation_codes = pkg_normalize_relation_types(pkg_db, dep_relations, use_code = TRUE)
	
	which = match.arg(which)[1]

	visited = rep(0, object@n_packages)
	visited[ind] = 1

	if(which == "upstream") {
		lt = object@lt_parents
		lt_relations = object@lt_parent_relations
	} else {
		lt = object@lt_children
		lt_relations = object@lt_child_relations
	}
		
	df = data.frame(parent = integer(0), child = integer(0), relation = integer(0), distance = integer(0))

	current = ind
	dist = 0
	while(TRUE) {
		dist = dist + 1
		df2 = do.call(rbind, lapply(current, function(i) {
			if(which == "upstream") {
				p = lt[[i]]
				cl = rep(i, length(p))
				r = lt_relations[[i]]
				l = r %in% dep_relation_codes
				p = p[l]
				cl = cl[l]
				r = r[l]
			} else {
				cl = lt[[i]]
				p = rep(i, length(cl))
				r = lt_relations[[i]]
				l = r %in% dep_relation_codes
				p = p[l]
				cl = cl[l]
				r = r[l]
			}
			data.frame(parent = p, child = cl, relation = r, dist = dist)
		}))
		df = rbind(df, df2)

		next = unique(df2[, 1])
		visited[next] = visited[next] + 1

		# get rid of cyclic/recursive dependency
		l = visited[next] < 2 & !object@external[next]

		current = next[l]

		if(length(current) == 0) {
			break
		}
	}

	if(use_id) {
		df$relation = object@meta_relations[, 1][df$relation]
		df
	} else {
		df$parent = object@meta_packages[, 1][df$parent]
		df$child = object@meta_packages[, 1][df$child]
		df$relation = object@meta_relations[, 1][df$relation]
		df
	}
})

setMethod("pkg_upstream_dependency", signature = "package_db", 
	definition = function(pkg_db, package, dep_relations = "strong", use_id = FALSE) {

	pkg_remote_dependency(pkg_db = pkg_db, package = package, which = "parents", dep_relations = dep_relations,
		use_id = use_id)

})


setMethod("pkg_downstream_dependency", signature = "package_db", 
	definition = function(pkg_db, package, dep_relations = "strong", use_id = FALSE) {

	pkg_remote_dependency(pkg_db = pkg_db, package = package, which = "children", dep_relations = dep_relations,
		use_id = use_id)

})


setMethod("pkg_strong_dependencies", signature = "package_db", 
	definition = function(pkg_db, package, ignore = NULL) {

	ind = pkg_get_index(pkg_db, package)
	if(is.null(ind)) {
		return(NULL)
	}

	ind_ignore = which(pkg_db@meta_packages[, 1] %in% ignore)
	if(ind %in% ind_ignore) {
		return(character(0))
	}
	ignore_type = match.arg(ignore_type)[1]

	dep_relation_codes = pkg_normalize_relation_types(pkg_db, dep_relations, use_code = TRUE)

	visited = rep(0, object@n_packages)
	visited[ind] = 1

	if(ind_ignore) {
		visited[ind_ignore] = 1
	}

	lt_parents = object@lt_parents
	lt_parent_relations = object@lt_parent_relations
	
	v = integer(0)

	current = ind
	while(TRUE) {
		p = unique(unlist(lapply(current, function(i) {
			p = lt[[i]]
			r = lt_relations[[i]]
			l = r %in% dep_relation_codes
			p = p[l]

			l = visited[p] < 1 & !object@external[p]
			p[l]
			
		})))
		
		v = c(v, p)

		visited[p] = visited[p] + 1

		l = visited[p] < 1 & !object@external[p]

		current = p[l]

		if(length(current) == 0) {
			break
		}
	}

	v = unique(v)
	df@meta_packages[, 1][v]
})

setMethod("pkg_total_dependencies", signature = "package_db", 
	definition = function(pkg_db, package) {

	df = pkg_parent_dependency(pkg_db, package, dep_relations = c("strong", "weak"))
	
	if(nrow(df)) {
		v = unlist(lapply(df[, 1], function(p) {
			pkg_strong_dependencies(pkg_db, p)
		}))
		v = union(v, df[, 1])
	} else {
		character(0)
	}

})


setMethod("pkg_heaviness", signature = "package_db",
	definition = function(pkg_db, package, dep_relations = c("strong", "weak")) {

	parent_strong_deps = pkg_parent_dependency(pkg_db, package, dep_relations = "strong")[, 1]
	v1 = pkg_strong_dependencies(pkg_db, package)
	n1 = length(v1)

	heaviness1 = integer(nrow(parent_strong_deps))
	names(heaviness1) = parent_strong_deps[, 1]
	for(p in parent_deps) {
		v2 = pkg_strong_dependencies(pkg_db, package, ignore = p)
		n2 = length(v2)
		heaviness1[p] = n1 - n2
	}

	parent_weak_deps = pkg_parent_dependency(pkg_db, package, dep_relations = "weak")[, 1]
	heaviness2 = integer(nrow(parent_weak_deps))
	names(heaviness2) = parent_weak_deps[, 1]

	for(p in parent_deps) {
		v2 = pkg_strong_dependencies(pkg_db, p)
		n2 = length(union(v1, v2))
		heaviness2[p] = n2 - n1
	}

	c(heaviness1, heaviness2)

})



validate_pkg_db = function(db) {
	all_pkgs = names(db$dependency)
	all_pkgs = union(all_pkgs, c(BASE_PKGS, RECOMMENDED_PKGS))

	lt = list()

	all_strong = lapply(db$dependency, function(x) x[x[, "dep_fields"] %in% c("Depends", "Imports", "LinkingTo"), "dependency"])
	all_strong = unique(unlist(all_strong))
	not_in = setdiff(all_strong, all_pkgs)
	lt$strong = not_in

	all = lapply(db$dependency, function(x) x[x[, "dep_fields"] %in% c("Suggests"), "dependency"])
	all = unique(unlist(all))
	not_in = setdiff(all, all_pkgs)
	lt$suggests = not_in

	all = lapply(db$dependency, function(x) x[x[, "dep_fields"] %in% c("Enhances"), "dependency"])
	all = unique(unlist(all))
	not_in = setdiff(all, all_pkgs)
	lt$enhances = not_in

	return(lt)
}

