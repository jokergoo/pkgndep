

# == title
# Load pre-computed objects
#
# == param
# -file File name.
#
# == details
# The pathway of the file can be set via `pkgndep_opt`$db_file_template.
#
# Internally used.
load_from_heaviness_db = function(file) {
	version = pkgndep_opt$heaviness_db_version
	file = pkgndep_opt$db_file_template(file, version)
	if(grepl("^http", file)) {
		tmp_file = tempfile(fileext = ".rds")
		on.exit(file.remove(tmp_file))
		oe = try(download.file(file, tmp_file, quiet = TRUE), silent = TRUE)
		if(inherits(oe, "try-error")) {
			stop_wrap("The analysis depends on pre-calculated data objects hosted on GitHub. The link '@{file}' is not reachable.")
		}
		readRDS(tmp_file)
	} else {
		readRDS(file)
	}
}

# == title
# Load all package dependency statistics
#
# == details
# It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via `pkgndep_opt`$heaviness_db_version.
#
# == value
# A data frame.
#
# == example
# \dontrun{
# df = load_pkg_stat_snapshot()
# head(df)
# }
load_pkg_stat_snapshot = function() {
	version = pkgndep_opt$heaviness_db_version
	bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
	file = paste0("pkg_stat_snapshot_", bioc_version, ".rds")
	if(is.null(ENV$pkg_stat_snapshot)) {
		df = load_from_heaviness_db(file)
		ENV$pkg_stat_snapshot = df
		ENV$pkg_db_snapshot_version = version
	} else {
		if(ENV$pkg_db_snapshot_version != version) {
			df = load_from_heaviness_db(file)
			ENV$pkg_stat_snapshot = df
			ENV$pkg_db_snapshot_version = version
		}
	}
	invisible(ENV$pkg_stat_snapshot)
}

# == title
# The complete table of dependency heaviness for all CRAN/Bioconductor packages
#
# == value
# The returned data frame is directly from `load_pkg_stat_snapshot`, but with only a subset of columns of heaviness metrics.
all_pkg_stat_snapshot = function() {
	df = load_pkg_stat_snapshot()
	df[, c("package",
			"repository",
			"n_by_strong",
			"n_by_all",
			"n_parents",
			"max_heaviness_from_parents",
			"adjusted_max_heaviness_from_parents",
			"total_heaviness_from_parents",
			"adjusted_total_heaviness_from_parents",
			"max_co_heaviness_from_parents",
			"max_co_heaviness_parents_pair",
			"max_co_heaviness_parents_pair_type",
			"n_children",
			"heaviness_on_children",
			"adjusted_heaviness_on_children",
			"n_downstream",
			"heaviness_on_downstream",
			"adjusted_heaviness_on_downstream",
			"n_indirect_downstream",
			"heaviness_on_indirect_downstream",
			"adjusted_heaviness_on_indirect_downstream")]
}

# == title
# Load downstream dependency paths for all packages
#
# == details
# It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via `pkgndep_opt`$heaviness_db_version.
#
# == value
# A list.
#
# == example
# \dontrun{
# downstream_path_list = load_pkg_downstream_dependency_path_snapshot()
# downstream_path_list[["ComplexHeatmap"]]
# }
load_pkg_downstream_dependency_path_snapshot = function() {
	version = pkgndep_opt$heaviness_db_version
	bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
	file = paste0("pkg_downstream_dependency_path_snapshot_", bioc_version, ".rds")
	if(is.null(ENV$pkg_downstream_dependency_path_snapshot)) {
		lt = load_from_heaviness_db(file)
		ENV$pkg_downstream_dependency_path_snapshot = lt
		ENV$pkg_db_snapshot_version = version
	} else {
		if(ENV$pkg_db_snapshot_version != version) {
			lt = load_from_heaviness_db(file)
			ENV$pkg_downstream_dependency_path_snapshot = lt
			ENV$pkg_db_snapshot_version = version
		}
	}
	invisible(ENV$pkg_downstream_dependency_path_snapshot)
}



# == title
# Load dependency analysis results of all packages
#
# == param
# -hash Whether to convert the named list to a hash table by `hash::hash`.
#
# == details
# It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via `pkgndep_opt`$heaviness_db_version.
#
# == value
# A list (as a hash table) of ``pkgndep`` objects where each element corresponds to the analysis on one package.
#
# == example
# \dontrun{
# lt = load_all_pkg_dep()
# length(lt)
# head(names(lt))
# lt[["ggplot2"]]
# }
load_all_pkg_dep = function(hash = TRUE) {
	version = pkgndep_opt$heaviness_db_version
	bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
	file = paste0("all_pkgs_", bioc_version, ".rds")
	if(is.null(ENV$all_pkg_dep)) {
		lt = load_from_heaviness_db(file)
		if(hash) {
			ENV$all_pkg_dep = hash::hash(names(lt), lt)
		} else {
			ENV$all_pkg_dep = lt
		}
		ENV$pkg_db_snapshot_version = version
	} else {
		if(ENV$pkg_db_snapshot_version != version) {
			lt = load_from_heaviness_db(file)
			if(hash) {
				ENV$all_pkg_dep = hash::hash(names(lt), lt)
			} else {
				ENV$all_pkg_dep = lt
			}
			ENV$pkg_db_snapshot_version = version
		}
	}
	invisible(invisible(ENV$all_pkg_dep))
}



# == title
# Load package database
#
# == param
# -lib Local library path. If the value is ``NA``, only remote package database is used.
# -online If the value is ``TRUE``, it will directly use the newest package database file from CRAN/Bioconductor. If the 
#        value is ``FALSE``, it uses the pre-computated package database on a specific CRAN/Bioconductor snapshot. 
#        The version of the pre-computated package database can be set via `pkgndep_opt`$heaviness_db_version.
# -db A pre-computed ``pkg_db`` object.
# -verbose Whetehr to print messages.
#
# == details
# It loads the package database from CRAN/Bioconductor and locally installed packages.
#
# The database object internaly is cached for repeated use of other functions in this package.
#
# == value
# A ``pkg_db`` class object. See `reformat_db` for how to use the ``pkg_db`` object.
#
# == example
# \dontrun{
# pkg_db = load_pkg_db(lib = NA)
# pkg_db
# }
load_pkg_db = function(lib = NULL, online = TRUE, db = NULL, verbose = TRUE) {

	if(!is.null(db)) {
		ENV$pkg_db = db
		ENV$pkg_db_snapshot = db
		ENV$pkg_db_snapshot_version = "db_object"
		invisible(ENV$pkg_db)
	} else {
		if(online) {
			if(is.null(ENV$pkg_db)) {
				ENV$pkg_db = prepare_db(lib = lib, verbose = verbose)
				ENV$pkg_db_snapshot_version = "online"
			} else {
				if(ENV$pkg_db_snapshot_version != "online") {
					ENV$pkg_db = prepare_db(lib = lib, verbose = verbose)
					ENV$pkg_db_snapshot_version = "online"
				}
			}
			invisible(ENV$pkg_db)
		} else {
			version = pkgndep_opt$heaviness_db_version
			bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
			file = paste0("pkg_db_snapshot_", bioc_version, ".rds")
			
			if(is.null(ENV$pkg_db_snapshot)) {
				ENV$pkg_db_snapshot = load_from_heaviness_db(file)	
				ENV$pkg_db_snapshot_version = version
			} else {
				if(ENV$pkg_db_snapshot_version != version) {
					ENV$pkg_db_snapshot = load_from_heaviness_db(file)	
					ENV$pkg_db_snapshot_version = version	
				}
			}
			invisible(ENV$pkg_db_snapshot)
		}
	}
}

# == title
# Load heaviness statistics at all time points
#
# == details
# Used internally.
#
# == value
# A list of data frames.
load_heaviness_timeline = function() {
	if(is.null(ENV$lt_history)) {
		ENV$lt_history = load_from_heaviness_db("../lt_history.rds")
	}
	invisible(ENV$lt_history)
}


# == title
# Load DESCRIPTION files of all packages
#
# == details
# It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via `pkgndep_opt`$heaviness_db_version.
#
# == value
# A list of character vectors.
#
# == example
# \dontrun{
# lt = load_pkg_description()
# lt[1:2]
# }
load_pkg_description = function() {
	version = pkgndep_opt$heaviness_db_version
	bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
	file = paste0("pkg_description_", bioc_version, ".rds")
	if(is.null(ENV$pkg_description)) {
		lt = load_from_heaviness_db(file)
		ENV$pkg_description = lt
		ENV$pkg_db_snapshot_version = version
	} else {
		if(ENV$pkg_db_snapshot_version != version) {
			lt = load_from_heaviness_db(file)
			ENV$pkg_description = lt
			ENV$pkg_db_snapshot_version = version
		}
	}
	invisible(ENV$pkg_description)
}

# == title
# Load NAMESPACE files of all packages
#
# == details
# It is calculated based on a specific CRAN/Bioconductor snapshot. The version is set via `pkgndep_opt`$heaviness_db_version.
#
# == value
# A list of character vectors.
#
# == example
# \dontrun{
# lt = load_pkg_namespace()
# lt[1:2]
# }
load_pkg_namespace = function() {
	version = pkgndep_opt$heaviness_db_version
	bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == version]
	file = paste0("pkg_namespace_", bioc_version, ".rds")
	if(is.null(ENV$pkg_namespace)) {
		lt = load_from_heaviness_db(file)
		ENV$pkg_namespace = lt
		ENV$pkg_db_snapshot_version = version
	} else {
		if(ENV$pkg_db_snapshot_version != version) {
			lt = load_from_heaviness_db(file)
			ENV$pkg_namespace = lt
			ENV$pkg_db_snapshot_version = version
		}
	}
	invisible(ENV$pkg_namespace)
}

