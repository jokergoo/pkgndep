

load_from_pkgndep_db = function(file) {

	if(is.null(env$db[[file]])) {
		tmp_file = tempfile(fileext = ".rds")
		on.exit(file.remove(tmp_file))
		download.file(paste0("https://pkgndep.github.io/", file), tmp_file, quiet = TRUE)
		env$db[[file]] = readRDS(tmp_file)
	}
	env$db[[file]]
}

# == title
# Load all package dependency statistics
#
# == details
# It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
#
# == value
# A data frame of various columns.
#
# == example
# \dontrun{
# df = load_pkg_stat_snapshot()
# head(df)
# }
load_pkg_stat_snapshot = function() {
	if(is.null(env$pkg_stat_snapshot)) {
		df = load_from_pkgndep_db("pkg_stat_snapshot.rds")
		env$pkg_stat_snapshot = df
	}
	invisible(env$pkg_stat_snapshot)
}

# == title
# Load downstream dependency paths for all packages
#
# == details
# It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
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
	if(is.null(env$pkg_downstream_dependency_path_snapshot)) {
		lt = load_from_pkgndep_db("pkg_downstream_dependency_path_snapshot.rds")
		env$pkg_downstream_dependency_path_snapshot = lt
	}
	invisible(env$pkg_downstream_dependency_path_snapshot)
}



# == title
# Load dependency data of all packages
#
# == param
# -hash Whether to convert the named list to a hash table by `hash::hash`.
#
# == details
# It loads the package dependency analysis for all CRAN/Biocondutor packages done on 2021-10-28.
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
	if(is.null(env$all_pkg_dep)) {
		lt = load_from_pkgndep_db("all_pkgs.rds")
		if(hash) env$all_pkg_dep = hash::hash(names(lt), lt)
	}
	invisible(invisible(env$all_pkg_dep))
}



# == title
# Load package database
#
# == param
# -lib Local library path. If the value is ``NA``, only remote package database is used.
# -snapshot Internally used. If it is ``TRUE``, the package database generated on 2021-10-28 is used.
# -verbose Whetehr to print messages.
# -online If the value is ``TRUE``, it will directly use the package database file from CRAN/Bioconductor. If the 
#        value is ``FALSE``, it uses the cached package database retrieved on 2021-10-28.
#
# == details
# It loads the package database from CRAN/Bioconductor and locally installed packages.
#
# The database object internaly is cached for repeated use of other functions in this package.
#
# == value
# A ``pkg_db`` class object.
#
# == example
# \dontrun{
# pkg_db = load_pkg_db(lib = NA)
# pkg_db
# }
load_pkg_db = function(lib = NULL, snapshot = FALSE, verbose = TRUE, online = TRUE) {
	if(snapshot) {
		if(is.null(env$pkg_db_snapshot)) {
			env$pkg_db_snapshot = load_from_pkgndep_db("pkg_db_snapshot.rds")
		}
		invisible(env$pkg_db_snapshot)
	} else {
		if(!online) {
			env$pkg_db = load_from_pkgndep_db("pkg_db_snapshot.rds")
		} else {
			if(is.null(env$pkg_db)) {
				env$pkg_db = prepare_db(lib = lib, verbose = verbose)
			}
		}
		invisible(env$pkg_db)
	}
}
