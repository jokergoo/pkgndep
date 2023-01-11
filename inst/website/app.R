
cat("- Load packages for building website.\n")

# suppressPackageStartupMessages(library(pkgndep))
# load_namespace = function (dir) {
#     package.lib = dirname(dir)
#     package = basename(dir)
#     foo = parseNamespaceFile(package, package.lib = package.lib)
#     for (x in sapply(foo$imports, "[[", 1)) {
#         if (!any(grepl(paste0("package:", x), search()))) {
#             suppressPackageStartupMessages(try({
#                 t <- microbenchmark::microbenchmark(attachNamespace(x),
#                   times = 1)$time[1]/1e+06
#                 cat("load namespace ", x, ", ", round(t, 2),
#                   " ms\n", sep = "")
#             }))
#         }
#     }
#     if (file.exists(paste0(dir, "/src"))) {
#         for (f in list.files(paste0(dir, "/src"), pattern = "\\.cpp$",
#             full.names = TRUE)) {
#             if (basename(f) == "RcppExports.cpp")
#                 next
#             cat("compiling", f, "\n")
#             Rcpp::sourceCpp(f)
#         }
#     }
#     desc = read.dcf(paste0(dir, "/DESCRIPTION"))
#     if ("Collate" %in% colnames(desc)) {
#         all_R_files = strsplit(desc[, "Collate"], "\\s")[[1]]
#         all_R_files = paste0(dir, "/R/", all_R_files)
#     }
#     else {
#         all_R_files = list.files(paste0(dir, "/R"), full.names = TRUE)
#     }
#     for (f in all_R_files) {
#         if (basename(f) == "RcppExports.R")
#             next
#         cat("sourcing", f, "\n")
#         source(f)
#     }
#     fun_name = GetoptLong::qq("load_@{package}")
#     fun = function(pattern = NULL) {
#         if (is.null(pattern)) {
#             load_namespace(dir)
#         }
#         else {
#             all_R_files = list.files(paste0(dir, "/R"), full.names = TRUE)
#             l = grepl(pattern, basename(all_R_files))
#             if (any(l)) {
#                 for (f in all_R_files[l]) {
#                   cat("sourcing", f, "\n")
#                   source(f)
#                 }
#             }
#         }
#     }
#     assign(fun_name, fun, envir = .GlobalEnv)
#     return(invisible(NULL))
# }
# load_namespace("~/project/development/pkgndep"); set_var()

suppressPackageStartupMessages(library(pkgndep))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(brew))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(grid))

# version = commandArgs(trailingOnly = TRUE)[1]
version = ALL_BIOC_RELEASES$Release[1]
r_version = ALL_BIOC_RELEASES$R[1]
pkgndep_opt$heaviness_db_version = version
# pkgndep_opt$db_file_template = function(file, version) paste0("~/project/development/pkgndep.github.io/", version, "/", file)

bioc_version = ALL_BIOC_RELEASES$Release[ALL_BIOC_RELEASES$Date == pkgndep_opt$heaviness_db_version]

BASE_PKGS = pkgndep:::BASE_PKGS
DEFAULT_LOADED_BASE_PKGS = pkgndep:::DEFAULT_LOADED_BASE_PKGS
FIELDS = pkgndep:::FIELDS

env = new.env()
env$figure_dir = tempdir()


# system.file = function(..., package) {
# 	paste0("~/project/development/pkgndep/inst/", paste(..., sep = "/"))
# }

source(system.file("website", "lib.R", package = "pkgndep"))


qqcat("- Load heaviness database version @{r_version}.\n")
load_pkg_db(online = FALSE, verbose = FALSE)

cat("- Load pre-calculated dependency results of all packages.\n")
load_all_pkg_dep()

cat("- Load pre-calculated heaviness metrics of all packages.\n")
CUTOFF = new.env()
stat_df = load_pkg_stat_snapshot()

cat("- Load DESCRIPTION and NAMESPACE of all packages.\n")
load_pkg_description()
load_pkg_namespace()

load_heaviness_timeline()
cat("- Load downstream dependency paths.\n")
load_pkg_downstream_dependency_path_snapshot()

v = stat_df$adjusted_max_heaviness_from_parents; v = v[v > 0]
CUTOFF$adjusted_max_heaviness_from_parents = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)
v = stat_df$adjusted_heaviness_on_children; v = v[v > 0]
CUTOFF$adjusted_heaviness_on_children = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)
v = stat_df$adjusted_heaviness_on_indirect_downstream; v = v[v > 0]
CUTOFF$adjusted_heaviness_on_indirect_downstream = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)

suppressPackageStartupMessages(library(Rook))

httpd = Rhttpd$new()
suppressMessages(httpd$start(quiet = TRUE))


httpd$add(app = File$new(system.file("www", "_js", package = "pkgndep")), name = 'js')
httpd$add(app = File$new(system.file("www", "_css", package = "pkgndep")), name = 'css')
httpd$add(app = File$new(system.file("www", "_image", package = "pkgndep")), name = 'image')

request_log = function(page, param) {

	param = param[names(param) != ""]

	if(length(param) == 0) {
		msg = page
	} else {
		msg = paste0(page, "?", paste(names(param), "=", param, sep = "", collapse = "&"))
	}

	msg = paste0("[", as.character(Sys.time()), "] ", msg)
	message(msg)
}

httpd$add(name = "main",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("main", param)

	package = param[["package"]]
	order_by = param[["order_by"]]
	if(is.null(order_by)) {
		order_by = "adjusted_heaviness_on_children"
	}

	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else {
		page = as.numeric(page)
	}
	records_per_page = param[["records_per_page"]]
	if(is.null(records_per_page)) {
		records_per_page = 20
	}
	records_per_page = as.numeric(records_per_page)

	only_reducible = param[["reducible"]]
	if(is.null(only_reducible)) {
		only_reducible = FALSE
	} else if(only_reducible == "") {
		only_reducible = FALSE
	} else {
		only_reducible = TRUE
	}

	exclude_children = param[["exclude_children"]]
	if(is.null(exclude_children)) {
		exclude_children = FALSE
	} else if(exclude_children == "") {
		exclude_children = FALSE
	} else {
		exclude_children = TRUE
	}

	if(exclude_children && order_by == "adjusted_heaviness_on_downstream") {
		order_by = "adjusted_heaviness_on_indirect_downstream"
	}
	if(!exclude_children && order_by == "adjusted_heaviness_on_indirect_downstream") {
		order_by = "adjusted_heaviness_on_downstream"
	}

	version = param[["version"]]
	if(is.null(version)) {

	} else if(version == "") {

	} else {
		pkgndep_opt$heaviness_db_version = version
	}

	response = Response$new()

	html_main_page(response, package = package, order_by = order_by, page = page, records_per_page = records_per_page, 
		only_reducible = only_reducible, exclude_children = exclude_children)

	response$finish()
})

httpd$add(name = "package",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("package", param)

	package = param[["package"]]

	response = Response$new()

	html_single_package(response, package = package)

	response$finish()
})

httpd$add(name = "parent_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("parent_dependency", param)

	package = param[["package"]]
	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else if(page == "") {
		page = 1
	} else {
		page = as.numeric(page)
	}

	response = Response$new()

	html_parent_dependency(response, package = package, page = page)

	response$finish()
})

httpd$add(name = "upstream_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("upstream_dependency", param)

	package = param[["package"]]
	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else if(page == "") {
		page = 1
	} else {
		page = as.numeric(page)
	}

	response = Response$new()

	html_upstream_dependency(response, package = package, page = page)

	response$finish()
})

httpd$add(name = "downstream_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("downstream_dependency", param)

	package = param[["package"]]
	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else if(page == "") {
		page = 1
	} else {
		page = as.numeric(page)
	}
	records_per_page = param[["records_per_page"]]
	if(is.null(records_per_page)) {
		records_per_page = 20
	} else {
		records_per_page = as.numeric(records_per_page)
	}

	min_depth = param[["min_depth"]]
	if(is.null(min_depth)) {
		min_depth = 0
	} else {
		min_depth = as.numeric(min_depth)
	}
	max_depth = param[["max_depth"]]
	if(is.null(max_depth)) {
		max_depth = Inf
	} else {
		max_depth = as.numeric(max_depth)
	}

	response = Response$new()

	html_downstream_dependency(response, package = package, page = page, records_per_page = records_per_page, min_depth = min_depth, max_depth = max_depth)

	response$finish()
})

httpd$add(name = "child_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("child_dependency", param)

	package = param[["package"]]
	page = param[["page"]]
	records_per_page = param[["records_per_page"]]
	if(is.null(page)) {
		page = 1
	} else {
		page = as.numeric(page)
	}
	if(is.null(records_per_page)) {
		records_per_page = 20
	} else {
		records_per_page = as.numeric(records_per_page)
	}
	child_dep_prioritize_reducible = param[["child_dep_prioritize_reducible"]]
	if(is.null(child_dep_prioritize_reducible)) {
		child_dep_prioritize_reducible = FALSE
	} else {
		if(child_dep_prioritize_reducible == "0") {
			child_dep_prioritize_reducible = FALSE
		} else {
			child_dep_prioritize_reducible = TRUE
		}
	}
	child_dep_internal_ordering = param[["child_dep_internal_ordering"]]
	if(is.null(child_dep_internal_ordering)) {
		child_dep_internal_ordering = FALSE
	} else {
		if(child_dep_internal_ordering == "0") {
			child_dep_internal_ordering = FALSE
		} else {
			child_dep_internal_ordering = TRUE
		}
	}

	response = Response$new()

	html_child_dependency(response, package = package, page = page, records_per_page = records_per_page,
		child_dep_prioritize_reducible = child_dep_prioritize_reducible, child_dep_internal_ordering = child_dep_internal_ordering)

	response$finish()
})

httpd$add(name = "global_heaviness_analysis",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("global_heaviness_analysis", param)

	response = Response$new()

	html_global_heaviness_analysis(response)

	response$finish()
})

httpd$add(name = "timeline",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("timeline", param)

	response = Response$new()

	html_timeline(response)

	response$finish()
})

httpd$add(name = "change_version",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("change_version", param)

	version = param[["version"]]
	pkgndep_opt$heaviness_db_version = version

	qqcat("- Load heaviness database version @{version}.\n")
	load_pkg_db(online = FALSE, verbose = FALSE)

	cat("- Load pre-calculated dependency results of all packages.\n")
	load_all_pkg_dep()


	cat("- Load pre-calculated heaviness metrics of all packages.\n")
	stat_df = load_pkg_stat_snapshot()

	cat("- Load DESCRIPTION and NAMESPACE of all packages.\n")
	load_pkg_description()
	load_pkg_namespace()

	load_heaviness_timeline()
	cat("- Load downstream dependency paths.\n")
	load_pkg_downstream_dependency_path_snapshot()

	v = stat_df$adjusted_max_heaviness_from_parents; v = v[v > 0]
	CUTOFF$adjusted_max_heaviness_from_parents = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)
	v = stat_df$adjusted_heaviness_on_children; v = v[v > 0]
	CUTOFF$adjusted_heaviness_on_children = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)
	v = stat_df$adjusted_heaviness_on_indirect_downstream; v = v[v > 0]
	CUTOFF$adjusted_heaviness_on_indirect_downstream = round(quantile(v, 1-min(25, length(v)*0.1)/length(v), na.rm = TRUE), 2)

	url = param[["url"]]
	if(is.null(url)) {
		url = "main"
	} else if(url == "") {
		url = "main"
	}

	response = Response$new()

	response$write(qq("<html>
  <head>
    <meta http-equiv='Refresh' content='0; url=@{url}' />
  </head>
</html>"))

	response$finish()

})


httpd$add(name = "show_namespace",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("show_namespace", param)

	response = Response$new()

	html_show_namespace(response, package = param[["package"]])

	response$finish()
})

httpd$add(name = "show_description",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("show_description", param)

	response = Response$new()

	html_show_description(response, package = param[["package"]])

	response$finish()
})

httpd$add(name = "compare_to_other_versions",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	request_log("compare_to_other_versions", param)

	response = Response$new()

	html_compare_to_other_versions(response, package = param[["package"]])

	response$finish()
})


address = qq("http://@{httpd$listenAddr}:@{httpd$listenPort}/custom/main")

cat("\n")
cat("Please copy and paste the link to your web browser.\n\n")
cat("  ", address)
cat("\n\n")
cat("Press Ctrl + C to exit.\n")

while(TRUE) Sys.sleep(300)
