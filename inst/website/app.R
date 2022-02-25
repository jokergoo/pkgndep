
cat("- Load packages for building website.\n")

suppressPackageStartupMessages(library(pkgndep))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(brew))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(grid))

cat("- Load package database.\n")
load_pkg_db(snapshot = TRUE, verbose = FALSE)

cat("- Load pre-calculated dependency results of all packages.\n")
load_all_pkg_dep()

source(system.file("website", "lib.R", package = "pkgndep"))
# source("~/project/development/pkgndep/inst/website/lib.R")
suppressPackageStartupMessages(library(Rook))

BASE_PKGS = pkgndep:::BASE_PKGS
DEFAULT_LOADED_BASE_PKGS = pkgndep:::DEFAULT_LOADED_BASE_PKGS
FIELDS = pkgndep:::FIELDS

env = new.env()
env$figure_dir = tempdir()

cat("- Generate heaviness plots.\n")
make_heaviness_plot()

cat("- Load website components.\n")
httpd = Rhttpd$new()
suppressMessages(httpd$start(quiet = TRUE))

if(identical(unname(Sys.info()[c("sysname", "user")]), c("Darwin", "guz"))) {
	httpd$add(app = File$new("~/project/development/pkgndep/inst/www/_js"), name = 'js')
	httpd$add(app = File$new("~/project/development/pkgndep/inst/www/_css"), name = 'css')
} else {
	httpd$add(app = File$new(system.file("www", "_js", package = "pkgndep")), name = 'js')
	httpd$add(app = File$new(system.file("www", "_css", package = "pkgndep")), name = 'css')
}

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
	} else {
		only_reducible = TRUE
	}

	exclude_children = param[["exclude_children"]]
	if(is.null(exclude_children)) {
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

address = qq("http://@{httpd$listenAddr}:@{httpd$listenPort}/custom/main")

cat("\n")
cat("Please copy and paste the link to your web browser.\n\n")
cat("  ", address)
cat("\n\n")
cat("Press Ctrl + C to exit.\n")

while(TRUE) Sys.sleep(300)
