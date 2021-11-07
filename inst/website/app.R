
suppressPackageStartupMessages(library(pkgndep))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(brew))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))

load_all_pkg_dep()
load_pkg_db(snapshot = TRUE, verbose = FALSE)

source(system.file("website", "lib.R", package = "pkgndep"))
# source("~/project/development/pkgndep/inst/website/lib.R")
suppressPackageStartupMessages(library(Rook))

BASE_PKGS = pkgndep:::BASE_PKGS
DEFAULT_LOADED_BASE_PKGS = pkgndep:::DEFAULT_LOADED_BASE_PKGS
FIELDS = pkgndep:::FIELDS

env = new.env()
env$figure_dir = tempdir()


httpd = Rhttpd$new()
suppressMessages(httpd$start(quiet = TRUE))

httpd$add(app = File$new(system.file("www", "_js", package = "pkgndep")), name = 'js')
httpd$add(app = File$new(system.file("www", "_css", package = "pkgndep")), name = 'css')

# httpd$add(app = File$new("~/project/development/pkgndep/inst/www/_js"), name = 'js')
# httpd$add(app = File$new("~/project/development/pkgndep/inst/www/_css"), name = 'css')

httpd$add(name = "main",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	package = param[["package"]]
	order_by = param[["order_by"]]
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

	only_improvable = param[["improvable"]]
	if(is.null(only_improvable)) {
		only_improvable = FALSE
	} else {
		only_improvable = TRUE
	}

	response = Response$new()

	html_main_page(response, package = package, order_by = order_by, page = page, records_per_page = records_per_page, only_improvable = only_improvable)

	response$finish()
})

httpd$add(name = "package",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	package = param[["package"]]

	response = Response$new()

	html_single_package(response, package = package)

	response$finish()
})

httpd$add(name = "parent_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

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

	package = param[["package"]]
	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else {
		page = as.numeric(page)
	}

	response = Response$new()

	html_downstream_dependency(response, package = package, page = page)

	response$finish()
})

httpd$add(name = "children_dependency",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	package = param[["package"]]
	page = param[["page"]]
	if(is.null(page)) {
		page = 1
	} else {
		page = as.numeric(page)
	}

	response = Response$new()

	html_children_dependency(response, package = package, page = page)

	response$finish()
})

httpd$add(name = "global_heaviness_plot",
	app = function(env) {

	request = Request$new(env)
	param = request$GET()

	response = Response$new()

	html_global_heaviness_plot(response)

	response$finish()
})

address = qq("http://@{httpd$listenAddr}:@{httpd$listenPort}/custom/main")

cat("\n")
cat("Please copy and paste the link to your web browser.\n\n")
cat("  ", address)
cat("\n\n")
cat("Press Ctrl + C to exit.\n")

while(TRUE) Sys.sleep(300)
