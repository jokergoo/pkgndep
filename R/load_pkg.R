

#' Loaded packages 
#'
#' @param pkg A package name.
#' @param verbose Whether to print messages.
#' 
#' @details
#' It loads ``pkg`` into a new R session and collects which other packages are 
#' loaded by parsing the output from `utils::sessionInfo`.
#'
#' @return
#' A data frame of dependency packages.
#' @export
#' 
#' @examples
#' loaded_packages("ComplexHeatmap")
loaded_packages = function(pkg, verbose = TRUE) {
	if(verbose) qqcat("Loading @{pkg} to a new R session... ")

	check_pkg("callr", bioc = FALSE)
		
	tb = callr::r(load_pkg_freshly, args = list(pkg = pkg), user_profile = FALSE)
	if(is.null(tb)) {
    	if(verbose) qqcat("@{pkg} cannot be loaded.\n")
    	return(NULL)
    } else {
    	for(i in seq_len(ncol(tb))) {
    		tb[, i] = as.vector(tb[, i])
    	}
	    nr = nrow(tb)
	    if(verbose) qqcat("@{nr} namespace@{ifelse(nr == 1, '', 's')} loaded.\n")
	}
	
    return(tb)
}

load_pkg_freshly = function(pkg) {
	tmp_file = tempfile()
	base::sink(tmp_file)
	oe = try(suppressWarnings(suppressPackageStartupMessages(tm <- system.time(library(pkg, character.only = TRUE)))), silent = TRUE)
	base::sink()
	base::unlink(tmp_file)

	if(inherits(oe, "try-error")) {
		cat("\n")
	} else {
		foo = utils::sessionInfo()
		df1 = data.frame(pkg = foo$basePkgs, type = rep("basePkgs", length(foo$basePkgs)))
		df2 = data.frame(pkg = names(foo$loadedOnly), type = rep("loadedOnly", length(foo$loadedOnly)))
		df3 = data.frame(pkg = names(foo$otherPkgs), type = rep("otherPkgs", length(foo$otherPkgs)))
		df = base::rbind(df1, df2, df3)
		df = df[df[, 1] != pkg ,]
		df$loading_time = tm[3]
		print(df, row.names = FALSE)
	}
}
