

# == title
# Loaded packages 
#
# == param
# -pkg A package name.
# -verbose Whether to print messages.
#
# == details
# It loads ``pkg`` into a new R session and collects which other packages are loaded by parsing the output from `utils::sessionInfo`.
#
# == value
# A data frame.
#
# == example
# loaded_packages("ComplexHeatmap")
loaded_packages = function(pkg, verbose = TRUE) {
	if(verbose) qqcat("Loading @{pkg} to a new R session... ")

	if(!requireNamespace("callr", quietly = TRUE)) {
		stop("You need to install 'callr' package.")
	}
		
	tb = callr::r(load_pkg, args = list(pkg = pkg), user_profile = FALSE)
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

load_pkg = function(pkg) {
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
