
dep = function(pkg, verbose = TRUE) {
	if(verbose) cat(silver("Loading"), green(pkg), silver("to a new R session... "))
		
	if(is.null(env$loaded_ns[[pkg]])) {
		
		tb = r(load_pkg, args = list(pkg = pkg), user_profile = FALSE)
		if(is.null(tb)) {
	    	if(verbose) cat(red(GetoptLong::qq("@{pkg} cannot be loaded.\n")))
	    	return(NULL)
	    } else {
	    	for(i in seq_len(ncol(tb))) {
	    		tb[, i] = as.vector(tb[, i])
	    	}
		    nr = nrow(tb)
		    if(verbose) cat(green(nr), silver(GetoptLong::qq("namespace@{ifelse(nr == 1, '', 's')} loaded.\n")))
		}
		env$loaded_ns[[pkg]] = tb
	} else {
		tb = env$loaded_ns[[pkg]]
		nr = nrow(tb)
		if(verbose) cat(green(nr), silver(GetoptLong::qq("namespace@{ifelse(nr == 1, '', 's')} loaded.\n")))
	}
    return(tb)
}

load_pkg = function(pkg) {
	tmp_file = tempfile()
	sink(tmp_file)
	oe = try(suppressWarnings(suppressPackageStartupMessages(tm <- system.time(library(pkg, character.only = TRUE)))), silent = TRUE)
	sink()
	unlink(tmp_file)

	# library(pkg) does not mean loading all packages in Depends/Imports
	# x = packageDescription(pkg)
	# if(is.null(x$Depends)) {
	# 	depends = character(0)
	# } else {
	# 	depends = x$Depends
	# 	depends = gsub("\\s*\\(.*?\\)", "", depends)
	# 	depends = strsplit(depends, "\\s*,\\s*")[[1]]
	# 	depends = depends[depends != "R"]
	# 	for(d in depends) {
	# 		library(d, character.only = TRUE)
	# 	}
	# }
	# if(is.null(x$Imports)) {
	# 	imports = character(0)
	# } else {
	# 	imports = x$Imports
	# 	imports = gsub("\\s*\\(.*?\\)", "", imports)
	# 	imports = strsplit(imports, "\\s*,\\s*")[[1]]
	# 	for(d in imports) {
	# 		library(d, character.only = TRUE)
	# 	}
	# }

	if(inherits(oe, "try-error")) {
		cat("\n")
	} else {
		foo = sessionInfo()
		df1 = data.frame(pkg = foo$basePkgs, type = rep("basePkgs", length(foo$basePkgs)))
		df2 = data.frame(pkg = names(foo$loadedOnly), type = rep("loadedOnly", length(foo$loadedOnly)))
		df3 = data.frame(pkg = names(foo$otherPkgs), type = rep("otherPkgs", length(foo$otherPkgs)))
		df = base::rbind(df1, df2, df3)
		df = df[df[, 1] != pkg ,]
		df$tm = tm[3]
		print(df, row.names = FALSE)
	}
}
