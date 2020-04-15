

args = commandArgs(trailingOnly = TRUE)

pkg = args[1]

tmp_file = tempfile()
sink(tmp_file)
oe = try(suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE))), silent = TRUE)
sink()
unlink(tmp_file)

if(inherits(oe, "try-error")) {
	cat("\n")
} else {
	foo = sessionInfo()
	df1 = data.frame(pkg = foo$basePkgs, type = rep("basePkgs", length(foo$basePkgs)))
	df2 = data.frame(pkg = names(foo$loadedOnly), type = rep("loadedOnly", length(foo$loadedOnly)))
	df3 = data.frame(pkg = names(foo$otherPkgs), type = rep("otherPkgs", length(foo$otherPkgs)))
	df = rbind(df1, df2, df3)
	df = df[df[, 1] != pkg ,]
	print(df, row.names = FALSE)
}
