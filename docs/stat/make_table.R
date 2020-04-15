
if(normalizePath("~") == "/Users/jokergoo") {
	setwd("/Users/jokergoo/project/pkgndep/docs/stat")
} else {
	setwd("/desktop-home/guz/project/development/pkgndep/docs/stat")
}

lt = list(pkg = NULL, n1 = NULL, n2 = NULL)

for(pkg in installed.packages()[, 1]) {
	x = pkgndep(pkg)
	p = unavailable_pkg(x)
	# if(length(p)) {
	# 	BiocManager::install(p, update = FALSE, ask = FALSE)
	# 	x = pkgndep(pkg)
	# }
	pdf(NULL)
	size = plot(x)
	dev.off()

	if(!is.null(size)) {
		png(qq("image/@{pkg}.png"), width = as.numeric(size[1]), height = as.numeric(size[2]), units = "mm", res = 150)
		plot(x)
		dev.off()
	}

	lt$pkg = c(lt$pkg, pkg)
	lt$n1 = c(lt$n1, x$n1)
	lt$n2 = c(lt$n2, x$n2)
}

lt$url = qq("[view](image/@{lt$pkg}.png)", collapse = FALSE)
lt$url[!file.exists(qq("image/@{lt$pkg}.png", collapse = FALSE))] = ""

df = as.data.frame(lt)
df = df[order(df$n1, df$n2, decreasing = TRUE), ]

write.csv(df[, 1:3], file = "dep.csv", row.names = FALSE)

writeLines(c(
"This table shows the dependency analysis on the packages installed on my computer.",
"\n",
"In the following table, column **# Namespace** is the number of namespaces loaded into the fresh R session if packages in 'Depends' and 'Imports' fields are loaded.",
"Column **also include packages in Suggests** lists the number of namespaces where the packages in 'Suggests' are also loaded.",
"\n",
"The table can be downloaded as [a csv file](dep.csv).",
"\n"
), con = "index.md")

saveRDS(df, file = "dep.rds")
sink("index.md", append = TRUE)
print(knitr::kable(df, row.names = FALSE,
	col.names = c("Package", "# Namespaces", "also load packages in Suggests", "Heatmap"),
	align = c("l", "r", "r", "r")))
sink()

library(markdown)
markdownToHTML("index.md", "index.html", stylesheet = "rep.css")

