lt = list(pkg = NULL, n1 = NULL, n2 = NULL)

for(pkg in installed.packages()[, 1]) {
	cat("=====================\n")
	x = pkgndep(pkg)
	p = unavailable_pkg(x)
	if(length(p)) {
		BiocManager::install(p, update = FALSE, ask = FALSE)
	}
	x = pkgndep(pkg)
	pdf(NULL)
	size = plot(x)
	dev.off()

	if(!is.null(size)) {
		png(qq("/Users/jokergoo/project/pkgndep/docs/image/@{pkg}.png"), width = as.numeric(size[1]), height = as.numeric(size[2]), units = "mm", res = 150)
		plot(x)
		dev.off()
	}

	lt$pkg = c(lt$pkg, pkg)
	lt$n1 = c(lt$n1, x$n1)
	lt$n2 = c(lt$n2, x$n2)
}

lt$url = qq("[image](image/@{lt$pkg}.png)", collapse = FALSE)

df = as.data.frame(lt)
df = df[order(df$n2, decreasing = TRUE), ]

knitr::kable(df)