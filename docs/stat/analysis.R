library(pkgndep)
library(GetoptLong)

setwd("~/project/development/pkgndep_analysis/")

all_pkg = installed.packages()[, 1]
all_pkg = sort(all_pkg)

lt = list()
for(i in seq_along(all_pkg)) {

	qqcat("\n++++++++++++++ @{i}/@{length(all_pkg)} ++++++++++++++\n")
	pkg = all_pkg[i]
	x = pkgndep(pkg)
	plot(x, file = qq("_image/@{pkg}.svg"))

	lt[[pkg]] = x

	if(i %% 1000 == 0) gc()
}

saveRDS(lt, file = "all_pkgs.rds")


for(i in seq_along(lt)) {
	qqcat("\n++++++++++++++ @{i}/@{length(lt)} ++++++++++++++\n")
	pkg = lt[[i]]
	plot(pkg, file = qq("_image/@{pkg$package}.svg"))
}


# a global dependency network
nt = do.call(rbind, lapply(lt, function(pkg) {
	p = rownames(pkg$m)
	data.frame(pkg = rep(pkg$package, length(p)), dep = p, category = pkg$pkg_category, stringsAsFactors = FALSE)
}))

lt = lapply(lt, function(x) {
	x$gini_index = gini_index(x)
	x
})

## heaviness to children packages, heaviness to the ccomplete CRAN/Bioconductor

lt = lapply(lt, function(x) {
	tb = children_dependency(x)
	if(nrow(tb) == 0) {
		s = 0
	} else {
		s = mean(tb$heaviness)
	}
	x$heaviness_to_children = s
	x
})

#################### generate html #####################

for(i in seq_along(lt)) {
	qqcat("\n++++++++++++++ @{i}/@{length(lt)} ++++++++++++++\n")
	pkg = lt[[i]]
	html_single_package(pkg)
}


df = data.frame(
	package = sapply(lt, function(x) x$package),
	version = sapply(lt, function(x) x$version),
	repo = ifelse(sapply(lt, function(x) x$bioc), "Bioconductor", "CRAN"),
	n_dep = sapply(lt, function(x) sum(x$pkg_category %in% c("Depends", "Imports"))),
	n_by_depends_imports = sapply(lt, function(x) x$n_by_depends_imports),
	n_by_all = sapply(lt, function(x) x$n_by_all),
	gini = round(sapply(lt, gini_index), 3),
	stringsAsFactors = FALSE
)

df = df[order(df$n_by_depends_imports, df$n_by_all, decreasing = TRUE), ]
write.csv(df, file = "r_pkg_dep.csv", row.names = FALSE)


url_prefix = ifelse(df$repo == "CRAN", "https://CRAN.R-project.org/package=", "https://bioconductor.org/packages/")
df$repo = qq("<a href='@{url_prefix}@{df$package}' target='_blank'>@{df$repo}</a>", collapse = FALSE)

df$package = qq("<a href='_html/@{df$package}.html'>@{df$package}</a>", collapse = FALSE)

library(DT)

datatable(df, escape = FALSE, rownames = FALSE, colnames = c("Package", "Version", "Repository", "Dependencies", "Loaded namespaces", "Loaded namespeces (include 'Suggests')", "Gini index"))


total1 = sum(sapply(lt, function(x) x$n_by_depends_imports))

lt2 = adjust_dep("cowplot")

total2 = sum(sapply(lt2, function(x) x$n_by_depends_imports))

