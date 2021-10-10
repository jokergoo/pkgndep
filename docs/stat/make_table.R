library(pkgndep)
library(GetoptLong)

setwd("~/../development/pkgndep_analysis/")

all_pkg = installed.packages()[, 1]
all_pkg = sort(all_pkg)

lt = list()
for(i in seq_along(all_pkg)) {

	qqcat("\n++++++++++++++ @{i}/@{length(all_pkg)} ++++++++++++++\n")
	pkg = all_pkg[i]
	x = pkgndep(pkg)
	plot(x, file = qq("_image/@{pkg}.png"))

	lt[[i]] = x

	if(i %% 1000 == 0) gc()
}

saveRDS(lt, file = "all_pkgs.rds")


df = data.frame(
	package = sapply(lt, function(x) x$package),
	version = sapply(lt, function(x) x$version),
	bioc = sapply(lt, function(x) x$bioc),
	n_by_depends_imports = sapply(lt, function(x) x$n_by_depends_imports),
	n_by_all = sapply(lt, function(x) x$n_by_all),
	stringsAsFactors = FALSE
)

df = df[order(df$n_by_depends_imports, df$n_by_all, decreasing = TRUE), ]
write.csv(df, file = "r_pkg_dep.csv", row.names = FALSE)


for(i in seq_along(lt)) {
	pkg = lt[[i]]
	con = file(qq("_html/@{pkg$package}.html"), "w")
	writeLines(qq("<h3>Dependency heatmap for '@{pkg$package}'</h3>"), con = con)
	writeLines(qq("<p><img src='../_image/@{pkg$package}.png' width='75%' /></p>"), con = con)
	if(!is.null(pkg$df_imports)) {
		loaded_ns = apply(pkg$m, 1, function(x) sum(!is.na(x)))
		row_order = order(pkg$pkg_category, loaded_ns)
		tb = pkg$df_imports[row_order, , drop = FALSE]
		tb = cbind("Package"= rownames(tb), tb)
		tb[, 1] = qq("<a href='@{tb[, 1]}.html'>@{tb[, 1]}</a>", collapse = FALSE)

		tb = cbind(tb, "Loaded namespece" = loaded_ns[row_order], "Category" = pkg$pkg_category[row_order])
		writeLines(as.character(kable(tb, format = "html", row.names = FALSE, escape = FALSE)), con = con)
	}
	close(con)
}

lt$url = qq("[view](_image/@{lt$pkg}.png)", collapse = FALSE)
lt$url[!file.exists(qq("_image/@{lt$pkg}.png", collapse = FALSE))] = ""

url_prefix = ifelse(df$bioc, "https://CRAN.R-project.org/package=", "https://bioconductor.org/packages/")
df$package = paste("[", url_prefix, df$package, "](", df$package, ")", sep = "")
df$bioc = ifelse(df$bioc, "Bioc", "CRAN")

writeLines(c(
"In the following table, column **# Namespace** is the number of namespaces loaded into the fresh R session if packages in 'Depends' and 'Imports' fields are loaded.",
"Column **also include packages in Suggests** lists the number of namespaces where the packages in 'Suggests' are also loaded.",
"\n",
"The table can be downloaded as [a csv file](r_pkg_dep.csv). All packages are installed on 03 ~ 07 Oct, 2021",
"\n"
), con = "index.md")

sink("index.md", append = TRUE)
print(knitr::kable(df, row.names = FALSE,
	col.names = c("Package", "# Namespaces", "also load packages in Suggests", "Heatmap"),
	align = c("l", "r", "r", "r")))
sink()

library(markdown)
markdownToHTML("index.md", "index.html", stylesheet = "rep.css")

############## network analysis ############

n1 = sapply(lt, function(pkg) pkg$n_by_depends_imports)
names(n1) = sapply(lt, function(pkg) pkg$package0)

mm = do.call(rbind, lapply(lt, function(pkg) {
	m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
	x = setdiff(rownames(m), pkgndep:::BASE_PKGS)

	v1 = sum(apply(m, 2, function(x) any(!is.na(x))))
	v = numeric(0)
	for(i in seq_along(x)) {
		v2 = sum(apply(m[rownames(m) != x[i], , drop = FALSE], 2, function(x) any(!is.na(x))))
		v[i] = (v1 + 10)/(v2 + 10)
	}

	data.frame(pkg = rep(pkg$package, length(x)),
		       dep = x,
		       value = v)
}))


lt2 = lt
# assign value
for(i in seq_along(lt2)) {
	pkg = lt2[[i]]
	m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
	x = rownames(m)

	if(length(x)) {
		v1 = sum(apply(m, 2, function(x) any(!is.na(x))))
		v = numeric(0)
		for(k in seq_along(x)) {
			v2 = sum(apply(m[rownames(m) != x[k], , drop = FALSE], 2, function(x) any(!is.na(x))))
			v[k] = (v1 + 10)/(v2 + 10)
		}
		move = rownames(m) == x[which.max(v)]
		qqcat("package '@{pkg$package}': move @{x[which.max(v)]} to Suggesets\n")
		pkg$pkg_category[move] = "Suggests"
		lt2[[i]] = pkg
	}
}

prev_hash = ""
while(1) {
	# now adjust m and n_by_imports_suggests
	imp_lt = lapply(lt2, function(pkg) {
		m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
		colnames(m)[apply(m, 2, function(x) any(!is.na(x)))]
	})

	hash = digest::digest(imp_lt)
	if(prev_hash == hash) {
		break
	} else {
		prev_hash = hash
	}
	for(i in seq_along(lt2)) {
		pkg = lt2[[i]]
		m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]

		for(nm in rownames(m)) {
			l = colnames(pkg$m) %in% imp_lt[[nm]] & !is.na(pkg$m[nm, ])
			if(any(l)) {
				qqcat("@{pkg$package}: removed @{sum(l)} namespaces for '@{nm}'\n")
				pkg$m[nm, l] = NA
			}	
		}
		
		n_by_depends_imports = sum(apply(pkg$m[pkg$pkg_category %in% c("Depends", "Imports"), , drop = FALSE], 2, function(x) any(!is.na(x))))
		if(n_by_depends_imports != pkg$n_by_depends_imports) {
			qqcat("  - @{pkg$package}: n_by_depends_imports has been adjusted from @{pkg$n_by_depends_imports} to @{n_by_depends_imports}\n")
		}
		pkg$n_by_depends_imports = n_by_depends_imports
		lt2[[i]] = pkg
	}
}
