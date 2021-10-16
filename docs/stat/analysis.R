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

	lt[[pkg]] = x

	if(i %% 1000 == 0) gc()
}

saveRDS(lt, file = "all_pkgs.rds", compress = "xz")


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

############## global dependency analysis ############


hm = do.call(rbind, lapply(lt, function(pkg) {
	v = heaviness(pkg)
	v = v[pkg$pkg_category %in% c("Imports", "Depends")]
	data.frame(pkg = rep(pkg$package, length(v)), depends = names(v), heaviness = v, stringsAsFactors = FALSE)
}))


ml = split(hm, hm$pkg)

gini_index = function(pkg, a = 2) {
	l = pkg$pkg_category %in% c("Imports", "Depends")
	if(sum(l) < 2) {
		0
	} else {
		DescTools::Gini(pkg$heaviness[l] + a)
	}
}


adjust_dep = function(package = NULL) {
	lt2 = lt
	# assign value
	for(i in seq_along(lt2)) {
		if(is.null(package)) { 
			pkg = lt2[[i]]
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
			x = rownames(m)

			if(length(x)) {
				v = heaviness(pkg, FALSE)
				v = v[x]
				move = rownames(m) == x[which.max(v)]
				qqcat("package '@{pkg$package}': move '@{x[which.max(v)]}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		} else {
			pkg = lt2[[i]]
			m = pkg$m[pkg$pkg_category %in% c("Imports", "Depends"), , drop = FALSE]
			x = rownames(m)

			if(package %in% x) {
				move = rownames(m) == package
				qqcat("package '@{pkg$package}': move '@{package}' to Suggesets\n")
				pkg$pkg_category[move] = "Suggests"
				lt2[[i]] = pkg
			}
		}
	}

	prev_hash = ""
	round = 0
	while(1) {
		round = round + 1
		# now adjust m and n_by_imports_suggests
		imp_lt = lapply(lt2, function(pkg) {
			loaded_namespaces(pkg)
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
					qqcat("round @{round} @{pkg$package}: removed @{sum(l)} namespaces for '@{nm}'\n")
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

	return(lt2)
}

total1 = sum(sapply(lt, function(x) x$n_by_depends_imports))

lt2 = adjust_dep("cowplot")

total2 = sum(sapply(lt2, function(x) x$n_by_depends_imports))

