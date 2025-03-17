

# http://bioconductor.org/about/release-announcements/#release-versions
bioc_tb = read.table(textConnection(
"Release;Date;Software packages;R;URL
3.16;2022-11-02;2183;4.2.2;https://bioconductor.org/packages/3.16/
3.15;2022-04-27;2140;4.2.0;https://bioconductor.org/packages/3.15/
3.14;2021-10-27;2083;4.1.2;https://bioconductor.org/packages/3.14/
3.13;2021-05-20;2042;4.1.0;https://bioconductor.org/packages/3.13/
3.12;2020-10-28;1974;4.0.3;https://bioconductor.org/packages/3.12/
3.11;2020-04-28;1903;4.0.0;https://bioconductor.org/packages/3.11/
3.10;2019-10-30;1823;3.6.1;https://bioconductor.org/packages/3.10/
3.9;2019-05-03;1741;3.6.0;https://bioconductor.org/packages/3.9/
3.8;2018-10-31;1649;3.5.1;https://bioconductor.org/packages/3.8/
3.7;2018-05-01;1560;3.5.0;https://bioconductor.org/packages/3.7/
3.6;2017-10-31;1473;3.4.2;https://bioconductor.org/packages/3.6/
3.5;2017-04-25;1383;3.4.0;https://bioconductor.org/packages/3.5/
3.4;2016-10-18;1296;3.3.1;https://bioconductor.org/packages/3.4/
3.3;2016-05-04;1211;3.3.0;https://bioconductor.org/packages/3.3/
3.2;2015-10-14;1104;3.2.2;https://bioconductor.org/packages/3.2/
3.1;2015-04-17;1024;3.2.0;https://bioconductor.org/packages/3.1/
3.0;2014-10-14;934;3.1.1;https://bioconductor.org/packages/3.0/
2.14;2014-04-14;824;3.1.0;https://bioconductor.org/packages/2.14/
2.13;2013-10-15;749;3.0.2;https://bioconductor.org/packages/2.13/
2.12;2013-04-04;671;3.0.0;https://bioconductor.org/packages/2.12/
2.11;2012-10-03;610;2.15.1;https://bioconductor.org/packages/2.11/
2.10;2012-04-02;554;2.15.0;https://bioconductor.org/packages/2.10/
2.9;2011-11-01;517;2.14.0;https://bioconductor.org/packages/2.9/
2.8;2011-04-14;466;2.13.0;https://bioconductor.org/packages/2.8/
2.7;2010-10-18;418;2.12.0;https://bioconductor.org/packages/2.7/
2.6;2010-04-23;389;2.11.0;https://bioconductor.org/packages/2.6/
2.5;2009-10-28;352;2.10.0;https://bioconductor.org/packages/2.5/
2.4;2009-04-21;320;2.9.0;https://bioconductor.org/packages/2.4/BiocViews.html
2.3;2008-10-22;294;2.8.0;https://bioconductor.org/packages/2.3/BiocViews.html
2.2;2008-05-01;260;2.7.0;https://bioconductor.org/packages/2.2/BiocViews.html
2.1;2007-10-08;233;2.6.0;https://bioconductor.org/packages/2.1/BiocViews.html
2.0;2007-04-26;214;2.5.0;https://bioconductor.org/packages/2.0/BiocViews.html
1.9;2006-10-04;188;2.4.0;https://bioconductor.org/packages/1.9/BiocViews.html
1.8;2006-04-27;172;2.3.0;https://bioconductor.org/packages/1.8/BiocViews.html
1.7;2005-10-14;141;2.2.0;https://bioconductor.org/packages/bioc/1.7/src/contrib/html/
1.6;2005-05-18;123;2.1.0;https://bioconductor.org/packages/bioc/1.6/src/contrib/html/
1.5;2004-10-25;100;2.0.0;https://bioconductor.org/packages/bioc/1.5/src/contrib/html/
#1.4;2004-05-17;81;1.9;
#1.3;2003-10-30;49;1.8;
#1.2;2003-05-29;30;1.7;
#1.1;2002-11-19;20;1.6;
#1.0;2002-05-01;15;1.5;
"), header = TRUE, sep = ";", colClasses = rep("character", 5))



library(rversions)
all_rv = r_versions()
l = grepl("^\\d+\\.\\d+\\.\\d+$", all_rv$version)
all_rv$version[!l] = paste0(all_rv$version[!l], ".0")

all_rv$date = as.Date(all_rv$date)

all_df = readRDS("~/workspace/R_evolution/all_packages_df.rds")
all_df = as.data.frame(all_df)
all_df$Date = as.Date(all_df$Date)
all_df$Repository = ifelse(all_df$BiocVersion == "", "CRAN", "Bioconductor")
removed = readRDS("~/workspace/R_evolution/cran_removed.rds")

removed = lapply(removed, as.Date)

closest_pkg = function(package, date) {
	df = all_df[all_df$Package == package, , drop = FALSE]
	date = as.Date(date)
	diff = as.Date(df$Date) - date
	l = diff < 0
	df2 = df[l, , drop = FALSE]
	diff2 = diff[l]
	if(length(diff2)) {
		i = which.max(diff2)
		df2[i, , drop = FALSE]
	} else {
		l = diff > 0
		df2 = df[l, , drop = FALSE]
		diff2 = diff[l]
		if(length(diff2)) {
			i = which.min(diff2)
			df2[i, , drop = FALSE]
		} else {
			NULL
		}
	}
}

get_db = function(bioc_version) {
	date = bioc_tb$Date[bioc_tb$Release == bioc_version]
	date = as.Date(date)

	bioc_db = all_df[all_df$BiocVersion == bioc_version, , drop = FALSE]

	cran_db = all_df[all_df$BiocVersion == "", , drop = FALSE]
	cran_pkgs = unique(cran_db$Package)
	l = sapply(lt_duration[cran_pkgs], function(x) {
		date >= x[1] & date < x[2]  # left-closed right-open
	})

	cran_pkgs = cran_pkgs[l]

	cran_db = do.call(rbind, lapply(cran_pkgs, closest_pkg, date))

	cran_db = cran_db[!cran_db$Package %in% bioc_db$Package, ]

	rbind(cran_db, bioc_db)
}

package_duration_cran = function(package, all_df) {
	ind = which(all_df$Package == package)
	dd = as.Date(all_df$Date[ind])
	first_date = min(dd)

	if(is.null(removed[[package]])) {
		return(c(first_date, Sys.Date()))
	} else {
		if(removed[[package]] < max(dd)) {
			return(c(first_date, Sys.Date()))
		} else {
			return(c(first_date, removed[[package]]))
		}
	}
}

package_duration_bioc = function(package, all_df) {
	v = all_df$BiocVersion[all_df$Package == package]

	ind = which(bioc_tb$Release %in% v)

	ind2 = ind[1] - 1
	if(ind2 == 0) ind2 = 1

	ind = c(ind2, ind)
	dd = as.Date(bioc_tb$Date[ind])
	first_date = min(dd)
	last_date = max(dd)

	if(first_date == last_date) {
		first_date = first_date
		last_date = Sys.Date()
	}

	if(last_date == as.Date(bioc_tb$Date)[1]) {
		last_date = Sys.Date()
	}

	return(c(first_date, last_date))
}

package_duration = function(package) {
	cran_df = all_df[all_df$BiocVersion == "", ]
	bioc_df = all_df[all_df$BiocVersion != "", ]

	l1 = cran_df$Package == package
	l2 = bioc_df$Package == package

	if(any(l1) && any(l2)) {
		d1 = package_duration_cran(package, cran_df)
		d2 = package_duration_bioc(package, bioc_df)
		d = c(d1, d2)
		c(min(d), max(d))
	} else if(any(l1) && !any(l2)) {
		package_duration_cran(package, cran_df)
	} else if(!any(l1) && any(l2)) {
		package_duration_bioc(package, bioc_df)
	} else {
		stop("Cannot find", package)
	}
}

all_pkgs = unique(all_df$Package)
lt_duration = list()
for(i in seq_along(all_pkgs)) {
	pkg = all_pkgs[i]
	qqcat("- @{pkg}, @{i}/@{length(all_pkgs)}\n")
	lt_duration[[pkg]] = package_duration(pkg)
}

saveRDS(lt_duration, file = "~/workspace/R_evolution/lt_duration.rds")



get_db("1.5")
lt_ns = readRDS("~/workspace/R_evolution/all_packages_namespaces.rds")
lt_desc = readRDS("~/workspace/R_evolution/all_packages_description.rds")

for(k in seq_len(nrow(bioc_tb))) {
	version = bioc_tb$Release[k]
	qqcat(" - bioc @{version}\n")
	db = get_db(version)
	
	saveRDS(db, file = qq("~/project/development/pkgndep.github.io/@{bioc_tb$Date[k]}/pkg_df_@{version}.rds"))

	pkgs = ifelse(db[, "Repository"] == "CRAN", db[, "Package"], paste0(version, "/", db[, "Package"]))
	pkgs = paste0(pkgs, "_", db[, "Version"])

	pkgs = intersect(pkgs, names(lt_ns))

	saveRDS(lt_ns[pkgs], file = qq("~/project/development/pkgndep.github.io/@{bioc_tb$Date[k]}/pkg_namespace_@{version}.rds"))

	pkgs = ifelse(db[, "Repository"] == "CRAN", db[, "Package"], paste0(version, "/", db[, "Package"]))
	pkgs = paste0(pkgs, "_", db[, "Version"])

	pkgs = intersect(pkgs, names(lt_desc))

	saveRDS(lt_desc[pkgs], file = qq("~/project/development/pkgndep.github.io/@{bioc_tb$Date[k]}/pkg_description_@{version}.rds"))
}
