
library(GetoptLong)

DATA_DIR = "~/workspace/R_evolution"


# http://bioconductor.org/about/release-announcements/#release-versions
tb = read.table(textConnection(
"Release;Date;Software packages;R;URL
3.17;2023-04-26;2183;4.3;https://bioconductor.org/packages/3.17/
3.16;2022-11-02;2183;4.2;https://bioconductor.org/packages/3.16/
3.15;2022-04-27;2140;4.2;https://bioconductor.org/packages/3.15/
3.14;2021-10-27;2083;4.1;https://bioconductor.org/packages/3.14/
3.13;2021-05-20;2042;4.1;https://bioconductor.org/packages/3.13/
3.12;2020-10-28;1974;4.0;https://bioconductor.org/packages/3.12/
3.11;2020-04-28;1903;4.0;https://bioconductor.org/packages/3.11/
3.10;2019-10-30;1823;3.6;https://bioconductor.org/packages/3.10/
3.9;2019-05-03;1741;3.6;https://bioconductor.org/packages/3.9/
3.8;2018-10-31;1649;3.5;https://bioconductor.org/packages/3.8/
3.7;2018-05-01;1560;3.5;https://bioconductor.org/packages/3.7/
3.6;2017-10-31;1473;3.4;https://bioconductor.org/packages/3.6/
3.5;2017-04-25;1383;3.4;https://bioconductor.org/packages/3.5/
3.4;2016-10-18;1296;3.3;https://bioconductor.org/packages/3.4/
3.3;2016-05-04;1211;3.3;https://bioconductor.org/packages/3.3/
3.2;2015-10-14;1104;3.2;https://bioconductor.org/packages/3.2/
3.1;2015-04-17;1024;3.2;https://bioconductor.org/packages/3.1/
3.0;2014-10-14;934;3.1;https://bioconductor.org/packages/3.0/
2.14;2014-04-14;824;3.1;https://bioconductor.org/packages/2.14/
2.13;2013-10-15;749;3.0;https://bioconductor.org/packages/2.13/
2.12;2013-04-04;671;3.0;https://bioconductor.org/packages/2.12/
2.11;2012-10-03;610;2.15;https://bioconductor.org/packages/2.11/
2.10;2012-04-02;554;2.15;https://bioconductor.org/packages/2.10/
2.9;2011-11-01;517;2.14;https://bioconductor.org/packages/2.9/
2.8;2011-04-14;466;2.13;https://bioconductor.org/packages/2.8/
2.7;2010-10-18;418;2.12;https://bioconductor.org/packages/2.7/
2.6;2010-04-23;389;2.11;https://bioconductor.org/packages/2.6/
2.5;2009-10-28;352;2.10;https://bioconductor.org/packages/2.5/
2.4;2009-04-21;320;2.9;https://bioconductor.org/packages/2.4/BiocViews.html
2.3;2008-10-22;294;2.8;https://bioconductor.org/packages/2.3/BiocViews.html
2.2;2008-05-01;260;2.7;https://bioconductor.org/packages/2.2/BiocViews.html
2.1;2007-10-08;233;2.6;https://bioconductor.org/packages/2.1/BiocViews.html
2.0;2007-04-26;214;2.5;https://bioconductor.org/packages/2.0/BiocViews.html
1.9;2006-10-04;188;2.4;https://bioconductor.org/packages/1.9/BiocViews.html
1.8;2006-04-27;172;2.3;https://bioconductor.org/packages/1.8/BiocViews.html
1.7;2005-10-14;141;2.2;https://bioconductor.org/packages/bioc/1.7/src/contrib/html/
1.6;2005-05-18;123;2.1;https://bioconductor.org/packages/bioc/1.6/src/contrib/html/
1.5;2004-10-25;100;2.0;https://bioconductor.org/packages/bioc/1.5/src/contrib/html/
#1.4;2004-05-17;81;1.9;
#1.3;2003-10-30;49;1.8;
#1.2;2003-05-29;30;1.7;
#1.1;2002-11-19;20;1.6;
#1.0;2002-05-01;15;1.5;
"), header = TRUE, sep = ";", colClasses = rep("character", 5))



wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/1.8/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/1.9/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.0/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.1/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.2/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.3/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.4/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.5/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.6/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.7/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.8/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/2.9/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.0/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.1/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.2/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.3/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.4/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.5/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.6/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.7/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.8/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.9/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.10/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.11/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.12/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.13/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.14/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.15/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.16/bioc/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.16/books/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.16/data/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.16/workflows/"

wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.17/bioc/src/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.17/books/src/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.17/data/annotation/src/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.17/data/experiment/src/"
wget -r -N --no-parent -nH --reject-regex='/bin/|/html/' "https://ftp.gwdg.de/pub/misc/bioconductor/packages/3.17/workflows/src/"


mirror = "https://bioconductor.org/"
# mirror = "http://bioconductor.statistik.tu-dortmund.de/"


## change the mirror
l = grepl("\\d/$", tb$URL)
l = 1:which(tb$Release == "2.8")
tb$URL[l] = gsub("https://bioconductor.org/", mirror, tb$URL[l])

library(rvest)

# very old bioc versions
list_bioc_old2 = function(url) {
	html = read_html(url)

	links = html %>% html_elements("table td a") %>% html_attr("href")
	paste0(url, links)
}

get_bioc_pkg_link_old2 = function(url) {
	html = readLines(url(url))
	html = paste(html, collapse = "\n")
	html = gsub("<doi:.*?>", "", html)
	html = read_html(html)
	
	links = html %>% html_elements("a") %>% html_attr("href")
	links = links[grepl("tar.gz$", links)]

	if(length(links)) {
		paste0(dirname(url), "/", gsub("\\s+", "", links))
	} else {
		NULL
	}
}

# old bioc versions
list_bioc_old = function(url) {
	if(!grepl("/2\\.0/", url)) {
		html = read_html(gsub("BiocViews.html", "Software.html", url))
		links1 = html %>% html_elements("table td a") %>% html_attr("href")

		html = read_html(gsub("BiocViews.html", "AnnotationData.html", url))
		links2 = html %>% html_elements("table td a") %>% html_attr("href")

		html = read_html(gsub("BiocViews.html", "ExperimentData.html", url))
		links3 = html %>% html_elements("table td a") %>% html_attr("href")

		c(links2, links3, links1)

	} else {
		ll = NULL
		for(sub in c("Microarray", "Annotation", "Visualization", "Statistics",
                     "GraphsAndNetworks", "Technology", "Infrastructure")) {
			html = read_html(gsub("BiocViews.html", qq("@{sub}.html"), url))
			ll = c(ll, html %>% html_elements("table td a") %>% html_attr("href"))
		}
		ll = unique(ll)

		html = read_html(gsub("BiocViews.html", "AnnotationData.html", url))
		links2 = html %>% html_elements("table td a") %>% html_attr("href")

		html = read_html(gsub("BiocViews.html", "ExperimentData.html", url))
		links3 = html %>% html_elements("table td a") %>% html_attr("href")

		c(links2, links3, ll)
	}
}

get_bioc_pkg_link_old = function(url) {
	html = readLines(url(url))
	html = paste(html, collapse = "\n")
	html = gsub("<doi:.*?>", "", html)
	html = read_html(html)
	
	links = html %>% html_elements("table.downloads td a") %>% html_attr("href")
	links = links[grepl("tar.gz$", links)]

	if(length(links)) {
		paste0(dirname(url), "/", gsub("\\s+", "", links))
	} else {
		NULL
	}
}

library(jsonlite)
list_bioc = function(version) {
	js = paste0(mirror, "/packages/json/", version, "/bioc/packages.js")
	code = readLines(url(js), warn = FALSE)
	code = gsub("var bioc_packages =", "", code)
	code = gsub(";$", "", code)
	all_pkg = fromJSON(code)[[1]][, 1]
	links1 = paste0(mirror, "/packages/", version, "/bioc/html/", all_pkg, ".html")

	js = paste0(mirror, "/packages/json/", version, "/data/annotation/packages.js")
	code = readLines(url(js), warn = FALSE)
	code = gsub("var data_annotation_packages =", "", code)
	code = gsub(";$", "", code)
	all_pkg = fromJSON(code)[[1]][, 1]
	links2 = paste0(mirror, "/packages/", version, "/data/annotation/html/", all_pkg, ".html")

	js = paste0(mirror, "/packages/json/", version, "/data/experiment/packages.js")
	code = readLines(url(js), warn = FALSE)
	code = gsub("var data_experiment_packages =", "", code)
	code = gsub(";$", "", code)
	all_pkg = fromJSON(code)[[1]][, 1]
	links3 = paste0(mirror, "/packages/", version, "/data/experiment/html/", all_pkg, ".html")

	c(links1, links2, links3)
}

get_bioc_pkg_link = function(url) {
	html = readLines(url(url))
	html = paste(html, collapse = "\n")
	html = gsub("<doi:.*?>", "", html)
	html = read_html(html)
	links = html %>% html_elements("td a") %>% html_attr("href")
	links = links[grepl("tar.gz$", links)]

	if(length(links)) {
		paste0(dirname(url), "/", gsub("\\s+", "", links))
	} else {
		NULL
	}
}

already_downloaded = function(url, all_files, version) {
	pkg = gsub("\\.html$", "", basename(url))
	ind = grep(paste0("^", pkg, "_\\d"), all_files)

	if(length(ind) > 1) {
		print(paste0(version, "/", all_files[ind]))
		file.remove(paste0(version, "/", all_files[ind]))
		return(FALSE)
	}

	if(length(ind) == 0) {
		return(FALSE)
	} else if(grepl("DESCRIPTION", all_files[ind])) {
		return(F)
	} else {
		fn = paste0(version, "/", all_files[ind])
		return(file.exists(fn))
		try(code <- system(qq("gunzip -t @{fn}"), ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
		if(code != 0) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	}
}

setwd("/Volumes/One Touch/bioc_mirror/packages")

successful = list()
all = list()

library(GetoptLong)

options("timeout" = 9999999)

n = nrow(tb)

for(i in n:1) {
	version = as.character(tb[i, "Release"])
	dir.create(version)

	all_files = dir(version)

	if(grepl("contrib/html/$", tb[i, "URL"])) {
		package_links = list_bioc_old2(tb[i, "URL"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			all[[package]] = TRUE
			if(is.null(successful[[package]])) {
				successful[[package]] = FALSE
			}
			if(already_downloaded(package, all_files, version)) {
				successful[[package]] = TRUE
				qqcat("[EXISTED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
			} else {
				link = get_bioc_pkg_link_old2(package)
				if(length(link)) {
					pkg_fn = basename(link)
					oe = try({
						download.file(link, dest = paste0(version, "/", pkg_fn), quiet = FALSE)
					}, silent = TRUE)
					if(inherits(oe, "try-error")) {
						file.remove(paste0(version, "/", pkg_fn))
						qqcat("[FAILED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					} else {
						successful[[package]] = TRUE
						qqcat("[SUCCESSFUL] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					}
				}
			}
		}
	} else if(grepl("BiocViews.html$", tb[i, "URL"])) {
		package_links = list_bioc_old(tb[i, "URL"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			all[[package]] = TRUE
			if(is.null(successful[[package]])) {
				successful[[package]] = FALSE
			}
			if(already_downloaded(package, all_files, version)) {
				successful[[package]] = TRUE
				qqcat("[EXISTED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
			} else {
				link = get_bioc_pkg_link_old(package)
				if(length(link)) {
					pkg_fn = basename(link)
					oe = try({
						download.file(link, dest = paste0(version, "/", pkg_fn), quiet = FALSE)
					}, silent = TRUE)
					if(inherits(oe, "try-error")) {
						file.remove(paste0(version, "/", pkg_fn))
						qqcat("[FAILED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					} else {
						successful[[package]] = TRUE
						qqcat("[SUCCESSFUL] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					}
				}
			}
		}
	} else {
		package_links = list_bioc(tb[i, "Release"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			all[[package]] = TRUE
			if(is.null(successful[[package]])) {
				successful[[package]] = FALSE
			}
			if(already_downloaded(package, all_files, version)) {
				successful[[package]] = TRUE
				qqcat("[EXISTED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
			} else {
				link = get_bioc_pkg_link(package)
				if(length(link)) {
					pkg_fn = basename(link)
					oe = try({
						download.file(link, dest = paste0(version, "/", pkg_fn), quiet = FALSE)
					}, silent = TRUE)
					if(inherits(oe, "try-error")) {
						file.remove(paste0(version, "/", pkg_fn))
						qqcat("[FAILED] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					} else {
						successful[[package]] = TRUE
						qqcat("[SUCCESSFUL] @{version}: @{k}/@{length(package_links)} @{basename(package)}\n")
					}
				}
			}
		}
	}
}

setdiff(names(all), names(successful))


# ### remove large .tar.gz files, only keep the DESCRIPTION file
# ### current no need to do that because we have enough disk space
# library(archive)
# setwd("/Volumes/One Touch/bioc_mirror/packages")
# all_versions = dir()

# cutoff = 1024*1024*50
# for(v in all_versions)  {

# 	df = read.table(pipe(qq('ls -l @{v}')), skip = 1)
# 	df = df[, c(5, 9)]
# 	colnames(df) = c("size", "file")

# 	l = df$size > cutoff
# 	if(any(l)) {
# 		for(i in which(l)) {
# 			f = df$file[i]
# 			fo = df$size[i]
# 			qqcat("- @{v}/@{f}, filesize = @{round(fo/1024/1024)}MB\n")

# 			ff = qq("@{v}/@{f}")
# 			ff2 = gsub("\\.tar.gz$", ".DESCRIPTION", ff)
# 			pkg = gsub("_.*$", "", f)
# 			writeLines(readLines(archive_read(ff, file = qq("@{pkg}/DESCRIPTION"))), con = ff2)
# 			file.remove(ff)
# 		}
# 	}
# }


# URL of bioc packages. Recent versions of bioc have formatted URL, but not for the older versions.
# So it is mainly for the older versions
setwd("/Volumes/One Touch/bioc_mirror/packages")

all_bioc_links = list()

n = nrow(tb)

for(i in n:1) {
	version = as.character(tb[i, "Release"])

	if(grepl("contrib/html/$", tb[i, "URL"])) {
		package_links = list_bioc_old2(tb[i, "URL"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			qqcat("- @{version}/@{package}\n")
			link = get_bioc_pkg_link_old2(package)
			if(length(link)) {
				all_bioc_links[[paste0(version, "/", package)]] = link
			}
		}
	} else if(grepl("BiocViews.html$", tb[i, "URL"])) {
		package_links = list_bioc_old(tb[i, "URL"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			qqcat("- @{version}/@{package}\n")
			link = get_bioc_pkg_link_old(package)
			if(length(link)) {
				all_bioc_links[[paste0(version, "/", package)]] = link
			}
		}
	} else {
		package_links = list_bioc(tb[i, "Release"])

		for(k in seq_along(package_links)) {
			package = package_links[k]
			qqcat("- @{version}/@{package}\n")
			link = get_bioc_pkg_link(package)
			if(length(link)) {
				all_bioc_links[[paste0(version, "/", package)]] = link
			}
		}
	}
}

saveRDS(all_bioc_links, file = qq("@{DATA_DIR}/all_bioc_links.rds"))

