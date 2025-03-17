

library(archive)
library(GetoptLong)

DATA_DIR = "~/workspace/R_evolution"

setwd("/Volumes/One Touch/cran_mirror/src/contrib/Archive")




### cran
lt1 = list()
all_pkgs = dir()

failed = list()
for(i in seq_along(all_pkgs)) {
	pkg = all_pkgs[i]
	files = dir(path = pkg)
	files = files[grep("gz$", files)]
	for(f in files) {
		qqcat("- [@{i}/@{length(all_pkgs)}] @{pkg}/@{f}...\n")
		pkg2 = gsub("_.*$", "", f)  # e.g. ARES -> ares
		oe = try(lt1[[f]] <- read.dcf(archive_read(qq("@{pkg}/@{f}"), file = qq("@{pkg2}/DESCRIPTION"))))
		if(inherits(oe, "try-error")) {
			failed[[f]] = TRUE
		} else {
			if("Package" %in% colnames(lt1[[f]])) {
				if(pkg2 != lt1[[f]][, "Package"]) {
					lt1[[f]] = NULL
				}
			} else if("Bundle" %in% colnames(lt1[[f]])) {
				if(pkg2 != lt1[[f]][, "Bundle"]) {
					lt1[[f]] = NULL
				}
			}
		}
	}
}

## base packages
setwd("/Volumes/One Touch/cran_mirror/src/base")

library(rversions)
all_rv = r_versions()
l = grepl("^\\d+\\.\\d+\\.\\d+$", all_rv$version)
all_rv$version[!l] = paste0(all_rv$version[!l], ".0")

all_versions = dir()
all_versions = setdiff(all_versions, c("R-0", "R-1"))  # R-0 has no DESCRIPTION file

for(v in all_versions) {
	files = dir(path = v)
	files = files[!grepl("revised|-w|recommended", files)]
	files = files[grepl("z$", files)]

	for(f in files) {
		rv = gsub("^R-(.*?)\\.(tar\\.gz|tgz)$", "\\1", f)
		if(!grepl("\\d$", rv)) {
			next
		}

		ftb = archive(qq("@{v}/@{f}"))
		core_pkg = ftb$path[ grep("src/library/.*/DESCRIPTION.*", ftb$path)]
		for(df in core_pkg) {
			qqcat("- @{df}...\n")
			pkg = gsub("^.*/library/(.*)/DESCRIPTION.*$", "\\1", df)
			lt1[[qq("@{pkg}_@{rv}")]] = read.dcf(archive_read(qq("@{v}/@{f}"), file = df))
			lt1[[qq("@{pkg}_@{rv}")]][, "Version"] = rv
			lt1[[qq("@{pkg}_@{rv}")]] = cbind(lt1[[qq("@{pkg}_@{rv}")]], Date = as.character(all_rv$date[all_rv$version == rv]))
		}
	}
}


## bioconductor packages
setwd("/Volumes/One Touch/bioc_mirror/packages")

lt2 = list()
all_versions = dir()

for(v in all_versions) {
	files = dir(path = v)
	for(i in seq_along(files)) {
		f = files[i]
		nm = paste0(v, "/", f)
		pkg = gsub("_.*$", "", f)
		qqcat("- [@{i}/@{length(files)}] @{v}/@{f}...\n")
		if(grepl("\\.tar\\.gz$", f)) {
			lt2[[nm]] = read.dcf(archive_read(qq("@{v}/@{f}"), file = qq("@{pkg}/DESCRIPTION")))
		} else {
			lt2[[nm]] = read.dcf(qq("@{v}/@{f}"))
		}
		if(nrow(lt2[[nm]]) > 1) {
			lt2[[nm]] = rbind(apply(lt2[[nm]], 2, function(x) {
				ind = which(!is.na(x))
				if(length(ind)) {
					x[ind[1]]
				} else {
					""
				}
			}))
		}
		lt2[[nm]] = cbind(lt2[[nm]], "BiocVersion" = v)
	}
}


lt = c(lt1, lt2)

names(lt) = gsub("\\.(tar.gz|DESCRIPTION)$", "", names(lt))

saveRDS(lt, file = qq("@{DATA_DIR}/all_packages_dcf.rds"))

### convert to a data frame

unique(unlist(lapply(lt, colnames)))

fields = c("Package", "Bundle",
	"Title", "Version", 
	"Date", "Date/Publication", "Packaged", "Created",
	"Depends", "%Depends", "Dependencies",
	"Imports", "Import", "import", 
	"Suggests", "SUGGESTS", "Suggest", "Suggets",
	"LinkingTo", "LinkingdTo",
	"Enhances", "Enhancements", 
	"BiocVersion")
nr = length(lt)

df = do.call(cbind, lapply(fields, function(x) character(nr)))
colnames(df) = fields


for(i in seq_along(lt)) {
	cat(strrep("\b", 100))
	qqcat("@{i}/@{nr}")
	f =  intersect(fields, colnames(lt[[i]]))
	df[i, f] = lt[[i]][1, f]
}
cat("\n")


df2 = df

df2[, "Package"] = apply(df[, c("Package", "Bundle")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		x = gsub(":\\s+.*$", "", x)
		x
	} else {
		""
	}
})

which(df2[, "Package"] == "")

df2[, "Date"] = apply(df[, c("Date", "Date/Publication", "Packaged", "Created")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		x[1]
	} else {
		""
	}
})

bioc_tb = tb
library(rvest)
get_time = function(x) {

	if("Package" %in% colnames(x)) {
		pkg = x[, "Package"]
	} else {
		pkg = x[, "Bundle"]
	}
	pkg = gsub(":\\s+.*$", "", pkg)
	version = x[, "Version"]
	version = gsub("\\s+.*$", "", version)
	if("BiocVersion" %in% colnames(x)) {
		i = bioc_tb$Release == x[, "BiocVersion"]
		bioc_tb$Date[i]
	} else {
		version = gsub("\\s+.*$", "", version)
		qqcat("- @{pkg}, @{version}\n")
		oe = try(html <- read_html(qq("https://cran.r-project.org/src/contrib/Archive/@{pkg}/")))
		if(inherits(oe, "try-error")) {
			return("")
		}
		tb = html %>% html_element("table") %>% html_table() %>% as.data.frame()
		tb = tb[-2, , drop = FALSE]
		ind = which(tb[, 2] == qq("@{pkg}_@{version}.tar.gz"))
		if(length(ind)) {
			res = tb[ind-1, 3]
			if(res == "") {
				tb[ind, 3]
			} else {
				res
			}
		} else {
			""
		}
	}
}

ind = which(df2[, "Date"] == "")
for(i in ind) {
	df2[i, "Date"] = get_time(lt[[i]])
}


df2[, "Depends"] = apply(df[, c("Depends", "%Depends", "Dependencies")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
		
	} else {
		""
	}
})

df2[, "Imports"] = apply(df[, c("Imports", "Import", "import")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
		
	} else {
		""
	}
})

df2[, "Suggests"] = apply(df[, c("Suggests", "SUGGESTS", "Suggest", "Suggets")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
		
	} else {
		""
	}
})

df2[, "LinkingTo"] = apply(df[, c("LinkingTo", "LinkingdTo")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
		
	} else {
		""
	}
})

df2[, "Enhances"] = apply(df[, c("Enhances", "Enhancements")], 1, function(x) {
	x = x[x!=""]
	if(length(x)) {
		paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
		
	} else {
		""
	}
})

df2[, "Date"] = gsub("^(\\d+-\\d+-\\d+).*$", "\\1", df2[, "Date"])
df2[, "Date"] = gsub("^.*(\\d{4}-\\d{1,2}-\\d{1,2}).*$", "\\1", df2[, "Date"])

# remove weekdays, remove hour:min:sec
df2[, "Date"] = gsub("Mon|Tue|Wed|Thu|Fri|Sat|Sun|rs", "", df2[, "Date"])
df2[, "Date"] = gsub("\\d+:\\d+:\\d+", "", df2[, "Date"])

# remove those only having years
l = grepl("^\\d+$", df2[, "Date"])
df2[l, "Date"] = ""

## remove ;xxx
df2[, "Date"] = gsub("(\\d+); \\w+$", "\\1", df2[, "Date"])
## remove EDT 2010
df2[, "Date"] = gsub("[A-Z]+ (\\d+)$", "\\1", df2[, "Date"])


library(anytime)
### month day 2010
l = grepl("[a-zA-Z]{3} \\d+.*\\d{4}", df2[, "Date"])
t = as.character(anytime(df2[l, "Date"]))
l2 = !is.na(t)
df2[l, "Date"][l2] = t[l2]
df2[, "Date"] = gsub("^(\\d+-\\d+-\\d+).*$", "\\1", df2[, "Date"])

# 2000/10/10
df2[, "Date"] = gsub("^(\\d{4})/(\\d+)/(\\d+)$", "\\1-\\2-\\3", df2[, "Date"])
# 2000.10/10
df2[, "Date"] = gsub("^(\\d{4})\\.(\\d+)\\.(\\d+)$", "\\1-\\2-\\3", df2[, "Date"])
# 10/10/2000
df2[, "Date"] = gsub("^(\\d{1,2})/(\\d{1,2})/(\\d{4})$", "\\3-\\2-\\1", df2[, "Date"])


df2[, "Date"] = gsub("^(\\d{4})-(\\d{1})-(\\d{1})$", "\\1-0\\2-0\\3", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{4})-(\\d{2}) -(\\d{2})$", "\\1-\\2-\\3", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{4})-(\\d{2})-(\\d{1})$", "\\1-\\2-0\\3", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{4})-(\\d{1})-(\\d{2})$", "\\1-0\\2-\\3", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{2})-(\\d{2})-(\\d{4})$", "\\3-\\2-\\1", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{2})\\.(\\d{2})\\.(\\d{4})$", "\\3-\\2-\\1", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{2})\\.(\\d{1})\\.(\\d{4})\\.$", "\\3-0\\2-\\1", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d{4})-(\\d{2})-$", "\\1-\\2-01", df2[, "Date"])

df2[, "Date"] = gsub("^(\\d{4})-(\\d)$", "\\1-0\\2-01", df2[, "Date"])
df2[, "Date"] = gsub("^(\\d+) (\\d{4})$", "\\2-\\1-01", df2[, "Date"])

l = grepl("^\\d+-\\d+-\\d+$", df2[, "Date"])
df2[!l, "Date"] = gsub("^.*(\\d+-\\d+-\\d+).*$", "\\1", df2[!l, "Date"])

l = grepl("^\\d{4}-\\d{2}$", df2[, "Date"])
df2[l, "Date"] = paste0(df2[l, "Date"], "-01")


df2[, "Date"] = gsub("Octobre", "Oct", df2[, "Date"])
df2[, "Date"] = gsub("Mai", "May", df2[, "Date"])
df2[, "Date"] = gsub("Maerz", "March", df2[, "Date"])
df2[, "Date"] = gsub("Juni", "Jun", df2[, "Date"])
df2[, "Date"] = gsub("Februar", "Feb", df2[, "Date"])
df2[, "Date"] = gsub("Januar", "Jan", df2[, "Date"])
df2[, "Date"] = gsub("Oktober", "Oct", df2[, "Date"])

# 01 Mar 2000
l = grepl("\\d+(\\.)? [a-zA-Z]+\\.? \\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(df2[l, "Date"]))


## Mar 01 2000
df2[, "Date"] = gsub("Febraury", "Feb", df2[, "Date"])
df2[, "Date"] = gsub("Fabruary", "Feb", df2[, "Date"])
df2[, "Date"] = gsub("Novemeber", "Nov", df2[, "Date"])
l = grepl("[a-zA-Z]+\\.? *\\d+,? *\\d{4}", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(df2[l, "Date"]))

## 2000--01--01
l = grepl("^\\d{4}--\\d{2}--\\d{2}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d{4})--(\\d{2})--(\\d{2})$", "\\1-\\2-\\3", df2[l, "Date"])
l = grepl("^\\d{4}--\\d{1}--\\d{2}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d{4})--(\\d{1})--(\\d{2})$", "\\1-0\\2-\\3", df2[l, "Date"])
l = grepl("^\\d{4}--\\d{2}--\\d{1}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d{4})--(\\d{2})--(\\d{1})$", "\\1-0\\2-0\\3", df2[l, "Date"])

## 2000-0101
l = grepl("^\\d{4}-\\d{4}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d{4})-(\\d\\d)(\\d\\d)$", "\\1-\\2-\\3", df2[l, "Date"])


## sep 2020
df2[df2[, "Date"] == "Sept. 2002", "Date"] = "Sep. 2002"
l = grepl("^[a-zA-Z]+(\\.|,)? +\\d{4}$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(paste0("01 ", df2[l, "Date"])))

## 4.2024
l = grepl("^\\d+\\.\\d{4}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d+)\\.(\\d{4})$", "\\2-\\1-01", df2[l, "Date"])

## 2020-Oct-19
df2[df2[, "Date"] == "2012-Dic-17", "Date"] = "2012-Dec-17"
df2[, "Date"] = gsub("Jany", "Jan", df2[, "Date"])
df2[, "Date"] = gsub("Feby", "Feb", df2[, "Date"])
l = grepl("^\\d{4}-[a-zA-Z]+-\\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(df2[l, "Date"]))


## 14NOV06
l = grepl("^\\d+[A-Z]+\\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^(\\d+)([A-Z]+)(\\d+)$", "\\1 \\2 20\\3", df2[l, "Date"])))
l = grepl("^\\d+[a-zA-Z]+\\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^(\\d+)([a-zA-Z]+)(\\d+)$", "\\1 \\2 20\\3", df2[l, "Date"])))

## 2004/JUL/20
l = grepl("^\\d+/[a-zA-Z]+/\\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^(\\d+)/([a-zA-Z]+)/(\\d+)$", "\\3 \\2 \\1", df2[l, "Date"])))


## 2020-10199
l = grepl("^\\d{4}-\\d{5}$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d{4})-(\\d\\d)(\\d\\d)\\d$", "\\1-\\2-\\3", df2[l, "Date"])

## 31 Mar 2014 xxx
l = grepl("\\d+ [a-zA-Z]{3,} \\d{4}.+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^\\W*(\\d+ [a-zA-Z]{3,} \\d{4})(.+)$", "\\1", df2[l, "Date"])))



## 2009-01.30
l = grepl("^\\d+-\\d+\\.\\d+$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d+)-(\\d+)\\.(\\d+)$", "\\1-\\2-\\3", df2[l, "Date"])


### "$Date: 2004/06/03  $"
l = grepl(" \\d{4}/\\d{2}/\\d{2} +", df2[, "Date"])
df2[l, "Date"] = gsub("^.*: (\\d{4})/(\\d{2})/(\\d{2}) +\\$$", "\\1-\\2-\\3", df2[l, "Date"])


## 28-July-2011
l = grep("^\\d+-[a-zA-Z]+-\\d{4}$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("([a-zA-Z]{3})[a-zA-Z]*", "\\1", df2[l, "Date"])))

## 28-July-2011
l = grepl("^\\d+-[a-zA-Z]+-\\d{2}$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("-(\\d\\d)$", "-20\\1", df2[l, "Date"])))

# 09.09.09
l = grepl("^\\d\\d(\\.|-|/)\\d\\d(\\.|-|/)\\d\\d$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d\\d)(\\.|-|/)(\\d\\d)(\\.|-|/)(\\d\\d)$", "20\\5-\\3-\\1", df2[l, "Date"])


## October, 22 2014
l = grepl("^[a-zA-Z]+,? \\d+,? \\d+", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^([a-zA-Z]{3})[a-zA-Z]*", "\\1", df2[l, "Date"])))


# Jan 25, 2005
df2[ df2[, "Date"] == "Fabruary 21, 2012", "Date"] = "Feb 21, 2012"
l = grepl("^[a-zA-Z]+ \\d+ ?,? \\d+", df2[, "Date"])
df2[l, "Date"] = gsub(" ,", ",", df2[l, "Date"])
df2[l, "Date"] = as.character(anytime(gsub("^([a-zA-Z]{3})[a-zA-Z]*", "\\1", df2[l, "Date"])))


#Dec-12, 2003
l = grepl("^[a-zA-Z]+-\\d+, \\d+$", df2[, "Date"])
df2[l, "Date"] = as.character(anytime(df2[l, "Date"]))

# 2001/09
l = grepl("^\\d+(/|\\.)\\d+$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d+)(/|\\.)(\\d+)$", "\\1-\\3-01", df2[l, "Date"])

# 09/2001
l = grepl("^\\d+(/|\\.)\\d+$", df2[, "Date"])
df2[l, "Date"] = gsub("^(\\d+)(/|\\.)(\\d+)$", "\\3-\\1-01", df2[l, "Date"])


l = grepl("^\\d+ -\\d+-\\d+$", df2[, "Date"])
df2[l, "Date"] = gsub(" ", "", df2[l, "Date"])


##
l = !grepl("^\\d+-\\d+-\\d+$", df2[, "Date"])
ind = which(l)
for(i in ind) {
	df2[i, "Date"] = get_time(lt[[i]])
}
df2[, "Date"] = gsub("^(\\d+-\\d+-\\d+).*$", "\\1", df2[, "Date"])


l = is.na(as.Date(df2[, "Date"]))

## 20010
df2[l, "Date"] = gsub("200(\\d\\d)", "20\\1", df2[l, "Date"])
# 2-21-12
df2[l, "Date"] = gsub("^(\\d{1,2})-(\\d{1,2})-(\\d{1,2})$", "20\\3-\\1-\\2", df2[l ,"Date"])
# "2016-010-04"
df2[l, "Date"] = gsub("^(\\d{4})-0(\\d\\d)-(\\d{2})$", "\\1-\\2-\\3", df2[l ,"Date"])
# "7-2013-01"
df2[l, "Date"] = gsub("^(\\d+)-(\\d{4})-(\\d+)$", "\\2-\\3-\\1", df2[l, "Date"])
# "1-16-2015"
df2[l, "Date"] = gsub("^(\\d{1,2})-(\\d{1,2})-(\\d{4})$", "\\3-\\1-\\2", df2[l, "Date"])
# "2012-221-11"
df2[l, "Date"] = gsub("^(\\d{4})-\\d(\\d\\d)-(\\d{2})$", "\\1-\\2-\\3", df2[l ,"Date"])

l = is.na(as.Date(df2[, "Date"]))
df2[l, "Date"] = gsub("^(\\d{4})-(\\d{1,2})-(\\d{1,2})$", "\\1-\\3-\\2", df2[l, "Date"])

l = is.na(as.Date(df2[, "Date"]))
ind = which(l)
for(i in ind) {
	df2[i, "Date"] = get_time(lt[[i]])
}
df2[, "Date"] = gsub("^(\\d+-\\d+-\\d+).*$", "\\1", df2[, "Date"])


l = !grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", df2[, "Date"]) 
ind = which(l)
for(i in ind) {
	df2[i, "Date"] = get_time(lt[[i]])
}
df2[, "Date"] = gsub("^(\\d+-\\d+-\\d+).*$", "\\1", df2[, "Date"])

l = grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", df2[, "Date"]) & !is.na(as.Date(df2[, "Date"]))
df2 = df2[l, ]

df2[, "Date"] = gsub("^00(\\d\\d-)", "20\\1", df2[, "Date"])

### clean version 
df2[, "Version"] = gsub(" \\(.*\\)$", "", df2[, "Version"])
df2[, "Version"] = gsub("R", "", df2[, "Version"])
l = grepl("^\\d+[-_.](\\d+[-_.])*\\d+$", df2[, "Version"])


df2 = df2[, c("Package", "Title", "Version", 
	"Date", 
	"Depends", "Imports", "Suggests", "LinkingTo",
	"Enhances",
	"BiocVersion")]
df2 = cbind(df2, Repository = ifelse(df2[, "BiocVersion"] == "", "CRAN", "Bioconductor"))

df2[, "Depends"] = gsub("requires rmutil library", "rmutil", df2[, "Depends"])

saveRDS(df2, file = qq("@{DATA_DIR}/all_packages_df.rds"))
write.csv(df2, file = qq("@{DATA_DIR}/all_packages_df.csv"), row.names = FALSE)



#####################################################
##### all packages' namespace file

setwd("/Volumes/One Touch/cran_mirror/src/contrib/Archive")

### cran
lt1 = list()
all_pkgs = dir()

failed = list()
for(i in seq_along(all_pkgs)) {
	pkg = all_pkgs[i]
	files = dir(path = pkg)
	files = files[grep("gz$", files)]
	for(f in files) {
		qqcat("- [@{i}/@{length(all_pkgs)}] @{pkg}/@{f}...\n")
		pkg2 = gsub("_.*$", "", f)  # e.g. ARES -> ares
		oe = try(lt1[[f]] <- readLines(archive_read(qq("@{pkg}/@{f}"), file = qq("@{pkg2}/NAMESPACE"))))
		if(inherits(oe, "try-error")) {
			failed[[f]] = TRUE
		}
	}
}


## bioconductor packages
setwd("/Volumes/One Touch/bioc_mirror/packages")

lt2 = list()
all_versions = dir()

for(v in all_versions) {
	files = dir(path = v)
	for(i in seq_along(files)) {
		f = files[i]
		nm = paste0(v, "/", f)
		pkg = gsub("_.*$", "", f)
		qqcat("- [@{i}/@{length(files)}] @{v}/@{f}...\n")
		oe = try(lt2[[nm]] <- readLines(archive_read(qq("@{v}/@{f}"), file = qq("@{pkg}/NAMESPACE"))))
		if(inherits(oe, "try-error")) {
			failed[[nm]] = TRUE
		}
	}
}


lt = c(lt1, lt2)

names(lt) = gsub("\\.tar.gz$", "", names(lt))

saveRDS(lt, file = qq("@{DATA_DIR}/all_packages_namespaces.rds"))


#####################################################
##### all packages' desc file

setwd("/Volumes/One Touch/cran_mirror/src/contrib/Archive")

### cran
lt1 = list()
all_pkgs = dir()

failed = list()
for(i in seq_along(all_pkgs)) {
	pkg = all_pkgs[i]
	files = dir(path = pkg)
	files = files[grep("gz$", files)]
	for(f in files) {
		qqcat("- [@{i}/@{length(all_pkgs)}] @{pkg}/@{f}...\n")
		pkg2 = gsub("_.*$", "", f)  # e.g. ARES -> ares
		oe = try(lt1[[f]] <- readLines(archive_read(qq("@{pkg}/@{f}"), file = qq("@{pkg2}/DESCRIPTION"))))
		if(inherits(oe, "try-error")) {
			failed[[f]] = TRUE
		}
	}
}


## bioconductor packages
setwd("/Volumes/One Touch/bioc_mirror/packages")

lt2 = list()
all_versions = dir()

for(v in all_versions) {
	files = dir(path = v)
	for(i in seq_along(files)) {
		f = files[i]
		nm = paste0(v, "/", f)
		pkg = gsub("_.*$", "", f)
		qqcat("- [@{i}/@{length(files)}] @{v}/@{f}...\n")
		oe = try(lt2[[nm]] <- readLines(archive_read(qq("@{v}/@{f}"), file = qq("@{pkg}/DESCRIPTION"))))
		if(inherits(oe, "try-error")) {
			failed[[nm]] = TRUE
		}
	}
}


lt = c(lt1, lt2)

names(lt) = gsub("\\.tar.gz$", "", names(lt))

saveRDS(lt, file = qq("@{DATA_DIR/all_packages_description.rds"))

