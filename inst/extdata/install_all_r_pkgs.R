
### al the packages should be installed because it will be easier to process NAMESPACE data

.libPaths("/Volumes/Elements/all_pkg_lib/")

options(repos = BiocManager::repositories(version = "3.15"))

options(timeout = 10000000)

# get available package names
av_df <- available.packages(repos = BiocManager::repositories(version = "3.15"))

av = av_df[, 1]

exc = dir("/Volumes/Elements/all_pkg_lib")

ins <- av[!av %in% exc]

for(i in seq_len(floor(length(ins)/50))) {
	ind = 50*(i-1) + 1:50
	ind = intersect(ind, 1:length(ins))
	try(BiocManager::install(ins[ind], 
		type = "source", dependency =TRUE,
		lib = "/Volumes/Elements/all_pkg_lib"))
}


tb1 = available.packages(repos = BiocManager::repositories(version = "3.15"))
tb2 = installed.packages(lib.loc = "/Volumes/Elements/all_pkg_lib/")

cn = intersect(tb1[, "Package"], tb2[, "Package"])

l = which(tb1[cn, "Version"] != tb2[cn, "Version"])

ins = cn[l]


