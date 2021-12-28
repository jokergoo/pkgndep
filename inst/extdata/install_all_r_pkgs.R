
.libPaths("/Volumes/Elements/all_pkg_lib/")

options(repos = BiocManager::repositories())

options(timeout = 100000)

# get available package names
av_df <- available.packages(repos = BiocManager::repositories())

av = av_df[, 1]

exc = dir("/Volumes/Elements/all_pkg_lib")

ins <- av[!av %in% exc]

for(i in seq_len(floor(length(ins)/50))) {
	ind = 50*(i-1) + 1:50
	ind = intersect(ind, 1:length(ins))
	try(install.packages(ins[ind], 
		type = "source", dependencies = FALSE,
		lib = "/Volumes/Elements/all_pkg_lib"))
}


tb1 = available.packages(repos = BiocManager::repositories())
tb2 = installed.packages(lib.loc = "/Volumes/Elements/all_pkg_lib/")

cn = intersect(tb1[, "Package"], tb2[, "Package"])

l = which(tb1[cn, "Version"] != tb2[cn, "Version"])

ins = cn[l]

