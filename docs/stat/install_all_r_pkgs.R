
.libPaths("/Volumes/Elements/all_pkg_lib/")

setRepositories(ind = 1:4)
chooseCRANmirror(ind = 1)


# get available package names
av <- names(available.packages()[,1])


packs <- installed.packages(lib.loc = "/Volumes/Elements/all_pkg_lib")
exc <- names(packs[,'Package'])

# create loooong string
ins <- av[!av %in% exc]
for(i in seq_len(floor(length(ins)/50))) {
	ind = 50*(i-1) + 1:50
	try(install.packages(ins[ind], type = "mac.binary", lib = "/Volumes/Elements/all_pkg_lib"))
}

for(x in av) {
	oe = try(library(x, character.only = TRUE))
	if(inherits(oe, "try-error")) {
		remove.packages(x)
	}
}
