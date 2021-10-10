setRepositories(ind = 1:4)
chooseCRANmirror(ind = 1)

packs <- installed.packages()
exc <- names(packs[,'Package'])

# get available package names
av <- names(available.packages()[,1])

# create loooong string
ins <- av[!av %in% exc]
for(i in seq_len(floor(length(ins)/50))) {
	ind = 50*(i-1) + 1:50
	try(install.packages(ins[ind], lib = "C:/Users/guz/Documents/R/win-library/4.1"))
}

