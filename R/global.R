

env = new.env()

# BASE_PKGS = c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
#         "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")


DEFAULT_LOADED_BASE_PKGS = c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods")

FIELDS = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")


if(grepl("devel", R.version$status)) {  # only for CRAN check on R-devel where BiocManager::version() throws error 
    bioc_version = packageVersion("BiocVersion")[, 1:2]
    bioc_version = as.character(bioc_version)
} else {
    bioc_version = BiocManager::version()
    bioc_version = as.character(bioc_version)
}   


# == title
# Global parameters for pkgndep
#
# == param
# -... Arguments for the parameters, see "details" section
# -RESET Reset to default values.
# -READ.ONLY Please ignore.
# -LOCAL Pllease ignore.
# -ADD Please ignore.
# 
# == details
# There are following parameters:
# 
# -``bioc_version`` The bioconductor version. By default it is the version corresponding to the R version under use.
#
# == example
# pkgndep_opt
pkgndep_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}

pkgndep_opt = setGlobalOptions(
    tmp_dir = list(.value = ".", .visible = FALSE),
    add_link = list(.value = FALSE, .visible = FALSE),
    bioc_version = bioc_version
)


env$db = list()
