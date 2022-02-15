

env = new.env()

BASE_PKGS = c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
        "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")

DEFAULT_LOADED_BASE_PKGS = c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods")

FIELDS = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")


bioc_version = read.dcf(file = system.file("DESCRIPTION", package = "BiocVersion"), fields = "Version")
bioc_version = gsub("^(\\d+\\.\\d+).*$", "\\1", bioc_version)
    

pkgndep_opt = setGlobalOptions(
    tmp_dir = ".",
    add_link = FALSE,
    bioc_version = bioc_version
)


env$db = list()
