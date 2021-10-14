

env = new.env()
env$loaded_ns = list()

.onAttach = function(libname, pkgname) {
    env$loaded_ns = list()
}


BASE_PKGS = c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
        "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
