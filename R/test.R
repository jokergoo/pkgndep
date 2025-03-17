available.packages = function (contriburl = contrib.url(repos, type), method, fields = NULL,
    type = getOption("pkgType"), filters = NULL, repos = getOption("repos"),
    ignore_repo_cache = FALSE, max_repo_cache_age, quiet = TRUE,
    ...)
{
    browser()
    if (!is.character(type))
        stop("invalid 'type'; must be a character string")
    requiredFields <- c(tools:::.get_standard_repository_db_fields(),
        "File")
    if (is.null(fields))
        fields <- requiredFields
    else {
        stopifnot(is.character(fields))
        fields <- unique(c(requiredFields, fields))
    }
    if (missing(max_repo_cache_age))
        max_repo_cache_age <- as.numeric(Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE",
            "3600"))
    timestamp <- Sys.time()
    res <- matrix(NA_character_, 0L, length(fields) + 1L, dimnames = list(NULL,
        c(fields, "Repository")))
    for (repos in contriburl) {
        localcran <- startsWith(repos, "file:")
        if (localcran) {
            if (startsWith(repos, "file:///")) {
                tmpf <- paste0(substring(repos, 8L), "/PACKAGES")
                if (.Platform$OS.type == "windows") {
                  if (length(grep("^/[A-Za-z]:", tmpf)))
                    tmpf <- substring(tmpf, 2L)
                }
            }
            else {
                tmpf <- paste0(substring(repos, 6L), "/PACKAGES")
            }
            res0 <- if (file.exists(dest <- paste0(tmpf, ".rds")))
                readRDS(dest)
            else read.dcf(file = tmpf)
            if (length(res0))
                rownames(res0) <- res0[, "Package"]
        }
        else {
            used_dest <- FALSE
            if (ignore_repo_cache) {
                dest <- tempfile()
            }
            else {
                dest <- file.path(tempdir(), paste0("repos_",
                  URLencode(repos, TRUE), ".rds"))
                if (file.exists(dest)) {
                  age <- difftime(timestamp, file.mtime(dest),
                    units = "secs")
                  if (isTRUE(age < max_repo_cache_age)) {
                    res0 <- readRDS(dest)
                    used_dest <- TRUE
                    if (length(res0))
                      rownames(res0) <- res0[, "Package"]
                  }
                  else unlink(dest)
                }
            }
            if (!used_dest) {
                need_dest <- FALSE
                op <- options(warn = -1L)
                z <- tryCatch({
                  download.file(url = paste0(repos, "/PACKAGES.rds"),
                    destfile = dest, method = method, cacheOK = FALSE,
                    quiet = quiet, mode = "wb", ...)
                }, error = identity)
                options(op)
                if (!inherits(z, "error")) {
                  z <- res0 <- tryCatch(readRDS(dest), error = identity)
                  if (ignore_repo_cache)
                    unlink(dest)
                }
                if (inherits(z, "error")) {
                  if (!ignore_repo_cache)
                    need_dest <- TRUE
                  tmpf <- tempfile()
                  on.exit(unlink(tmpf))
                  op <- options(warn = -1L)
                  z <- tryCatch({
                    download.file(url = paste0(repos, "/PACKAGES.gz"),
                      destfile = tmpf, method = method, cacheOK = FALSE,
                      quiet = quiet, mode = "wb", ...)
                  }, error = identity)
                  if (inherits(z, "error"))
                    z <- tryCatch({
                      download.file(url = paste0(repos, "/PACKAGES"),
                        destfile = tmpf, method = method, cacheOK = FALSE,
                        quiet = quiet, mode = "wb", ...)
                    }, error = identity)
                  options(op)
                  if (!inherits(z, "error"))
                    z <- res0 <- tryCatch(read.dcf(file = tmpf),
                      error = identity)
                  unlink(tmpf)
                  on.exit()
                }
                if (inherits(z, "error")) {
                  warning(gettextf("unable to access index for repository %s",
                    repos), ":\n  ", conditionMessage(z), call. = FALSE,
                    immediate. = TRUE, domain = NA)
                  unlink(dest)
                  next
                }
                if (length(res0)) {
                  rownames(res0) <- res0[, "Package"]
                  if (need_dest)
                    saveRDS(res0, dest, compress = TRUE)
                }
                else if (!need_dest) {
                  unlink(dest)
                }
            }
        }
        if (length(res0)) {
            missingFields <- fields[!(fields %in% colnames(res0))]
            if (length(missingFields)) {
                toadd <- matrix(NA_character_, nrow = nrow(res0),
                  ncol = length(missingFields), dimnames = list(NULL,
                    missingFields))
                res0 <- cbind(res0, toadd)
            }
            if ("Path" %in% colnames(res0)) {
                rp <- rep.int(repos, nrow(res0))
                path <- res0[, "Path"]
                rp[!is.na(path)] <- paste(repos, path[!is.na(path)],
                  sep = "/")
            }
            else rp <- repos
            res0 <- cbind(res0[, fields, drop = FALSE], Repository = rp)
            res <- rbind(res, res0, deparse.level = 0L)
        }
    }
    if (!length(res))
        return(res)
    if (is.null(filters)) {
        filters <- getOption("available_packages_filters")
        if (is.null(filters))
            filters <- available_packages_filters_default
    }
    if (is.list(filters)) {
        if (isTRUE(filters$add)) {
            filters$add <- NULL
            filters <- c(available_packages_filters_default,
                filters)
        }
    }
    for (f in filters) {
        if (!length(res))
            break
        if (is.character(f)) {
            f <- available_packages_filters_db[[f[1L]]]
        }
        if (!is.function(f))
            stop("invalid 'filters' argument.")
        res <- f(res)
    }
    res
}