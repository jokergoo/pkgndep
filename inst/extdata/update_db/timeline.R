setwd("~/project/development/pkgndep.github.io")



all_df = readRDS("~/workspace/R_evolution/all_packages_df.rds")
all_df = as.data.frame(all_df)
all_df$Date = as.Date(all_df$Date)
all_df$Repository = ifelse(all_df$BiocVersion == "", "CRAN", "Bioconductor")


bioc_pkgs = unique(all_df$Package[all_df$BiocVersion != ""])
cran_pkgs = unique(all_df$Package[all_df$BiocVersion == ""])

repo = tapply(all_df$Repository, all_df$Package, function(x) ifelse("Bioconductor" %in% x, "Bioconductor", "CRAN"))

lt_duration = readRDS("~/workspace/R_evolution/lt_duration.rds")

tb = data.frame(package = names(lt_duration), 
    start = sapply(lt_duration, function(x) x[1]), 
    end = sapply(lt_duration, function(x) x[2]))
tb$repo = repo[tb$package]


# jaccard_dist = function(x, y) {
#     if(x[2] < y[1]) {
#         0
#     } else if(y[2] < x[1]) {
#         0
#     } else if(x[1] <= y[1] & x[2] >= y[2]) {
#         (y[2] - y[1]+1)/(x[2] - x[1]+1)
#     } else if(y[1] <= x[1] & y[2] >= x[2]) {
#         (x[2] - x[1]+1)/(y[2] - y[1]+1)
#     } else if(x[1] <= y[1] & x[2] >= y[1] & x[2] <= y[2]) {
#         (x[2] - y[1]+1)/(y[2] - x[1]+1)
#     } else if(y[1] <= x[1] & y[2] >= x[1] & y[2] <= x[2]) {
#         (y[2] - x[1]+1)/(x[2] - y[1]+1)
#     } else {
#         NA
#     }
# }

library(Rcpp)
cppFunction('
NumericMatrix jdist(NumericVector x1, NumericVector x2) {

    int n = x1.size();
    NumericMatrix d(n, n);
    for(int i = 0; i < n - 1; i ++) {
        for(int j = i+1; j < n; j ++) {
            if(x2[i] < x1[j]) {
                d(i, j) = 0;
            } else if(x2[j] < x1[i]) {
                d(i, j) = 0;
            } else if(x1[i] <= x1[j] & x2[i] >= x2[j]) {
                d(i, j) = (x2[j] - x1[j]+1)/(x2[i] - x1[i]+1);
            } else if(x1[j] <= x1[i] & x2[j] >= x2[i]) {
                d(i, j) = (x2[i] - x1[i]+1)/(x2[j] - x1[j]+1);
            } else if(x1[i] <= x1[j] & x2[i] >= x1[j] & x2[i] <= x2[j]) {
                d(i, j) = (x2[i] - x1[j]+1)/(x2[j] - x1[i]+1);
            } else if(x1[j] <= x1[i] & x2[j] >= x1[i] & x2[j] <= x2[i]) {
                d(i, j) = (x2[j] - x1[i]+1)/(x2[i] - x1[j]+1);
            }
            d(j, i) = d(i, j);
        }
    }
    for(int i = 0; i < n; i ++) {
        d(i, i) = 0;
    }
    return d;
}')

l_cran = tb$repo == "CRAN"
l_bioc = tb$repo == "Bioconductor"

library(ComplexHeatmap)
tb2 = tb[l_cran, ]
d = 1 - jdist(tb2$start, tb2$end)
rownames(d) = colnames(d) = tb2$package
d = as.dist(d)
hc = hclust(d, method = "centroid")
dend = as.dendrogram(hc)
dend = reorder(dend, -(tb2$end - tb2$start))
od1 = order.dendrogram(dend)

tb2 = tb[l_bioc, ]
d = 1 - jdist(tb2$start, tb2$end)
rownames(d) = colnames(d) = tb2$package
d = as.dist(d)
hc = hclust(d, method = "centroid")
dend = as.dendrogram(hc)
dend = reorder(dend, -(tb2$end - tb2$start))
od2 = order.dendrogram(dend)

tb = rbind(tb[l_cran, ][od1, ], tb[l_bioc, ][od2, ])
saveRDS(tb, file = "timeline_tb.rds", compress = "xz")

library(grid)

png("~/project/development/pkgndep.github.io/image/timeline.png", width = 1200*2, height = 600*2, res = 72*2)
grid.newpage()
pushViewport(viewport(xscale = c(min(tb$start), max(tb$end)), yscale = c(0, nrow(tb)*1.1), width = 0.95, height = 0.95))
grid.segments(tb$start, 1:nrow(tb), tb$end, 1:nrow(tb), default.units = "native",
    gp = gpar(col = ifelse(tb$repo == "CRAN", "#FF000040", "#0000FF40")))

for(i in seq_len(nrow(ALL_BIOC_RELEASES))) {
    grid.lines(rep(as.Date(ALL_BIOC_RELEASES[i, "Date"]), 2), unit(c(0, 1), "npc"), default.units = "native", 
        gp = gpar(col = ifelse(i%%2, "#FF8080", "#8080FF"), lty = 2))
    grid.text(paste0(ALL_BIOC_RELEASES[i, "R"], "\n", ALL_BIOC_RELEASES[i, "Date"]), as.Date(ALL_BIOC_RELEASES[i, "Date"]), unit(1, "npc") - unit(8, "mm")*(i%%2), 
        default.units = "native", gp = gpar(col = ifelse(i%%2, "#FF0000", "#0000FF"), cex = 0.5), just = "top")
}
popViewport()

grid.text(qq("In total @{nrow(tb)} packages"), gp = gpar(fontsize = 14), x = unit(1, "cm"), y = unit(0.5, "npc") + unit(1.5, "cm"), just = "left")
lgd = Legend(title = "Repository", legend_gp = gpar(fill = c("#FF0000", "#0000FF")), at = c("CRAN", "Bioconductor"))
draw(lgd, x = unit(1, "cm"), y = unit(0.5, "npc"), just = "left")
dev.off()

##### removed
removed = readRDS("~/workspace/R_evolution/cran_removed.rds")
removed = lapply(removed, as.Date)
removed = data.frame(package = names(removed), date = do.call("c", removed))
removed$bioc_date = as.Date("2001-01-01")
for(i in seq_len(nrow(removed))) {
    if(all(as.Date(ALL_BIOC_RELEASES$Date) >= removed[i, 2])) {
        removed$bioc_date[i] = min(as.Date(ALL_BIOC_RELEASES$Date))
    } else if(all(as.Date(ALL_BIOC_RELEASES$Date) < removed[i, 2])) {
        removed$bioc_date[i] = NA
    } else {
        ind = which(removed[i, 2] - as.Date(ALL_BIOC_RELEASES$Date) > 0)[1] - 1
        if(!is.na(ind)) {
            if(ind > 0) {
                removed$bioc_date[i] = as.Date(ALL_BIOC_RELEASES$Date)[ind]
            }
        }
    }
}

tb = table(removed$bioc_date)
tb = data.frame(date = as.Date(names(tb)), n_pkgs = as.vector(tb), repo = "CRAN")

removed2 = numeric(0)
for(i in seq_len(nrow(ALL_BIOC_RELEASES)-1)) {
    bioc_version1 = ALL_BIOC_RELEASES$Release[i]
    bioc_version2 = ALL_BIOC_RELEASES$Release[i+1]

    pkgs = setdiff(all_df$Package[all_df$BiocVersion == bioc_version2], all_df$Package[all_df$BiocVersion == bioc_version1])
    removed2[ALL_BIOC_RELEASES$Date[i]] = length(pkgs)
}

tb2 = data.frame(date = as.Date(names(removed2)), n_pkgs = removed2, repo = "Bioconductor")
tb = rbind(tb, tb2)

png("~/project/development/pkgndep.github.io/image/package_removed.png", width = 1000*2, height = 400*2, res = 72*2)
ggplot(tb, aes(x = date, y = n_pkgs, xend = date, yend = 0, col = repo)) + geom_segment() + geom_point() +
    ggtitle("Number of removed packages between R versions") +
    scale_x_continuous(breaks = tb$date) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()

######

library(GetoptLong)

lt1 = list()
for(i in seq_len(nrow(ALL_BIOC_RELEASES))) {
    lt1[[ ALL_BIOC_RELEASES$Date[i] ]] = readRDS(qq("@{ALL_BIOC_RELEASES$Date[i]}/all_pkgs.rds"))
}

x = sapply(lt1, function(x) length(x))
n = length(lt1)
tb = data.frame(date = as.Date(names(x)),
    n_pkgs = x,
    n_pkgs_rate = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)
)

x = sapply(lt1, function(x) sum(sapply(x, function(y) y$repo == "CRAN")))
tb$n_pkgs_cran = x
tb$n_pkgs_rate_cran = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)

x = sapply(lt1, function(x) sum(sapply(x, function(y) y$repo == "Bioconductor")))
tb$n_pkgs_bioc = x
tb$n_pkgs_rate_bioc = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)

x = sapply(lt1, function(x) sum(sapply(x, function(y) sum(y$which_required))))
tb$n_deps = x
tb$n_deps_rate = n_pkgs_rate = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)

x = sapply(lt1, function(x) sum(sapply(x, function(y) ifelse(y$repo == "CRAN", sum(y$which_required), 0))))
tb$n_deps_cran = x
tb$n_deps_rate_cran = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)

x = sapply(lt1, function(x) sum(sapply(x, function(y) ifelse(y$repo == "Bioconductor", sum(y$which_required), 0))))
tb$n_deps_bioc = x
tb$n_deps_rate_bioc = c((x[1:(n-1)] - x[2:n])/x[2:n], NA)

tb$deps_per_pkg = tb$n_deps/tb$n_pkgs
tb$deps_per_pkg_rate = c((tb$deps_per_pkg[1:(n-1)] - tb$deps_per_pkg[2:n])/tb$deps_per_pkg[2:n], NA)

tb$deps_per_pkg_cran = tb$n_deps_cran/tb$n_pkgs_cran
tb$deps_per_pkg_rate_cran = c((tb$deps_per_pkg_cran[1:(n-1)] - tb$deps_per_pkg_cran[2:n])/tb$deps_per_pkg_cran[2:n], NA)

tb$deps_per_pkg_bioc = tb$n_deps_bioc/tb$n_pkgs_bioc
tb$deps_per_pkg_rate_bioc = c((tb$deps_per_pkg_bioc[1:(n-1)] - tb$deps_per_pkg_bioc[2:n])/tb$deps_per_pkg_bioc[2:n], NA)

png("~/project/development/pkgndep.github.io/image/timeline_number_of_pkgs.png", width = 1000*2, height = 400*2, res = 72*2)
tb2 = rbind({foo = tb[, c("date", "n_pkgs_cran")];colnames(foo) = c("date", "n_pkgs");foo$repo = "CRAN"; foo},
            {foo = tb[, c("date", "n_pkgs_bioc")];colnames(foo) = c("date", "n_pkgs");foo$repo = "Bioconductor"; foo})
ggplot(tb2, aes(x = date, y = n_pkgs, col = repo)) + geom_line() + geom_point() +
    ggtitle("Number of packages") +
    scale_x_continuous(breaks = tb$date) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()

png("~/project/development/pkgndep.github.io/image/timeline_number_of_deps.png", width = 1000*2, height = 400*2, res = 72*2)
tb2 = rbind({foo = tb[, c("date", "n_deps_cran")];colnames(foo) = c("date", "n_deps");foo$repo = "CRAN"; foo},
            {foo = tb[, c("date", "n_deps_bioc")];colnames(foo) = c("date", "n_deps");foo$repo = "Bioconductor"; foo})
ggplot(tb2, aes(x = date, y = n_deps, col = repo)) + geom_line() + geom_point() +
    ggtitle("Number of direct strong dependency relations") +
    scale_x_continuous(breaks = tb$date) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()

png("~/project/development/pkgndep.github.io/image/timeline_number_of_deps_per_pkg.png", width = 1000*2, height = 400*2, res = 72*2)
tb2 = rbind({foo = tb[, c("date", "deps_per_pkg_cran")];colnames(foo) = c("date", "deps_per_pkg");foo$repo = "CRAN"; foo},
            {foo = tb[, c("date", "deps_per_pkg_bioc")];colnames(foo) = c("date", "deps_per_pkg");foo$repo = "Bioconductor"; foo})
ggplot(tb2, aes(x = date, y = deps_per_pkg, col = repo)) + geom_line() + geom_point() +
    ggtitle("Average number of direct strong dependencies per packages") +
    scale_x_continuous(breaks = tb$date) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()



lt_history = readRDS("~/project/development/pkgndep.github.io/lt_history.rds")
lt_history_df = do.call(rbind, lt_history)

for(field in c("n_by_strong", "n_parents", "max_heaviness_from_parents", "adjusted_max_heaviness_from_parents", "n_children", "heaviness_on_children", "adjusted_heaviness_on_children", 
               "n_downstream", "heaviness_on_downstream", "adjusted_heaviness_on_downstream", "n_indirect_downstream", "heaviness_on_indirect_downstream", "adjusted_heaviness_on_indirect_downstream")) {

    png(qq("~/project/development/pkgndep.github.io/image/timeline_metric_@{field}.png"), width = 1000*2, height = 500*2, res = 72*2)

    m = max( sapply(lt_history, function(x) max(x[[field]], na.rm = T)) )

    if(field == "max_heaviness_from_parents") {
        par(mar = c(6, 4, 4, 8))
    } else {
        par(mar = c(6, 4, 4, 4))
    }
    rg = range(as.Date(ALL_BIOC_RELEASES$Date))
    rg[2] = rg[2]
    plot(x = as.Date(ALL_BIOC_RELEASES$Date), y = rep(0, nrow(ALL_BIOC_RELEASES)), 
        type = "n", xlim = rg, ylim = c(0, m),
        ylab = "values", axes = FALSE, xlab = "", main = field)
    axis(side = 1, at = as.Date(ALL_BIOC_RELEASES$Date), labels = ALL_BIOC_RELEASES$Date, las = 3)
    axis(side = 2)
    x = NULL
    y = NULL
    for(i in seq_along(lt_history)) {
        cat(strrep("\b", 100))
        qqcat("@{i}/@{length(lt_history)}...")
        x = c(x, as.Date(lt_history[[i]]$date), NA)
        y = c(y, lt_history[[i]][[field]], NA)
    }
    cat("\n")

    lines(x, y, col = "#00000020")

    mean = tapply(lt_history_df[[field]], lt_history_df$date, mean)
    lines(as.Date(names(mean)), mean, col = "orange")

    legend("topleft", legend = c("Mean trend", "Top packages"), lty = 1, col = c("orange", "red"))

    od = order(sapply(lt_history, function(x) {
        l = as.Date(x$date) >= as.Date(ALL_BIOC_RELEASES$Date[4])
        if(sum(l) < 2) {
            0
        } else {
            mean(x[[field]][l])
        }
    }), decreasing = TRUE)
    y0 = NULL
    x0 = NULL
    labels = NULL
    for(i in od[1:20]) {
        lines(as.Date(lt_history[[i]]$date), lt_history[[i]][[field]], col = "#FF000080")
        y0 = c(y0, lt_history[[i]][[field]][1])
        x0 = c(x0, as.Date(lt_history[[i]]$date[1]))
        labels = c(labels, names(lt_history)[i])
    }

    oddd = order(y0)
    y0 = y0[oddd]
    x0 = x0[oddd]
    labels = labels[oddd]

    library(circlize)
    th = strheight("foo", cex = 0.7)
    pos = smartAlign2(y0-th*1.5/2, y0 + th*1.5/2, range = c(0, m*1.1))

    par(xpd = NA)
    text(x0+200, rowMeans(pos), labels, adj = 0, cex = 0.7)

    segments(x0, y0, x0 + 200, rowMeans(pos), lty = 3, col = "#0000FF80")

    dev.off()

}


