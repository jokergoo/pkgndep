

library(GetoptLong)

lt_all = list()
for(date in ALL_BIOC_RELEASES$Date) {
	lt_all[[date]] = readRDS(qq("~/project/development/pkgndep.github.io/@{date}/pkg_stat_snapshot.rds"))
}

all_pkgs = unique(unlist(lapply(lt_all, function(x) x$package)))

lt_history = vector("list", length(all_pkgs))
names(lt_history) = all_pkgs

fields = c("package", "version", "n_by_strong", "n_by_all", "n_parents", "max_heaviness_from_parents", "adjusted_max_heaviness_from_parents", "n_children",
	"heaviness_on_children", "adjusted_heaviness_on_children", "n_downstream", "heaviness_on_downstream", "adjusted_heaviness_on_downstream", 
	"n_indirect_downstream", "heaviness_on_indirect_downstream", "adjusted_heaviness_on_indirect_downstream")

for(date in names(lt_all)) {
	qqcat("- @{date}...\n")
	df = lt_all[[date]][, fields]
	df$date = date
	
	for(i in seq_len(nrow(df))) {
		lt_history[[ df$package[i] ]] = rbind(lt_history[[ df$package[i] ]], df[i, , drop = FALSE])
	}
}

saveRDS(lt_history, file = "~/project/development/pkgndep.github.io/lt_history.rds", compress = "xz")
